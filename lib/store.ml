open Core
module Db = Rel_sqlite3

type t = Db.t

type ingest =
  | Inserted of int64
  | Existing of int64

let ( let* ) x f = Result.bind x ~f

let db_result result =
  Result.map_error result ~f:(fun e -> Error.of_string (Db.Error.message e))
;;

let transaction db f =
  match Db.with_transaction `Immediate db f with
  | Ok result -> result
  | Error e -> Error (Error.of_string (Db.Error.message e))
;;

let statement sql result = Rel_sql.Stmt.v sql ~rev_args:[] ~result
let integer = Rel.Row.(t1 (int "value"))

let migrate db =
  transaction db (fun db ->
    let* version = db_result (Db.first db (statement "PRAGMA user_version" integer)) in
    match version with
    | Some 0 -> db_result (Db.exec_sql db Db_schema.create)
    | Some 1 -> db_result (Db.exec_sql db Db_schema.upgrade_v1)
    | Some 2 -> Ok ()
    | _ -> Or_error.error_string "unsupported database schema version")
;;

let with_open path f =
  let* db = db_result (Db.open' ~foreign_keys:true ~mode:Read_write_create path) in
  let result =
    try
      let* () = db_result (Db.busy_timeout_ms db 5000) in
      let* () = migrate db in
      f db
    with
    | exn ->
      ignore (Db.close db);
      raise exn
  in
  let closed = db_result (Db.close db) in
  match result with
  | Error _ -> result
  | Ok _ ->
    let* () = closed in
    result
;;

let source_row =
  Rel.Row.(
    unit (fun id venue account kind source_id occurred_at_ms occurred_on payload ->
      id, venue, account, kind, source_id, occurred_at_ms, occurred_on, payload)
    * int64 "id"
    * text "venue"
    * text "account"
    * text "kind"
    * text "source_id"
    * col "occurred_at_ms" Rel.Type.(option int64)
    * col "occurred_on" Rel.Type.(option text)
    * blob "payload")
;;

let find_source =
  Rel_sql.Stmt.(
    func
      "SELECT id,venue,account,kind,source_id,occurred_at_ms,occurred_on,payload FROM \
       source_events WHERE venue=? AND account=? AND kind=? AND source_id=?"
      (text @-> text @-> text @-> text @-> ret source_row))
;;

let insert_source =
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       source_events(venue,account,kind,source_id,occurred_at_ms,occurred_on,payload) \
       VALUES (?,?,?,?,?,?,?)"
      (text @-> text @-> text @-> text @-> option int64 @-> option text @-> blob @-> unit))
;;

let occurrence_columns (e : Source_event.t) =
  match e.occurred with
  | At_ms ms -> Some ms, None
  | On_date date -> None, Some (Date.to_string date)
;;

let ingest_in_transaction db (e : Source_event.t) =
  let at, on = occurrence_columns e in
  let* existing =
    db_result (Db.first db (find_source e.venue e.account e.kind e.source_id))
  in
  match existing with
  | Some (id, _, _, _, _, previous_at, previous_on, payload) ->
    if
      Option.equal Int64.equal at previous_at
      && Option.equal String.equal on previous_on
      && String.equal payload e.payload
    then Ok (Existing id)
    else
      Or_error.error_string
        "source identity already exists with different timestamp/date or payload"
  | None ->
    let* () =
      db_result
        (Db.exec db (insert_source e.venue e.account e.kind e.source_id at on e.payload))
    in
    Ok (Inserted (Db.last_insert_rowid db))
;;

let ingest db e = transaction db (fun db -> ingest_in_transaction db e)

let collect db stmt =
  let* rows = db_result (Db.fold db stmt (fun row rows -> row :: rows) []) in
  Ok (List.rev rows)
;;

let traverse f xs = Or_error.combine_errors (List.map xs ~f)

type bank_import =
  { document_id : int64
  ; statements : int
  ; inserted : int
  ; existing : int
  }

let find_document =
  Rel_sql.Stmt.(
    func
      "SELECT id,contents FROM import_documents WHERE institution=? AND sha256=?"
      (text @-> text @-> ret Rel.Row.(t2 (int64 "id") (blob "contents"))))
;;

let insert_document =
  Rel_sql.Stmt.(
    func
      "INSERT INTO import_documents(institution,sha256,contents) VALUES (?,?,?)"
      (text @-> text @-> blob @-> unit))
;;

let link_document =
  Rel_sql.Stmt.(
    func
      "INSERT INTO import_document_events(document_id,event_id) VALUES (?,?) ON CONFLICT \
       DO NOTHING"
      (int64 @-> int64 @-> unit))
;;

let import_ofx db ~institution (document : Bank_ofx.t) =
  let* institution = Validation.text "institution" institution in
  let sha256 = Digestif.SHA256.(to_hex (digest_string document.contents)) in
  transaction db (fun db ->
    let* archived = db_result (Db.first db (find_document institution sha256)) in
    let* document_id =
      match archived with
      | Some (id, contents) ->
        if String.equal contents document.contents
        then Ok id
        else Or_error.error_string "archive checksum collision or corrupt document"
      | None ->
        let* () =
          db_result (Db.exec db (insert_document institution sha256 document.contents))
        in
        Ok (Db.last_insert_rowid db)
    in
    let rec import_transactions inserted existing = function
      | [] ->
        Ok
          { document_id
          ; statements = List.length document.statements
          ; inserted
          ; existing
          }
      | (account, (trn : Bank_ofx.transaction)) :: rest ->
        let* event =
          Source_event.create_dated
            ~venue:institution
            ~account:(Bank_ofx.account_identity account)
            ~kind:"bank-transaction"
            ~source_id:trn.fitid
            ~occurred_on:trn.booked_on
            ~payload:trn.source_payload
        in
        let* result = ingest_in_transaction db event in
        let event_id, inserted, existing =
          match result with
          | Inserted id -> id, inserted + 1, existing
          | Existing id -> id, inserted, existing + 1
        in
        let* () = db_result (Db.exec db (link_document document_id event_id)) in
        import_transactions inserted existing rest
    in
    let transactions =
      List.concat_map document.statements ~f:(fun statement ->
        List.map statement.transactions ~f:(fun trn -> statement.account, trn))
    in
    import_transactions 0 0 transactions)
;;

let events db =
  let* rows =
    collect
      db
      (statement
         "SELECT id,venue,account,kind,source_id,occurred_at_ms,occurred_on,payload FROM \
          source_events ORDER BY coalesce(occurred_on, date(occurred_at_ms / 1000, \
          'unixepoch')),occurred_at_ms,id"
         source_row)
  in
  traverse
    (fun (id, venue, account, kind, source_id, at, on, payload) ->
       let* e =
         match at, on with
         | Some occurred_at_ms, None ->
           Source_event.create ~venue ~account ~kind ~source_id ~occurred_at_ms ~payload
         | None, Some date ->
           let* occurred_on = Or_error.try_with (fun () -> Date.of_string date) in
           Source_event.create_dated
             ~venue
             ~account
             ~kind
             ~source_id
             ~occurred_on
             ~payload
         | _ -> Or_error.error_string "invalid source event occurrence"
       in
       Ok (id, e))
    rows
;;

let posting_row = Rel.Row.(t3 (text "account") (text "commodity") (text "quantity"))

let select_postings =
  Rel_sql.Stmt.(
    func
      "SELECT account,commodity,quantity FROM postings WHERE entry_id=? ORDER BY ordinal"
      (int64 @-> ret posting_row))
;;

let read_entry db id date description =
  let* rows = collect db (select_postings id) in
  let* postings =
    traverse
      (fun (account, commodity, text) ->
         let* quantity = Or_error.try_with (fun () -> Bigdecimal.of_string text) in
         Accounting.posting ~account ~commodity quantity)
      rows
  in
  let* date = Or_error.try_with (fun () -> Date.of_string date) in
  Accounting.entry ~date ~description postings
;;

let find_entry =
  Rel_sql.Stmt.(
    func
      "SELECT id,date,description FROM journal_entries WHERE event_id=? AND \
       projection_key=?"
      (int64 @-> text @-> ret Rel.Row.(t3 (int64 "id") (text "date") (text "description"))))
;;

let insert_entry =
  Rel_sql.Stmt.(
    func
      "INSERT INTO journal_entries(event_id,projection_key,date,description) VALUES \
       (?,?,?,?)"
      (int64 @-> text @-> text @-> text @-> unit))
;;

let insert_posting =
  Rel_sql.Stmt.(
    func
      "INSERT INTO postings(entry_id,ordinal,account,commodity,quantity) VALUES \
       (?,?,?,?,?)"
      (int64 @-> int @-> text @-> text @-> text @-> unit))
;;

let same_entry (a : Accounting.entry) (b : Accounting.entry) =
  Date.equal a.date b.date
  && String.equal a.description b.description
  && List.length a.postings = List.length b.postings
  && List.for_all2_exn
       a.postings
       b.postings
       ~f:(fun (a : Accounting.posting) (b : Accounting.posting) ->
         String.equal a.account b.account
         && String.equal a.commodity b.commodity
         && Bigdecimal.equal a.quantity b.quantity)
;;

let record_entry_in_transaction db ~event_id ~key (entry : Accounting.entry) =
  let* key = Validation.text "projection key" key in
  let* existing = db_result (Db.first db (find_entry event_id key)) in
  match existing with
  | Some (id, date, description) ->
    let* previous = read_entry db id date description in
    if same_entry previous entry
    then Ok (Existing id)
    else
      Or_error.error_string
        "projection key already exists with different accounting entries"
  | None ->
    let* () =
      db_result
        (Db.exec
           db
           (insert_entry event_id key (Date.to_string entry.date) entry.description))
    in
    let id = Db.last_insert_rowid db in
    let rec write ordinal = function
      | [] -> Ok (Inserted id)
      | (p : Accounting.posting) :: rest ->
        let* () =
          db_result
            (Db.exec
               db
               (insert_posting
                  id
                  ordinal
                  p.account
                  p.commodity
                  (Bigdecimal.to_string_no_sn p.quantity)))
        in
        write (ordinal + 1) rest
    in
    write 0 entry.postings
;;

let record_entry db ~event_id ~key entry =
  transaction db (fun db -> record_entry_in_transaction db ~event_id ~key entry)
;;

let entries db =
  let row =
    Rel.Row.(
      t5
        (int64 "id")
        (int64 "event_id")
        (text "projection_key")
        (text "date")
        (text "description"))
  in
  let* rows =
    collect
      db
      (statement
         "SELECT id,event_id,projection_key,date,description FROM journal_entries ORDER \
          BY date,event_id,projection_key"
         row)
  in
  traverse
    (fun (id, event_id, key, date, description) ->
       let* entry = read_entry db id date description in
       Ok (event_id, key, entry))
    rows
;;

type projection_report =
  { inserted : int
  ; existing : int
  }

let project_bank db =
  transaction db (fun db ->
    let* source_events = events db in
    let rec project inserted existing = function
      | [] -> Ok { inserted; existing }
      | (event_id, (event : Source_event.t)) :: rest ->
        if not (String.equal event.kind "bank-transaction")
        then project inserted existing rest
        else
          let* entry = Bank_projection.entry event in
          let* result =
            record_entry_in_transaction db ~event_id ~key:Bank_projection.key entry
          in
          (match result with
           | Inserted _ -> project (inserted + 1) existing rest
           | Existing _ -> project inserted (existing + 1) rest)
    in
    project 0 0 source_events)
;;
