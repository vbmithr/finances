open Base
open Rel
module Db = Rel_sqlite3

type t =
  { conn : Db.t
  ; registry : Registry.t
  }

type ingest =
  | Inserted of int64
  | Existing of int64

let ( let* ) x f = Result.bind x ~f

let db_result result =
  Result.map_error result ~f:(fun e -> Error.of_string (Db.Error.message e))
;;

let transaction db f =
  match Db.with_transaction `Immediate db.conn (fun _ -> f db) with
  | Ok result -> result
  | Error e -> Error (Error.of_string (Db.Error.message e))
;;

let statement sql result = Rel_sql.Stmt.v sql ~rev_args:[] ~result
let integer = Row.(t1 (int "value"))

let initialize db =
  transaction db (fun db ->
    let* version =
      db_result (Db.first db.conn (statement "PRAGMA user_version" integer))
    in
    match version with
    | Some 0 -> db_result (Db.exec_sql db.conn Db_schema.create)
    | Some version when version = Db_schema.version -> Ok ()
    | _ ->
      Or_error.errorf
        "unsupported database schema; this experimental build accepts only schema %d. \
         Back it up with fin backup if you need it, then run fin delete and fin init, \
         and sync your statement folders again"
        Db_schema.version)
;;

let with_open ~registry path f =
  let* conn = db_result (Db.open' ~foreign_keys:true ~mode:Read_write_create path) in
  let db = { conn; registry } in
  let result =
    try
      let* () = db_result (Db.busy_timeout_ms conn 5000) in
      let* () = initialize db in
      f db
    with
    | exn ->
      ignore (Db.close conn);
      raise exn
  in
  let closed = db_result (Db.close conn) in
  match result with
  | Error _ -> result
  | Ok _ ->
    let* () = closed in
    result
;;

let backup db path =
  if Stdlib.Sys.file_exists path
  then Or_error.errorf "%s already exists" path
  else (
    let quoted = "'" ^ String.substr_replace_all path ~pattern:"'" ~with_:"''" ^ "'" in
    db_result (Db.exec_sql db.conn ("VACUUM INTO " ^ quoted)))
;;

(* Read-only result projections share their column types with the table model. *)
let read_column col = Col.make (Col.name col) (Col.type' col) Col.no_proj

(* Rel_query.Sql builds curried arguments inside out: its predicate receives
   parameters in reverse order. Keep public lookup arguments in key order. *)
let select_by_pair table first second value =
  let open Rel_query.Syntax in
  let select =
    Rel_query.Sql.(
      func
        (Col.type' first
         @-> Col.type' second
         @-> ret
               Row.(t1 (read_column value))
               (fun b a ->
                  let* row = Bag.table table in
                  Bag.where
                    (Coded.equal (Col.type' first) row#.first a
                     && Coded.equal (Col.type' second) row#.second b)
                    (Bag.yield row#.value))))
  in
  select
;;

let select_by_column table key value =
  let open Rel_query.Syntax in
  let select =
    Rel_query.Sql.(
      func
        (Col.type' key
         @-> ret
               Row.(t1 (read_column value))
               (fun x ->
                  let* row = Bag.table table in
                  Bag.where
                    (Coded.equal (Col.type' key) row#.key x)
                    (Bag.yield row#.value))))
  in
  select
;;

let source_row = Db_schema.Source_events.row

let find_source =
  let open Db_schema.Source_events in
  let open Rel_query.Syntax in
  let select =
    Rel_query.Sql.(
      func
        (text
         @-> text
         @-> text
         @-> text
         @-> ret row (fun v_source_id v_kind v_account v_venue ->
           let* r = Bag.table table in
           Bag.where
             (Text.(r#.venue = v_venue)
              && Text.(r#.account = v_account)
              && Text.(r#.kind = v_kind)
              && Text.(r#.source_id = v_source_id))
             (Bag.yield r))))
  in
  select
;;

let insert_source =
  let open Db_schema.Source_events in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_venue v_account v_kind v_source_id v_occurred_at_ms v_occurred_on v_payload ->
    insert
      Col.
        [ Value (venue, v_venue)
        ; Value (account, v_account)
        ; Value (kind, v_kind)
        ; Value (source_id, v_source_id)
        ; Value (occurred_at_ms, v_occurred_at_ms)
        ; Value (occurred_on, v_occurred_on)
        ; Value (payload, v_payload)
        ]
;;

let occurrence_columns (e : Source_event.t) =
  match e.occurred with
  | At_ms ms -> Some ms, None
  | On_date date -> None, Some (Date.to_string date)
;;

let ingest_in_transaction db (e : Source_event.t) =
  let at, on = occurrence_columns e in
  let* existing =
    db_result (Db.first db.conn (find_source e.venue e.account e.kind e.source_id))
  in
  match existing with
  | Some previous ->
    if
      Option.equal Int64.equal at previous.occurred_at_ms
      && Option.equal String.equal on previous.occurred_on
      && String.equal previous.payload e.payload
    then Ok (Existing previous.id)
    else
      Or_error.error_string
        "source identity already exists with different timestamp/date or payload"
  | None ->
    let* () =
      db_result
        (Db.exec
           db.conn
           (insert_source e.venue e.account e.kind e.source_id at on e.payload))
    in
    Ok (Inserted (Db.last_insert_rowid db.conn))
;;

let ingest db e = transaction db (fun db -> ingest_in_transaction db e)

let collect db stmt =
  let* rows = db_result (Db.fold db.conn stmt (fun row rows -> row :: rows) []) in
  Ok (List.rev rows)
;;

let traverse f xs = Or_error.combine_errors (List.map xs ~f)

let find_document =
  let open Db_schema.Import_documents in
  let result = Row.(t2 (read_column id) (read_column contents)) in
  let open Rel_query.Syntax in
  let select =
    Rel_query.Sql.(
      func
        (text
         @-> text
         @-> ret result (fun hash i ->
           let* r = Bag.table table in
           Bag.where
             (Text.(r#.institution = i) && Text.(r#.sha256 = hash))
             (Bag.yield (Bag.row (fun id contents -> id, contents) $ r#.id $ r#.contents)))
        ))
  in
  select
;;

let insert_document =
  let open Db_schema.Import_documents in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_institution v_sha256 v_contents ->
    insert
      Col.
        [ Value (institution, v_institution)
        ; Value (sha256, v_sha256)
        ; Value (contents, v_contents)
        ]
;;

let link_document =
  Rel_sql.Stmt.(
    func
      "INSERT INTO import_document_events(document_id,event_id) VALUES (?,?) ON CONFLICT \
       DO NOTHING"
      (int64 @-> int64 @-> unit))
;;

let archive_document db ~institution ~contents =
  let sha256 = Digestif.SHA256.(to_hex (digest_string contents)) in
  let* archived = db_result (Db.first db.conn (find_document institution sha256)) in
  match archived with
  | Some (id, previous) ->
    if String.equal previous contents
    then Ok id
    else Or_error.error_string "archive checksum collision or corrupt document"
  | None ->
    let* () = db_result (Db.exec db.conn (insert_document institution sha256 contents)) in
    Ok (Db.last_insert_rowid db.conn)
;;

let attempt_row =
  Row.(
    unit
      (fun
          id
           institution
           path
           format
           outcome
           document_id
           sha256
           inserted
           existing
           message
           attempted_at
         ->
         ( id
         , institution
         , path
         , format
         , outcome
         , document_id
         , sha256
         , inserted
         , existing
         , message
         , attempted_at ))
    * read_column Db_schema.Bank_import_attempts.id
    * read_column Db_schema.Bank_import_attempts.institution
    * read_column Db_schema.Bank_import_attempts.path
    * read_column Db_schema.Bank_import_attempts.format
    * read_column Db_schema.Bank_import_attempts.outcome
    * read_column Db_schema.Bank_import_attempts.document_id
    * col "sha256" Type.(option text)
    * read_column Db_schema.Bank_import_attempts.inserted_count
    * read_column Db_schema.Bank_import_attempts.existing_count
    * read_column Db_schema.Bank_import_attempts.error
    * read_column Db_schema.Bank_import_attempts.attempted_at)
;;

let attempt_select =
  "SELECT \
   a.id,a.institution,a.path,a.format,a.outcome,a.document_id,d.sha256,a.inserted_count,a.existing_count,a.error,a.attempted_at \
   FROM bank_import_attempts a LEFT JOIN import_documents d ON d.id=a.document_id"
;;

let decode_attempt
      ( attempt_id
      , institution
      , path
      , format
      , outcome
      , document_id
      , sha256
      , inserted
      , existing
      , message
      , attempted_at )
  =
  let* outcome = Bank_import.outcome_of_string outcome in
  Ok
    { Bank_import.attempt_id
    ; institution
    ; path
    ; format
    ; outcome
    ; document_id
    ; sha256
    ; inserted
    ; existing
    ; message
    ; attempted_at
    }
;;

let insert_attempt =
  let open Db_schema.Bank_import_attempts in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_institution
    v_path
    v_document_id
    v_format
    v_outcome
    v_inserted_count
    v_existing_count
    v_error ->
    insert
      Col.
        [ Value (institution, v_institution)
        ; Value (path, v_path)
        ; Value (document_id, v_document_id)
        ; Value (format, v_format)
        ; Value (outcome, v_outcome)
        ; Value (inserted_count, v_inserted_count)
        ; Value (existing_count, v_existing_count)
        ; Value (error, v_error)
        ]
;;

let read_attempt =
  Rel_sql.Stmt.(func (attempt_select ^ " WHERE a.id=?") (int64 @-> ret attempt_row))
;;

let record_attempt
      db
      ~institution
      ~path
      ~document_id
      ~format
      ~outcome
      ~inserted
      ~existing
      ~message
  =
  let* () =
    db_result
      (Db.exec
         db.conn
         (insert_attempt
            institution
            path
            document_id
            format
            (Bank_import.outcome_name outcome)
            inserted
            existing
            message))
  in
  let* row = db_result (Db.first db.conn (read_attempt (Db.last_insert_rowid db.conn))) in
  match row with
  | Some row -> decode_attempt row
  | None -> Or_error.error_string "import attempt was not recorded"
;;

let accepted_document =
  Rel_sql.Stmt.(
    func
      "SELECT EXISTS(SELECT 1 FROM import_document_events WHERE document_id=?) OR \
       EXISTS(SELECT 1 FROM bank_import_attempts WHERE document_id=? AND outcome IN \
       ('imported','unchanged'))"
      (int64 @-> int64 @-> ret Row.(t1 (bool "accepted"))))
;;

(* Companion observations keep their raw fields; the adapter's parser rebuilds
   typed rows when matching. Replaying a document must find identical fields. *)
let encode_fields fields =
  Yojson.Safe.to_string (`List (List.map fields ~f:(fun s -> `String s)))
;;

let decode_fields text =
  Or_error.try_with (fun () ->
    match Yojson.Safe.from_string text with
    | `List fields ->
      List.map fields ~f:(function
        | `String s -> s
        | _ -> failwith "invalid companion field")
    | _ -> failwith "invalid stored companion row")
;;

let store_companion_rows db ~document_id ~institution ~format rows =
  let find =
    let open Db_schema.Companion_rows in
    select_by_pair table document_id row_number fields_json
  in
  let insert =
    let open Db_schema.Companion_rows in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    fun v_document_id v_row_number v_institution v_format v_fields ->
      insert
        Col.
          [ Value (document_id, v_document_id)
          ; Value (row_number, v_row_number)
          ; Value (institution, v_institution)
          ; Value (format, v_format)
          ; Value (fields_json, v_fields)
          ]
  in
  let rec store added reused = function
    | [] -> Ok (added, reused)
    | (row_number, fields) :: rest ->
      let fields = encode_fields fields in
      let* previous = db_result (Db.first db.conn (find document_id row_number)) in
      (match previous with
       | Some previous when String.equal previous fields -> store added (reused + 1) rest
       | Some _ ->
         Or_error.error_string
           "stored companion observation differs from the archived row"
       | None ->
         let* () =
           db_result
             (Db.exec db.conn (insert document_id row_number institution format fields))
         in
         store (added + 1) reused rest)
  in
  store 0 0 rows
;;

(* A bank-stated period and balances, in the bank's own date or timestamp text. *)
let store_statement db ~document_id ~ordinal ~institution (s : Document.statement) =
  let insert =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         statements(document_id,ordinal,institution,source_account,reference,currency,period_start,period_end,opening_as_of,closing_as_of,opening,closing) \
         VALUES (?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT(document_id,ordinal) DO NOTHING"
        (int64
         @-> int
         @-> text
         @-> text
         @-> option text
         @-> text
         @-> text
         @-> text
         @-> text
         @-> text
         @-> text
         @-> text
         @-> unit))
  in
  let* () =
    db_result
      (Db.exec
         db.conn
         (insert
            document_id
            ordinal
            institution
            s.source_account
            s.reference
            (Iso4217.to_alpha_code s.currency)
            s.period_start
            s.period_end
            s.opening_as_of
            s.closing_as_of
            (Quantity.to_string s.opening)
            (Quantity.to_string s.closing)))
  in
  let find =
    let open Db_schema.Statements in
    select_by_pair table document_id ordinal id
  in
  let* id = db_result (Db.first db.conn (find document_id ordinal)) in
  Or_error.of_option id ~error:(Error.of_string "missing statement")
;;

let link_statement =
  Rel_sql.Stmt.(
    func
      "INSERT INTO statement_events(statement_id,event_id) VALUES (?,?) ON CONFLICT DO \
       NOTHING"
      (int64 @-> int64 @-> unit))
;;

(* Ingest events from one document, linking each to it and to its statement. *)
let ingest_linked ?statement_id db ~document_id events =
  let rec ingest inserted existing = function
    | [] -> Ok (inserted, existing)
    | event :: rest ->
      let* result = ingest_in_transaction db event in
      let id, inserted, existing =
        match result with
        | Inserted id -> id, inserted + 1, existing
        | Existing id -> id, inserted, existing + 1
      in
      let* () = db_result (Db.exec db.conn (link_document document_id id)) in
      let* () =
        match statement_id with
        | None -> Ok ()
        | Some statement_id ->
          db_result (Db.exec db.conn (link_statement statement_id id))
      in
      ingest inserted existing rest
  in
  ingest 0 0 events
;;

(* Without bank IDs, a day's sorted movement payloads must agree across
   documents; a partial or corrected day is refused for review. *)
(* Day inventories group dated events by account and booking day. *)
let day_groups events =
  let groups = Hashtbl.Poly.create () in
  let* () =
    traverse
      (fun (e : Source_event.t) ->
         match e.occurred with
         | On_date day ->
           Ok
             (Hashtbl.add_multi
                groups
                ~key:(e.account, Date.to_string day)
                ~data:e.payload)
         | At_ms _ ->
           Or_error.error_string "day inventories need events dated by booking day")
      events
    |> Or_error.map ~f:ignore
  in
  Ok (Hashtbl.to_alist groups)
;;

let check_day_inventories db ~institution ~document_id groups =
  let find =
    Rel_sql.Stmt.(
      func
        "SELECT signature FROM day_inventories WHERE institution=? AND source_account=? \
         AND booked_on=?"
        (text @-> text @-> text @-> ret Row.(t1 (text "signature"))))
  in
  let insert =
    let open Db_schema.Day_inventories in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    fun v_institution v_source_account v_booked_on v_signature v_document_id ->
      insert
        Col.
          [ Value (institution, v_institution)
          ; Value (source_account, v_source_account)
          ; Value (booked_on, v_booked_on)
          ; Value (signature, v_signature)
          ; Value (document_id, v_document_id)
          ]
  in
  let* _ =
    traverse
      (fun ((account, day), payloads) ->
         let signature =
           encode_fields (List.sort payloads ~compare:String.compare)
           |> Digestif.SHA256.digest_string
           |> Digestif.SHA256.to_hex
         in
         let* previous = db_result (Db.first db.conn (find institution account day)) in
         match previous with
         | Some old when not (String.equal old signature) ->
           Or_error.errorf
             "%s day %s differs from imported evidence; partial-day exports or \
              corrections need review"
             institution
             day
         | Some _ -> Ok ()
         | None ->
           db_result
             (Db.exec db.conn (insert institution account day signature document_id)))
      groups
  in
  Ok ()
;;

(* Hints a document was imported with (Revolut's account label) are part of
   its evidence: the same bytes cannot later mean another account. *)
let bind_document_context db ~document_id context =
  let find =
    let open Db_schema.Document_contexts in
    select_by_column table document_id context_json
  in
  let* previous = db_result (Db.first db.conn (find document_id)) in
  match previous with
  | Some old when not (String.equal old context) ->
    Or_error.error_string "document was already imported with a different context"
  | Some _ -> Ok ()
  | None ->
    let insert =
      let open Db_schema.Document_contexts in
      let insert = Rel_sql.insert_into_cols Db.dialect table in
      fun v_document_id v_context ->
        insert Col.[ Value (document_id, v_document_id); Value (context_json, v_context) ]
    in
    db_result (Db.exec db.conn (insert document_id context))
;;

let import_document
      db
      (module A : Adapter.S)
      ~institution
      ~format
      ~path
      ~contents
      ~context
  =
  transaction db (fun db ->
    let* document_id = archive_document db ~institution ~contents in
    let* accepted =
      db_result (Db.first db.conn (accepted_document document_id document_id))
    in
    let* (document : Document.t) = A.parse ~context ~format contents in
    let all_events =
      List.concat_map document.statements ~f:(fun s -> s.events) @ document.events
    in
    let* () =
      if
        List.for_all all_events ~f:(fun (e : Source_event.t) ->
          String.equal e.venue institution && List.mem A.kinds e.kind ~equal:String.equal)
      then Ok ()
      else Or_error.errorf "the %s adapter produced an event it does not own" institution
    in
    let* () =
      if List.is_empty document.context
      then Ok ()
      else
        bind_document_context
          db
          ~document_id
          (Yojson.Safe.to_string
             (`Assoc
                 (List.sort document.context ~compare:[%compare: string * string]
                  |> List.map ~f:(fun (k, v) -> k, `String v))))
    in
    let* () =
      match A.overlap with
      | Source_ids -> Ok ()
      | Day_inventory ->
        let* groups = day_groups all_events in
        check_day_inventories db ~institution ~document_id groups
    in
    let* counts =
      traverse
        (fun (ordinal, (statement : Document.statement)) ->
           let* statement_id =
             store_statement db ~document_id ~ordinal ~institution statement
           in
           ingest_linked db ~document_id ~statement_id statement.events)
        (List.mapi document.statements ~f:(fun i s -> i, s))
    in
    let* loose = ingest_linked db ~document_id document.events in
    let counts = loose :: counts in
    let* message =
      if List.is_empty document.companion_rows
      then Ok None
      else
        let* added, reused =
          store_companion_rows
            db
            ~document_id
            ~institution
            ~format
            document.companion_rows
        in
        Ok
          (Some
             (Printf.sprintf
                "companion observations: added=%d reused=%d; no transactions created"
                added
                reused))
    in
    record_attempt
      db
      ~institution
      ~path
      ~document_id:(Some document_id)
      ~format
      ~outcome:(if Option.value accepted ~default:false then Unchanged else Imported)
      ~inserted:(List.sum (module Int) counts ~f:fst)
      ~existing:(List.sum (module Int) counts ~f:snd)
      ~message)
;;

let import_bank_file ?account_label db ~institution ~path ~contents =
  let* institution = Validation.text "institution" institution in
  let* path = Validation.text "path" path in
  let fail ~format outcome message =
    transaction db (fun db ->
      let* document_id = archive_document db ~institution ~contents in
      record_attempt
        db
        ~institution
        ~path
        ~document_id:(Some document_id)
        ~format
        ~outcome
        ~inserted:0
        ~existing:0
        ~message:(Some message))
  in
  let unknown = Bank_import.unknown_format in
  match Registry.find db.registry institution with
  | None ->
    fail
      ~format:unknown
      Rejected
      (Printf.sprintf
         "no adapter is registered for institution %s; known institutions: %s"
         institution
         (String.concat ~sep:", " (Registry.institutions db.registry)))
  | Some ((module A : Adapter.S) as adapter) ->
    let matches = Registry.probe db.registry ~filename:path contents in
    let own =
      List.filter_map matches ~f:(fun (owner, format) ->
        Option.some_if (String.equal owner institution) format)
    in
    (match own, matches with
     | [ format ], _ ->
       if Option.is_some account_label && not A.takes_account_label
       then
         fail
           ~format
           Rejected
           (Printf.sprintf "--account-id is not accepted for %s exports" institution)
       else (
         match
           import_document
             db
             adapter
             ~institution
             ~format
             ~path
             ~contents
             ~context:{ account_label }
         with
         | Ok report -> Ok report
         | Error e -> fail ~format Rejected (Error.to_string_hum e))
     | _ :: _ :: _, _ ->
       fail
         ~format:unknown
         Rejected
         (Printf.sprintf
            "file matches several %s formats: %s"
            institution
            (String.concat ~sep:", " own))
     | [], (owner, format) :: _ ->
       fail
         ~format
         Rejected
         (Printf.sprintf
            "this is a %s export (%s); sync it with fin bank sync %s"
            owner
            format
            owner)
     | [], [] ->
       fail ~format:unknown Unsupported "no registered bank recognizes this file")
;;

let record_bank_read_error db ~institution ~path ~message =
  let* institution = Validation.text "institution" institution in
  let* path = Validation.text "path" path in
  transaction db (fun db ->
    record_attempt
      db
      ~institution
      ~path
      ~document_id:None
      ~format:Bank_import.unknown_format
      ~outcome:Unreadable
      ~inserted:0
      ~existing:0
      ~message:(Some message))
;;

let bank_import_history ?(limit = 100) db =
  if limit <= 0 || limit > 10000
  then Or_error.error_string "history limit must be between 1 and 10000"
  else (
    let select =
      Rel_sql.Stmt.(
        func (attempt_select ^ " ORDER BY a.id DESC LIMIT ?") (int @-> ret attempt_row))
    in
    let* rows = collect db (select limit) in
    traverse decode_attempt rows)
;;

let events db =
  let* rows =
    collect
      db
      (statement
         "SELECT \
          id,venue,account,kind,source_id,occurred_at_ms,occurred_on,retrieved_at,payload \
          FROM source_events ORDER BY coalesce(occurred_on, date(occurred_at_ms / 1000, \
          'unixepoch')),occurred_at_ms,id"
         source_row)
  in
  traverse
    (fun ({ id
          ; venue
          ; account
          ; kind
          ; source_id
          ; occurred_at_ms = at
          ; occurred_on = on
          ; payload
          ; _
          } :
           Db_schema.Source_events.t) ->
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

let posting_row =
  Row.(
    t3
      (read_column Db_schema.Postings.account)
      (read_column Db_schema.Postings.commodity)
      (read_column Db_schema.Postings.quantity))
;;

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
         let* quantity = Or_error.try_with (fun () -> Quantity.of_string text) in
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
      (int64
       @-> text
       @-> ret
             Row.(
               t3
                 (read_column Db_schema.Journal_entries.id)
                 (read_column Db_schema.Journal_entries.date)
                 (read_column Db_schema.Journal_entries.description))))
;;

let insert_entry =
  let open Db_schema.Journal_entries in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_event_id v_projection_key v_date v_description ->
    insert
      Col.
        [ Value (event_id, v_event_id)
        ; Value (projection_key, v_projection_key)
        ; Value (date, v_date)
        ; Value (description, v_description)
        ]
;;

let insert_posting =
  let open Db_schema.Postings in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_entry_id v_ordinal v_account v_commodity v_quantity ->
    insert
      Col.
        [ Value (entry_id, v_entry_id)
        ; Value (ordinal, v_ordinal)
        ; Value (account, v_account)
        ; Value (commodity, v_commodity)
        ; Value (quantity, v_quantity)
        ]
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
         && Quantity.equal a.quantity b.quantity)
;;

let record_entry_in_transaction db ~event_id ~key (entry : Accounting.entry) =
  let* key = Validation.text "projection key" key in
  let* existing = db_result (Db.first db.conn (find_entry event_id key)) in
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
           db.conn
           (insert_entry event_id key (Date.to_string entry.date) entry.description))
    in
    let id = Db.last_insert_rowid db.conn in
    let rec write ordinal = function
      | [] -> Ok (Inserted id)
      | (p : Accounting.posting) :: rest ->
        let* () =
          db_result
            (Db.exec
               db.conn
               (insert_posting
                  id
                  ordinal
                  p.account
                  p.commodity
                  (Quantity.to_string p.quantity)))
        in
        write (ordinal + 1) rest
    in
    write 0 entry.postings
;;

let find_mapping =
  let open Db_schema.Bank_account_mappings in
  select_by_pair table institution source_account ledger_account
;;

let find_category =
  Rel_sql.Stmt.(
    func
      "SELECT account,reason FROM bank_categories WHERE event_id=?"
      (int64
       @-> ret
             Row.(
               t2
                 (read_column Db_schema.Bank_categories.account)
                 (read_column Db_schema.Bank_categories.reason))))
;;

let find_linked_transfer =
  Rel_sql.Stmt.(
    func
      "SELECT m.first_event_id,m.second_event_id,d.reason FROM bank_transfer_memberships \
       m JOIN bank_transfer_decisions d ON d.first_event_id=m.first_event_id AND \
       d.second_event_id=m.second_event_id WHERE m.event_id=? AND d.state='confirmed'"
      (int64
       @-> ret
             Row.(
               t3
                 (read_column Db_schema.Bank_transfer_memberships.first_event_id)
                 (read_column Db_schema.Bank_transfer_memberships.second_event_id)
                 (read_column Db_schema.Bank_transfer_decisions.reason))))
;;

let linked_transfer db event_id =
  db_result (Db.first db.conn (find_linked_transfer event_id))
;;

let category_account db event_id =
  let* category = db_result (Db.first db.conn (find_category event_id)) in
  Ok (Option.map category ~f:fst)
;;

let counter_account db event_id =
  let* transfer = linked_transfer db event_id in
  match transfer with
  | Some (first, second, _) -> Ok (Some (Bank_transfer.account first second))
  | None -> category_account db event_id
;;

let mapped_bank_entry db event_id (event : Source_event.t) =
  let* bank_account =
    db_result (Db.first db.conn (find_mapping event.venue event.account))
  in
  let* counter_account = counter_account db event_id in
  let* movement = Registry.movement db.registry event in
  Bank_projection.entry ?bank_account ?counter_account ~institution:event.venue movement
;;

let record_entry db ~event_id ~key entry =
  transaction db (fun db -> record_entry_in_transaction db ~event_id ~key entry)
;;

let entries db =
  let row =
    Row.(
      t5
        (read_column Db_schema.Journal_entries.id)
        (read_column Db_schema.Journal_entries.event_id)
        (read_column Db_schema.Journal_entries.projection_key)
        (read_column Db_schema.Journal_entries.date)
        (read_column Db_schema.Journal_entries.description))
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

let movements db =
  let* source = events db in
  traverse
    (fun (id, event) ->
       let* movement = Registry.movement db.registry event in
       Ok (id, event, movement))
    (List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e))
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
        if not (Registry.is_movement db.registry event)
        then project inserted existing rest
        else
          let* entry = mapped_bank_entry db event_id event in
          let* result =
            record_entry_in_transaction db ~event_id ~key:Bank_projection.key entry
          in
          (match result with
           | Inserted _ -> project (inserted + 1) existing rest
           | Existing _ -> project inserted (existing + 1) rest)
    in
    project 0 0 source_events)
;;

let bank_accounts db =
  let* source_events = events db in
  let bank_events =
    List.filter_map source_events ~f:(fun (_, event) ->
      if Registry.is_movement db.registry event then Some event else None)
    |> List.dedup_and_sort ~compare:(fun a b ->
      match String.compare a.Source_event.venue b.venue with
      | 0 -> String.compare a.account b.account
      | n -> n)
  in
  traverse
    (fun (event : Source_event.t) ->
       let* transaction = Registry.movement db.registry event in
       let account = transaction.account in
       let* mapped =
         db_result (Db.first db.conn (find_mapping event.venue event.account))
       in
       let* ledger_account =
         match mapped with
         | Some name -> Ok name
         | None -> Bank_projection.ledger_account ~institution:event.venue account
       in
       Bank_account_mapping.create
         ~institution:event.venue
         ~bank_id:account.bank_id
         ~account_id:account.account_id
         ~account_type:account.account_type
         ~ledger_account)
    bank_events
;;

let upsert_mapping =
  Rel_sql.Stmt.(
    func
      "INSERT INTO bank_account_mappings(institution,source_account,ledger_account) \
       VALUES (?,?,?) ON CONFLICT(institution,source_account) DO UPDATE SET \
       ledger_account=excluded.ledger_account"
      (text @-> text @-> text @-> unit))
;;

let audit_mapping =
  let open Db_schema.Bank_account_mapping_changes in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_institution v_source_account v_previous_ledger_account v_ledger_account ->
    insert
      Col.
        [ Value (institution, v_institution)
        ; Value (source_account, v_source_account)
        ; Value (previous_ledger_account, v_previous_ledger_account)
        ; Value (ledger_account, v_ledger_account)
        ]
;;

let update_posting v_account v_entry_id v_ordinal =
  let open Db_schema.Postings in
  Rel_sql.update
    Db.dialect
    table
    ~set:Col.[ Value (account, v_account) ]
    ~where:Col.[ Value (entry_id, v_entry_id); Value (ordinal, v_ordinal) ]
;;

let rename_posting account entry_id = update_posting account entry_id 0

let set_bank_account_mappings_in_transaction db mappings =
  let* mappings = Bank_account_mapping.of_json (Bank_account_mapping.to_json mappings) in
  let* source_events = events db in
  let rec apply changed = function
    | [] -> Ok changed
    | (mapping : Bank_account_mapping.t) :: rest ->
      let source_account = Bank_account_mapping.source_account mapping in
      let matching =
        List.filter source_events ~f:(fun (_, event) ->
          Registry.is_movement db.registry event
          && String.equal event.venue mapping.institution
          && String.equal event.account source_account)
      in
      if List.is_empty matching
      then
        Or_error.error_string
          "mapping references an unknown source account; import its records first"
      else
        let* previous =
          db_result (Db.first db.conn (find_mapping mapping.institution source_account))
        in
        if Option.equal String.equal previous (Some mapping.ledger_account)
        then apply changed rest
        else
          let* previous_name =
            match previous with
            | Some name -> Ok name
            | None ->
              let* transaction =
                Registry.movement db.registry (snd (List.hd_exn matching))
              in
              let account = transaction.account in
              Bank_projection.ledger_account ~institution:mapping.institution account
          in
          let* _ =
            traverse
              (fun (event_id, event) ->
                 let* existing =
                   db_result (Db.first db.conn (find_entry event_id Bank_projection.key))
                 in
                 match existing with
                 | None -> Ok ()
                 | Some (id, date, description) ->
                   let* stored = read_entry db id date description in
                   let* counter_account = counter_account db event_id in
                   let* movement = Registry.movement db.registry event in
                   let* expected =
                     Bank_projection.entry
                       ~bank_account:previous_name
                       ?counter_account
                       ~institution:event.venue
                       movement
                   in
                   if not (same_entry stored expected)
                   then
                     Or_error.error_string
                       "account mapping conflicts with an existing bank projection"
                   else
                     db_result
                       (Db.exec db.conn (rename_posting mapping.ledger_account id)))
              matching
          in
          let* () =
            db_result
              (Db.exec
                 db.conn
                 (upsert_mapping
                    mapping.institution
                    source_account
                    mapping.ledger_account))
          in
          let* () =
            db_result
              (Db.exec
                 db.conn
                 (audit_mapping
                    mapping.institution
                    source_account
                    previous
                    mapping.ledger_account))
          in
          apply (changed + 1) rest
  in
  apply 0 mappings
;;

let set_bank_account_mappings db mappings =
  transaction db (fun db -> set_bank_account_mappings_in_transaction db mappings)
;;

type companion_observation =
  { row : Companion.row
  ; matching : Bank_match.result option
  }

type companion_report =
  { matched : int
  ; unmatched : int
  ; ambiguous : int
  ; conflicts : int
  ; changed : int
  }

let csv_row id document_id institution row_number fields_json =
  Row.(
    t5
      (read_column id)
      (read_column document_id)
      (read_column institution)
      (read_column row_number)
      (read_column fields_json))
;;

let csv_match_row row_id event_id state reason candidates_json policy =
  Row.(
    unit (fun row_id event_id state reason candidates policy ->
      row_id, event_id, state, reason, candidates, policy)
    * read_column row_id
    * read_column event_id
    * read_column state
    * read_column reason
    * read_column candidates_json
    * read_column policy)
;;

let decode_csv_match (row_id, event_id, state, reason, candidates, policy) =
  let* state = Bank_match.state_of_string state in
  let* candidates =
    Or_error.try_with (fun () ->
      match Yojson.Safe.from_string candidates with
      | `List values ->
        List.map values ~f:(function
          | `Int n -> Int64.of_int n
          | `Intlit s -> Int64.of_string s
          | _ -> failwith "invalid candidate ID")
      | _ -> failwith "invalid candidate list")
  in
  Ok ({ Bank_match.row_id; event_id; state; reason; candidates }, policy)
;;

(* One table holds every bank's companion observations, told apart by format. *)
(* One table holds every bank's companion observations, told apart by
   institution and format. *)
let companion_rows db ~institution ~format =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT id,document_id,institution,row_number,fields_json FROM companion_rows \
            WHERE institution=? AND format=? ORDER BY document_id,row_number"
           (text
            @-> text
            @-> ret
                  Db_schema.Companion_rows.(
                    csv_row id document_id institution row_number fields_json)))
         institution
         format)
  in
  traverse
    (fun (row_id, document_id, institution, row_number, fields) ->
       let* fields = decode_fields fields in
       Ok { Companion.row_id; document_id; institution; format; row_number; fields })
    rows
;;

let companion_matches db ~institution ~format =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT m.row_id,m.event_id,m.state,m.reason,m.candidates_json,m.policy FROM \
            companion_matches m JOIN companion_rows r ON r.id=m.row_id WHERE \
            r.institution=? AND r.format=? ORDER BY m.row_id"
           (text
            @-> text
            @-> ret
                  Db_schema.Companion_matches.(
                    csv_match_row row_id event_id state reason candidates_json policy)))
         institution
         format)
  in
  traverse decode_csv_match rows
;;

let upsert_companion_match =
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       companion_matches(row_id,event_id,state,reason,candidates_json,policy) VALUES \
       (?,?,?,?,?,?) ON CONFLICT(row_id) DO UPDATE SET \
       event_id=excluded.event_id,state=excluded.state,reason=excluded.reason,candidates_json=excluded.candidates_json,policy=excluded.policy,updated_at=strftime('%Y-%m-%dT%H:%M:%fZ','now')"
      (int64 @-> option int64 @-> text @-> text @-> text @-> text @-> unit))
;;

let audit_companion_match =
  let open Db_schema.Companion_match_changes in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_row_id
    v_previous_event_id
    v_event_id
    v_previous_state
    v_state
    v_reason
    v_candidates_json
    v_policy ->
    insert
      Col.
        [ Value (row_id, v_row_id)
        ; Value (previous_event_id, v_previous_event_id)
        ; Value (event_id, v_event_id)
        ; Value (previous_state, v_previous_state)
        ; Value (state, v_state)
        ; Value (reason, v_reason)
        ; Value (candidates_json, v_candidates_json)
        ; Value (policy, v_policy)
        ]
;;

let save_csv_results
      db
      ~matching_policy
      (previous_rows : (Bank_match.result * string) list)
      results
  =
  let previous = Hashtbl.create (module Int64) in
  List.iter previous_rows ~f:(fun (result, policy) ->
    Hashtbl.set previous ~key:result.row_id ~data:(result, policy));
  let equal (a : Bank_match.result) (b : Bank_match.result) =
    Option.equal Int64.equal a.event_id b.event_id
    && Poly.equal a.state b.state
    && String.equal a.reason b.reason
    && List.equal Int64.equal a.candidates b.candidates
  in
  let rec save changed = function
    | [] -> Ok changed
    | (result : Bank_match.result) :: rest ->
      let old = Hashtbl.find previous result.row_id in
      if
        Option.exists old ~f:(fun (row, policy) ->
          equal row result && String.equal policy matching_policy)
      then save changed rest
      else (
        let candidates =
          Yojson.Safe.to_string
            (`List
                (List.map result.candidates ~f:(fun id -> `Intlit (Int64.to_string id))))
        in
        let state = Bank_match.state_name result.state in
        let* () =
          db_result
            (Db.exec
               db.conn
               (upsert_companion_match
                  result.row_id
                  result.event_id
                  state
                  result.reason
                  candidates
                  matching_policy))
        in
        let* () =
          db_result
            (Db.exec
               db.conn
               (audit_companion_match
                  result.row_id
                  (Option.bind old ~f:(fun (row, _) -> row.event_id))
                  result.event_id
                  (Option.map old ~f:(fun (row, _) -> Bank_match.state_name row.state))
                  state
                  result.reason
                  candidates
                  matching_policy))
        in
        save (changed + 1) rest)
  in
  let* changed = save 0 results in
  let count state =
    List.count results ~f:(fun result -> Poly.equal result.Bank_match.state state)
  in
  Ok
    { matched = count Matched
    ; unmatched = count Unmatched
    ; ambiguous = count Ambiguous
    ; conflicts = count Conflict
    ; changed
    }
;;

let registered_companions ?institution db =
  List.filter (Registry.companions db.registry) ~f:(fun (owner, _) ->
    Option.for_all institution ~f:(String.equal owner))
;;

(* Each companion format is matched by its adapter against the owning
   institution's own movements; results and their audit are stored here. *)
let match_companions ?institution db =
  transaction db (fun db ->
    let* source = events db in
    let* reports =
      traverse
        (fun (owner, (companion : Companion.t)) ->
           let* rows = companion_rows db ~institution:owner ~format:companion.format in
           let own =
             List.filter source ~f:(fun (_, (e : Source_event.t)) ->
               String.equal e.venue owner && Registry.is_movement db.registry e)
           in
           let* results = companion.match_rows rows own in
           let* previous =
             companion_matches db ~institution:owner ~format:companion.format
           in
           save_csv_results db ~matching_policy:companion.policy previous results)
        (registered_companions ?institution db)
    in
    Ok
      (List.fold
         reports
         ~init:{ matched = 0; unmatched = 0; ambiguous = 0; conflicts = 0; changed = 0 }
         ~f:(fun (a : companion_report) (b : companion_report) ->
           { matched = a.matched + b.matched
           ; unmatched = a.unmatched + b.unmatched
           ; ambiguous = a.ambiguous + b.ambiguous
           ; conflicts = a.conflicts + b.conflicts
           ; changed = a.changed + b.changed
           })))
;;

let companion_observations ?institution db =
  let* lists =
    traverse
      (fun (owner, (companion : Companion.t)) ->
         let* rows = companion_rows db ~institution:owner ~format:companion.format in
         let* matches =
           companion_matches db ~institution:owner ~format:companion.format
         in
         let matching = Hashtbl.create (module Int64) in
         List.iter matches ~f:(fun (result, _) ->
           Hashtbl.set matching ~key:result.Bank_match.row_id ~data:result);
         Ok
           (List.map rows ~f:(fun (row : Companion.row) ->
              { row; matching = Hashtbl.find matching row.row_id })))
      (registered_companions ?institution db)
  in
  Ok (List.concat lists)
;;

type statement_reconciliation =
  { statement_id : int64
  ; document_id : int64
  ; institution : string
  ; source_account : string
  ; reference : string option
  ; currency : Iso4217.t
  ; period_start : string
  ; period_end : string
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; movements : Quantity.t
  ; entries : int
  ; difference : Quantity.t
  }

let statement_reconciliations ?institution db =
  let row =
    Row.(
      unit
        (fun
            id
             document_id
             institution
             source_account
             reference
             currency
             period_start
             period_end
             opening
             closing
           ->
           ( id
           , document_id
           , institution
           , source_account
           , reference
           , currency
           , period_start
           , period_end
           , opening
           , closing ))
      * read_column Db_schema.Statements.id
      * read_column Db_schema.Statements.document_id
      * read_column Db_schema.Statements.institution
      * read_column Db_schema.Statements.source_account
      * read_column Db_schema.Statements.reference
      * read_column Db_schema.Statements.currency
      * read_column Db_schema.Statements.period_start
      * read_column Db_schema.Statements.period_end
      * read_column Db_schema.Statements.opening
      * read_column Db_schema.Statements.closing)
  in
  let* statements =
    collect
      db
      (statement
         "SELECT \
          id,document_id,institution,source_account,reference,currency,period_start,period_end,opening,closing \
          FROM statements ORDER BY id"
         row)
  in
  let statements =
    List.filter statements ~f:(fun (_, _, i, _, _, _, _, _, _, _) ->
      Option.for_all institution ~f:(String.equal i))
  in
  let* movements = movements db in
  let amounts = Hashtbl.create (module Int64) in
  List.iter movements ~f:(fun (id, _, (m : Movement.t)) ->
    Hashtbl.set amounts ~key:id ~data:m.amount);
  traverse
    (fun ( statement_id
         , document_id
         , institution
         , source_account
         , reference
         , currency
         , period_start
         , period_end
         , opening
         , closing ) ->
       let find =
         let open Db_schema.Statement_events in
         select_by_column table statement_id event_id
       in
       let* ids = collect db (find statement_id) in
       let* amounts =
         traverse
           (fun id ->
              Or_error.of_option
                (Hashtbl.find amounts id)
                ~error:(Error.of_string "missing statement movement"))
           ids
       in
       let* opening, closing =
         Or_error.try_with (fun () ->
           Quantity.of_string opening, Quantity.of_string closing)
       in
       let* currency = Validation.currency "stored statement currency" currency in
       let movements = List.fold amounts ~init:Quantity.zero ~f:Quantity.( + ) in
       Ok
         { statement_id
         ; document_id
         ; institution
         ; source_account
         ; reference
         ; currency
         ; period_start
         ; period_end
         ; opening
         ; closing
         ; movements
         ; entries = List.length ids
         ; difference = Quantity.(opening + movements - closing)
         })
    statements
;;

let set_bank_categories db decisions =
  let* decisions =
    traverse
      (fun (d : Bank_category.t) ->
         Bank_category.create ~event_id:d.event_id ~account:d.account ~reason:d.reason)
      decisions
  in
  if
    List.contains_dup
      (List.map decisions ~f:(fun d -> d.Bank_category.event_id))
      ~compare:Int64.compare
  then Or_error.error_string "duplicate event_id in category batch"
  else
    transaction db (fun db ->
      let* source = events db in
      let rec apply changed = function
        | [] -> Ok changed
        | (d : Bank_category.t) :: rest ->
          let* event =
            Or_error.of_option
              (List.Assoc.find source d.event_id ~equal:Int64.equal)
              ~error:(Error.of_string "unknown category event_id")
          in
          let* transfer = linked_transfer db d.event_id in
          if Option.is_some transfer
          then
            Or_error.error_string
              "reject the confirmed transfer before changing its category"
          else if not (Registry.is_movement db.registry event)
          then Or_error.error_string "category decision requires a bank event"
          else
            let* previous = db_result (Db.first db.conn (find_category d.event_id)) in
            if
              Option.equal [%equal: string * string] previous (Some (d.account, d.reason))
            then apply changed rest
            else
              let* existing =
                db_result (Db.first db.conn (find_entry d.event_id Bank_projection.key))
              in
              let* () =
                match existing with
                | None -> Ok ()
                | Some (id, date, description) ->
                  let* stored = read_entry db id date description in
                  let* expected = mapped_bank_entry db d.event_id event in
                  if not (same_entry stored expected)
                  then
                    Or_error.error_string
                      "category change conflicts with existing bank projection"
                  else (
                    let update =
                      fun account entry_id -> update_posting account entry_id 1
                    in
                    db_result (Db.exec db.conn (update d.account id)))
              in
              let upsert =
                Rel_sql.Stmt.(
                  func
                    "INSERT INTO bank_categories(event_id,account,reason) VALUES (?,?,?) \
                     ON CONFLICT(event_id) DO UPDATE SET \
                     account=excluded.account,reason=excluded.reason"
                    (int64 @-> text @-> text @-> unit))
              in
              let audit =
                let open Db_schema.Bank_category_changes in
                let insert = Rel_sql.insert_into_cols Db.dialect table in
                fun v_event_id v_previous_account v_account v_reason ->
                  insert
                    Col.
                      [ Value (event_id, v_event_id)
                      ; Value (previous_account, v_previous_account)
                      ; Value (account, v_account)
                      ; Value (reason, v_reason)
                      ]
              in
              let* () =
                db_result (Db.exec db.conn (upsert d.event_id d.account d.reason))
              in
              let* () =
                db_result
                  (Db.exec
                     db.conn
                     (audit d.event_id (Option.map previous ~f:fst) d.account d.reason))
              in
              apply (changed + 1) rest
      in
      apply 0 decisions)
;;

type category_change =
  { id : int64
  ; event_id : int64
  ; previous_account : string option
  ; account : string
  ; reason : string
  ; changed_at : string
  }

let bank_category_history db =
  let row =
    Row.(
      unit (fun id event_id previous_account account reason changed_at ->
        { id; event_id; previous_account; account; reason; changed_at })
      * read_column Db_schema.Bank_category_changes.id
      * read_column Db_schema.Bank_category_changes.event_id
      * read_column Db_schema.Bank_category_changes.previous_account
      * read_column Db_schema.Bank_category_changes.account
      * read_column Db_schema.Bank_category_changes.reason
      * read_column Db_schema.Bank_category_changes.changed_at)
  in
  collect
    db
    (statement
       "SELECT id,event_id,previous_account,account,reason,changed_at FROM \
        bank_category_changes ORDER BY id"
       row)
;;

type bank_review =
  { event_id : int64
  ; institution : string
  ; transaction : Movement.t
  ; counter_account : string
  ; reason : string option
  }

let bank_review db =
  let* source = events db in
  traverse
    (fun (event_id, event) ->
       let* transaction = Registry.movement db.registry event in
       let* category = db_result (Db.first db.conn (find_category event_id)) in
       let* transfer = linked_transfer db event_id in
       Ok
         { event_id
         ; institution = event.venue
         ; transaction
         ; counter_account =
             (match transfer with
              | Some (first, second, _) -> Bank_transfer.account first second
              | None -> Option.value_map category ~default:"equity:unclassified" ~f:fst)
         ; reason =
             (match transfer with
              | Some (_, _, reason) -> Some reason
              | None -> Option.map category ~f:snd)
         })
    (List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e))
;;

let find_account_iban =
  let open Db_schema.Bank_account_identities in
  select_by_pair table institution source_account iban
;;

(* IBANs are validated when written. A stored value that no longer parses (for
   example after a registry update) is reported, never silently ignored. *)
let registered_iban db institution source_account =
  let* stored =
    db_result (Db.first db.conn (find_account_iban institution source_account))
  in
  match stored with
  | None -> Ok None
  | Some text ->
    let* iban = Or_error.tag (Iban.of_string text) ~tag:"stored account IBAN" in
    Ok (Some iban)
;;

let account_iban db (event : Source_event.t) (tx : Movement.t) =
  let* registered = registered_iban db event.venue event.account in
  let imported =
    match tx.own_iban with
    | Known iban -> Some iban
    | Unknown | Not_applicable -> None
  in
  match imported, registered with
  | Some a, Some b when not (Iban.equal a b) ->
    Or_error.error_string "registered IBAN contradicts source account identifier"
  | Some a, _ -> Ok (Some a, "imported")
  | None, Some b -> Ok (Some b, "registered")
  | None, None ->
    Ok
      ( None
      , match tx.own_iban with
        | Not_applicable -> "not_applicable"
        | Known _ | Unknown -> "missing" )
;;

(* Every movement of an account reports the same own-IBAN status; the first
   one stands for the account. *)
let own_iban db source (account : Bank_account_mapping.t) =
  let identity = Bank_account_mapping.source_account account in
  match
    List.find source ~f:(fun (_, (e : Source_event.t)) ->
      Registry.is_movement db.registry e
      && String.equal e.venue account.institution
      && String.equal e.account identity)
  with
  | None -> Or_error.error_string "account has no imported movements"
  | Some (_, event) ->
    let* movement = Registry.movement db.registry event in
    Ok movement.Movement.own_iban
;;

let transfer_iban_evidence db source a ta b tb =
  let* a_iban, _ = account_iban db a ta in
  let* b_iban, _ = account_iban db b tb in
  let a_counterparty = ta.Movement.counterparty_iban
  and b_counterparty = tb.Movement.counterparty_iban in
  let* known =
    traverse
      (fun (_, event) ->
         let* tx = Registry.movement db.registry event in
         let* iban, _ = account_iban db event tx in
         Ok (event, iban))
      (List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e)
       |> List.dedup_and_sort ~compare:(fun (_, a) (_, b) ->
         [%compare: string * string]
           (a.Source_event.venue, a.account)
           (b.Source_event.venue, b.account)))
  in
  let assess counterparty target target_iban =
    match counterparty, target_iban with
    | None, _ -> None
    | Some cp, Some own when Iban.equal cp own ->
      Some (true, "counterparty IBAN matches the other source account")
    | Some _, Some _ ->
      Some (false, "counterparty IBAN differs from the other source account's IBAN")
    | Some cp, None ->
      let conflicting =
        List.find_map known ~f:(fun (event, iban) ->
          if
            String.equal event.Source_event.venue target.Source_event.venue
            && String.equal event.account target.account
          then None
          else if Option.equal Iban.equal iban (Some cp)
          then Some event.venue
          else None)
      in
      Option.map conflicting ~f:(fun institution ->
        ( false
        , "counterparty IBAN belongs to a different known " ^ institution ^ " account" ))
  in
  let evidence =
    List.filter_opt [ assess a_counterparty b b_iban; assess b_counterparty a a_iban ]
  in
  let conflicts =
    List.filter_map evidence ~f:(fun (matches, detail) ->
      if matches then None else Some detail)
  in
  Ok (List.is_empty conflicts, List.exists evidence ~f:fst, List.map evidence ~f:snd)
;;

let transfer_decision_row =
  Row.(
    t4
      (read_column Db_schema.Bank_transfer_decisions.first_event_id)
      (read_column Db_schema.Bank_transfer_decisions.second_event_id)
      (read_column Db_schema.Bank_transfer_decisions.state)
      (read_column Db_schema.Bank_transfer_decisions.reason))
;;

let find_transfer_decision =
  Rel_sql.Stmt.(
    func
      "SELECT first_event_id,second_event_id,state,reason FROM bank_transfer_decisions \
       WHERE first_event_id=? AND second_event_id=?"
      (int64 @-> int64 @-> ret transfer_decision_row))
;;

let decode_transfer_decision (first_event_id, second_event_id, state, reason) =
  let* state = Bank_transfer.state_of_string state in
  Bank_transfer.create ~first_event_id ~second_event_id ~state ~reason
;;

let bank_transfer_decisions db =
  let* rows =
    collect
      db
      (statement
         "SELECT first_event_id,second_event_id,state,reason FROM \
          bank_transfer_decisions ORDER BY first_event_id,second_event_id"
         transfer_decision_row)
  in
  traverse decode_transfer_decision rows
;;

let verify_bank_entry db event_id event =
  let* existing =
    db_result (Db.first db.conn (find_entry event_id Bank_projection.key))
  in
  match existing with
  | None -> Ok None
  | Some (id, date, description) ->
    let* stored = read_entry db id date description in
    let* expected = mapped_bank_entry db event_id event in
    if same_entry stored expected
    then Ok (Some id)
    else
      Or_error.error_string "transfer decision conflicts with an existing bank projection"
;;

let set_bank_transfers db decisions =
  let* decisions =
    traverse
      (fun (d : Bank_transfer.decision) ->
         Bank_transfer.create
           ~first_event_id:d.first_event_id
           ~second_event_id:d.second_event_id
           ~state:d.state
           ~reason:d.reason)
      decisions
  in
  if
    List.contains_dup
      (List.map decisions ~f:(fun d -> d.Bank_transfer.first_event_id, d.second_event_id))
      ~compare:[%compare: int64 * int64]
  then Or_error.error_string "duplicate transfer pair in batch"
  else
    transaction db (fun db ->
      let* source = events db in
      let get id =
        Or_error.of_option
          (List.Assoc.find source id ~equal:Int64.equal)
          ~error:(Error.of_string "unknown transfer event_id")
      in
      let upsert =
        Rel_sql.Stmt.(
          func
            "INSERT INTO \
             bank_transfer_decisions(first_event_id,second_event_id,state,reason) VALUES \
             (?,?,?,?) ON CONFLICT(first_event_id,second_event_id) DO UPDATE SET \
             state=excluded.state,reason=excluded.reason"
            (int64 @-> int64 @-> text @-> text @-> unit))
      in
      let add =
        Rel_sql.Stmt.(
          func
            "INSERT INTO \
             bank_transfer_memberships(event_id,first_event_id,second_event_id) VALUES \
             (?,?,?) ON CONFLICT(event_id) DO NOTHING"
            (int64 @-> int64 @-> int64 @-> unit))
      in
      let remove =
        let open Db_schema.Bank_transfer_memberships in
        fun v_first_event_id v_second_event_id ->
          Rel_sql.delete_from
            Db.dialect
            table
            ~where:
              Col.
                [ Value (first_event_id, v_first_event_id)
                ; Value (second_event_id, v_second_event_id)
                ]
      in
      let update = fun account entry_id -> update_posting account entry_id 1 in
      let audit =
        let open Db_schema.Bank_transfer_changes in
        let insert = Rel_sql.insert_into_cols Db.dialect table in
        fun v_first_event_id v_second_event_id v_previous_state v_state v_reason ->
          insert
            Col.
              [ Value (first_event_id, v_first_event_id)
              ; Value (second_event_id, v_second_event_id)
              ; Value (previous_state, v_previous_state)
              ; Value (state, v_state)
              ; Value (reason, v_reason)
              ]
      in
      let rec apply changed = function
        | [] -> Ok changed
        | (d : Bank_transfer.decision) :: rest ->
          let first = d.first_event_id
          and second = d.second_event_id in
          let* a = get first in
          let* b = get second in
          let* ta = Registry.movement db.registry a in
          let* tb = Registry.movement db.registry b in
          let* previous =
            db_result (Db.first db.conn (find_transfer_decision first second))
          in
          let state = Bank_transfer.state_name d.state in
          if
            Option.exists previous ~f:(fun (_, _, old, reason) ->
              String.equal old state && String.equal reason d.reason)
          then apply changed rest
          else (
            let confirming = Poly.equal d.state Bank_transfer.Confirmed in
            let was_confirmed =
              Option.exists previous ~f:(fun (_, _, state, _) ->
                String.equal state "confirmed")
            in
            let* iban_compatible, _, _ = transfer_iban_evidence db source a ta b tb in
            let* () =
              if confirming && not iban_compatible
              then
                Or_error.error_string
                  "counterparty IBAN contradicts this transfer pairing"
              else Ok ()
            in
            let* () =
              if confirming && not (Bank_transfer.eligible a ta b tb)
              then
                Or_error.error_string
                  "transfer requires opposite principal amounts, same currency, distinct \
                   accounts and dates within seven days"
              else Ok ()
            in
            let* _ =
              traverse
                (fun id ->
                   let* existing = linked_transfer db id in
                   let* category = category_account db id in
                   if
                     confirming
                     && Option.exists existing ~f:(fun (x, y, _) ->
                       not (Int64.equal x first && Int64.equal y second))
                   then
                     Or_error.error_string
                       "a movement already belongs to another confirmed transfer"
                   else if
                     confirming
                     && Option.exists category ~f:(fun name ->
                       not (String.equal name "equity:unclassified"))
                   then
                     Or_error.error_string
                       "return reviewed categories to equity:unclassified before \
                        confirming a transfer"
                   else Ok ())
                [ first; second ]
            in
            let* entries =
              if confirming || was_confirmed
              then
                traverse
                  (fun (id, event) ->
                     let* entry = verify_bank_entry db id event in
                     Ok (id, entry))
                  [ first, a; second, b ]
              else Ok []
            in
            let* () = db_result (Db.exec db.conn (upsert first second state d.reason)) in
            let* () =
              if confirming
              then
                let* _ =
                  traverse
                    (fun id -> db_result (Db.exec db.conn (add id first second)))
                    [ first; second ]
                in
                Ok ()
              else db_result (Db.exec db.conn (remove first second))
            in
            let* _ =
              traverse
                (fun (id, entry) ->
                   match entry with
                   | None -> Ok ()
                   | Some entry_id ->
                     let* account = counter_account db id in
                     db_result
                       (Db.exec
                          db.conn
                          (update
                             (Option.value account ~default:"equity:unclassified")
                             entry_id)))
                entries
            in
            let* () =
              db_result
                (Db.exec
                   db.conn
                   (audit
                      first
                      second
                      (Option.map previous ~f:(fun (_, _, state, _) -> state))
                      state
                      d.reason))
            in
            apply (changed + 1) rest)
      in
      apply 0 decisions)
;;

type transfer_change =
  { id : int64
  ; first_event_id : int64
  ; second_event_id : int64
  ; previous_state : string option
  ; state : Bank_transfer.state
  ; reason : string
  ; changed_at : string
  }

let bank_transfer_history db =
  let row =
    Row.(
      unit (fun id first second previous state reason at ->
        id, first, second, previous, state, reason, at)
      * read_column Db_schema.Bank_transfer_changes.id
      * read_column Db_schema.Bank_transfer_changes.first_event_id
      * read_column Db_schema.Bank_transfer_changes.second_event_id
      * read_column Db_schema.Bank_transfer_changes.previous_state
      * read_column Db_schema.Bank_transfer_changes.state
      * read_column Db_schema.Bank_transfer_changes.reason
      * read_column Db_schema.Bank_transfer_changes.changed_at)
  in
  let* rows =
    collect
      db
      (statement
         "SELECT \
          id,first_event_id,second_event_id,previous_state,state,reason,changed_at FROM \
          bank_transfer_changes ORDER BY id"
         row)
  in
  traverse
    (fun (id, first_event_id, second_event_id, previous_state, state, reason, changed_at) ->
       let* state = Bank_transfer.state_of_string state in
       Ok
         { id
         ; first_event_id
         ; second_event_id
         ; previous_state
         ; state
         ; reason
         ; changed_at
         })
    rows
;;

type transfer_candidate =
  { first_event_id : int64
  ; second_event_id : int64
  ; first : Source_event.t
  ; second : Source_event.t
  ; first_transaction : Movement.t
  ; second_transaction : Movement.t
  ; days_apart : int
  ; state : Bank_transfer.state option
  ; reason : string option
  ; unique : bool
  ; iban_compatible : bool
  ; iban_match : bool
  ; iban_evidence : string list
  }

let transfer_candidates ?(include_reviewed = false) db =
  let* source = events db in
  let* items =
    traverse
      (fun (id, e) ->
         let* tx = Registry.movement db.registry e in
         let* linked = linked_transfer db id in
         Ok (id, e, tx, Option.is_some linked))
      (List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e))
  in
  let* decisions = bank_transfer_decisions db in
  let indexed = Hashtbl.Poly.create () in
  List.iter decisions ~f:(fun d ->
    Hashtbl.set indexed ~key:(d.Bank_transfer.first_event_id, d.second_event_id) ~data:d);
  let rec pairs acc = function
    | [] -> Ok acc
    | (id, a, ta, la) :: rest ->
      let* next =
        traverse
          (fun (other, b, tb, lb) ->
             if not (Bank_transfer.eligible a ta b tb)
             then Ok None
             else (
               let id, a, ta, other, b, tb =
                 if Int64.(id < other)
                 then id, a, ta, other, b, tb
                 else other, b, tb, id, a, ta
               in
               let decision = Hashtbl.find indexed (id, other) in
               let* iban_compatible, iban_match, iban_evidence =
                 transfer_iban_evidence db source a ta b tb
               in
               if
                 (not include_reviewed)
                 && (la || lb || Option.is_some decision || not iban_compatible)
               then Ok None
               else
                 Ok
                   (Some
                      { first_event_id = id
                      ; second_event_id = other
                      ; first = a
                      ; second = b
                      ; first_transaction = ta
                      ; second_transaction = tb
                      ; days_apart = Int.abs (Date.diff ta.booked_on tb.booked_on)
                      ; state = Option.map decision ~f:(fun d -> d.state)
                      ; reason = Option.map decision ~f:(fun d -> d.reason)
                      ; unique = false
                      ; iban_compatible
                      ; iban_match
                      ; iban_evidence
                      })))
          rest
      in
      pairs (List.filter_opt next @ acc) rest
  in
  let* candidates = pairs [] items in
  let claims = Hashtbl.create (module Int64) in
  List.iter candidates ~f:(fun c ->
    Hashtbl.incr claims c.first_event_id;
    Hashtbl.incr claims c.second_event_id);
  Ok
    (List.map candidates ~f:(fun c ->
       { c with
         unique =
           Hashtbl.find_exn claims c.first_event_id = 1
           && Hashtbl.find_exn claims c.second_event_id = 1
       })
     |> List.sort ~compare:(fun a b ->
       [%compare: int64 * int64]
         (a.first_event_id, a.second_event_id)
         (b.first_event_id, b.second_event_id)))
;;

let find_settlement =
  let open Db_schema.Bank_account_settlements in
  select_by_pair table institution source_account settlement_account
;;

type bank_account_info =
  { mapping : Bank_account_mapping.t
  ; iban : Iban.t option
  ; iban_source : string
  ; currency : Iso4217.t
  ; movements : int
  ; first_on : Date.t
  ; last_on : Date.t
  ; settlement_account : Bank_account_config.reference option
  }

let bank_account_info db =
  let* mappings = bank_accounts db in
  let* source = events db in
  traverse
    (fun mapping ->
       let members =
         List.filter source ~f:(fun (_, e) ->
           Registry.is_movement db.registry e
           && String.equal e.Source_event.venue mapping.Bank_account_mapping.institution
           && String.equal e.account (Bank_account_mapping.source_account mapping))
       in
       let event = snd (List.hd_exn members) in
       let* tx = Registry.movement db.registry event in
       let* iban, iban_source = account_iban db event tx in
       let* settlement =
         db_result
           (Db.first
              db.conn
              (find_settlement
                 mapping.institution
                 (Bank_account_mapping.source_account mapping)))
       in
       let* settlement_account =
         match settlement with
         | None -> Ok None
         | Some text ->
           let* json = Or_error.try_with (fun () -> Yojson.Safe.from_string text) in
           let* reference = Bank_account_config.reference_of_json json in
           Ok (Some reference)
       in
       let* dates =
         traverse
           (fun (_, e) ->
              let* tx = Registry.movement db.registry e in
              Ok tx.booked_on)
           members
       in
       Ok
         { mapping
         ; iban
         ; iban_source
         ; currency = tx.account.currency
         ; movements = List.length members
         ; first_on = List.min_elt dates ~compare:Date.compare |> Option.value_exn
         ; last_on = List.max_elt dates ~compare:Date.compare |> Option.value_exn
         ; settlement_account
         })
    mappings
;;

let set_bank_account_identities_in_transaction db identities =
  let* identities =
    traverse
      (fun (i : Account_identity.t) ->
         Account_identity.create
           ~institution:i.institution
           ~bank_id:i.bank_id
           ~account_id:i.account_id
           ~account_type:i.account_type
           ~iban:i.iban)
      identities
  in
  if
    List.contains_dup
      (List.map identities ~f:(fun i ->
         i.Account_identity.institution, Account_identity.source_account i))
      ~compare:[%compare: string * string]
  then Or_error.error_string "duplicate account identity"
  else
    let* accounts = bank_accounts db in
    let* source = events db in
    let upsert =
      Rel_sql.Stmt.(
        func
          "INSERT INTO bank_account_identities(institution,source_account,iban) VALUES \
           (?,?,?) ON CONFLICT(institution,source_account) DO UPDATE SET \
           iban=excluded.iban"
          (text @-> text @-> text @-> unit))
    in
    let audit =
      let open Db_schema.Bank_account_identity_changes in
      let insert = Rel_sql.insert_into_cols Db.dialect table in
      fun v_institution v_source_account v_previous_iban v_iban ->
        insert
          Col.
            [ Value (institution, v_institution)
            ; Value (source_account, v_source_account)
            ; Value (previous_iban, v_previous_iban)
            ; Value (iban, v_iban)
            ]
    in
    let rec apply count = function
      | [] -> Ok count
      | i :: rest ->
        let identity = Account_identity.source_account i in
        let account =
          List.find accounts ~f:(fun a ->
            String.equal a.Bank_account_mapping.institution i.institution
            && String.equal (Bank_account_mapping.source_account a) identity)
        in
        let* account =
          Or_error.of_option
            account
            ~error:(Error.of_string "unknown account identity; import the account first")
        in
        let* own_iban = own_iban db source account in
        let* () =
          match own_iban with
          | Not_applicable ->
            Or_error.error_string
              "credit-card accounts have no IBAN; register the associated bank account \
               instead"
          | Known old when not (Iban.equal old i.iban) ->
            Or_error.error_string "IBAN contradicts imported account identifier"
          | Known _ | Unknown -> Ok ()
        in
        let* previous =
          db_result (Db.first db.conn (find_account_iban i.institution identity))
        in
        (* The previous value stays text, so an entry that today's rules reject
           can still be corrected and is audited exactly as stored. *)
        let iban = Iban.to_string i.iban in
        if Option.equal String.equal previous (Some iban)
        then apply count rest
        else
          let* () = db_result (Db.exec db.conn (upsert i.institution identity iban)) in
          let* () =
            db_result
              (Db.exec db.conn (audit i.institution identity previous (Some iban)))
          in
          apply (count + 1) rest
    in
    let* changed = apply 0 identities in
    let* transfers = bank_transfer_decisions db in
    let* _ =
      traverse
        (fun (d : Bank_transfer.decision) ->
           if Poly.equal d.state Rejected
           then Ok ()
           else (
             let get id =
               Or_error.of_option
                 (List.Assoc.find source id ~equal:Int64.equal)
                 ~error:(Error.of_string "missing transfer source")
             in
             let* a = get d.first_event_id in
             let* b = get d.second_event_id in
             let* ta = Registry.movement db.registry a in
             let* tb = Registry.movement db.registry b in
             let* compatible, _, _ = transfer_iban_evidence db source a ta b tb in
             if compatible
             then Ok ()
             else
               Or_error.error_string
                 "new identity contradicts a confirmed transfer; reject the transfer \
                  before correcting identities"))
        transfers
    in
    Ok changed
;;

let set_bank_account_identities db identities =
  transaction db (fun db -> set_bank_account_identities_in_transaction db identities)
;;

type account_change =
  { id : int64
  ; kind : string
  ; institution : string
  ; source_account : string
  ; previous_value : string option
  ; value : string option
  ; changed_at : string
  }

let bank_account_history db =
  let row =
    Row.(
      unit (fun id kind institution source_account previous_value value changed_at ->
        { id; kind; institution; source_account; previous_value; value; changed_at })
      * int64 "id"
      * text "kind"
      * text "institution"
      * text "source_account"
      * col "previous_value" Type.(option text)
      * col "value" Type.(option text)
      * text "changed_at")
  in
  collect
    db
    (statement
       "SELECT id,'ledger_account' AS \
        kind,institution,source_account,previous_ledger_account AS \
        previous_value,ledger_account AS value,changed_at FROM \
        bank_account_mapping_changes UNION ALL SELECT \
        id,'iban',institution,source_account,previous_iban,iban,changed_at FROM \
        bank_account_identity_changes UNION ALL SELECT \
        id,'settlement_account',institution,source_account,previous_settlement,settlement_account,changed_at \
        FROM bank_account_settlement_changes ORDER BY changed_at,kind,id"
       row)
;;

type account_config_report =
  { mapping_changes : int
  ; identity_changes : int
  ; settlement_changes : int
  ; changes : account_change list
  }

let export_bank_accounts db =
  let* accounts = bank_account_info db in
  Ok
    (List.map accounts ~f:(fun info ->
       let a = info.mapping in
       let reference =
         { Bank_account_config.institution = a.institution
         ; bank_id = a.bank_id
         ; account_id = a.account_id
         ; account_type = a.account_type
         }
       in
       { Bank_account_config.reference
       ; ledger_account = Some a.ledger_account
       ; iban =
           Option.value_map info.iban ~default:Bank_account_config.Clear ~f:(fun s ->
             Set s)
       ; settlement_account =
           Option.value_map
             info.settlement_account
             ~default:Bank_account_config.Clear
             ~f:(fun r -> Set r)
       }))
;;

let apply_bank_accounts ?(dry_run = false) db config =
  let* config = Bank_account_config.of_json (Bank_account_config.to_json config) in
  transaction db (fun db ->
    let* () =
      if dry_run
      then db_result (Db.exec_sql db.conn "SAVEPOINT account_preview")
      else Ok ()
    in
    let* before_history = bank_account_history db in
    let* info = bank_account_info db in
    let find r =
      List.find info ~f:(fun i ->
        String.equal i.mapping.institution r.Bank_account_config.institution
        && String.equal
             (Bank_account_mapping.source_account i.mapping)
             (Bank_account_config.source_account r))
    in
    let describe (r : Bank_account_config.reference) =
      Printf.sprintf "%s %s %s" r.institution r.account_type r.account_id
    in
    let resolve r =
      Or_error.of_option
        (find r)
        ~error:
          (Error.of_string
             (Printf.sprintf
                "%s has no imported statements; sync them with fin bank sync %s FOLDER \
                 before applying the configuration"
                (describe r)
                r.institution))
    in
    let* () =
      match
        List.filter config ~f:(fun row ->
          Option.is_none (find row.Bank_account_config.reference))
      with
      | [] -> Ok ()
      | missing ->
        Or_error.errorf
          "%d configured account%s ha%s no imported statements: %s. Sync each one's \
           statement folder with fin bank sync BANK PATH, then apply the configuration \
           again."
          (List.length missing)
          (if List.length missing = 1 then "" else "s")
          (if List.length missing = 1 then "s" else "ve")
          (String.concat
             ~sep:"; "
             (List.map missing ~f:(fun row -> describe row.Bank_account_config.reference)))
    in
    let* mappings =
      traverse
        (fun row ->
           match row.Bank_account_config.ledger_account with
           | None -> Ok None
           | Some ledger_account ->
             let* current = resolve row.reference in
             if String.equal current.mapping.ledger_account ledger_account
             then Ok None
             else (
               let r = row.reference in
               let* mapping =
                 Bank_account_mapping.create
                   ~institution:r.institution
                   ~bank_id:r.bank_id
                   ~account_id:r.account_id
                   ~account_type:r.account_type
                   ~ledger_account
               in
               Ok (Some mapping)))
        config
    in
    let* mapping_changes =
      set_bank_account_mappings_in_transaction db (List.filter_opt mappings)
    in
    let remove_iban =
      let open Db_schema.Bank_account_identities in
      fun v_institution v_source_account ->
        Rel_sql.delete_from
          Db.dialect
          table
          ~where:
            Col.
              [ Value (institution, v_institution)
              ; Value (source_account, v_source_account)
              ]
    in
    let audit_iban =
      let open Db_schema.Bank_account_identity_changes in
      let insert = Rel_sql.insert_into_cols Db.dialect table in
      fun v_institution v_source_account v_previous_iban ->
        insert
          Col.
            [ Value (institution, v_institution)
            ; Value (source_account, v_source_account)
            ; Value (previous_iban, Some v_previous_iban)
            ; Value (iban, None)
            ]
    in
    let* clears =
      traverse
        (fun row ->
           match row.Bank_account_config.iban with
           | Keep | Set _ -> Ok 0
           | Clear ->
             let r = row.reference in
             let* info = resolve r in
             let* () =
               if String.equal info.iban_source "imported"
               then Or_error.error_string "an imported source IBAN cannot be cleared"
               else Ok ()
             in
             let source = Bank_account_config.source_account r in
             let* old =
               db_result (Db.first db.conn (find_account_iban r.institution source))
             in
             (match old with
              | None -> Ok 0
              | Some iban ->
                let* () =
                  db_result (Db.exec db.conn (remove_iban r.institution source))
                in
                let* () =
                  db_result (Db.exec db.conn (audit_iban r.institution source iban))
                in
                Ok 1))
        config
    in
    let* sets =
      traverse
        (fun row ->
           match row.Bank_account_config.iban with
           | Keep | Clear -> Ok None
           | Set iban ->
             let* current = resolve row.reference in
             if Option.equal Iban.equal current.iban (Some iban)
             then Ok None
             else (
               let r = row.reference in
               let* identity =
                 Account_identity.create
                   ~institution:r.institution
                   ~bank_id:r.bank_id
                   ~account_id:r.account_id
                   ~account_type:r.account_type
                   ~iban
               in
               Ok (Some identity)))
        config
    in
    let* identity_sets =
      set_bank_account_identities_in_transaction db (List.filter_opt sets)
    in
    let set =
      Rel_sql.Stmt.(
        func
          "INSERT INTO \
           bank_account_settlements(institution,source_account,settlement_account) \
           VALUES (?,?,?) ON CONFLICT(institution,source_account) DO UPDATE SET \
           settlement_account=excluded.settlement_account"
          (text @-> text @-> text @-> unit))
    in
    let remove =
      let open Db_schema.Bank_account_settlements in
      fun v_institution v_source_account ->
        Rel_sql.delete_from
          Db.dialect
          table
          ~where:
            Col.
              [ Value (institution, v_institution)
              ; Value (source_account, v_source_account)
              ]
    in
    let audit =
      let open Db_schema.Bank_account_settlement_changes in
      let insert = Rel_sql.insert_into_cols Db.dialect table in
      fun v_institution v_source_account v_previous_settlement v_settlement_account ->
        insert
          Col.
            [ Value (institution, v_institution)
            ; Value (source_account, v_source_account)
            ; Value (previous_settlement, v_previous_settlement)
            ; Value (settlement_account, v_settlement_account)
            ]
    in
    let* settlements =
      traverse
        (fun row ->
           match row.Bank_account_config.settlement_account with
           | Keep -> Ok 0
           | update ->
             let r = row.reference in
             let source = Bank_account_config.source_account r in
             let* child = resolve r in
             let* value =
               match update with
               | Keep | Clear -> Ok None
               | Set parent ->
                 let* parent_info = resolve parent in
                 if
                   (not (String.equal child.iban_source "not_applicable"))
                   || String.equal parent_info.iban_source "not_applicable"
                   || not (Iso4217.equal child.currency parent_info.currency)
                 then
                   Or_error.error_string
                     "card settlement must link a credit-card account to a non-card \
                      account in the same currency"
                 else
                   Ok
                     (Some
                        (Yojson.Safe.to_string
                           (Bank_account_config.reference_to_json parent)))
             in
             let* previous =
               db_result (Db.first db.conn (find_settlement r.institution source))
             in
             if Option.equal String.equal previous value
             then Ok 0
             else
               let* () =
                 match value with
                 | None -> db_result (Db.exec db.conn (remove r.institution source))
                 | Some value ->
                   db_result (Db.exec db.conn (set r.institution source value))
               in
               let* () =
                 db_result (Db.exec db.conn (audit r.institution source previous value))
               in
               Ok 1)
        config
    in
    let* after_history = bank_account_history db in
    let changes =
      List.filter after_history ~f:(fun change ->
        not
          (List.exists before_history ~f:(fun old ->
             String.equal old.kind change.kind && Int64.equal old.id change.id)))
    in
    let result =
      { mapping_changes
      ; identity_changes = identity_sets + List.sum (module Int) clears ~f:Fn.id
      ; settlement_changes = List.sum (module Int) settlements ~f:Fn.id
      ; changes
      }
    in
    let* () =
      if dry_run
      then
        db_result
          (Db.exec_sql db.conn "ROLLBACK TO account_preview; RELEASE account_preview")
      else Ok ()
    in
    Ok result)
;;

let registry t = t.registry
