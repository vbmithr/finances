open Core
open Rel
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
let integer = Row.(t1 (int "value"))

let migrate db =
  transaction db (fun db ->
    let* version = db_result (Db.first db (statement "PRAGMA user_version" integer)) in
    match version with
    | Some 0 ->
      let* () = db_result (Db.exec_sql db Db_schema.create) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v2) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v3) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v4) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v5) in
      db_result (Db.exec_sql db Db_schema.upgrade_v6)
    | Some 1 ->
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v1) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v2) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v3) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v4) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v5) in
      db_result (Db.exec_sql db Db_schema.upgrade_v6)
    | Some 2 ->
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v2) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v3) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v4) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v5) in
      db_result (Db.exec_sql db Db_schema.upgrade_v6)
    | Some 3 ->
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v3) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v4) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v5) in
      db_result (Db.exec_sql db Db_schema.upgrade_v6)
    | Some 4 ->
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v4) in
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v5) in
      db_result (Db.exec_sql db Db_schema.upgrade_v6)
    | Some 5 ->
      let* () = db_result (Db.exec_sql db Db_schema.upgrade_v5) in
      db_result (Db.exec_sql db Db_schema.upgrade_v6)
    | Some 6 -> db_result (Db.exec_sql db Db_schema.upgrade_v6)
    | Some 7 -> Ok ()
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
  Row.(
    unit (fun id venue account kind source_id occurred_at_ms occurred_on payload ->
      id, venue, account, kind, source_id, occurred_at_ms, occurred_on, payload)
    * int64 "id"
    * text "venue"
    * text "account"
    * text "kind"
    * text "source_id"
    * col "occurred_at_ms" Type.(option int64)
    * col "occurred_on" Type.(option text)
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
      (text @-> text @-> ret Row.(t2 (int64 "id") (blob "contents"))))
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

let import_ofx_in_transaction db ~institution (document : Bank_ofx.t) =
  let* institution = Validation.text "institution" institution in
  let sha256 = Digestif.SHA256.(to_hex (digest_string document.contents)) in
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
      Ok { document_id; statements = List.length document.statements; inserted; existing }
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
  import_transactions 0 0 transactions
;;

let import_ofx db ~institution document =
  transaction db (fun db -> import_ofx_in_transaction db ~institution document)
;;

let archive_document db ~institution ~contents =
  let sha256 = Digestif.SHA256.(to_hex (digest_string contents)) in
  let* archived = db_result (Db.first db (find_document institution sha256)) in
  match archived with
  | Some (id, previous) ->
    if String.equal previous contents
    then Ok id
    else Or_error.error_string "archive checksum collision or corrupt document"
  | None ->
    let* () = db_result (Db.exec db (insert_document institution sha256 contents)) in
    Ok (Db.last_insert_rowid db)
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
    * int64 "id"
    * text "institution"
    * text "path"
    * text "format"
    * text "outcome"
    * col "document_id" Type.(option int64)
    * col "sha256" Type.(option text)
    * int "inserted_count"
    * int "existing_count"
    * col "error" Type.(option text)
    * text "attempted_at")
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
  let* format = Bank_import.format_of_string format in
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
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       bank_import_attempts(institution,path,document_id,format,outcome,inserted_count,existing_count,error) \
       VALUES (?,?,?,?,?,?,?,?)"
      (text
       @-> text
       @-> option int64
       @-> text
       @-> text
       @-> int
       @-> int
       @-> option text
       @-> unit))
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
         db
         (insert_attempt
            institution
            path
            document_id
            (Bank_import.format_name format)
            (Bank_import.outcome_name outcome)
            inserted
            existing
            message))
  in
  let* row = db_result (Db.first db (read_attempt (Db.last_insert_rowid db))) in
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

let insert_csv_row =
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       bank_csv_rows(document_id,row_number,institution,account_id,booked_on,value_on,amount,description,suggested_description,category,parent_category,comment,account_label,reported_balance,pointage,fields_json) \
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT(document_id,row_number) DO \
       NOTHING"
      (int64
       @-> int
       @-> text
       @-> text
       @-> text
       @-> option text
       @-> text
       @-> text
       @-> option text
       @-> option text
       @-> option text
       @-> option text
       @-> option text
       @-> option text
       @-> option text
       @-> text
       @-> unit))
;;

let existing_csv_row =
  Rel_sql.Stmt.(
    func
      "SELECT fields_json FROM bank_csv_rows WHERE document_id=? AND row_number=?"
      (int64 @-> int @-> ret Row.(t1 (text "fields_json"))))
;;

let import_csv_rows db ~institution ~contents rows =
  let* document_id = archive_document db ~institution ~contents in
  let rec insert inserted existing = function
    | [] -> Ok (document_id, inserted, existing)
    | (row : Bank_csv.row) :: rest ->
      let fields =
        Yojson.Safe.to_string (`List (List.map row.fields ~f:(fun s -> `String s)))
      in
      let* previous =
        db_result (Db.first db (existing_csv_row document_id row.row_number))
      in
      (match previous with
       | Some previous ->
         if String.equal fields previous
         then insert inserted (existing + 1) rest
         else Or_error.error_string "stored CSV observation differs from the archived row"
       | None ->
         let* () =
           db_result
             (Db.exec
                db
                (insert_csv_row
                   document_id
                   row.row_number
                   institution
                   row.account_id
                   (Date.to_string row.booked_on)
                   (Option.map row.value_on ~f:Date.to_string)
                   (Bigdecimal.to_string_no_sn row.amount)
                   row.description
                   row.suggested_description
                   row.category
                   row.parent_category
                   row.comment
                   row.account_label
                   (Option.map row.reported_balance ~f:Bigdecimal.to_string_no_sn)
                   row.pointage
                   fields))
         in
         insert (inserted + 1) existing rest)
  in
  insert 0 0 rows
;;

let import_wise db ~institution ~contents (document : Wise_camt.t) =
  let* document_id = archive_document db ~institution ~contents in
  let inserted = ref 0
  and existing = ref 0 in
  let* _ =
    traverse
      (fun (ordinal, (statement : Wise_camt.statement)) ->
         let insert =
           Rel_sql.Stmt.(
             func
               "INSERT INTO \
                wise_statements(document_id,ordinal,institution,source_account,statement_reference,currency,period_start,period_end,opening_at,closing_at,opening,closing) \
                VALUES (?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT(document_id,ordinal) DO \
                NOTHING"
               (int64
                @-> int
                @-> text
                @-> text
                @-> text
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
                db
                (insert
                   document_id
                   ordinal
                   institution
                   (Wise_camt.account_identity statement.account)
                   statement.statement_id
                   statement.account.currency
                   statement.period_start
                   statement.period_end
                   statement.opening_at
                   statement.closing_at
                   (Bigdecimal.to_string_no_sn statement.opening)
                   (Bigdecimal.to_string_no_sn statement.closing)))
         in
         let find =
           Rel_sql.Stmt.(
             func
               "SELECT id FROM wise_statements WHERE document_id=? AND ordinal=?"
               (int64 @-> int @-> ret Row.(t1 (int64 "id"))))
         in
         let* statement_id = db_result (Db.first db (find document_id ordinal)) in
         let* statement_id =
           Or_error.of_option
             statement_id
             ~error:(Error.of_string "missing Wise statement")
         in
         let* _ =
           traverse
             (fun (trn : Wise_camt.transaction) ->
                let* event =
                  Source_event.create
                    ~venue:institution
                    ~account:(Wise_camt.account_identity trn.account)
                    ~kind:"wise-transaction"
                    ~source_id:trn.source_id
                    ~occurred_at_ms:trn.occurred_at_ms
                    ~payload:trn.payload
                in
                let* result = ingest_in_transaction db event in
                let id =
                  match result with
                  | Inserted id ->
                    incr inserted;
                    id
                  | Existing id ->
                    incr existing;
                    id
                in
                let* () = db_result (Db.exec db (link_document document_id id)) in
                let link =
                  Rel_sql.Stmt.(
                    func
                      "INSERT INTO wise_statement_events(statement_id,event_id) VALUES \
                       (?,?) ON CONFLICT DO NOTHING"
                      (int64 @-> int64 @-> unit))
                in
                db_result (Db.exec db (link statement_id id)))
             statement.transactions
         in
         Ok ())
      (List.mapi document.statements ~f:(fun i x -> i, x))
  in
  Ok
    { document_id
    ; statements = List.length document.statements
    ; inserted = !inserted
    ; existing = !existing
    }
;;

let import_wise_csv db ~institution ~contents rows =
  let* document_id = archive_document db ~institution ~contents in
  let insert =
    Rel_sql.Stmt.(
      func
        "INSERT INTO wise_csv_rows(document_id,row_number,institution,fields_json) \
         VALUES (?,?,?,?) ON CONFLICT(document_id,row_number) DO NOTHING"
        (int64 @-> int @-> text @-> text @-> unit))
  in
  let* _ =
    traverse
      (fun (row : Wise_csv.row) ->
         db_result
           (Db.exec
              db
              (insert
                 document_id
                 row.row_number
                 institution
                 (Yojson.Safe.to_string
                    (`List (List.map row.fields ~f:(fun x -> `String x)))))))
      rows
  in
  Ok document_id
;;

let import_bunq db ~institution ~contents (document : Bunq_mt940.t) =
  let* document_id = archive_document db ~institution ~contents in
  let days = String.Table.create () in
  List.iter document.transactions ~f:(fun tx ->
    Hashtbl.add_multi days ~key:(Date.to_string tx.Bunq_transaction.booked_on) ~data:tx);
  let find =
    Rel_sql.Stmt.(
      func
        "SELECT signature FROM bunq_day_inventories WHERE source_account=? AND \
         booked_on=?"
        (text @-> text @-> ret Row.(t1 (text "signature"))))
  in
  let insert_day =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         bunq_day_inventories(source_account,booked_on,signature,document_id) VALUES \
         (?,?,?,?)"
        (text @-> text @-> text @-> int64 @-> unit))
  in
  let* _ =
    traverse
      (fun (day, transactions) ->
         let account = Bunq_transaction.account_identity (List.hd_exn transactions) in
         let payloads =
           List.map transactions ~f:(fun tx -> tx.Bunq_transaction.payload)
           |> List.sort ~compare:String.compare
         in
         let signature =
           Yojson.Safe.to_string (`List (List.map payloads ~f:(fun s -> `String s)))
           |> Digestif.SHA256.digest_string
           |> Digestif.SHA256.to_hex
         in
         let* previous = db_result (Db.first db (find account day)) in
         match previous with
         | Some old when not (String.equal old signature) ->
           Or_error.errorf
             "Bunq day %s differs from imported evidence; partial-day exports or \
              corrections need review"
             day
         | Some _ -> Ok ()
         | None -> db_result (Db.exec db (insert_day account day signature document_id)))
      (Hashtbl.to_alist days)
  in
  let rec ingest inserted existing = function
    | [] -> Ok { document_id; statements = 1; inserted; existing }
    | tx :: rest ->
      let* event =
        Source_event.create_dated
          ~venue:institution
          ~account:(Bunq_transaction.account_identity tx)
          ~kind:"bunq-transaction"
          ~source_id:tx.source_id
          ~occurred_on:tx.booked_on
          ~payload:tx.payload
      in
      let* result = ingest_in_transaction db event in
      let id, inserted, existing =
        match result with
        | Inserted id -> id, inserted + 1, existing
        | Existing id -> id, inserted, existing + 1
      in
      let* () = db_result (Db.exec db (link_document document_id id)) in
      ingest inserted existing rest
  in
  let* result = ingest 0 0 document.transactions in
  let stmt =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         bunq_statements(document_id,account_id,currency,opening_on,closing_on,opening,closing) \
         VALUES (?,?,?,?,?,?,?) ON CONFLICT(document_id) DO NOTHING"
        (int64 @-> text @-> text @-> text @-> text @-> text @-> text @-> unit))
  in
  let* () =
    db_result
      (Db.exec
         db
         (stmt
            document_id
            document.account_id
            document.currency
            (Date.to_string document.opening_on)
            (Date.to_string document.closing_on)
            (Bigdecimal.to_string_no_sn document.opening)
            (Bigdecimal.to_string_no_sn document.closing)))
  in
  Ok result
;;

let import_bunq_csv db ~institution ~contents rows =
  let* document_id = archive_document db ~institution ~contents in
  let insert =
    Rel_sql.Stmt.(
      func
        "INSERT INTO bunq_csv_rows(document_id,row_number,institution,fields_json) \
         VALUES (?,?,?,?) ON CONFLICT(document_id,row_number) DO NOTHING"
        (int64 @-> int @-> text @-> text @-> unit))
  in
  let* _ =
    traverse
      (fun (row : Bunq_csv.row) ->
         db_result
           (Db.exec
              db
              (insert
                 document_id
                 row.row_number
                 institution
                 (Yojson.Safe.to_string
                    (`List (List.map row.fields ~f:(fun s -> `String s)))))))
      rows
  in
  Ok document_id
;;

let import_bank_file db ~institution ~path ~contents =
  let* institution = Validation.text "institution" institution in
  let* path = Validation.text "path" path in
  let format = Bank_import.detect_format ~path contents in
  let fail outcome message =
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
  match format with
  | Ofx ->
    (match Bank_ofx.parse contents with
     | Error e -> fail Rejected (Error.to_string_hum e)
     | Ok document ->
       let result =
         transaction db (fun db ->
           let sha256 = Digestif.SHA256.(to_hex (digest_string contents)) in
           let* previous = db_result (Db.first db (find_document institution sha256)) in
           let* known =
             match previous with
             | None -> Ok false
             | Some (id, _) ->
               let* result = db_result (Db.first db (accepted_document id id)) in
               Ok (Option.value result ~default:false)
           in
           let* imported = import_ofx_in_transaction db ~institution document in
           record_attempt
             db
             ~institution
             ~path
             ~document_id:(Some imported.document_id)
             ~format
             ~outcome:(if known then Unchanged else Imported)
             ~inserted:imported.inserted
             ~existing:imported.existing
             ~message:None)
       in
       (match result with
        | Ok report -> Ok report
        | Error e -> fail Rejected (Error.to_string_hum e)))
  | Bunq_mt940 | Bunq_csv ->
    if not (String.equal institution "bunq")
    then fail Rejected "Bunq exports require --institution bunq"
    else (
      let result =
        transaction db (fun db ->
          let* document_id = archive_document db ~institution ~contents in
          let* accepted =
            db_result (Db.first db (accepted_document document_id document_id))
          in
          let* inserted, existing, message =
            match format with
            | Bunq_mt940 ->
              let* document = Bunq_mt940.parse contents in
              let* imported = import_bunq db ~institution ~contents document in
              Ok (imported.inserted, imported.existing, None)
            | _ ->
              let* rows = Bunq_csv.parse contents in
              let* _ = import_bunq_csv db ~institution ~contents rows in
              Ok
                ( 0
                , 0
                , Some
                    (sprintf
                       "Bunq CSV observations: %d; no transactions created"
                       (List.length rows)) )
          in
          record_attempt
            db
            ~institution
            ~path
            ~document_id:(Some document_id)
            ~format
            ~outcome:
              (if Option.value accepted ~default:false then Unchanged else Imported)
            ~inserted
            ~existing
            ~message)
      in
      match result with
      | Ok report -> Ok report
      | Error e -> fail Rejected (Error.to_string_hum e))
  | Camt053 | Wise_csv ->
    if not (String.equal institution "wise")
    then fail Rejected "Wise exports require --institution wise"
    else (
      let result =
        transaction db (fun db ->
          let* document_id = archive_document db ~institution ~contents in
          let* accepted =
            db_result (Db.first db (accepted_document document_id document_id))
          in
          let* inserted, existing, message =
            match format with
            | Camt053 ->
              let* document = Wise_camt.parse contents in
              let* result = import_wise db ~institution ~contents document in
              Ok (result.inserted, result.existing, None)
            | _ ->
              let* rows = Wise_csv.parse contents in
              let* _ = import_wise_csv db ~institution ~contents rows in
              Ok
                ( 0
                , 0
                , Some
                    (sprintf
                       "Wise CSV observations: %d; no transactions created"
                       (List.length rows)) )
          in
          record_attempt
            db
            ~institution
            ~path
            ~document_id:(Some document_id)
            ~format
            ~outcome:
              (if Option.value accepted ~default:false then Unchanged else Imported)
            ~inserted
            ~existing
            ~message)
      in
      match result with
      | Ok report -> Ok report
      | Error e -> fail Rejected (Error.to_string_hum e))
  | Csv when Bank_csv.recognizes contents ->
    (match Bank_csv.parse contents with
     | Error e -> fail Rejected (Error.to_string_hum e)
     | Ok rows ->
       let result =
         transaction db (fun db ->
           let* document_id, added, reused =
             import_csv_rows db ~institution ~contents rows
           in
           let* accepted =
             db_result (Db.first db (accepted_document document_id document_id))
           in
           record_attempt
             db
             ~institution
             ~path
             ~document_id:(Some document_id)
             ~format
             ~outcome:
               (if added = 0 && Option.value accepted ~default:false
                then Unchanged
                else Imported)
             ~inserted:0
             ~existing:0
             ~message:
               (Some
                  (sprintf
                     "CSV observations: added=%d reused=%d; no transactions created"
                     added
                     reused)))
       in
       (match result with
        | Ok report -> Ok report
        | Error e -> fail Rejected (Error.to_string_hum e)))
  | Csv ->
    fail
      Unsupported
      "CSV layout not supported; expected a supported BoursoBank, Wise or Bunq export"
  | Qif | Spreadsheet | Unknown ->
    fail Unsupported ("format not implemented: " ^ Bank_import.format_name format)
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
      ~format:Unknown
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

let posting_row = Row.(t3 (text "account") (text "commodity") (text "quantity"))

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
      (int64 @-> text @-> ret Row.(t3 (int64 "id") (text "date") (text "description"))))
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

let find_mapping =
  Rel_sql.Stmt.(
    func
      "SELECT ledger_account FROM bank_account_mappings WHERE institution=? AND \
       source_account=?"
      (text @-> text @-> ret Row.(t1 (text "ledger_account"))))
;;

let find_category =
  Rel_sql.Stmt.(
    func
      "SELECT account,reason FROM bank_categories WHERE event_id=?"
      (int64 @-> ret Row.(t2 (text "account") (text "reason"))))
;;

let category_account db event_id =
  let* category = db_result (Db.first db (find_category event_id)) in
  Ok (Option.map category ~f:fst)
;;

let mapped_bank_entry db event_id (event : Source_event.t) =
  let* bank_account = db_result (Db.first db (find_mapping event.venue event.account)) in
  let* counter_account = category_account db event_id in
  Bank_projection.entry ?bank_account ?counter_account event
;;

let record_entry db ~event_id ~key entry =
  transaction db (fun db -> record_entry_in_transaction db ~event_id ~key entry)
;;

let entries db =
  let row =
    Row.(
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
        if not (Bank_transaction.is_event event)
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
      if Bank_transaction.is_event event then Some event else None)
    |> List.dedup_and_sort ~compare:(fun a b ->
      match String.compare a.Source_event.venue b.venue with
      | 0 -> String.compare a.account b.account
      | n -> n)
  in
  traverse
    (fun (event : Source_event.t) ->
       let* transaction = Bank_transaction.of_event event in
       let account = transaction.account in
       let* mapped = db_result (Db.first db (find_mapping event.venue event.account)) in
       let* ledger_account =
         match mapped with
         | Some name -> Ok name
         | None -> Bank_projection.ledger_account_for ~institution:event.venue account
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
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       bank_account_mapping_changes(institution,source_account,previous_ledger_account,ledger_account) \
       VALUES (?,?,?,?)"
      (text @-> text @-> option text @-> text @-> unit))
;;

let rename_posting =
  Rel_sql.Stmt.(
    func
      "UPDATE postings SET account=? WHERE entry_id=? AND ordinal=0"
      (text @-> int64 @-> unit))
;;

let set_bank_account_mappings db mappings =
  let* mappings = Bank_account_mapping.of_json (Bank_account_mapping.to_json mappings) in
  transaction db (fun db ->
    let* source_events = events db in
    let rec apply changed = function
      | [] -> Ok changed
      | (mapping : Bank_account_mapping.t) :: rest ->
        let source_account = Bank_account_mapping.source_account mapping in
        let matching =
          List.filter source_events ~f:(fun (_, event) ->
            Bank_transaction.is_event event
            && String.equal event.venue mapping.institution
            && String.equal event.account source_account)
        in
        if List.is_empty matching
        then
          Or_error.error_string
            "mapping references an unknown source account; import its records first"
        else
          let* previous =
            db_result (Db.first db (find_mapping mapping.institution source_account))
          in
          if Option.equal String.equal previous (Some mapping.ledger_account)
          then apply changed rest
          else
            let* previous_name =
              match previous with
              | Some name -> Ok name
              | None ->
                let* transaction =
                  Bank_transaction.of_event (snd (List.hd_exn matching))
                in
                let account = transaction.account in
                Bank_projection.ledger_account_for
                  ~institution:mapping.institution
                  account
            in
            let* _ =
              traverse
                (fun (event_id, event) ->
                   let* existing =
                     db_result (Db.first db (find_entry event_id Bank_projection.key))
                   in
                   match existing with
                   | None -> Ok ()
                   | Some (id, date, description) ->
                     let* stored = read_entry db id date description in
                     let* counter_account = category_account db event_id in
                     let* expected =
                       Bank_projection.entry
                         ~bank_account:previous_name
                         ?counter_account
                         event
                     in
                     if not (same_entry stored expected)
                     then
                       Or_error.error_string
                         "account mapping conflicts with an existing bank projection"
                     else
                       db_result (Db.exec db (rename_posting mapping.ledger_account id)))
                matching
            in
            let* () =
              db_result
                (Db.exec
                   db
                   (upsert_mapping
                      mapping.institution
                      source_account
                      mapping.ledger_account))
            in
            let* () =
              db_result
                (Db.exec
                   db
                   (audit_mapping
                      mapping.institution
                      source_account
                      previous
                      mapping.ledger_account))
            in
            apply (changed + 1) rest
    in
    apply 0 mappings)
;;

type csv_observation =
  { row_id : int64
  ; document_id : int64
  ; institution : string
  ; row : Bank_csv.row
  ; matching : Bank_csv_match.result option
  }

type csv_report =
  { matched : int
  ; unmatched : int
  ; ambiguous : int
  ; conflicts : int
  ; changed : int
  }

let csv_row =
  Row.(
    t5
      (int64 "id")
      (int64 "document_id")
      (text "institution")
      (int "row_number")
      (text "fields_json"))
;;

let csv_match_row =
  Row.(
    unit (fun row_id event_id state reason candidates policy ->
      row_id, event_id, state, reason, candidates, policy)
    * int64 "row_id"
    * col "event_id" Type.(option int64)
    * text "state"
    * text "reason"
    * text "candidates_json"
    * text "policy")
;;

let decode_csv_match (row_id, event_id, state, reason, candidates, policy) =
  let* state = Bank_csv_match.state_of_string state in
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
  Ok ({ Bank_csv_match.row_id; event_id; state; reason; candidates }, policy)
;;

let read_csv_matches db =
  let* rows =
    collect
      db
      (statement
         "SELECT row_id,event_id,state,reason,candidates_json,policy FROM \
          bank_csv_matches ORDER BY row_id"
         csv_match_row)
  in
  traverse decode_csv_match rows
;;

let read_csv_inputs db =
  let* rows =
    collect
      db
      (statement
         "SELECT id,document_id,institution,row_number,fields_json FROM bank_csv_rows \
          ORDER BY document_id,row_number"
         csv_row)
  in
  traverse
    (fun (row_id, document_id, institution, row_number, fields) ->
       let* fields =
         Or_error.try_with (fun () ->
           match Yojson.Safe.from_string fields with
           | `List fields ->
             List.map fields ~f:(function
               | `String s -> s
               | _ -> failwith "invalid CSV field")
           | _ -> failwith "invalid stored CSV row")
       in
       let* row = Bank_csv.of_fields ~row_number fields in
       Ok { Bank_csv_match.row_id; document_id; institution; row })
    rows
;;

let bank_csv_observations db =
  let* inputs = read_csv_inputs db in
  let* matches = read_csv_matches db in
  let matching = Int64.Table.create () in
  List.iter matches ~f:(fun (result, _) ->
    Hashtbl.set matching ~key:result.row_id ~data:result);
  Ok
    (List.map inputs ~f:(fun (input : Bank_csv_match.input) ->
       { row_id = input.row_id
       ; document_id = input.document_id
       ; institution = input.institution
       ; row = input.row
       ; matching = Hashtbl.find matching input.row_id
       }))
;;

let upsert_csv_match =
  Rel_sql.Stmt.(
    func
      "INSERT INTO bank_csv_matches(row_id,event_id,state,reason,candidates_json,policy) \
       VALUES (?,?,?,?,?,?) ON CONFLICT(row_id) DO UPDATE SET \
       event_id=excluded.event_id,state=excluded.state,reason=excluded.reason,candidates_json=excluded.candidates_json,policy=excluded.policy,updated_at=strftime('%Y-%m-%dT%H:%M:%fZ','now')"
      (int64 @-> option int64 @-> text @-> text @-> text @-> text @-> unit))
;;

let audit_csv_match =
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       bank_csv_match_changes(row_id,previous_event_id,event_id,previous_state,state,reason,candidates_json,policy) \
       VALUES (?,?,?,?,?,?,?,?)"
      (int64
       @-> option int64
       @-> option int64
       @-> option text
       @-> text
       @-> text
       @-> text
       @-> text
       @-> unit))
;;

let save_csv_results
      db
      ~matching_policy
      ~upsert
      ~audit
      (previous_rows : (Bank_csv_match.result * string) list)
      results
  =
  let previous = Int64.Table.create () in
  List.iter previous_rows ~f:(fun (result, policy) ->
    Hashtbl.set previous ~key:result.row_id ~data:(result, policy));
  let equal (a : Bank_csv_match.result) (b : Bank_csv_match.result) =
    Option.equal Int64.equal a.event_id b.event_id
    && Poly.equal a.state b.state
    && String.equal a.reason b.reason
    && List.equal Int64.equal a.candidates b.candidates
  in
  let rec save changed = function
    | [] -> Ok changed
    | (result : Bank_csv_match.result) :: rest ->
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
        let state = Bank_csv_match.state_name result.state in
        let* () =
          db_result
            (Db.exec
               db
               (upsert
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
               db
               (audit
                  result.row_id
                  (Option.bind old ~f:(fun (row, _) -> row.event_id))
                  result.event_id
                  (Option.map old ~f:(fun (row, _) -> Bank_csv_match.state_name row.state))
                  state
                  result.reason
                  candidates
                  matching_policy))
        in
        save (changed + 1) rest)
  in
  let* changed = save 0 results in
  let count state =
    List.count results ~f:(fun result -> Poly.equal result.Bank_csv_match.state state)
  in
  Ok
    { matched = count Matched
    ; unmatched = count Unmatched
    ; ambiguous = count Ambiguous
    ; conflicts = count Conflict
    ; changed
    }
;;

let match_bank_csv db =
  transaction db (fun db ->
    let* inputs = read_csv_inputs db in
    let* source_events = events db in
    let* candidates =
      traverse
        (fun (event_id, (event : Source_event.t)) ->
           let* account, trn = Bank_ofx.source_transaction event in
           Ok
             { Bank_csv_match.event_id
             ; institution = event.venue
             ; account_id = account.account_id
             ; booked_on = trn.booked_on
             ; amount = trn.amount
             ; descriptions = List.filter_opt [ trn.name; trn.memo ]
             })
        (List.filter source_events ~f:(fun (_, e) ->
           String.equal e.Source_event.kind "bank-transaction"))
    in
    let results = Bank_csv_match.match_rows inputs candidates in
    let* previous_rows = read_csv_matches db in
    save_csv_results
      db
      ~matching_policy:Bank_csv_match.policy
      ~upsert:upsert_csv_match
      ~audit:audit_csv_match
      previous_rows
      results)
;;

let wise_transactions db =
  let* source = events db in
  traverse
    (fun (id, event) ->
       let* trn = Wise_camt.source_transaction event in
       Ok (id, trn))
    (List.filter source ~f:(fun (_, e) ->
       String.equal e.Source_event.kind "wise-transaction"))
;;

let read_wise_inputs db =
  let* rows =
    collect
      db
      (statement
         "SELECT id,document_id,institution,row_number,fields_json FROM wise_csv_rows \
          ORDER BY document_id,row_number"
         csv_row)
  in
  traverse
    (fun (row_id, document_id, _, row_number, fields) ->
       let* fields =
         Or_error.try_with (fun () ->
           Yojson.Safe.from_string fields
           |> Yojson.Safe.Util.to_list
           |> List.map ~f:Yojson.Safe.Util.to_string)
       in
       let* row = Wise_csv.of_fields ~row_number fields in
       Ok { Wise_csv.row_id; document_id; row })
    rows
;;

let read_wise_matches db =
  let* rows =
    collect
      db
      (statement
         "SELECT row_id,event_id,state,reason,candidates_json,policy FROM \
          wise_csv_matches ORDER BY row_id"
         csv_match_row)
  in
  traverse decode_csv_match rows
;;

let upsert_wise_match =
  Rel_sql.Stmt.(
    func
      "INSERT INTO wise_csv_matches(row_id,event_id,state,reason,candidates_json,policy) \
       VALUES (?,?,?,?,?,?) ON CONFLICT(row_id) DO UPDATE SET \
       event_id=excluded.event_id,state=excluded.state,reason=excluded.reason,candidates_json=excluded.candidates_json,policy=excluded.policy,updated_at=strftime('%Y-%m-%dT%H:%M:%fZ','now')"
      (int64 @-> option int64 @-> text @-> text @-> text @-> text @-> unit))
;;

let audit_wise_match =
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       wise_csv_match_changes(row_id,previous_event_id,event_id,previous_state,state,reason,candidates_json,policy) \
       VALUES (?,?,?,?,?,?,?,?)"
      (int64
       @-> option int64
       @-> option int64
       @-> option text
       @-> text
       @-> text
       @-> text
       @-> text
       @-> unit))
;;

let match_wise_csv db =
  transaction db (fun db ->
    let* inputs = read_wise_inputs db in
    let* candidates = wise_transactions db in
    let results = Wise_csv.match_rows inputs candidates in
    let* previous_rows = read_wise_matches db in
    save_csv_results
      db
      ~matching_policy:"wise-csv-v1"
      ~upsert:upsert_wise_match
      ~audit:audit_wise_match
      previous_rows
      results)
;;

type wise_csv_observation =
  { row_id : int64
  ; document_id : int64
  ; row : Wise_csv.row
  ; matching : Bank_csv_match.result option
  }

let wise_csv_observations db =
  let* inputs = read_wise_inputs db in
  let* matches = read_wise_matches db in
  Ok
    (List.map inputs ~f:(fun (input : Wise_csv.input) ->
       { row_id = input.row_id
       ; document_id = input.document_id
       ; row = input.row
       ; matching =
           List.find_map matches ~f:(fun (m, _) ->
             if Int64.equal m.row_id input.row_id then Some m else None)
       }))
;;

type wise_reconciliation =
  { statement_id : int64
  ; document_id : int64
  ; source_account : string
  ; currency : string
  ; opening : Bigdecimal.t
  ; closing : Bigdecimal.t
  ; movements : Bigdecimal.t
  ; entries : int
  ; difference : Bigdecimal.t
  }

let wise_reconciliations db =
  let row =
    Row.(
      unit (fun id doc account currency opening closing ->
        id, doc, account, currency, opening, closing)
      * int64 "id"
      * int64 "document_id"
      * text "source_account"
      * text "currency"
      * text "opening"
      * text "closing")
  in
  let* statements =
    collect
      db
      (statement
         "SELECT id,document_id,source_account,currency,opening,closing FROM \
          wise_statements ORDER BY id"
         row)
  in
  let* transactions = wise_transactions db in
  traverse
    (fun (statement_id, document_id, source_account, currency, opening, closing) ->
       let find =
         Rel_sql.Stmt.(
           func
             "SELECT event_id FROM wise_statement_events WHERE statement_id=?"
             (int64 @-> ret Row.(t1 (int64 "event_id"))))
       in
       let* ids = collect db (find statement_id) in
       let* amounts =
         traverse
           (fun id ->
              match List.Assoc.find transactions id ~equal:Int64.equal with
              | None -> Or_error.error_string "missing Wise statement movement"
              | Some trn -> Ok trn.Wise_camt.amount)
           ids
       in
       let* opening, closing =
         Or_error.try_with (fun () ->
           Bigdecimal.of_string opening, Bigdecimal.of_string closing)
       in
       let movements = List.fold amounts ~init:Bigdecimal.zero ~f:Bigdecimal.( + ) in
       let difference = Bigdecimal.(opening + movements - closing) in
       Ok
         { statement_id
         ; document_id
         ; source_account
         ; currency
         ; opening
         ; closing
         ; movements
         ; entries = List.length ids
         ; difference
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
          if not (Bank_transaction.is_event event)
          then Or_error.error_string "category decision requires a bank event"
          else
            let* previous = db_result (Db.first db (find_category d.event_id)) in
            if
              Option.equal
                (Tuple2.equal ~eq1:String.equal ~eq2:String.equal)
                previous
                (Some (d.account, d.reason))
            then apply changed rest
            else
              let* existing =
                db_result (Db.first db (find_entry d.event_id Bank_projection.key))
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
                      Rel_sql.Stmt.(
                        func
                          "UPDATE postings SET account=? WHERE entry_id=? AND ordinal=1"
                          (text @-> int64 @-> unit))
                    in
                    db_result (Db.exec db (update d.account id)))
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
                Rel_sql.Stmt.(
                  func
                    "INSERT INTO \
                     bank_category_changes(event_id,previous_account,account,reason) \
                     VALUES (?,?,?,?)"
                    (int64 @-> option text @-> text @-> text @-> unit))
              in
              let* () = db_result (Db.exec db (upsert d.event_id d.account d.reason)) in
              let* () =
                db_result
                  (Db.exec
                     db
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
      * int64 "id"
      * int64 "event_id"
      * col "previous_account" Type.(option text)
      * text "account"
      * text "reason"
      * text "changed_at")
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
  ; transaction : Bank_transaction.t
  ; counter_account : string
  ; reason : string option
  }

let bank_review db =
  let* source = events db in
  traverse
    (fun (event_id, event) ->
       let* transaction = Bank_transaction.of_event event in
       let* category = db_result (Db.first db (find_category event_id)) in
       Ok
         { event_id
         ; institution = event.venue
         ; transaction
         ; counter_account =
             Option.value_map category ~default:"equity:unclassified" ~f:fst
         ; reason = Option.map category ~f:snd
         })
    (List.filter source ~f:(fun (_, e) -> Bank_transaction.is_event e))
;;

let bunq_transactions db =
  let* source = events db in
  traverse
    (fun (id, event) ->
       let* trn = Bunq_transaction.source_transaction event in
       Ok (id, trn))
    (List.filter source ~f:(fun (_, e) ->
       String.equal e.Source_event.kind "bunq-transaction"))
;;

let read_bunq_inputs db =
  let* rows =
    collect
      db
      (statement
         "SELECT id,document_id,institution,row_number,fields_json FROM bunq_csv_rows \
          ORDER BY document_id,row_number"
         csv_row)
  in
  traverse
    (fun (row_id, document_id, _, row_number, fields) ->
       let* fields =
         Or_error.try_with (fun () ->
           Yojson.Safe.from_string fields
           |> Yojson.Safe.Util.to_list
           |> List.map ~f:Yojson.Safe.Util.to_string)
       in
       let* row = Bunq_csv.of_fields ~row_number fields in
       Ok { Bunq_csv.row_id; document_id; row })
    rows
;;

let read_bunq_matches db =
  let* rows =
    collect
      db
      (statement
         "SELECT row_id,event_id,state,reason,candidates_json,policy FROM \
          bunq_csv_matches ORDER BY row_id"
         csv_match_row)
  in
  traverse decode_csv_match rows
;;

let upsert_bunq_match =
  Rel_sql.Stmt.(
    func
      "INSERT INTO bunq_csv_matches(row_id,event_id,state,reason,candidates_json,policy) \
       VALUES (?,?,?,?,?,?) ON CONFLICT(row_id) DO UPDATE SET \
       event_id=excluded.event_id,state=excluded.state,reason=excluded.reason,candidates_json=excluded.candidates_json,policy=excluded.policy,updated_at=strftime('%Y-%m-%dT%H:%M:%fZ','now')"
      (int64 @-> option int64 @-> text @-> text @-> text @-> text @-> unit))
;;

let audit_bunq_match =
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       bunq_csv_match_changes(row_id,previous_event_id,event_id,previous_state,state,reason,candidates_json,policy) \
       VALUES (?,?,?,?,?,?,?,?)"
      (int64
       @-> option int64
       @-> option int64
       @-> option text
       @-> text
       @-> text
       @-> text
       @-> text
       @-> unit))
;;

let match_bunq_csv db =
  transaction db (fun db ->
    let* inputs = read_bunq_inputs db in
    let* candidates = bunq_transactions db in
    let results = Bunq_csv.match_rows inputs candidates in
    let* previous_rows = read_bunq_matches db in
    save_csv_results
      db
      ~matching_policy:"bunq-csv-v1"
      ~upsert:upsert_bunq_match
      ~audit:audit_bunq_match
      previous_rows
      results)
;;

type bunq_csv_observation =
  { row_id : int64
  ; document_id : int64
  ; row : Bunq_csv.row
  ; matching : Bank_csv_match.result option
  }

let bunq_csv_observations db =
  let* inputs = read_bunq_inputs db in
  let* matches = read_bunq_matches db in
  Ok
    (List.map inputs ~f:(fun (input : Bunq_csv.input) ->
       { row_id = input.row_id
       ; document_id = input.document_id
       ; row = input.row
       ; matching =
           List.find_map matches ~f:(fun (m, _) ->
             if Int64.equal m.row_id input.row_id then Some m else None)
       }))
;;

type bunq_reconciliation =
  { document_id : int64
  ; account_id : string
  ; currency : string
  ; entries : int
  ; difference : Bigdecimal.t
  }

let bunq_reconciliations db =
  let* statements =
    collect
      db
      (statement
         "SELECT document_id,account_id,currency,opening,closing FROM bunq_statements \
          ORDER BY document_id"
         Row.(
           t5
             (int64 "document_id")
             (text "account_id")
             (text "currency")
             (text "opening")
             (text "closing")))
  in
  let* source = bunq_transactions db in
  traverse
    (fun (document_id, account_id, currency, opening, closing) ->
       let find =
         Rel_sql.Stmt.(
           func
             "SELECT event_id FROM import_document_events WHERE document_id=?"
             (int64 @-> ret Row.(t1 (int64 "event_id"))))
       in
       let* ids = collect db (find document_id) in
       let* amounts =
         traverse
           (fun id ->
              match List.Assoc.find source id ~equal:Int64.equal with
              | None -> Or_error.error_string "missing Bunq statement event"
              | Some tx -> Ok tx.Bunq_transaction.amount)
           ids
       in
       let* difference =
         Or_error.try_with (fun () ->
           Bigdecimal.(
             of_string opening + List.fold amounts ~init:zero ~f:( + ) - of_string closing))
       in
       Ok { document_id; account_id; currency; entries = List.length ids; difference })
    statements
;;
