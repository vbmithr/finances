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

let initialize db =
  transaction db (fun db ->
    let* version = db_result (Db.first db (statement "PRAGMA user_version" integer)) in
    match version with
    | Some 0 -> db_result (Db.exec_sql db Db_schema.create)
    | Some version when version = Db_schema.version -> Ok ()
    | _ ->
      Or_error.errorf
        "unsupported database schema; this experimental build accepts only schema %d. \
         Reimport the original records into a new database"
        Db_schema.version)
;;

let with_open path f =
  let* db = db_result (Db.open' ~foreign_keys:true ~mode:Read_write_create path) in
  let result =
    try
      let* () = db_result (Db.busy_timeout_ms db 5000) in
      let* () = initialize db in
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
    db_result (Db.first db (find_source e.venue e.account e.kind e.source_id))
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
  let open Db_schema.Bank_csv_rows in
  select_by_pair table document_id row_number fields_json
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
           let open Db_schema.Wise_statements in
           select_by_pair table document_id ordinal id
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
    let open Db_schema.Bunq_day_inventories in
    select_by_pair table source_account booked_on signature
  in
  let insert_day =
    let open Db_schema.Bunq_day_inventories in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    fun v_source_account v_booked_on v_signature v_document_id ->
      insert
        Col.
          [ Value (source_account, v_source_account)
          ; Value (booked_on, v_booked_on)
          ; Value (signature, v_signature)
          ; Value (document_id, v_document_id)
          ]
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

let import_revolut db ~institution ~account_id (document : Revolut_csv.t) =
  let* document_id = archive_document db ~institution ~contents:document.contents in
  let context =
    let open Db_schema.Revolut_documents in
    select_by_column table document_id account_id
  in
  let* previous = db_result (Db.first db (context document_id)) in
  let* () =
    match previous with
    | Some old when not (String.equal old account_id) ->
      Or_error.error_string "Revolut document already belongs to a different account_id"
    | Some _ -> Ok ()
    | None ->
      let insert =
        let open Db_schema.Revolut_documents in
        let insert = Rel_sql.insert_into_cols Db.dialect table in
        fun v_document_id v_account_id ->
          insert
            Col.[ Value (document_id, v_document_id); Value (account_id, v_account_id) ]
      in
      db_result (Db.exec db (insert document_id account_id))
  in
  let days = Hashtbl.Poly.create () in
  List.iter document.transactions ~f:(fun tx ->
    Hashtbl.add_multi
      days
      ~key:(Revolut_csv.account_identity tx, Date.to_string tx.booked_on)
      ~data:tx);
  let find =
    let open Db_schema.Revolut_day_inventories in
    select_by_pair table source_account booked_on signature
  in
  let insert_day =
    let open Db_schema.Revolut_day_inventories in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    fun v_source_account v_booked_on v_signature v_document_id ->
      insert
        Col.
          [ Value (source_account, v_source_account)
          ; Value (booked_on, v_booked_on)
          ; Value (signature, v_signature)
          ; Value (document_id, v_document_id)
          ]
  in
  let* _ =
    traverse
      (fun ((account, day), transactions) ->
         let payloads =
           List.map transactions ~f:(fun tx -> tx.Revolut_csv.payload)
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
             "Revolut day %s differs from imported evidence; partial-day exports or \
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
          ~account:(Revolut_csv.account_identity tx)
          ~kind:"revolut-transaction"
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
  ingest 0 0 document.transactions
;;

let import_bank_file ?account_id db ~institution ~path ~contents =
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
  | Revolut_csv ->
    if not (String.equal institution "revolut")
    then fail Rejected "Revolut exports require --institution revolut"
    else (
      match account_id with
      | None -> fail Rejected "Revolut CSV has no account identifier; supply --account-id"
      | Some account_id ->
        let result =
          transaction db (fun db ->
            let* document_id = archive_document db ~institution ~contents in
            let* accepted =
              db_result (Db.first db (accepted_document document_id document_id))
            in
            let* document = Revolut_csv.parse ~account_id contents in
            let* imported = import_revolut db ~institution ~account_id document in
            record_attempt
              db
              ~institution
              ~path
              ~document_id:(Some imported.document_id)
              ~format
              ~outcome:
                (if Option.value accepted ~default:false then Unchanged else Imported)
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

let linked_transfer db event_id = db_result (Db.first db (find_linked_transfer event_id))

let category_account db event_id =
  let* category = db_result (Db.first db (find_category event_id)) in
  Ok (Option.map category ~f:fst)
;;

let counter_account db event_id =
  let* transfer = linked_transfer db event_id in
  match transfer with
  | Some (first, second, _) -> Ok (Some (Bank_transfer.account first second))
  | None -> category_account db event_id
;;

let mapped_bank_entry db event_id (event : Source_event.t) =
  let* bank_account = db_result (Db.first db (find_mapping event.venue event.account)) in
  let* counter_account = counter_account db event_id in
  Bank_projection.entry ?bank_account ?counter_account event
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
              let* transaction = Bank_transaction.of_event (snd (List.hd_exn matching)) in
              let account = transaction.account in
              Bank_projection.ledger_account_for ~institution:mapping.institution account
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
                   let* counter_account = counter_account db event_id in
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
                   else db_result (Db.exec db (rename_posting mapping.ledger_account id)))
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
  apply 0 mappings
;;

let set_bank_account_mappings db mappings =
  transaction db (fun db -> set_bank_account_mappings_in_transaction db mappings)
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
         Db_schema.Bank_csv_matches.(
           csv_match_row row_id event_id state reason candidates_json policy))
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
         Db_schema.Bank_csv_rows.(
           csv_row id document_id institution row_number fields_json))
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
  let open Db_schema.Bank_csv_match_changes in
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
         Db_schema.Wise_csv_rows.(
           csv_row id document_id institution row_number fields_json))
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
         Db_schema.Wise_csv_matches.(
           csv_match_row row_id event_id state reason candidates_json policy))
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
  let open Db_schema.Wise_csv_match_changes in
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
      * read_column Db_schema.Wise_statements.id
      * read_column Db_schema.Wise_statements.document_id
      * read_column Db_schema.Wise_statements.source_account
      * read_column Db_schema.Wise_statements.currency
      * read_column Db_schema.Wise_statements.opening
      * read_column Db_schema.Wise_statements.closing)
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
         let open Db_schema.Wise_statement_events in
         select_by_column table statement_id event_id
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
          let* transfer = linked_transfer db d.event_id in
          if Option.is_some transfer
          then
            Or_error.error_string
              "reject the confirmed transfer before changing its category"
          else if not (Bank_transaction.is_event event)
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
                      fun account entry_id -> update_posting account entry_id 1
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
         Db_schema.Bunq_csv_rows.(
           csv_row id document_id institution row_number fields_json))
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
         Db_schema.Bunq_csv_matches.(
           csv_match_row row_id event_id state reason candidates_json policy))
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
  let open Db_schema.Bunq_csv_match_changes in
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
         let open Db_schema.Import_document_events in
         select_by_column table document_id event_id
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

let find_account_iban =
  let open Db_schema.Bank_account_identities in
  select_by_pair table institution source_account iban
;;

let account_iban db (event : Source_event.t) (tx : Bank_transaction.t) =
  let* registered =
    db_result (Db.first db (find_account_iban event.venue event.account))
  in
  let imported =
    if String.equal tx.account.account_type "CREDITCRD"
    then None
    else Account_identity.inferred_iban tx.account.account_id
  in
  match imported, registered with
  | Some a, Some b when not (String.equal a b) ->
    Or_error.error_string "registered IBAN contradicts source account identifier"
  | Some a, _ -> Ok (Some a, "imported")
  | None, Some b -> Ok (Some b, "registered")
  | None, None ->
    Ok
      ( None
      , if String.equal tx.account.account_type "CREDITCRD"
        then "not_applicable"
        else "missing" )
;;

let counterparty_iban (event : Source_event.t) =
  if String.equal event.kind "bunq-transaction"
  then
    let* tx = Bunq_transaction.source_transaction event in
    Ok (Account_identity.inferred_iban tx.counterparty)
  else Ok None
;;

let transfer_iban_evidence db source a ta b tb =
  let* a_iban, _ = account_iban db a ta in
  let* b_iban, _ = account_iban db b tb in
  let* a_counterparty = counterparty_iban a in
  let* b_counterparty = counterparty_iban b in
  let* known =
    traverse
      (fun (_, event) ->
         let* tx = Bank_transaction.of_event event in
         let* iban, _ = account_iban db event tx in
         Ok (event, iban))
      (List.filter source ~f:(fun (_, e) -> Bank_transaction.is_event e)
       |> List.dedup_and_sort ~compare:(fun (_, a) (_, b) ->
         Tuple2.compare
           ~cmp1:String.compare
           ~cmp2:String.compare
           (a.Source_event.venue, a.account)
           (b.Source_event.venue, b.account)))
  in
  let assess counterparty target target_iban =
    match counterparty, target_iban with
    | None, _ -> None
    | Some cp, Some own when String.equal cp own ->
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
          else if Option.equal String.equal iban (Some cp)
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
  let* existing = db_result (Db.first db (find_entry event_id Bank_projection.key)) in
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
      ~compare:(Tuple2.compare ~cmp1:Int64.compare ~cmp2:Int64.compare)
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
          let* ta = Bank_transaction.of_event a in
          let* tb = Bank_transaction.of_event b in
          let* previous = db_result (Db.first db (find_transfer_decision first second)) in
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
            let* () = db_result (Db.exec db (upsert first second state d.reason)) in
            let* () =
              if confirming
              then
                let* _ =
                  traverse
                    (fun id -> db_result (Db.exec db (add id first second)))
                    [ first; second ]
                in
                Ok ()
              else db_result (Db.exec db (remove first second))
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
                          db
                          (update
                             (Option.value account ~default:"equity:unclassified")
                             entry_id)))
                entries
            in
            let* () =
              db_result
                (Db.exec
                   db
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
  ; first_transaction : Bank_transaction.t
  ; second_transaction : Bank_transaction.t
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
         let* tx = Bank_transaction.of_event e in
         let* linked = linked_transfer db id in
         Ok (id, e, tx, Option.is_some linked))
      (List.filter source ~f:(fun (_, e) -> Bank_transaction.is_event e))
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
  let claims = Int64.Table.create () in
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
       Tuple2.compare
         ~cmp1:Int64.compare
         ~cmp2:Int64.compare
         (a.first_event_id, a.second_event_id)
         (b.first_event_id, b.second_event_id)))
;;

let find_settlement =
  let open Db_schema.Bank_account_settlements in
  select_by_pair table institution source_account settlement_account
;;

type bank_account_info =
  { mapping : Bank_account_mapping.t
  ; iban : string option
  ; iban_source : string
  ; currency : string
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
           Bank_transaction.is_event e
           && String.equal e.Source_event.venue mapping.Bank_account_mapping.institution
           && String.equal e.account (Bank_account_mapping.source_account mapping))
       in
       let event = snd (List.hd_exn members) in
       let* tx = Bank_transaction.of_event event in
       let* iban, iban_source = account_iban db event tx in
       let* settlement =
         db_result
           (Db.first
              db
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
              let* tx = Bank_transaction.of_event e in
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
      ~compare:(Tuple2.compare ~cmp1:String.compare ~cmp2:String.compare)
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
        let* () =
          if String.equal account.account_type "CREDITCRD"
          then
            Or_error.error_string
              "credit-card accounts have no IBAN; register the associated bank account \
               instead"
          else Ok ()
        in
        let* () =
          match Account_identity.inferred_iban account.account_id with
          | Some old when not (String.equal old i.iban) ->
            Or_error.error_string "IBAN contradicts imported account identifier"
          | _ -> Ok ()
        in
        let* previous =
          db_result (Db.first db (find_account_iban i.institution identity))
        in
        if Option.equal String.equal previous (Some i.iban)
        then apply count rest
        else
          let* () = db_result (Db.exec db (upsert i.institution identity i.iban)) in
          let* () =
            db_result (Db.exec db (audit i.institution identity previous (Some i.iban)))
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
             let* ta = Bank_transaction.of_event a in
             let* tb = Bank_transaction.of_event b in
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
      if dry_run then db_result (Db.exec_sql db "SAVEPOINT account_preview") else Ok ()
    in
    let* before_history = bank_account_history db in
    let* info = bank_account_info db in
    let resolve r =
      Or_error.of_option
        (List.find info ~f:(fun i ->
           String.equal i.mapping.institution r.Bank_account_config.institution
           && String.equal
                (Bank_account_mapping.source_account i.mapping)
                (Bank_account_config.source_account r)))
        ~error:
          (Error.of_string
             "unknown source account; import its statements before configuring it")
    in
    let* _ =
      traverse
        (fun row ->
           let* _ = resolve row.Bank_account_config.reference in
           Ok ())
        config
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
             let* () =
               if
                 (not (String.equal r.account_type "CREDITCRD"))
                 && Option.is_some (Account_identity.inferred_iban r.account_id)
               then Or_error.error_string "an imported source IBAN cannot be cleared"
               else Ok ()
             in
             let source = Bank_account_config.source_account r in
             let* old =
               db_result (Db.first db (find_account_iban r.institution source))
             in
             (match old with
              | None -> Ok 0
              | Some iban ->
                let* () = db_result (Db.exec db (remove_iban r.institution source)) in
                let* () = db_result (Db.exec db (audit_iban r.institution source iban)) in
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
             if Option.equal String.equal current.iban (Some iban)
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
                   (not (String.equal r.account_type "CREDITCRD"))
                   || String.equal parent.account_type "CREDITCRD"
                   || not (String.equal child.currency parent_info.currency)
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
               db_result (Db.first db (find_settlement r.institution source))
             in
             if Option.equal String.equal previous value
             then Ok 0
             else
               let* () =
                 match value with
                 | None -> db_result (Db.exec db (remove r.institution source))
                 | Some value -> db_result (Db.exec db (set r.institution source value))
               in
               let* () =
                 db_result (Db.exec db (audit r.institution source previous value))
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
        db_result (Db.exec_sql db "ROLLBACK TO account_preview; RELEASE account_preview")
      else Ok ()
    in
    Ok result)
;;
