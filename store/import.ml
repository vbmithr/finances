open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Setup
open Sources

let document_contexts db ~institution =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT c.context_json FROM document_contexts c JOIN import_documents d ON \
         d.id=c.document_id WHERE d.institution=? ORDER BY d.id DESC"
        (text @-> ret Row.(t1 (text "context_json"))))
  in
  collect db (query institution)
;;

let classification_rules db =
  let* rows =
    collect
      db
      (statement
         "SELECT \
          mode,account_type,priority,name,pattern,institution,match_type,case_mode,action_kind,target,action_type,field \
          FROM classification_rules ORDER BY priority,name"
         Db_schema.Classification_rules.row)
  in
  traverse
    (fun (r : Db_schema.Classification_rules.t) ->
       let* case_sensitive =
         match r.case_mode with
         | "sensitive" -> Ok true
         | "insensitive" -> Ok false
         | _ -> Or_error.error_string "unknown rule case mode"
       in
       let* action =
         Classification_rule.action_of_fields r.action_kind r.target r.action_type
       in
       let* mode =
         match r.mode with
         | "automatic" -> Ok Classification_rule.Automatic
         | "review" -> Ok Classification_rule.Review
         | "disabled" -> Ok Classification_rule.Disabled
         | _ -> Or_error.error_string "unknown rule application mode"
       in
       Classification_rule.create
         ~mode
         ~account_type:r.account_type
         ~priority:r.priority
         ~name:r.name
         ~pattern:r.pattern
         ~institution:r.institution
         ~match_type:(Option.map r.match_type ~f:Classification_rule.condition_of_string)
         ~case_sensitive
         ~action
         ?field:r.field
         ())
    rows
;;

let registered_companions ?institution db =
  List.filter (Registry.companions db.registry) ~f:(fun (owner, _) ->
    Option.for_all institution ~f:(String.equal owner))
;;

let add_classification_rule db (rule : Classification_rule.t) =
  let* rule =
    Classification_rule.create
      ~mode:rule.mode
      ~account_type:rule.account_type
      ~priority:rule.priority
      ~name:rule.name
      ~pattern:rule.pattern
      ~institution:rule.institution
      ~match_type:rule.match_type
      ~case_sensitive:rule.case_sensitive
      ~action:rule.action
      ?field:rule.field
      ()
  in
  Hashtbl.clear db.source_typing_cache;
  Hashtbl.clear db.source_evidence_cache;
  let open Db_schema.Classification_rules in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  db_result
    (Db.exec
       db.conn
       (insert
          Col.
            [ Value
                ( mode
                , match rule.mode with
                  | Automatic -> "automatic"
                  | Review -> "review"
                  | Disabled -> "disabled" )
            ; Value (account_type, rule.account_type)
            ; Value (priority, rule.priority)
            ; Value (Db_schema.Classification_rules.name, rule.name)
            ; Value (pattern, rule.pattern)
            ; Value (institution, rule.institution)
            ; Value
                ( match_type
                , Option.map rule.match_type ~f:Classification_rule.condition_name )
            ; Value (case_mode, if rule.case_sensitive then "sensitive" else "insensitive")
            ; Value (action_kind, Classification_rule.action_name rule.action)
            ; Value (target, Classification_rule.action_target rule.action)
            ; Value
                ( action_type
                , match rule.action with
                  | Type_and_category (t, _) -> Some (Movement_type.to_string t)
                  | _ -> None )
            ; Value (field, rule.field)
            ]))
;;

let set_institution_definition
      ?(dry_run = false)
      db
      ~name
      ~definition
      ~expected
      ~expected_rules
      ~rules
      ~source_unchanged
  =
  transaction db (fun db ->
    let* definitions = institution_definitions db in
    let current = List.Assoc.find definitions name ~equal:String.equal in
    if not (Option.equal String.equal current expected)
    then
      Or_error.error_string "institution definition changed since preview; review again"
    else
      let* existing_rules = classification_rules db in
      if not (Poly.equal existing_rules expected_rules)
      then Or_error.error_string "rules changed since preview; review again"
      else
        let* names = institutions db in
        if
          not
            (List.mem names name ~equal:String.equal
             || Registry.is_registered db.registry name)
        then Or_error.errorf "unknown institution: %s" name
        else (
          let count =
            Rel_sql.Stmt.(
              func
                "SELECT (SELECT count(*) FROM source_events WHERE venue=?) + (SELECT \
                 count(*) FROM companion_rows WHERE institution=?)"
                (text @-> text @-> ret integer))
          in
          let* count = db_result (Db.first db.conn (count name name)) in
          if
            Option.value count ~default:0 > 0
            && (not source_unchanged)
            && not (Option.equal String.equal current (Some definition))
          then
            Or_error.error_string
              "institution has imported evidence; remove it before changing the \
               definition"
          else if dry_run
          then
            Ok
              (List.count rules ~f:(fun (r : Classification_rule.t) ->
                 not
                   (List.exists existing_rules ~f:(fun old ->
                      String.equal old.name r.name))))
          else (
            let sql =
              Rel_sql.Stmt.(
                func
                  "INSERT INTO institution_definitions(name,definition_json) VALUES \
                   (?,?) ON CONFLICT(name) DO UPDATE SET \
                   definition_json=excluded.definition_json"
                  (text @-> text @-> unit))
            in
            let* () = db_result (Db.exec db.conn (sql name definition)) in
            let missing =
              List.filter rules ~f:(fun (r : Classification_rule.t) ->
                not
                  (List.exists existing_rules ~f:(fun old -> String.equal old.name r.name)))
            in
            let* () =
              Or_error.combine_errors_unit
                (List.map missing ~f:(add_classification_rule db))
            in
            Ok (List.length missing))))
;;

let delete_classification_rule db rule_name =
  Hashtbl.clear db.source_typing_cache;
  Hashtbl.clear db.source_evidence_cache;
  transaction db (fun db ->
    let* rules = classification_rules db in
    if
      not
        (List.exists rules ~f:(fun rule ->
           String.equal rule.Classification_rule.name rule_name))
    then Or_error.errorf "unknown rule: %s" rule_name
    else
      let open Db_schema.Classification_rules in
      let query =
        Rel_sql.delete_from
          Db.dialect
          table
          ~where:Col.[ Value (Db_schema.Classification_rules.name, rule_name) ]
      in
      db_result (Db.exec db.conn query))
;;

let replace_classification_rule db (rule : Classification_rule.t) =
  Hashtbl.clear db.source_typing_cache;
  Hashtbl.clear db.source_evidence_cache;
  transaction db (fun db ->
    let* rules = classification_rules db in
    if
      not
        (List.exists rules ~f:(fun existing ->
           String.equal existing.Classification_rule.name rule.name))
    then Or_error.errorf "unknown rule: %s" rule.name
    else (
      let query =
        Rel_sql.Stmt.(
          func "DELETE FROM classification_rules WHERE name=?" (text @-> unit))
      in
      let* () = db_result (Db.exec db.conn (query rule.name)) in
      add_classification_rule db rule))
;;

let set_classification_rule_mode db name mode =
  Hashtbl.clear db.source_typing_cache;
  Hashtbl.clear db.source_evidence_cache;
  transaction db (fun db ->
    let* rules = classification_rules db in
    let* rule =
      List.find rules ~f:(fun rule -> String.equal rule.Classification_rule.name name)
      |> Or_error.of_option ~error:(Error.of_string "unknown rule")
    in
    let* () =
      match mode, rule.action with
      | Classification_rule.Review, (Card_reference | Withdrawal_reversal_hint) ->
        Or_error.error_string "evidence rules must be automatic or disabled"
      | Classification_rule.Review, Type _ when Option.is_some rule.field ->
        Or_error.error_string "a type rule that names a field must be automatic"
      | Classification_rule.Automatic, Settlement _ ->
        Or_error.error_string "settlement rules require review"
      | _ -> Ok ()
    in
    let value =
      match mode with
      | Automatic -> "automatic"
      | Review -> "review"
      | Disabled -> "disabled"
    in
    let query =
      Rel_sql.Stmt.(
        func "UPDATE classification_rules SET mode=? WHERE name=?" (text @-> text @-> unit))
    in
    db_result (Db.exec db.conn (query value name)))
;;

let rename_classification_rule db ~old_name ~new_name =
  transaction db (fun db ->
    let* rules = classification_rules db in
    match
      List.find rules ~f:(fun rule -> String.equal rule.Classification_rule.name old_name)
    with
    | None -> Or_error.errorf "unknown rule: %s" old_name
    | Some rule ->
      if
        List.exists rules ~f:(fun rule ->
          String.equal rule.Classification_rule.name new_name)
      then Or_error.errorf "rule already exists: %s" new_name
      else
        let* renamed =
          Classification_rule.create
            ~mode:rule.mode
            ~account_type:rule.account_type
            ~priority:rule.priority
            ~name:new_name
            ~pattern:rule.pattern
            ~institution:rule.institution
            ~match_type:rule.match_type
            ~case_sensitive:rule.case_sensitive
            ~action:rule.action
            ?field:rule.field
            ()
        in
        let* () = add_classification_rule db renamed in
        let open Db_schema.Classification_rules in
        let query =
          Rel_sql.delete_from
            Db.dialect
            table
            ~where:Col.[ Value (Db_schema.Classification_rules.name, old_name) ]
        in
        db_result (Db.exec db.conn query))
;;

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
let encode_fields = Json.encode (Jsont.list Jsont.string)
let decode_fields = Json.decode (Jsont.list Jsont.string)

let store_companion_rows db ~document_id ~institution ~format ~context ~companions rows =
  let* (companion : Companion.t) =
    List.find companions ~f:(fun (c : Companion.t) -> String.equal c.format format)
    |> Or_error.of_option ~error:(Error.of_string "unregistered companion format")
  in
  let find =
    let open Db_schema.Companion_rows in
    select_by_pair table document_id row_number fields_json
  in
  let insert =
    let open Db_schema.Companion_rows in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    fun v_document_id v_row_number v_institution v_format v_fields v_display_name ->
      insert
        Col.
          [ Value (document_id, v_document_id)
          ; Value (row_number, v_row_number)
          ; Value (institution, v_institution)
          ; Value (format, v_format)
          ; Value (fields_json, v_fields)
          ; Value (display_name, v_display_name)
          ]
  in
  (* The row is not stored yet, so it has no ID; annotating never reads one. *)
  let display_name row_number fields =
    match companion.display_name with
    | No_display_name -> Ok None
    | Stored_field _ | From_annotation ->
      let* annotation =
        companion.annotate
          { row_id = 0L; document_id; institution; format; row_number; fields; context }
      in
      Ok
        (Option.bind annotation.display_name ~f:(fun name ->
           let name = String.strip name in
           Option.some_if (not (String.is_empty name)) name))
  in
  let rec store added reused = function
    | [] -> Ok (added, reused)
    | (row_number, raw) :: rest ->
      let fields = encode_fields raw in
      let* previous = db_result (Db.first db.conn (find document_id row_number)) in
      (match previous with
       | Some previous when String.equal previous fields -> store added (reused + 1) rest
       | Some _ ->
         Or_error.error_string
           "stored companion observation differs from the archived row"
       | None ->
         let* name = display_name row_number raw in
         let* () =
           db_result
             (Db.exec
                db.conn
                (insert document_id row_number institution format fields name))
         in
         store (added + 1) reused rest)
  in
  store 0 0 rows
;;

let store_balance db ~document_id ~institution (b : Document.balance) =
  let insert =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         stated_balances(document_id,institution,account,balance_type,commodity_json,quantity,as_of) \
         VALUES (?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"
        (int64
         @-> text
         @-> text
         @-> text
         @-> text
         @-> text
         @-> Db_schema.statement_time
         @-> unit))
  in
  db_result
    (Db.exec
       db.conn
       (insert
          document_id
          institution
          b.account
          (Records.balance_type_to_string b.balance_type)
          (Json.encode Commodity.codec b.commodity)
          (Quantity.to_string b.quantity)
          b.as_of))
;;

(* Statement dates and instants use Rel codecs at the SQLite boundary. *)
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
         @-> Db_schema.statement_time
         @-> Db_schema.statement_time
         @-> Db_schema.statement_time
         @-> Db_schema.statement_time
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

let store_total db ~document_id ~ordinal ~institution (claim : Document.total_claim) =
  let insert =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         statement_totals(document_id,ordinal,institution,source_account,reference,commodity_json,quantity,complete) \
         VALUES (?,?,?,?,?,?,?,?) ON CONFLICT(document_id,ordinal) DO NOTHING"
        (int64 @-> int @-> text @-> text @-> text @-> text @-> text @-> bool @-> unit))
  in
  let* () =
    db_result
      (Db.exec
         db.conn
         (insert
            document_id
            ordinal
            institution
            claim.source_account
            claim.reference
            (Json.encode Commodity.codec claim.amount.commodity)
            (Quantity.to_string claim.amount.quantity)
            claim.complete))
  in
  let find =
    let open Db_schema.Statement_totals in
    select_by_pair table document_id ordinal id
  in
  let* id = db_result (Db.first db.conn (find document_id ordinal)) in
  let* id = Or_error.of_option id ~error:(Error.of_string "missing statement total") in
  let link =
    Rel_sql.Stmt.(
      func
        "INSERT INTO statement_total_events(total_id,event_id) VALUES (?,?) ON CONFLICT \
         DO NOTHING"
        (int64 @-> int64 @-> unit))
  in
  List.fold_result claim.events ~init:() ~f:(fun () event ->
    let* stored =
      db_result
        (Db.first
           db.conn
           (find_source event.venue event.account event.kind event.source_id))
    in
    let* stored =
      Or_error.of_option stored ~error:(Error.of_string "missing total event")
    in
    db_result (Db.exec db.conn (link id stored.id)))
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
         | Temporal.Date day ->
           Ok
             (Hashtbl.add_multi
                groups
                ~key:(e.account, Date.to_string day)
                ~data:e.payload)
         | Temporal.Instant _ ->
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
           (* A record's details are enrichment: only what identifies it signs the day. *)
           encode_fields
             (List.sort
                (List.map payloads ~f:Records.identity_text)
                ~compare:String.compare)
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

(* A records file can name its original source, but that name alone is not
   proof of a replay. Compare it to archived source bytes, and require every
   source event to be exactly the same as in one already accepted document. *)
type representation =
  | Source_file of string
  | Records_file of string option

let representation ~institution contents =
  if Records.is_records_of ~institution contents
  then (
    let source =
      match Records.of_string contents with
      | Ok [ section ] ->
        (match section.sources with
         | [ source ] -> Some source.sha256
         | _ -> None)
      | _ -> None
    in
    Records_file source)
  else Source_file Digestif.SHA256.(digest_string contents |> to_hex)
;;

let same_source a b =
  match a, b with
  | Source_file digest, Records_file (Some stated)
  | Records_file (Some stated), Source_file digest -> String.equal digest stated
  | _ -> false
;;

let complete_representation_replay db ~institution ~document_id ~contents events =
  let* existing =
    traverse
      (fun (event : Source_event.t) ->
         let* found =
           db_result
             (Db.first
                db.conn
                (find_source event.venue event.account event.kind event.source_id))
         in
         let occurred_at, occurred_on =
           match event.occurred with
           | Temporal.Instant at -> Some at, None
           | Temporal.Date day -> None, Some (Date.to_string day)
         in
         Ok
           (Option.bind found ~f:(fun stored ->
              Option.some_if
                (Option.equal Ptime.equal occurred_at stored.occurred_at
                 && Option.equal String.equal occurred_on stored.occurred_on
                 && String.equal event.payload stored.payload)
                stored.id)))
      events
  in
  match Option.all existing with
  | None | Some [] -> Ok false
  | Some ids ->
    let ids = List.sort ids ~compare:Int64.compare in
    if List.contains_dup ids ~compare:Int64.compare
    then Ok false
    else (
      let candidates =
        Rel_sql.Stmt.(
          func
            "SELECT d.id,d.contents FROM import_documents d JOIN import_document_events \
             e ON e.document_id=d.id WHERE e.event_id=? AND d.institution=? AND d.id<>?"
            (int64 @-> text @-> int64 @-> ret Row.(t2 (int64 "id") (blob "contents"))))
      in
      let event_ids =
        Rel_sql.Stmt.(
          func
            "SELECT event_id FROM import_document_events WHERE document_id=? ORDER BY \
             event_id"
            (int64 @-> ret Row.(t1 (int64 "event_id"))))
      in
      let* candidates =
        collect db (candidates (List.hd_exn ids) institution document_id)
      in
      let incoming = representation ~institution contents in
      let rec find = function
        | [] -> Ok false
        | (id, archived) :: rest ->
          if not (same_source incoming (representation ~institution archived))
          then find rest
          else
            let* linked = collect db (event_ids id) in
            if List.equal Int64.equal ids linked then Ok true else find rest
      in
      find candidates)
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
          String.equal e.venue institution
          && (List.mem A.kinds e.kind ~equal:String.equal
              || List.mem A.operation_kinds e.kind ~equal:String.equal))
      then Ok ()
      else Or_error.errorf "the %s adapter produced an event it does not own" institution
    in
    (* An operation overlap is allowed only for a complete alternate
       representation of the same source. Normal ingestion still checks the
       stored operation proposal before accepting the document. *)
    let* () =
      if Option.value accepted ~default:false
      then Ok ()
      else (
        let operations =
          List.filter all_events ~f:(fun (e : Source_event.t) ->
            Registry.is_operation_kind db.registry e.kind)
        in
        let* overlaps =
          traverse
            (fun (e : Source_event.t) ->
               db_result
                 (Db.first db.conn (find_source e.venue e.account e.kind e.source_id)))
            operations
        in
        if List.for_all overlaps ~f:Option.is_none
        then Ok ()
        else
          let* replay =
            complete_representation_replay
              db
              ~institution
              ~document_id
              ~contents
              all_events
          in
          if replay
          then Ok ()
          else
            Or_error.error_string
              "a source row already exists in another document; overlap is ambiguous")
    in
    let* () =
      if List.is_empty document.context
      then Ok ()
      else
        bind_document_context
          db
          ~document_id
          (Json.encode
             Json.string_map
             (List.sort document.context ~compare:[%compare: string * string]))
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
    let* () =
      List.fold_result document.balances ~init:() ~f:(fun () balance ->
        store_balance db ~document_id ~institution balance)
    in
    let* loose = ingest_linked db ~document_id document.events in
    let* () =
      List.fold_result
        (List.mapi document.totals ~f:(fun i claim -> i, claim))
        ~init:()
        ~f:(fun () (ordinal, claim) ->
          store_total db ~document_id ~ordinal ~institution claim)
    in
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
            ~context:document.context
            ~companions:A.companions
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
            "this is a %s export (%s); import it with fin import %s"
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
