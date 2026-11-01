open Base
open Rel
module Db = Rel_sqlite3

type t =
  { conn : Db.t
  ; registry : Registry.t
  ; source_typing_cache : (string, Source_typing.t) Hashtbl.t
  ; source_evidence_cache : (string, Source_evidence.rule list) Hashtbl.t
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

let categories db =
  db_result
    (Db.fold
       db.conn
       (statement
          "SELECT account FROM category_catalog ORDER BY account"
          Row.(t1 (text "account")))
       (fun account acc -> account :: acc)
       [])
  |> Result.map ~f:List.rev
;;

let add_categories db names =
  let* names =
    List.map names ~f:(fun name ->
      let* decision =
        Bank_category.create ~event_id:1L ~account:name ~reason:"category"
      in
      if String.equal decision.account "equity:unclassified"
      then Or_error.error_string "category must be an income or expense account"
      else Ok decision.account)
    |> Or_error.combine_errors
  in
  transaction db (fun db ->
    let insert =
      Rel_sql.Stmt.(
        func "INSERT OR IGNORE INTO category_catalog(account) VALUES (?)" (text @-> unit))
    in
    List.fold_result names ~init:() ~f:(fun () name ->
      db_result (Db.exec db.conn (insert name))))
;;

let delete_category db name =
  transaction db (fun db ->
    let query =
      Rel_sql.Stmt.(
        func
          "SELECT EXISTS(SELECT 1 FROM category_catalog WHERE account=?), EXISTS(SELECT \
           1 FROM bank_categories WHERE account=?), EXISTS(SELECT 1 FROM \
           classification_rules WHERE action_kind IN ('category','type-category') AND \
           target=?)"
          (text
           @-> text
           @-> text
           @-> ret Row.(t3 (bool "exists") (bool "used") (bool "rule"))))
    in
    let* result = db_result (Db.first db.conn (query name name name)) in
    let exists, used, rule = Option.value_exn result in
    if not exists
    then Or_error.errorf "unknown category: %s" name
    else if used
    then
      Or_error.errorf
        "category %s is assigned to transactions; change their categories first"
        name
    else if rule
    then
      Or_error.errorf
        "category %s is used by a rule; change or delete the rule first"
        name
    else (
      let delete =
        Rel_sql.Stmt.(func "DELETE FROM category_catalog WHERE account=?" (text @-> unit))
      in
      db_result (Db.exec db.conn (delete name))))
;;

let institution_definitions db =
  db_result
    (Db.fold
       db.conn
       (statement
          "SELECT name,definition_json FROM institution_definitions ORDER BY name"
          Db_schema.Institution_definitions.row)
       (fun (row : Db_schema.Institution_definitions.t) acc ->
          (row.name, row.definition) :: acc)
       [])
  |> Result.map ~f:List.rev
;;

let institutions db =
  db_result
    (Db.fold
       db.conn
       (statement "SELECT name FROM institutions ORDER BY name" Row.(t1 (text "name")))
       (fun name names -> name :: names)
       [])
  |> Result.map ~f:List.rev
;;

let institution_profile db name =
  let open Rel_sql.Stmt in
  let query =
    func
      "SELECT profile FROM institutions WHERE name=?"
      (text @-> ret Row.(t1 (option Rel.Type.text "profile")))
  in
  db_result (Db.first db.conn (query name)) |> Result.map ~f:Option.join
;;

let institution_companion_profiles db =
  let row = Row.(t2 (text "name") (text "companion_profile")) in
  db_result
    (Db.fold
       db.conn
       (statement
          "SELECT name,companion_profile FROM institutions WHERE companion_profile IS \
           NOT NULL ORDER BY name"
          row)
       (fun item acc -> item :: acc)
       [])
  |> Result.map ~f:List.rev
;;

let institution_companion_profile db name =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT companion_profile FROM institutions WHERE name=?"
        (text @-> ret Row.(t1 (option Rel.Type.text "companion_profile"))))
  in
  db_result (Db.first db.conn (query name)) |> Result.map ~f:Option.join
;;

let set_institution_companion_profile ?(dry_run = false) db name ~expected profile =
  transaction db (fun db ->
    let* names = institutions db in
    if not (List.mem names name ~equal:String.equal)
    then Or_error.error_string "unknown user-added institution"
    else
      let* current = institution_companion_profile db name in
      if not (Option.equal String.equal current expected)
      then
        Or_error.error_string "companion profile changed since preview; review it again"
      else if Option.equal String.equal current (Some profile)
      then Ok ()
      else (
        let csv =
          Rel_sql.Stmt.(
            func
              "SELECT count(*) FROM source_events WHERE venue=? AND kind='csv-movement'"
              (text @-> ret integer))
        in
        let* count = db_result (Db.first db.conn (csv name)) in
        if Option.value count ~default:0 > 0
        then
          Or_error.error_string
            "institution has primary CSV movements; delete those accounts before \
             configuring CSV enrichment"
        else if dry_run
        then Ok ()
        else (
          let update =
            Rel_sql.Stmt.(
              func
                "UPDATE institutions SET companion_profile=? WHERE name=?"
                (text @-> text @-> unit))
          in
          db_result (Db.exec db.conn (update profile name)))))
;;

let clear_institution_companion_profile ?(dry_run = false) db name ~expected =
  transaction db (fun db ->
    let* current = institution_companion_profile db name in
    if not (Option.equal String.equal current (Some expected))
    then Or_error.error_string "companion profile changed since preview; review it again"
    else (
      let count =
        Rel_sql.Stmt.(
          func
            "SELECT count(*) FROM companion_rows WHERE institution=?"
            (text @-> ret integer))
      in
      let* count = db_result (Db.first db.conn (count name)) in
      if Option.value count ~default:0 > 0
      then
        Or_error.error_string
          "cannot remove the profile while companion rows exist; they need it for \
           matching and display"
      else if dry_run
      then Ok ()
      else (
        let update =
          Rel_sql.Stmt.(
            func
              "UPDATE institutions SET companion_profile=NULL WHERE name=?"
              (text @-> unit))
        in
        db_result (Db.exec db.conn (update name)))))
;;

let add_institution ?profile db name =
  let* name = Validation.token "institution name" name in
  if
    List.exists (Registry.institutions db.registry) ~f:(fun institution ->
      List.mem (Registry.aliases db.registry institution) name ~equal:String.equal)
  then Or_error.error_string "institution name is reserved for a built-in importer"
  else (
    let insert = Rel_sql.insert_into_cols Db.dialect Db_schema.Institutions.table in
    let value = name in
    db_result
      (Db.exec
         db.conn
         (insert
            Col.
              [ Value (Db_schema.Institutions.name, value)
              ; Value (Db_schema.Institutions.profile, profile)
              ])))
;;

let set_institution_profile_if_missing db name profile =
  let open Rel_sql.Stmt in
  let update =
    func
      "UPDATE institutions SET profile=? WHERE name=? AND profile IS NULL"
      (text @-> text @-> unit)
  in
  db_result (Db.exec db.conn (update profile name))
;;

let delete_institution ?(dry_run = false) db name =
  let open Rel_sql.Stmt in
  transaction db (fun db ->
    let* name = Validation.token "institution name" name in
    let* names = institutions db in
    let* definitions = institution_definitions db in
    if
      List.Assoc.mem definitions name ~equal:String.equal
      && not (List.mem names name ~equal:String.equal)
    then Or_error.error_string "built-in institutions cannot be deleted"
    else if List.Assoc.mem definitions name ~equal:String.equal
    then (
      let count =
        func
          "SELECT (SELECT count(*) FROM source_events WHERE venue=?) + (SELECT count(*) \
           FROM companion_rows WHERE institution=?)"
          (text @-> text @-> ret integer)
      in
      let* count = db_result (Db.first db.conn (count name name)) in
      if Option.value count ~default:0 > 0
      then
        Or_error.error_string
          "institution has imported evidence; remove its accounts and companion rows \
           first"
      else if dry_run
      then Ok ()
      else (
        let query =
          func "DELETE FROM institution_definitions WHERE name=?" (text @-> unit)
        in
        let* () = db_result (Db.exec db.conn (query name)) in
        let delete = func "DELETE FROM institutions WHERE name=?" (text @-> unit) in
        db_result (Db.exec db.conn (delete name))))
    else if not (List.mem names name ~equal:String.equal)
    then
      if
        List.exists (Registry.institutions db.registry) ~f:(fun institution ->
          List.mem (Registry.aliases db.registry institution) name ~equal:String.equal)
      then Or_error.error_string "built-in institutions cannot be deleted"
      else Or_error.errorf "unknown institution: %s" name
    else (
      let count =
        func "SELECT count(*) FROM source_events WHERE venue=?" (text @-> ret integer)
      in
      let* count = db_result (Db.first db.conn (count name)) in
      if Option.value count ~default:0 > 0
      then
        Or_error.error_string
          "institution has imported transactions; delete its accounts first"
      else (
        let companions =
          func
            "SELECT count(*) FROM companion_rows WHERE institution=?"
            (text @-> ret integer)
        in
        let* count = db_result (Db.first db.conn (companions name)) in
        if Option.value count ~default:0 > 0
        then Or_error.error_string "institution has imported companion rows"
        else if dry_run
        then Ok ()
        else (
          let delete = func "DELETE FROM institutions WHERE name=?" (text @-> unit) in
          db_result (Db.exec db.conn (delete name))))))
;;

let initialize db =
  transaction db (fun db ->
    let* version =
      db_result (Db.first db.conn (statement "PRAGMA user_version" integer))
    in
    match version with
    | Some 0 ->
      let* () = db_result (Db.exec_sql db.conn Db_schema.create) in
      let sql =
        Rel_sql.Stmt.(
          func
            "INSERT INTO \
             classification_rules(mode,account_type,priority,name,pattern,institution,match_type,case_mode,action_kind,target,action_type,field) \
             VALUES ('automatic',?,?,?,?,?,?,?,'type',?,NULL,?)"
            (option text
             @-> int
             @-> text
             @-> text
             @-> text
             @-> option text
             @-> text
             @-> text
             @-> option text
             @-> unit))
      in
      let* () =
        Registry.default_type_rules db.registry
        |> List.map ~f:(fun (rule : Type_default.t) ->
          db_result
            (Db.exec
               db.conn
               (sql
                  rule.account_type
                  rule.priority
                  rule.name
                  rule.pattern
                  rule.institution
                  rule.match_type
                  (if rule.case_sensitive then "sensitive" else "insensitive")
                  rule.target
                  rule.field)))
        |> Or_error.combine_errors_unit
      in
      let evidence_sql =
        Rel_sql.Stmt.(
          func
            "INSERT INTO \
             classification_rules(mode,account_type,priority,name,pattern,institution,match_type,case_mode,action_kind,target,action_type) \
             VALUES ('automatic',NULL,100,?,?,?,NULL,?,?,?,NULL)"
            (text @-> text @-> text @-> text @-> text @-> text @-> unit))
      in
      let* () =
        Registry.default_source_evidence db.registry
        |> List.map ~f:(fun (rule : Source_evidence.default) ->
          let* _ =
            Source_evidence.make_rule
              ~action:rule.action
              ~pattern:rule.pattern
              ~case_sensitive:rule.case_sensitive
              ~account_type:None
              ~match_type:None
          in
          let kind = Source_evidence.action_name rule.action in
          db_result
            (Db.exec
               db.conn
               (evidence_sql
                  rule.name
                  rule.pattern
                  rule.institution
                  (if rule.case_sensitive then "sensitive" else "insensitive")
                  kind
                  kind)))
        |> Or_error.combine_errors_unit
      in
      Ok ()
    | Some version when version = Db_schema.version -> Ok ()
    | _ ->
      Or_error.errorf
        "unsupported database schema; this experimental build accepts only schema %d. \
         Use the previous fin version to export account settings and run fin backup \
         before rebuilding. Then run fin delete and fin init, import your statement \
         folders again and import the account snapshot"
        Db_schema.version)
;;

let with_connection ~mode path f =
  let* conn = db_result (Db.open' ~foreign_keys:true ~mode path) in
  let result =
    try
      let* () = db_result (Db.busy_timeout_ms conn 5000) in
      f conn
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

let with_open ~registry path f =
  with_connection ~mode:Read_write_create path (fun conn ->
    let db =
      { conn
      ; registry
      ; source_typing_cache = Hashtbl.create (module String)
      ; source_evidence_cache = Hashtbl.create (module String)
      }
    in
    let* () = initialize db in
    f db)
;;

let backup db path =
  if Stdlib.Sys.file_exists path
  then Or_error.errorf "%s already exists" path
  else (
    let quoted = "'" ^ String.substr_replace_all path ~pattern:"'" ~with_:"''" ^ "'" in
    db_result (Db.exec_sql db.conn ("VACUUM INTO " ^ quoted)))
;;

let validate_backup conn =
  let* version = db_result (Db.first conn (statement "PRAGMA user_version" integer)) in
  let* () =
    match version with
    | Some version when version = Db_schema.version -> Ok ()
    | _ -> Or_error.errorf "backup must use database schema %d" Db_schema.version
  in
  let text = Row.(t1 (text "value")) in
  let* checks =
    db_result
      (Db.fold conn (statement "PRAGMA quick_check" text) (fun s rest -> s :: rest) [])
  in
  let* () =
    if List.equal String.equal checks [ "ok" ]
    then Ok ()
    else Or_error.error_string "backup failed SQLite integrity checks"
  in
  let* tables =
    db_result
      (Db.fold
         conn
         (statement "SELECT name FROM sqlite_schema WHERE type='table'" text)
         (fun s rest -> s :: rest)
         [])
  in
  let* () =
    if
      List.for_all (Schema.tables Db_schema.schema) ~f:(fun table ->
        List.mem tables (Table.name' table) ~equal:String.equal)
    then Ok ()
    else Or_error.error_string "backup is missing fin database tables"
  in
  let* violations =
    db_result
      (Db.first conn (statement "SELECT count(*) FROM pragma_foreign_key_check" integer))
  in
  if Option.equal Int.equal violations (Some 0)
  then Ok ()
  else Or_error.error_string "backup contains foreign-key violations"
;;

let restore ~source ~destination =
  let* () =
    if String.equal destination ":memory:"
    then Or_error.error_string "restore requires a persistent destination database"
    else Ok ()
  in
  let* existed =
    Or_error.try_with (fun () ->
      let input = Unix.stat source in
      if not (Poly.equal input.st_kind Unix.S_REG)
      then failwith "backup must be a regular SQLite file";
      try
        let output = Unix.lstat destination in
        if not (Poly.equal output.st_kind Unix.S_REG)
        then failwith "restore destination must be a regular file, not a symbolic link";
        if input.st_dev = output.st_dev && input.st_ino = output.st_ino
        then failwith "backup and destination are the same file";
        true
      with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> false)
  in
  with_connection ~mode:Read source (fun src ->
    (* Pin the checked source snapshot until copying has completed. The backup
       API holds the destination write transaction and rolls back on failure. *)
    db_result
      (Db.with_transaction `Deferred src (fun src ->
         let* () = validate_backup src in
         let* _ =
           Bos.OS.Dir.create ~mode:0o700 (Fpath.v (Stdlib.Filename.dirname destination))
           |> Result.map_error ~f:(fun (`Msg message) -> Error.of_string message)
         in
         let* () =
           Or_error.try_with (fun () ->
             if not existed
             then (
               let fd =
                 Unix.openfile
                   destination
                   [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ]
                   0o600
               in
               Unix.close fd))
         in
         let copied =
           with_connection ~mode:Read_write destination (fun dst ->
             let* copy = db_result (Db.Backup.init ~dst ~src ()) in
             let stepped =
               try db_result (Db.Backup.step copy ()) with
               | exn ->
                 ignore (Db.Backup.finish copy);
                 raise exn
             in
             let finished = db_result (Db.Backup.finish copy) in
             let* complete = stepped in
             let* () = finished in
             if complete
             then Ok ()
             else Or_error.error_string "database restore was incomplete")
         in
         match copied with
         | Ok () -> Ok ()
         | Error _ as error ->
           if not existed
           then ignore (Bos.OS.File.delete ~must_exist:false (Fpath.v destination));
           error))
    |> Result.join)
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
  fun v_venue v_account v_kind v_source_id v_occurred_at v_occurred_on v_payload ->
    insert
      Col.
        [ Value (venue, v_venue)
        ; Value (account, v_account)
        ; Value (kind, v_kind)
        ; Value (source_id, v_source_id)
        ; Value (occurred_at, v_occurred_at)
        ; Value (occurred_on, v_occurred_on)
        ; Value (payload, v_payload)
        ]
;;

let insert_movement =
  let open Db_schema.Movements in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_event_id (e : Source_event.t) (m : Movement.t) ->
    let principal = Movement.principal m in
    let negative = Quantity.(principal < zero) in
    let status, iban =
      match m.own_iban with
      | Known iban -> "known", Some (Iban.to_string iban)
      | Unknown -> "unknown", None
      | Not_applicable -> "not_applicable", None
    in
    insert
      Col.
        [ Value (event_id, v_event_id)
        ; Value (venue, e.venue)
        ; Value (account, e.account)
        ; Value (bank_id, m.account.bank_id)
        ; Value (account_id, m.account.account_id)
        ; Value (account_type, m.account.account_type)
        ; Value (currency, Iso4217.to_alpha_code m.account.currency)
        ; Value (own_iban_status, status)
        ; Value (own_iban, iban)
        ; Value (booked_on, Date.to_string m.booked_on)
        ; Value (value_on, Option.map m.value_on ~f:Date.to_string)
        ; Value (amount, Quantity.to_string m.amount)
        ; Value (fee, Quantity.to_string m.fee)
        ; Value
            ( principal_magnitude
            , Quantity.to_string (if negative then Quantity.neg principal else principal)
            )
        ; Value (principal_sign, if negative then "-" else "+")
        ; Value (counterparty_iban, Option.map m.counterparty_iban ~f:Iban.to_string)
        ; Value (descriptions, Json.encode (Jsont.list Jsont.string) m.descriptions)
        ; Value
            ( Db_schema.Movements.movement_type
            , Option.map m.movement_type ~f:Movement_type.to_string )
        ; Value (Db_schema.Movements.main_description, m.main_description)
        ]
;;

(* Every column or tag the source carried, in order, beside the event. *)
let store_event_fields db event_id fields =
  let insert =
    Rel_sql.Stmt.(
      func
        "INSERT INTO source_event_fields(event_id,ordinal,name,value) VALUES (?,?,?,?)"
        (int64 @-> int @-> text @-> text @-> unit))
  in
  List.mapi fields ~f:(fun ordinal (name, value) -> ordinal, name, value)
  |> List.fold_result ~init:() ~f:(fun () (ordinal, name, value) ->
    db_result (Db.exec db.conn (insert event_id ordinal name value)))
;;

(* The movement stored at import, rebuilt and validated again. *)
let movement_of_row (r : Db_schema.Movements.t) =
  Or_error.tag
    ~tag:(Printf.sprintf "stored movement %Ld" r.event_id)
    (let* currency = Validation.currency "currency" r.currency in
     let iban label s =
       Result.map_error (Iban.of_string s) ~f:(fun e -> Error.tag e ~tag:label)
     in
     let* own_iban =
       match r.own_iban_status, r.own_iban with
       | "known", Some s -> Result.map (iban "own IBAN" s) ~f:(fun i -> Movement.Known i)
       | "unknown", None -> Ok Movement.Unknown
       | "not_applicable", None -> Ok Movement.Not_applicable
       | _ -> Or_error.error_string "inconsistent own IBAN status"
     in
     let* counterparty_iban =
       match r.counterparty_iban with
       | None -> Ok None
       | Some s -> Result.map (iban "counterparty IBAN" s) ~f:Option.some
     in
     let* descriptions = Json.decode (Jsont.list Jsont.string) r.descriptions in
     let* booked_on, value_on, amount, fee =
       Or_error.try_with (fun () ->
         ( Date.of_string r.booked_on
         , Option.map r.value_on ~f:Date.of_string
         , Quantity.of_string r.amount
         , Quantity.of_string r.fee ))
     in
     Movement.create
       ~account:
         { bank_id = r.bank_id
         ; account_id = r.account_id
         ; account_type = r.account_type
         ; currency
         }
       ~own_iban
       ~booked_on
       ?value_on
       ~amount
       ~fee
       ?counterparty_iban
       ?movement_type:(Option.map r.movement_type ~f:Movement_type.of_string)
       ~main_description:r.main_description
       descriptions)
;;

let movement_columns =
  "event_id,venue,account,bank_id,account_id,account_type,currency,own_iban_status,own_iban,booked_on,value_on,amount,fee,principal_magnitude,principal_sign,counterparty_iban,descriptions,movement_type,main_description"
;;

let occurrence_columns (e : Source_event.t) =
  match e.occurred with
  | Temporal.Instant instant -> Some instant, None
  | Temporal.Date date -> None, Some (Date.to_string date)
;;

let source_typing_profile db institution =
  match Hashtbl.find db.source_typing_cache institution with
  | Some profile -> Ok profile
  | None ->
    let query =
      Rel_sql.Stmt.(
        func
          "SELECT pattern,account_type,case_mode,CASE WHEN action_kind='type-category' \
           THEN action_type ELSE target END,match_type,field FROM classification_rules \
           WHERE mode='automatic' AND action_kind IN ('type','type-category') AND \
           (institution IS NULL OR institution=?) ORDER BY priority,name"
          (text
           @-> ret
                 Row.(
                   unit (fun a b c d e f -> a, b, c, d, e, f)
                   * option Rel.Type.text "pattern"
                   * option Rel.Type.text "account_type"
                   * text "case_mode"
                   * text "target"
                   * option Rel.Type.text "match_type"
                   * option Rel.Type.text "field")))
    in
    let* rows =
      db_result (Db.fold db.conn (query institution) (fun row acc -> row :: acc) [])
    in
    let* rules =
      List.rev rows
      |> List.map ~f:(fun (pattern, account_type, case_mode, target, match_type, field) ->
        Result.map
          (Source_typing.make_rule
             ?field
             (Option.value pattern ~default:".*")
             account_type
             (String.equal case_mode "sensitive")
             target)
          ~f:(fun r -> Source_typing.with_match_type r match_type))
      |> Or_error.combine_errors
    in
    let profile = Source_typing.create ~institution rules in
    Hashtbl.set db.source_typing_cache ~key:institution ~data:profile;
    Ok profile
;;

let source_evidence_rules db institution =
  match Hashtbl.find db.source_evidence_cache institution with
  | Some rules -> Ok rules
  | None ->
    let query =
      Rel_sql.Stmt.(
        func
          "SELECT pattern,account_type,case_mode,action_kind,match_type FROM \
           classification_rules WHERE mode='automatic' AND action_kind IN \
           ('card-reference','withdrawal-reversal') AND (institution IS NULL OR \
           institution=?) ORDER BY priority,name"
          (text
           @-> ret
                 Row.(
                   t5
                     (text "pattern")
                     (option Rel.Type.text "account_type")
                     (text "case_mode")
                     (text "action_kind")
                     (option Rel.Type.text "match_type"))))
    in
    let* rows =
      db_result (Db.fold db.conn (query institution) (fun row acc -> row :: acc) [])
    in
    let* rules =
      List.rev rows
      |> List.map ~f:(fun (pattern, account_type, case_mode, kind, match_type) ->
        let action =
          if String.equal kind "card-reference"
          then Source_evidence.Card_reference
          else Withdrawal_reversal
        in
        Source_evidence.make_rule
          ~action
          ~pattern
          ~case_sensitive:(String.equal case_mode "sensitive")
          ~account_type
          ~match_type)
      |> Or_error.combine_errors
    in
    Hashtbl.set db.source_evidence_cache ~key:institution ~data:rules;
    Ok rules
;;

let write_source_evidence db event_id (evidence : Source_evidence.t) =
  let query =
    Rel_sql.Stmt.(
      func
        "INSERT INTO movement_evidence(event_id,card_reference,withdrawal_reversal) \
         VALUES (?,?,?) ON CONFLICT(event_id) DO UPDATE SET \
         card_reference=excluded.card_reference,withdrawal_reversal=excluded.withdrawal_reversal"
        (int64 @-> option text @-> text @-> unit))
  in
  db_result
    (Db.exec
       db.conn
       (query
          event_id
          evidence.card_reference
          (if evidence.withdrawal_reversal then "yes" else "no")))
;;

let apply_automatic_category
      db
      event_id
      (event : Source_event.t)
      (raw : Movement.t)
      (movement : Movement.t)
  =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT name,pattern,account_type,case_mode,target,match_type,action_kind FROM \
         classification_rules WHERE mode='automatic' AND action_kind IN \
         ('category','type-category') AND (institution IS NULL OR institution=?) ORDER \
         BY priority,name"
        (text
         @-> ret
               Row.(
                 unit (fun a b c d e f g -> a, b, c, d, e, f, g)
                 * text "name"
                 * option Rel.Type.text "pattern"
                 * option Rel.Type.text "account_type"
                 * text "case_mode"
                 * text "target"
                 * option Rel.Type.text "match_type"
                 * text "action_kind")))
  in
  let* rows =
    db_result (Db.fold db.conn (query event.venue) (fun row acc -> row :: acc) [])
  in
  let description =
    Option.value_map (List.hd movement.descriptions) ~default:"" ~f:String.strip
  in
  let rec select = function
    | [] -> Ok ()
    | (name, pattern, account_type, case_mode, target, match_type, action_kind) :: rest ->
      let* pattern_matches =
        match pattern with
        | None -> Ok true
        | Some text ->
          Result.map
            (Transaction_pattern.compile
               ~case_sensitive:(String.equal case_mode "sensitive")
               text)
            ~f:(fun p ->
              Transaction_pattern.matches
                p
                ~display_name:None
                ~descriptions:[ description ])
      in
      let matched_type =
        if String.equal action_kind "type-category"
        then raw.movement_type
        else movement.movement_type
      in
      let type_matches =
        Option.value_map match_type ~default:true ~f:(fun expected ->
          match expected, matched_type with
          | "unknown", None -> true
          | "unknown", Some _ -> false
          | _, Some kind -> String.equal expected (Movement_type.to_string kind)
          | _, None -> false)
      in
      if
        not
          (pattern_matches
           && type_matches
           && Option.value_map
                account_type
                ~default:true
                ~f:(String.equal movement.account.account_type))
      then select rest
      else (
        let reason = "Automatic rule " ^ name in
        let insert =
          Rel_sql.Stmt.(
            func
              "INSERT INTO bank_categories(event_id,account,reason) VALUES (?,?,?)"
              (int64 @-> text @-> text @-> unit))
        in
        let audit =
          Rel_sql.Stmt.(
            func
              "INSERT INTO \
               bank_category_changes(event_id,previous_account,account,reason) VALUES \
               (?,NULL,?,?)"
              (int64 @-> text @-> text @-> unit))
        in
        let catalog =
          Rel_sql.Stmt.(
            func
              "INSERT OR IGNORE INTO category_catalog(account) VALUES (?)"
              (text @-> unit))
        in
        let* () = db_result (Db.exec db.conn (catalog target)) in
        let* () = db_result (Db.exec db.conn (insert event_id target reason)) in
        db_result (Db.exec db.conn (audit event_id target reason)))
  in
  select (List.rev rows)
;;

let ingest_in_transaction db (e : Source_event.t) =
  let at, on = occurrence_columns e in
  let* existing =
    db_result (Db.first db.conn (find_source e.venue e.account e.kind e.source_id))
  in
  match existing with
  | Some previous ->
    if
      Option.equal Ptime.equal at previous.occurred_at
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
    let id = Db.last_insert_rowid db.conn in
    let* () = store_event_fields db id e.fields in
    (* Movements are decoded once, here, and stored for queries. *)
    let* () =
      if Registry.is_movement db.registry e
      then
        let* profile = source_typing_profile db e.venue in
        let raw_profile = Source_typing.create ~institution:e.venue [] in
        let* raw = Registry.movement ~source_typing:raw_profile db.registry e in
        let m = Source_typing.apply profile raw in
        let* () = db_result (Db.exec db.conn (insert_movement id e m)) in
        let* rules = source_evidence_rules db e.venue in
        let evidence = Source_evidence.apply rules m in
        let* () = write_source_evidence db id evidence in
        apply_automatic_category db id e raw m
      else Ok ()
    in
    Ok (Inserted id)
;;

let ingest db e = transaction db (fun db -> ingest_in_transaction db e)

let collect db stmt =
  let* rows = db_result (Db.fold db.conn stmt (fun row rows -> row :: rows) []) in
  Ok (List.rev rows)
;;

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

let traverse f xs = Or_error.combine_errors (List.map xs ~f)

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

let store_companion_rows db ~document_id ~institution ~format ~context:_ ~companions rows =
  let* (_ : Companion.t) =
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

let movement_rows db where =
  let* rows =
    collect
      db
      (statement
         ("SELECT " ^ movement_columns ^ " FROM movements" ^ where)
         Db_schema.Movements.row)
  in
  traverse
    (fun (r : Db_schema.Movements.t) ->
       let* m = movement_of_row r in
       Ok (r.event_id, m))
    rows
;;

(* Every stored movement by event ID, read in one query. *)
let movement_table db =
  let* rows = movement_rows db "" in
  Ok (Hashtbl.of_alist_exn (module Int64) rows)
;;

let movement db event_id =
  let* rows = movement_rows db (Printf.sprintf " WHERE event_id = %Ld" event_id) in
  match rows with
  | [ (_, m) ] -> Ok m
  | _ -> Or_error.errorf "no stored movement for event %Ld" event_id
;;

let stored table event_id =
  Or_error.of_option
    (Hashtbl.find table event_id)
    ~error:(Error.createf "no stored movement for event %Ld" event_id)
;;

let event_of_row
      ({ id
       ; venue
       ; account
       ; kind
       ; source_id
       ; occurred_at = at
       ; occurred_on = on
       ; payload
       ; _
       } :
        Db_schema.Source_events.t)
  =
  let* e =
    match at, on with
    | Some occurred_at, None ->
      Source_event.create ~venue ~account ~kind ~source_id ~occurred_at ~payload ()
    | None, Some date ->
      let* occurred_on = Or_error.try_with (fun () -> Date.of_string date) in
      Source_event.create_dated ~venue ~account ~kind ~source_id ~occurred_on ~payload ()
    | _ -> Or_error.error_string "invalid source event occurrence"
  in
  Ok (id, e)
;;

let events db =
  let* rows =
    collect
      db
      (statement
         "SELECT \
          id,venue,account,kind,source_id,occurred_at,occurred_on,retrieved_at,payload \
          FROM source_events ORDER BY coalesce(occurred_on, substr(occurred_at, 1, \
          10)),occurred_at,id"
         source_row)
  in
  let* decoded = traverse event_of_row rows in
  let* stored =
    collect
      db
      (statement
         "SELECT event_id,ordinal,name,value FROM source_event_fields ORDER BY \
          event_id,ordinal"
         Db_schema.Source_event_fields.row)
  in
  let by_event = Hashtbl.create (module Int64) in
  List.iter stored ~f:(fun (f : Db_schema.Source_event_fields.t) ->
    Hashtbl.add_multi by_event ~key:f.event_id ~data:(f.name, f.value));
  Ok
    (List.map decoded ~f:(fun (id, event) ->
       match Hashtbl.find by_event id with
       | None -> id, event
       | Some reversed -> id, Source_event.with_fields event (List.rev reversed)))
;;

(* One source event, without decoding the others. *)
let event_by_id db event_id =
  let* rows =
    collect
      db
      (statement
         (Printf.sprintf
            "SELECT \
             id,venue,account,kind,source_id,occurred_at,occurred_on,retrieved_at,payload \
             FROM source_events WHERE id = %Ld"
            event_id)
         source_row)
  in
  match rows with
  | [ row ] ->
    let* _, event = event_of_row row in
    let* stored =
      collect
        db
        (statement
           (Printf.sprintf
              "SELECT event_id,ordinal,name,value FROM source_event_fields WHERE \
               event_id = %Ld ORDER BY ordinal"
              event_id)
           Db_schema.Source_event_fields.row)
    in
    Ok
      (Source_event.with_fields
         event
         (List.map stored ~f:(fun (f : Db_schema.Source_event_fields.t) ->
            f.name, f.value)))
  | _ -> Or_error.error_string "unknown transaction event ID"
;;

type source_evidence_change =
  { event_id : int64
  ; institution : string
  ; description : string
  ; before : Source_evidence.t
  ; after : Source_evidence.t
  }

let preview_source_evidence ?institution db =
  let* source = events db in
  let* table = movement_table db in
  let* rows =
    collect
      db
      (statement
         "SELECT event_id,card_reference,withdrawal_reversal FROM movement_evidence"
         Row.(
           t3
             (int64 "event_id")
             (option Rel.Type.text "card_reference")
             (text "withdrawal_reversal")))
  in
  let old = Hashtbl.create (module Int64) in
  List.iter rows ~f:(fun (id, card_reference, reversal) ->
    Hashtbl.set
      old
      ~key:id
      ~data:
        { Source_evidence.card_reference
        ; withdrawal_reversal = String.equal reversal "yes"
        });
  source
  |> List.filter ~f:(fun (_, e) ->
    Registry.is_movement db.registry e
    && Option.value_map institution ~default:true ~f:(String.equal e.Source_event.venue))
  |> traverse (fun (event_id, event) ->
    let* movement = stored table event_id in
    let* rules = source_evidence_rules db event.Source_event.venue in
    let after = Source_evidence.apply rules movement in
    let before =
      Option.value (Hashtbl.find old event_id) ~default:Source_evidence.empty
    in
    Ok
      (Option.some_if
         (not (Poly.equal before after))
         { event_id
         ; institution = event.venue
         ; description = String.concat ~sep:" | " movement.descriptions
         ; before
         ; after
         }))
  |> Result.map ~f:List.filter_opt
;;

let refresh_source_evidence ?institution db ~expected =
  transaction db (fun db ->
    Hashtbl.clear db.source_evidence_cache;
    let* current = preview_source_evidence ?institution db in
    if not (Poly.equal current expected)
    then Or_error.error_string "source evidence changed since preview; review it again"
    else
      let* () =
        traverse
          (fun change -> write_source_evidence db change.event_id change.after)
          current
        |> Result.map ~f:ignore
      in
      Ok (List.length current))
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

let find_card_settlement =
  Rel_sql.Stmt.(
    func
      "SELECT ledger_account,reason FROM card_settlement_decisions WHERE event_id=?"
      (int64
       @-> ret
             Row.(
               t2
                 (read_column Db_schema.Card_settlement_decisions.ledger_account)
                 (read_column Db_schema.Card_settlement_decisions.reason))))
;;

let card_settlement db event_id =
  db_result (Db.first db.conn (find_card_settlement event_id))
;;

let reversal_account original reversal =
  Printf.sprintf "assets:reversals:pair-%Ld-%Ld" original reversal
;;

let linked_reversal db event_id =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT original,reversal,reason FROM reversal_pairs WHERE original=? OR \
         reversal=?"
        (int64
         @-> int64
         @-> ret Row.(t3 (int64 "original") (int64 "reversal") (text "reason"))))
  in
  db_result (Db.first db.conn (query event_id event_id))
;;

let counter_account db event_id =
  let* reversal = linked_reversal db event_id in
  match reversal with
  | Some (original, reversal, _) -> Ok (Some (reversal_account original reversal))
  | None ->
    let* transfer = linked_transfer db event_id in
    (match transfer with
     | Some (first, second, _) -> Ok (Some (Bank_transfer.account first second))
     | None ->
       let* settlement = card_settlement db event_id in
       (match settlement with
        | Some (account, _) -> Ok (Some account)
        | None -> category_account db event_id))
;;

let mapped_bank_entry db event_id (event : Source_event.t) =
  let* bank_account =
    db_result (Db.first db.conn (find_mapping event.venue event.account))
  in
  let* counter_account = counter_account db event_id in
  let* movement = movement db event_id in
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
  let* table = movement_table db in
  traverse
    (fun (id, event) ->
       let* movement = stored table id in
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

let bank_accounts_from db source_events ~movement =
  let bank_events =
    List.filter source_events ~f:(fun (_, event) ->
      Registry.is_movement db.registry event)
    |> List.dedup_and_sort ~compare:(fun (_, a) (_, b) ->
      match String.compare a.Source_event.venue b.venue with
      | 0 -> String.compare a.account b.account
      | n -> n)
  in
  traverse
    (fun (event_id, (event : Source_event.t)) ->
       let* transaction = movement event_id in
       let account = transaction.Movement.account in
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

let bank_accounts db =
  let* source = events db in
  bank_accounts_from db source ~movement:(movement db)
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
          let open Rel_sql.Stmt in
          let count =
            func
              "SELECT count(*) FROM card_settlement_decisions WHERE card_institution=? \
               AND card_source_account=?"
              (text @-> text @-> ret integer)
          in
          let* settlement_count =
            db_result (Db.first db.conn (count mapping.institution source_account))
          in
          let* () =
            if Option.value settlement_count ~default:0 > 0
            then Or_error.error_string "cannot rename a card with confirmed settlements"
            else Ok ()
          in
          let* previous_name =
            match previous with
            | Some name -> Ok name
            | None ->
              let* transaction = movement db (fst (List.hd_exn matching)) in
              let account = transaction.account in
              Bank_projection.ledger_account ~institution:mapping.institution account
          in
          let* _ =
            traverse
              (fun (event_id, (event : Source_event.t)) ->
                 let* existing =
                   db_result (Db.first db.conn (find_entry event_id Bank_projection.key))
                 in
                 match existing with
                 | None -> Ok ()
                 | Some (id, date, description) ->
                   let* stored = read_entry db id date description in
                   let* counter_account = counter_account db event_id in
                   let* movement = movement db event_id in
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
  let* candidates = Json.decode (Jsont.list Json.int64_codec) candidates in
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
           "SELECT \
            r.id,r.document_id,r.institution,r.row_number,r.fields_json,c.context_json \
            FROM companion_rows r LEFT JOIN document_contexts c ON \
            c.document_id=r.document_id WHERE r.institution=? AND r.format=? ORDER BY \
            r.document_id,r.row_number"
           (text
            @-> text
            @-> ret
                  Row.(
                    t6
                      (int64 "id")
                      (int64 "document_id")
                      (text "institution")
                      (int "row_number")
                      (text "fields_json")
                      (option Rel.Type.text "context_json"))))
         institution
         format)
  in
  traverse
    (fun (row_id, document_id, institution, row_number, fields, context) ->
       let* fields = decode_fields fields in
       let* context =
         match context with
         | None -> Ok []
         | Some json -> Json.decode Json.string_map json
       in
       Ok
         { Companion.row_id
         ; document_id
         ; institution
         ; format
         ; row_number
         ; fields
         ; context
         })
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
       event_id=excluded.event_id,state=excluded.state,reason=excluded.reason,candidates_json=excluded.candidates_json,policy=excluded.policy,updated_at=(strftime('%Y-%m-%dT%H:%M:%f','now') \
       || '000000000Z')"
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
        let candidates = Json.encode (Jsont.list Json.int64_codec) result.candidates in
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

(* The columns of the companion rows matched to each movement, by event, in row order. *)
let companion_columns ?institution db =
  let* matches =
    collect
      db
      (statement
         "SELECT row_id,event_id FROM companion_matches WHERE state='matched' AND \
          event_id IS NOT NULL"
         Row.(t2 (int64 "row_id") (int64 "event_id")))
  in
  let event_of = Hashtbl.of_alist_exn (module Int64) matches in
  let by_event = Hashtbl.create (module Int64) in
  let* () =
    traverse
      (fun (owner, (companion : Companion.t)) ->
         let* rows = companion_rows db ~institution:owner ~format:companion.format in
         traverse
           (fun (row : Companion.row) ->
              match Hashtbl.find event_of row.row_id with
              | None -> Ok ()
              | Some event_id ->
                let* columns = companion.fields row in
                Hashtbl.add_multi by_event ~key:event_id ~data:columns;
                Ok ())
           rows
         |> Result.map ~f:ignore)
      (registered_companions ?institution db)
    |> Result.map ~f:ignore
  in
  Ok (Hashtbl.map by_event ~f:(fun lists -> List.concat (List.rev lists)))
;;

(* A movement's type is derived: the first type rule that matches its own fields and
   the columns of its matched companion rows. Recompute it after anything that changes
   the rules or the matches. When [reason] is given, each change is also written to the
   type history. Returns how many movements changed type. *)
let retype ?institution ?reason db =
  let* source = events db in
  let source =
    List.filter source ~f:(fun (_, (e : Source_event.t)) ->
      Registry.is_movement db.registry e
      && Option.for_all institution ~f:(String.equal e.venue))
  in
  let* columns = companion_columns ?institution db in
  let* stored =
    collect
      db
      (statement
         "SELECT event_id,movement_type FROM movements"
         Row.(t2 (int64 "event_id") (option Rel.Type.text "movement_type")))
  in
  let stored = Hashtbl.of_alist_exn (module Int64) stored in
  let update =
    Rel_sql.Stmt.(
      func
        "UPDATE movements SET movement_type=? WHERE event_id=?"
        (option text @-> int64 @-> unit))
  in
  let log =
    Rel_sql.Stmt.(
      func
        "INSERT INTO movement_type_changes(event_id,previous_type,movement_type,reason) \
         VALUES (?,?,?,?)"
        (int64 @-> option text @-> option text @-> text @-> unit))
  in
  let* changed =
    traverse
      (fun (event_id, (event : Source_event.t)) ->
         let* empty = Source_typing.empty event.venue in
         let* parsed = Registry.movement ~source_typing:empty db.registry event in
         let parsed =
           match Hashtbl.find columns event_id with
           | None -> parsed
           | Some more -> Movement.with_fields parsed (parsed.fields @ more)
         in
         let* profile = source_typing_profile db event.venue in
         let after =
           (Source_typing.apply profile parsed).movement_type
           |> Option.map ~f:Movement_type.to_string
         in
         let before = Option.join (Hashtbl.find stored event_id) in
         if Option.equal String.equal before after
         then Ok 0
         else
           let* () = db_result (Db.exec db.conn (update after event_id)) in
           let* () =
             match reason with
             | None -> Ok ()
             | Some reason ->
               db_result (Db.exec db.conn (log event_id before after reason))
           in
           Ok 1)
      source
  in
  Ok (List.fold changed ~init:0 ~f:( + ))
;;

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
    let* (_ : int) = retype ?institution db in
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

(* Types follow the rules, so a change to the rules retypes the movements it can
   affect, in a second transaction of its own. *)
let retype_after db ?institution reason =
  transaction db (fun db -> retype ?institution ~reason db) |> Result.map ~f:ignore
;;

let add_classification_rule_in_transaction = add_classification_rule

let add_classification_rule db (rule : Classification_rule.t) =
  let* () = add_classification_rule db rule in
  retype_after db ?institution:rule.institution (Printf.sprintf "rule %s added" rule.name)
;;

let replace_classification_rule db (rule : Classification_rule.t) =
  let* () = replace_classification_rule db rule in
  retype_after
    db
    ?institution:rule.institution
    (Printf.sprintf "rule %s changed" rule.name)
;;

let delete_classification_rule db name =
  let* () = delete_classification_rule db name in
  retype_after db (Printf.sprintf "rule %s deleted" name)
;;

let set_classification_rule_mode db name mode =
  let* () = set_classification_rule_mode db name mode in
  retype_after db (Printf.sprintf "rule %s mode changed" name)
;;

let rename_classification_rule db ~old_name ~new_name =
  let* () = rename_classification_rule db ~old_name ~new_name in
  retype_after db (Printf.sprintf "rule %s renamed to %s" old_name new_name)
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
  let* result =
    set_institution_definition
      ~dry_run
      db
      ~name
      ~definition
      ~expected
      ~expected_rules
      ~rules
      ~source_unchanged
  in
  let* () =
    if dry_run then Ok () else retype_after db ~institution:name "definition installed"
  in
  Ok result
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
  ; period_start : Temporal.t
  ; period_end : Temporal.t
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

let set_bank_categories_in_transaction ?(only_unclassified = false) db decisions =
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
  else (
    let apply_batch db =
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
          let* settlement = card_settlement db d.event_id in
          let* reversal = linked_reversal db d.event_id in
          if Option.is_some reversal
          then
            Or_error.error_string
              "undo the confirmed reversal before changing its category"
          else if Option.is_some settlement
          then Or_error.error_string "transaction is a confirmed card settlement"
          else if Option.is_some transfer
          then
            Or_error.error_string
              "reject the confirmed transfer before changing its category"
          else if not (Registry.is_movement db.registry event)
          then Or_error.error_string "category decision requires a bank event"
          else
            let* previous = db_result (Db.first db.conn (find_category d.event_id)) in
            if
              only_unclassified
              && Option.exists previous ~f:(fun (account, _) ->
                not (String.equal account "equity:unclassified"))
            then
              Or_error.error_string
                "a selected transaction is no longer unclassified; review it again"
            else if
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
              let catalog =
                Rel_sql.Stmt.(
                  func
                    "INSERT OR IGNORE INTO category_catalog(account) VALUES (?)"
                    (text @-> unit))
              in
              let* () =
                if String.equal d.account "equity:unclassified"
                then Ok ()
                else db_result (Db.exec db.conn (catalog d.account))
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
      apply 0 decisions
    in
    apply_batch db)
;;

let set_bank_categories ?(only_unclassified = false) db decisions =
  transaction db (fun db ->
    set_bank_categories_in_transaction ~only_unclassified db decisions)
;;

let review_bank_categories db ~expected decisions =
  transaction db (fun db ->
    let* () =
      List.fold_result expected ~init:() ~f:(fun () (event_id, before) ->
        let* current = db_result (Db.first db.conn (find_category event_id)) in
        if Option.equal [%equal: string * string] current before
        then Ok ()
        else
          Or_error.error_string
            "a selected transaction changed since review; review the batch again")
    in
    if
      List.length expected <> List.length decisions
      || List.contains_dup (List.map expected ~f:fst) ~compare:Int64.compare
      || not
           (List.for_all decisions ~f:(fun (d : Bank_category.t) ->
              List.Assoc.mem expected d.event_id ~equal:Int64.equal))
    then Or_error.error_string "category review expectations do not match decisions"
    else set_bank_categories_in_transaction db decisions)
;;

type category_change =
  { id : int64
  ; event_id : int64
  ; previous_account : string option
  ; account : string
  ; reason : string
  ; changed_at : Ptime.t
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

type review_classification =
  | Unclassified
  | Category
  | Transfer
  | Settlement
  | Reversal

type bank_review =
  { event_id : int64
  ; institution : string
  ; occurred : Temporal.t
  ; transaction : Movement.t
  ; description : string
  ; movement_type : Movement_type.t option
  ; imported_movement_type : Movement_type.t option
  ; movement_type_override : Movement_type.t option
  ; source_evidence : Source_evidence.t
  ; detail_sources : int64 list
  ; classification : review_classification
  ; counter_account : string
  ; reason : string option
  }

(* Read review metadata once, rather than issuing two queries per movement. *)
let review_metadata db =
  let* categories =
    collect
      db
      (statement
         "SELECT event_id,account,reason FROM bank_categories"
         Row.(
           t3
             (read_column Db_schema.Bank_categories.event_id)
             (read_column Db_schema.Bank_categories.account)
             (read_column Db_schema.Bank_categories.reason)))
  in
  let* transfers =
    collect
      db
      (statement
         "SELECT m.event_id,m.first_event_id,m.second_event_id,d.reason FROM \
          bank_transfer_memberships m JOIN bank_transfer_decisions d ON \
          d.first_event_id=m.first_event_id AND d.second_event_id=m.second_event_id \
          WHERE d.state='confirmed'"
         Row.(
           t4
             (read_column Db_schema.Bank_transfer_memberships.event_id)
             (read_column Db_schema.Bank_transfer_memberships.first_event_id)
             (read_column Db_schema.Bank_transfer_memberships.second_event_id)
             (read_column Db_schema.Bank_transfer_decisions.reason)))
  in
  let* settlements =
    collect
      db
      (statement
         "SELECT event_id,ledger_account,reason FROM card_settlement_decisions"
         Row.(
           t3
             (read_column Db_schema.Card_settlement_decisions.event_id)
             (read_column Db_schema.Card_settlement_decisions.ledger_account)
             (read_column Db_schema.Card_settlement_decisions.reason)))
  in
  let* reversals =
    collect
      db
      (statement
         "SELECT original,reversal,reason FROM reversal_pairs"
         Db_schema.Reversal_pairs.row)
  in
  let reversal_map = Hashtbl.create (module Int64) in
  List.iter reversals ~f:(fun (r : Db_schema.Reversal_pairs.t) ->
    Hashtbl.set reversal_map ~key:r.original ~data:(r.original, r.reversal, r.reason);
    Hashtbl.set reversal_map ~key:r.reversal ~data:(r.original, r.reversal, r.reason));
  Ok
    ( Hashtbl.of_alist_exn
        (module Int64)
        (List.map categories ~f:(fun (id, account, reason) -> id, (account, reason)))
    , Hashtbl.of_alist_exn
        (module Int64)
        (List.map transfers ~f:(fun (id, first, second, reason) ->
           id, (first, second, reason)))
    , Hashtbl.of_alist_exn
        (module Int64)
        (List.map settlements ~f:(fun (id, account, reason) -> id, (account, reason)))
    , reversal_map )
;;

let bank_review_from
      db
      source
      table
      (categories, transfers, settlements, reversals)
      details
      overrides
      evidence
  =
  traverse
    (fun (event_id, (event : Source_event.t)) ->
       let* (transaction : Movement.t) = stored table event_id in
       let category = Hashtbl.find categories event_id in
       let transfer = Hashtbl.find transfers event_id in
       let settlement = Hashtbl.find settlements event_id in
       let reversal = Hashtbl.find reversals event_id in
       let observations = Option.value (Hashtbl.find details event_id) ~default:[] in
       let consensus ~compare f =
         match
           List.filter_map observations ~f:(fun (_, a) -> f a)
           |> List.dedup_and_sort ~compare
         with
         | [ value ] -> Some value
         | _ -> None
       in
       let preferred =
         consensus ~compare:String.compare (fun (a : Companion.annotation) ->
           Option.bind a.display_name ~f:(fun name ->
             Option.some_if (not (String.is_empty (String.strip name))) name))
       in
       let description =
         Option.value
           preferred
           ~default:
             (Option.value
                (List.nth transaction.descriptions transaction.main_description)
                ~default:"")
       in
       let* transfer_destination =
         match transfer with
         | None -> Ok None
         | Some (first, second, _) ->
           let other = if Int64.equal event_id first then second else first in
           let* (other_event : Source_event.t) =
             Or_error.of_option
               (List.Assoc.find source other ~equal:Int64.equal)
               ~error:(Error.of_string "confirmed transfer references missing movement")
           in
           let* other_movement = stored table other in
           let* mapped =
             db_result
               (Db.first db.conn (find_mapping other_event.venue other_event.account))
           in
           let* account =
             match mapped with
             | Some name -> Ok name
             | None ->
               Bank_projection.ledger_account
                 ~institution:other_event.venue
                 other_movement.account
           in
           Ok (Some account)
       in
       (* The stored type already follows the rules, companion columns included. *)
       let imported_movement_type = transaction.movement_type in
       let movement_type_override = Hashtbl.find overrides event_id in
       Ok
         { event_id
         ; institution = event.venue
         ; occurred = event.occurred
         ; transaction
         ; description
         ; movement_type = Option.first_some movement_type_override imported_movement_type
         ; imported_movement_type
         ; movement_type_override
         ; source_evidence =
             Option.value (Hashtbl.find evidence event_id) ~default:Source_evidence.empty
         ; detail_sources =
             List.map observations ~f:fst |> List.sort ~compare:Int64.compare
         ; classification =
             (match reversal, transfer, settlement, category with
              | Some _, _, _, _ -> Reversal
              | None, Some _, _, _ -> Transfer
              | None, None, Some _, _ -> Settlement
              | None, None, None, Some (account, _)
                when not (String.equal account "equity:unclassified") -> Category
              | _ -> Unclassified)
         ; counter_account =
             (match reversal, transfer with
              | Some (original, reversed, _), _ -> reversal_account original reversed
              | None, Some _ -> Option.value_exn transfer_destination
              | None, None ->
                (match settlement with
                 | Some (account, _) -> account
                 | None -> Option.value_map category ~default:"equity:unclassified" ~f:fst))
         ; reason =
             (match reversal, transfer with
              | Some (_, _, reason), _ | None, Some (_, _, reason) -> Some reason
              | None, None ->
                (match settlement with
                 | Some (_, reason) -> Some reason
                 | None -> Option.map category ~f:snd))
         })
    source
;;

let movement_source db =
  let* source = events db in
  Ok (List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e))
;;

let bank_review db =
  let* source = movement_source db in
  let* table = movement_table db in
  let* metadata = review_metadata db in
  let* override_rows =
    collect
      db
      (statement
         "SELECT event_id,movement_type FROM movement_type_overrides"
         Db_schema.Movement_type_overrides.row)
  in
  let overrides =
    Hashtbl.of_alist_exn
      (module Int64)
      (List.map override_rows ~f:(fun (r : Db_schema.Movement_type_overrides.t) ->
         r.event_id, Movement_type.of_string r.movement_type))
  in
  let* observations = companion_observations db in
  let companions = registered_companions db in
  let details = Hashtbl.create (module Int64) in
  let* () =
    traverse
      (fun (observation : companion_observation) ->
         match observation.matching with
         | Some { state = Matched; event_id = Some event_id; _ } ->
           let row = observation.row in
           let* companion =
             List.find companions ~f:(fun (owner, (c : Companion.t)) ->
               String.equal owner row.institution && String.equal c.format row.format)
             |> Or_error.of_option
                  ~error:(Error.of_string "unregistered companion format")
           in
           let* annotation = (snd companion).annotate row in
           Hashtbl.add_multi details ~key:event_id ~data:(row.row_id, annotation);
           Ok ()
         | _ -> Ok ())
      observations
    |> Or_error.map ~f:ignore
  in
  let* evidence_rows =
    collect
      db
      (statement
         "SELECT event_id,card_reference,withdrawal_reversal FROM movement_evidence"
         Row.(
           t3
             (int64 "event_id")
             (option Rel.Type.text "card_reference")
             (text "withdrawal_reversal")))
  in
  let evidence = Hashtbl.create (module Int64) in
  List.iter evidence_rows ~f:(fun (event_id, card_reference, reversal) ->
    Hashtbl.set
      evidence
      ~key:event_id
      ~data:
        { Source_evidence.card_reference
        ; withdrawal_reversal = String.equal reversal "yes"
        });
  bank_review_from db source table metadata details overrides evidence
;;

type category_restore_preview =
  { categories_to_add : string list
  ; rules_to_add : Classification_rule.t list
  ; decisions_to_apply : Bank_category.t list
  ; unchanged : int
  ; problems : string list
  }

let category_fields_hash (e : Source_event.t) =
  List.map e.fields ~f:(fun (name, value) ->
    Printf.sprintf "%d:%s%d:%s" (String.length name) name (String.length value) value)
  |> String.concat
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let category_snapshot db =
  transaction db (fun db ->
    let* categories = categories db in
    let* rules = classification_rules db in
    let rules =
      List.filter rules ~f:(fun r ->
        match r.Classification_rule.action with
        | Category _ | Type_and_category _ -> true
        | _ -> false)
    in
    let* events = events db in
    let events_by_id = Hashtbl.of_alist_exn (module Int64) events in
    let* review = bank_review db in
    let* decisions =
      List.filter_map review ~f:(fun r ->
        if
          Poly.equal r.classification Unclassified || Poly.equal r.classification Category
        then Option.map r.reason ~f:(fun reason -> r, reason)
        else None)
      |> traverse (fun (r, reason) ->
        let* event =
          Hashtbl.find events_by_id r.event_id
          |> Or_error.of_option
               ~error:(Error.of_string "category references missing event")
        in
        Ok
          Category_portable.
            { institution = event.venue
            ; source_account = event.account
            ; kind = event.kind
            ; source_id = event.source_id
            ; occurred = Temporal.to_string event.occurred
            ; payload_sha256 = Digestif.SHA256.(to_hex (digest_string event.payload))
            ; fields_sha256 = category_fields_hash event
            ; account = r.counter_account
            ; reason
            })
    in
    Ok Category_portable.{ categories; rules; decisions })
;;

let preview_category_restore db (snapshot : Category_portable.t) =
  let* existing_categories = categories db in
  let* existing_rules = classification_rules db in
  let* source = events db in
  let* review = bank_review db in
  let by_identity = Hashtbl.Poly.create () in
  let by_evidence = Hashtbl.Poly.create () in
  let review_by_id = Hashtbl.create (module Int64) in
  List.iter review ~f:(fun (r : bank_review) ->
    Hashtbl.set review_by_id ~key:r.event_id ~data:r);
  List.iter source ~f:(fun (id, (e : Source_event.t)) ->
    let digest = Digestif.SHA256.(to_hex (digest_string e.payload)) in
    Hashtbl.add_multi
      by_identity
      ~key:(e.venue, e.account, e.kind, e.source_id)
      ~data:(id, e, digest);
    Hashtbl.add_multi
      by_evidence
      ~key:
        ( e.venue
        , e.account
        , e.kind
        , Temporal.to_string e.occurred
        , if List.is_empty e.fields
          then "payload:" ^ digest
          else "fields:" ^ category_fields_hash e )
      ~data:id);
  let problems = ref [] in
  let problem s = problems := s :: !problems in
  if List.contains_dup snapshot.categories ~compare:String.compare
  then problem "duplicate category names";
  List.iter snapshot.categories ~f:(fun account ->
    match Bank_category.create ~event_id:1L ~account ~reason:"portable category" with
    | Error e -> problem (Error.to_string_hum e)
    | Ok _ when String.equal account "equity:unclassified" ->
      problem "unclassified is not a category"
    | Ok _ -> ());
  let categories_to_add =
    List.filter snapshot.categories ~f:(fun c ->
      not (List.mem existing_categories c ~equal:String.equal))
  in
  if
    List.contains_dup
      (List.map snapshot.rules ~f:(fun r -> r.Classification_rule.name))
      ~compare:String.compare
  then problem "duplicate rule names";
  let rules_to_add =
    List.filter snapshot.rules ~f:(fun r ->
      match
        List.find existing_rules ~f:(fun current ->
          String.equal current.Classification_rule.name r.name)
      with
      | None -> true
      | Some current when Poly.equal current r -> false
      | Some _ ->
        problem ("rule already exists with different definition: " ^ r.name);
        false)
  in
  let ids =
    List.map snapshot.decisions ~f:(fun d ->
      d.Category_portable.institution, d.source_account, d.kind, d.source_id)
  in
  if List.contains_dup ids ~compare:[%compare: string * string * string * string]
  then problem "duplicate transaction identities";
  let unchanged = ref 0 in
  let decisions_to_apply =
    List.filter_map snapshot.decisions ~f:(fun d ->
      let label = d.institution ^ "/" ^ d.source_id in
      let matches =
        Option.value
          (Hashtbl.find
             by_identity
             (d.institution, d.source_account, d.kind, d.source_id))
          ~default:[]
      in
      match matches with
      | [ (id, e, digest) ] ->
        if
          not
            (String.equal (Temporal.to_string e.occurred) d.occurred
             && String.equal digest d.payload_sha256
             && String.equal (category_fields_hash e) d.fields_sha256)
        then (
          problem ("source evidence differs: " ^ label);
          None)
        else if
          List.length
            (Option.value
               (Hashtbl.find
                  by_evidence
                  ( d.institution
                  , d.source_account
                  , d.kind
                  , d.occurred
                  , if List.is_empty e.fields
                    then "payload:" ^ d.payload_sha256
                    else "fields:" ^ d.fields_sha256 ))
               ~default:[])
          > 1
        then (
          problem ("ambiguous duplicate source evidence: " ^ label);
          None)
        else (
          match Hashtbl.find review_by_id id with
          | None ->
            problem ("not a bank movement: " ^ label);
            None
          | Some row ->
            (match
               Bank_category.create ~event_id:id ~account:d.account ~reason:d.reason
             with
             | Error e ->
               problem (label ^ ": " ^ Error.to_string_hum e);
               None
             | Ok decision ->
               if
                 not
                   (Poly.equal row.classification Unclassified
                    || Poly.equal row.classification Category)
               then (
                 problem ("linked transfer, settlement or reversal: " ^ label);
                 None)
               else if Option.is_some row.reason
               then
                 if String.equal row.counter_account d.account
                 then (
                   Int.incr unchanged;
                   None)
                 else (
                   problem ("existing decision differs: " ^ label);
                   None)
               else Some decision))
      | [] ->
        problem ("transaction not found: " ^ label);
        None
      | _ ->
        problem ("ambiguous transaction identity: " ^ label);
        None)
  in
  Ok
    { categories_to_add
    ; rules_to_add
    ; decisions_to_apply
    ; unchanged = !unchanged
    ; problems = List.rev !problems
    }
;;

let restore_categories db ~expected snapshot =
  transaction db (fun db ->
    let* current = preview_category_restore db snapshot in
    if not (Poly.equal current expected)
    then Or_error.error_string "restore preview changed; review it again"
    else if not (List.is_empty current.problems)
    then Or_error.error_string "resolve restore problems before applying"
    else (
      let insert =
        Rel_sql.Stmt.(
          func "INSERT OR IGNORE INTO category_catalog(account) VALUES (?)" (text @-> unit))
      in
      let* () =
        List.fold_result current.categories_to_add ~init:() ~f:(fun () account ->
          db_result (Db.exec db.conn (insert account)))
      in
      let* () =
        List.fold_result current.rules_to_add ~init:() ~f:(fun () rule ->
          add_classification_rule_in_transaction db rule)
      in
      let* _ =
        if List.is_empty current.rules_to_add
        then Ok 0
        else retype db ~reason:"restore category rules"
      in
      let* changed = set_bank_categories_in_transaction db current.decisions_to_apply in
      Ok changed))
;;

let observed_provider_movement_types db =
  let* rows = bank_review db in
  Ok
    (List.filter_map rows ~f:(fun r ->
       match r.imported_movement_type with
       | Some (Movement_type.Other name) -> Some name
       | _ -> None)
     |> List.dedup_and_sort ~compare:String.compare)
;;

type movement_type_history_entry =
  { id : int64
  ; event_id : int64
  ; previous_type : Movement_type.t option
  ; movement_type : Movement_type.t option
  ; reason : string
  ; changed_at : Ptime.t
  }

let movement_type_history db =
  let* rows =
    collect
      db
      (statement
         "SELECT id,event_id,previous_type,movement_type,reason,changed_at FROM \
          movement_type_changes ORDER BY id"
         Db_schema.Movement_type_changes.row)
  in
  Ok
    (List.map rows ~f:(fun (r : Db_schema.Movement_type_changes.t) ->
       { id = r.id
       ; event_id = r.event_id
       ; previous_type = Option.map r.previous_type ~f:Movement_type.of_string
       ; movement_type = Option.map r.movement_type ~f:Movement_type.of_string
       ; reason = r.reason
       ; changed_at = r.changed_at
       }))
;;

let write_movement_type_override db ~event_id ~previous ~movement_type ~reason =
  if Option.equal Poly.equal previous movement_type
  then Ok false
  else
    let* () =
      match movement_type with
      | None ->
        let query =
          Rel_sql.Stmt.(
            func "DELETE FROM movement_type_overrides WHERE event_id=?" (int64 @-> unit))
        in
        db_result (Db.exec db.conn (query event_id))
      | Some kind ->
        let query =
          Rel_sql.Stmt.(
            func
              "INSERT INTO movement_type_overrides(event_id,movement_type) VALUES (?,?) \
               ON CONFLICT(event_id) DO UPDATE SET movement_type=excluded.movement_type"
              (int64 @-> text @-> unit))
        in
        db_result (Db.exec db.conn (query event_id (Movement_type.to_string kind)))
    in
    let event_value = event_id
    and old_value = Option.map previous ~f:Movement_type.to_string
    and new_value = Option.map movement_type ~f:Movement_type.to_string
    and reason_value = reason in
    let open Db_schema.Movement_type_changes in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    let* () =
      db_result
        (Db.exec
           db.conn
           (insert
              Col.
                [ Value (Db_schema.Movement_type_changes.event_id, event_value)
                ; Value (previous_type, old_value)
                ; Value (Db_schema.Movement_type_changes.movement_type, new_value)
                ; Value (Db_schema.Movement_type_changes.reason, reason_value)
                ]))
    in
    Ok true
;;

let set_movement_type_override db ~event_id ~movement_type =
  transaction db (fun db ->
    let* rows = bank_review db in
    let* row =
      List.find rows ~f:(fun r -> Int64.equal r.event_id event_id)
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
    in
    let* () =
      match movement_type with
      | Some (Movement_type.Other name) ->
        if
          List.exists rows ~f:(fun r ->
            Poly.equal r.imported_movement_type (Some (Movement_type.Other name)))
        then Ok ()
        else Or_error.error_string "unknown movement type; see fin tx type list"
      | _ -> Ok ()
    in
    write_movement_type_override
      db
      ~event_id
      ~previous:row.movement_type_override
      ~movement_type
      ~reason:"Manual type override")
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
  | Some (event_id, _) ->
    let* movement = movement db event_id in
    Ok movement.Movement.own_iban
;;

let known_account_ibans db source ~movement =
  traverse
    (fun (event_id, event) ->
       let* tx = movement event_id in
       let* iban, _ = account_iban db event tx in
       Ok (event, tx.Movement.account, iban))
    (List.dedup_and_sort source ~compare:(fun (_, a) (_, b) ->
       [%compare: string * string]
         (a.Source_event.venue, a.account)
         (b.Source_event.venue, b.account)))
;;

let transfer_iban_evidence_with known ~a_iban ~b_iban a ta b tb =
  let a_counterparty = ta.Movement.counterparty_iban
  and b_counterparty = tb.Movement.counterparty_iban in
  let assess counterparty target target_account target_iban =
    match counterparty, target_iban with
    | None, _ -> None
    | Some cp, Some own when Iban.equal cp own ->
      Some (true, "counterparty IBAN matches the other source account")
    | Some _, Some _ ->
      Some (false, "counterparty IBAN differs from the other source account's IBAN")
    | Some cp, None ->
      let conflicting =
        List.find_map known ~f:(fun (event, (account : Movement.account), iban) ->
          if
            String.equal event.Source_event.venue target.Source_event.venue
            && String.equal account.bank_id target_account.Movement.bank_id
            && String.equal account.account_id target_account.account_id
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
    List.filter_opt
      [ assess a_counterparty b tb.account b_iban
      ; assess b_counterparty a ta.account a_iban
      ]
  in
  let conflicts =
    List.filter_map evidence ~f:(fun (matches, detail) ->
      if matches then None else Some detail)
  in
  Ok (List.is_empty conflicts, List.exists evidence ~f:fst, List.map evidence ~f:snd)
;;

let transfer_iban_evidence db source a ta b tb =
  let* a_iban, _ = account_iban db a ta in
  let* b_iban, _ = account_iban db b tb in
  let source = List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e) in
  let* known = known_account_ibans db source ~movement:(movement db) in
  transfer_iban_evidence_with known ~a_iban ~b_iban a ta b tb
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

type reversal_candidate =
  { original : bank_review
  ; reversal : bank_review
  }

let reversal_candidates db =
  let* rows = bank_review db in
  let eligible r = Poly.equal r.classification Unclassified && Option.is_none r.reason in
  let originals =
    List.filter rows ~f:(fun r ->
      eligible r && Poly.equal r.movement_type (Some Movement_type.Withdrawal))
  in
  let reversals =
    List.filter rows ~f:(fun r ->
      eligible r
      && Poly.equal r.movement_type (Some Movement_type.Withdrawal)
      && Quantity.(r.transaction.amount > zero)
      && r.source_evidence.withdrawal_reversal)
  in
  Ok
    (List.concat_map reversals ~f:(fun reversal ->
       List.filter_map originals ~f:(fun original ->
         let a = original.transaction
         and b = reversal.transaction in
         Option.some_if
           (String.equal original.institution reversal.institution
            && Poly.equal a.account b.account
            && Quantity.(a.amount < zero && b.amount > zero && a.amount + b.amount = zero)
            && Quantity.is_zero a.fee
            && Quantity.is_zero b.fee
            && Date.compare a.booked_on b.booked_on <= 0)
           { original; reversal })))
;;

let reversal_change db ~original ~reversal ~undo =
  let original_id = original
  and reversal_id = reversal in
  transaction db (fun db ->
    let* source = events db in
    let get id =
      List.Assoc.find source id ~equal:Int64.equal
      |> Or_error.of_option ~error:(Error.of_string "unknown event ID")
    in
    let* a = get original in
    let* b = get reversal in
    let* entries =
      traverse
        (fun (id, e) ->
           let* entry = verify_bank_entry db id e in
           Ok (id, entry))
        [ original, a; reversal, b ]
    in
    let* () =
      if undo
      then
        let* existing = linked_reversal db original in
        if
          not
            (Option.exists existing ~f:(fun (x, y, _) ->
               Int64.equal x original && Int64.equal y reversal))
        then Or_error.error_string "no confirmed reversal for those event IDs"
        else (
          let delete =
            Rel_sql.Stmt.(
              func
                "DELETE FROM reversal_pairs WHERE original=? AND reversal=?"
                (int64 @-> int64 @-> unit))
          in
          db_result (Db.exec db.conn (delete original reversal)))
      else
        let* candidates = reversal_candidates db in
        if
          not
            (List.exists candidates ~f:(fun c ->
               Int64.equal c.original.event_id original
               && Int64.equal c.reversal.event_id reversal))
        then Or_error.error_string "pair is not an eligible unreviewed reversal candidate"
        else
          let open Db_schema.Reversal_pairs in
          let insert = Rel_sql.insert_into_cols Db.dialect table in
          db_result
            (Db.exec
               db.conn
               (insert
                  Col.
                    [ Value (Db_schema.Reversal_pairs.original, original_id)
                    ; Value (Db_schema.Reversal_pairs.reversal, reversal_id)
                    ; Value (reason, "Confirmed withdrawal reversal")
                    ]))
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
                  (update_posting
                     (Option.value account ~default:"equity:unclassified")
                     entry_id
                     1)))
        entries
    in
    let open Db_schema.Reversal_changes in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    db_result
      (Db.exec
         db.conn
         (insert
            Col.
              [ Value (Db_schema.Reversal_changes.original, original_id)
              ; Value (Db_schema.Reversal_changes.reversal, reversal_id)
              ; Value (action, if undo then "undone" else "confirmed")
              ])))
;;

type reversal_action =
  | Reversal_confirmed
  | Reversal_undone

type reversal_history_entry =
  { id : int64
  ; original : int64
  ; reversal : int64
  ; action : reversal_action
  ; changed_at : Ptime.t
  }

let reversal_history db =
  let* rows =
    collect
      db
      (statement
         "SELECT id,original,reversal,action,changed_at FROM reversal_changes ORDER BY id"
         Db_schema.Reversal_changes.row)
  in
  traverse
    (fun (row : Db_schema.Reversal_changes.t) ->
       let* action =
         match row.action with
         | "confirmed" -> Ok Reversal_confirmed
         | "undone" -> Ok Reversal_undone
         | _ -> Or_error.error_string "unknown reversal history action"
       in
       Ok
         ({ id = row.id
          ; original = row.original
          ; reversal = row.reversal
          ; action
          ; changed_at = row.changed_at
          }
          : reversal_history_entry))
    rows
;;

let set_bank_transfers ?(require_unreviewed = false) ?expected_previous db decisions =
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
          let* ta = movement db first in
          let* tb = movement db second in
          let* previous =
            db_result (Db.first db.conn (find_transfer_decision first second))
          in
          let* () =
            match expected_previous with
            | None -> Ok ()
            | Some (expected : Bank_transfer.decision) ->
              if
                Int64.equal first expected.first_event_id
                && Int64.equal second expected.second_event_id
                && Option.exists previous ~f:(fun (_, _, state, reason) ->
                  String.equal state (Bank_transfer.state_name expected.state)
                  && String.equal reason expected.reason)
              then Ok ()
              else
                Or_error.error_string
                  "transfer decision changed since preview; review it again"
          in
          let* () =
            if require_unreviewed && Option.is_some previous
            then
              Or_error.error_string
                "transfer decision changed since review; review the batch again"
            else Ok ()
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
                   let* settlement = card_settlement db id in
                   let* reversal = linked_reversal db id in
                   if
                     confirming
                     && Option.exists existing ~f:(fun (x, y, _) ->
                       not (Int64.equal x first && Int64.equal y second))
                   then
                     Or_error.error_string
                       "a movement already belongs to another confirmed transfer"
                   else if confirming && Option.is_some settlement
                   then
                     Or_error.error_string
                       "a movement is already a confirmed card settlement"
                   else if confirming && Option.is_some reversal
                   then
                     Or_error.error_string
                       "a movement is already part of a confirmed reversal"
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
  ; changed_at : Ptime.t
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

(* Movement pairs that may be one transfer between owned accounts: exactly opposite
   principals in one currency, on different accounts, booked at most seven days
   apart. Bank_transfer.eligible states the same rule for confirmations. Pairs come
   in the order of a scan over events in their listing order: later first
   movements first, then their counterparts in order. *)
let transfer_pairs =
  statement
    "WITH ordered AS (SELECT id, row_number() OVER (ORDER BY coalesce(occurred_on, \
     substr(occurred_at, 1, 10)), occurred_at, id) AS position FROM source_events) \
     SELECT a.event_id, b.event_id FROM movements a JOIN movements b ON b.currency = \
     a.currency AND b.principal_magnitude = a.principal_magnitude AND b.principal_sign \
     <> a.principal_sign JOIN ordered oa ON oa.id = a.event_id JOIN ordered ob ON ob.id \
     = b.event_id WHERE oa.position < ob.position AND a.principal_magnitude <> '0' AND \
     NOT EXISTS (SELECT 1 FROM card_settlement_decisions s WHERE s.event_id IN \
     (a.event_id,b.event_id)) AND NOT (a.venue = b.venue AND a.account = b.account) AND \
     NOT EXISTS (SELECT 1 FROM reversal_pairs r WHERE r.original IN \
     (a.event_id,b.event_id) OR r.reversal IN (a.event_id,b.event_id)) AND \
     abs(julianday(a.booked_on) - julianday(b.booked_on)) <= 7 ORDER BY oa.position \
     DESC, ob.position"
    Row.(
      t2
        (Col.make "first" Type.int64 Col.no_proj)
        (Col.make "second" Type.int64 Col.no_proj))
;;

let transfer_candidates_from ?pairs ?(include_reviewed = false) db source table transfers =
  let* known = known_account_ibans db source ~movement:(stored table) in
  let* items =
    traverse
      (fun (id, e) ->
         let* tx = stored table id in
         Ok (id, (id, e, tx, Hashtbl.mem transfers id)))
      (List.filter source ~f:(fun (id, _) -> Hashtbl.mem table id))
  in
  let items = Hashtbl.of_alist_exn (module Int64) items in
  let* decisions = bank_transfer_decisions db in
  let indexed = Hashtbl.Poly.create () in
  List.iter decisions ~f:(fun d ->
    Hashtbl.set indexed ~key:(d.Bank_transfer.first_event_id, d.second_event_id) ~data:d);
  let candidate (id, a, ta, la) (other, b, tb, lb) =
    let id, a, ta, other, b, tb =
      if Int64.(id < other) then id, a, ta, other, b, tb else other, b, tb, id, a, ta
    in
    let decision = Hashtbl.find indexed (id, other) in
    let* a_iban, _ = account_iban db a ta in
    let* b_iban, _ = account_iban db b tb in
    let* iban_compatible, iban_match, iban_evidence =
      transfer_iban_evidence_with known ~a_iban ~b_iban a ta b tb
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
           })
  in
  let* pairs =
    match pairs with
    | Some pairs -> Ok pairs
    | None -> collect db transfer_pairs
  in
  let* found =
    traverse
      (fun (first, second) ->
         let* x =
           Or_error.of_option
             (Hashtbl.find items first)
             ~error:(Error.of_string "missing transfer movement")
         in
         let* y =
           Or_error.of_option
             (Hashtbl.find items second)
             ~error:(Error.of_string "missing transfer movement")
         in
         candidate x y)
      pairs
  in
  let candidates = List.filter_opt found in
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

let transfer_candidates ?include_reviewed db =
  let* source = movement_source db in
  let* table = movement_table db in
  let* _, transfers, _, _ = review_metadata db in
  transfer_candidates_from ?include_reviewed db source table transfers
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

let bank_account_info_from ~summaries db source table =
  let* mappings = bank_accounts_from db source ~movement:(stored table) in
  traverse
    (fun mapping ->
       let source_account = Bank_account_mapping.source_account mapping in
       let members =
         List.filter source ~f:(fun (_, e) ->
           Registry.is_movement db.registry e
           && String.equal e.Source_event.venue mapping.Bank_account_mapping.institution
           && String.equal e.account source_account)
       in
       let event_id, event = List.hd_exn members in
       let* tx = stored table event_id in
       let* iban, iban_source = account_iban db event tx in
       let* settlement =
         db_result (Db.first db.conn (find_settlement mapping.institution source_account))
       in
       let* settlement_account =
         match settlement with
         | None -> Ok None
         | Some text ->
           let* reference = Json.decode Bank_account_config.reference_jsont text in
           Ok (Some reference)
       in
       let* count, first_on, last_on =
         Or_error.of_option
           (Hashtbl.Poly.find summaries (mapping.institution, source_account))
           ~error:(Error.of_string "missing account summary")
       in
       Ok
         { mapping
         ; iban
         ; iban_source
         ; currency = tx.account.currency
         ; movements = count
         ; first_on
         ; last_on
         ; settlement_account
         })
    mappings
;;

(* Aggregate separately by event kind so an unregistered adapter's rows do not
   enter status counts. The registry check below uses a representative event. *)
let status_summaries db source =
  let recognized = Hash_set.create (module Int64) in
  List.iter source ~f:(fun (id, _) -> Hash_set.add recognized id);
  let* rows =
    collect
      db
      (statement
         "SELECT \
          min(m.event_id),m.venue,m.account,count(*),min(m.booked_on),max(m.booked_on),sum(CASE \
          WHEN t.event_id IS NULL AND s.event_id IS NULL AND r.original IS NULL AND \
          coalesce(c.account,'equity:unclassified')='equity:unclassified' THEN 1 ELSE 0 \
          END) FROM movements m JOIN source_events e ON e.id=m.event_id LEFT JOIN \
          bank_categories c ON c.event_id=m.event_id LEFT JOIN (SELECT t.event_id FROM \
          bank_transfer_memberships t JOIN bank_transfer_decisions d ON \
          d.first_event_id=t.first_event_id AND d.second_event_id=t.second_event_id \
          WHERE d.state='confirmed') t ON t.event_id=m.event_id LEFT JOIN \
          card_settlement_decisions s ON s.event_id=m.event_id LEFT JOIN reversal_pairs \
          r ON r.original=m.event_id OR r.reversal=m.event_id GROUP BY \
          m.venue,m.account,e.kind"
         Row.(
           unit (fun id venue account count first last unclassified ->
             id, venue, account, count, first, last, unclassified)
           * int64 "event_id"
           * text "venue"
           * text "account"
           * int "count"
           * text "first"
           * text "last"
           * int "unclassified"))
  in
  let summaries = Hashtbl.Poly.create () in
  let* counts =
    traverse
      (fun (id, venue, account, count, first, last, unclassified) ->
         if not (Hash_set.mem recognized id)
         then Ok (0, 0)
         else
           let* first = Or_error.try_with (fun () -> Date.of_string first) in
           let* last = Or_error.try_with (fun () -> Date.of_string last) in
           Hashtbl.update summaries (venue, account) ~f:(function
             | None -> count, first, last
             | Some (n, a, b) ->
               ( n + count
               , (if Date.compare a first < 0 then a else first)
               , if Date.compare b last > 0 then b else last ));
           Ok (count, unclassified))
      rows
  in
  Ok (summaries, List.sum (module Int) counts ~f:fst, List.sum (module Int) counts ~f:snd)
;;

(* Include both the chronological first row used for account inspection and
   the representative chosen by account discovery and IBAN evidence. *)
let account_representatives source =
  let needed = Hash_set.create (module Int64) in
  let accounts = Hashtbl.Poly.create () in
  List.iter source ~f:(fun (id, e) ->
    Hashtbl.add accounts ~key:(e.Source_event.venue, e.account) ~data:id |> ignore);
  Hashtbl.iter accounts ~f:(Hash_set.add needed);
  List.dedup_and_sort source ~compare:(fun (_, a) (_, b) ->
    [%compare: string * string]
      (a.Source_event.venue, a.account)
      (b.Source_event.venue, b.account))
  |> List.iter ~f:(fun (id, _) -> Hash_set.add needed id);
  needed
;;

let selected_movements db needed =
  (* Only database-generated int64 IDs are rendered here, never input text. *)
  let ids =
    Hash_set.to_list needed |> List.map ~f:Int64.to_string |> String.concat ~sep:","
  in
  let* rows = movement_rows db (" WHERE event_id IN (" ^ ids ^ ")") in
  Ok (Hashtbl.of_alist_exn (module Int64) rows)
;;

let bank_account_info db =
  let* source = movement_source db in
  let* summaries, _, _ = status_summaries db source in
  let* table = selected_movements db (account_representatives source) in
  bank_account_info_from ~summaries db source table
;;

let card_settlement_preview ?card_id db event_id =
  let* reviews = bank_review db in
  let* row =
    List.find reviews ~f:(fun r -> Int64.equal r.event_id event_id)
    |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
  in
  let* () =
    if
      (not (String.equal row.counter_account "equity:unclassified"))
      || Option.is_some row.reason
    then Or_error.error_string "transaction is already reviewed"
    else if
      Quantity.(row.transaction.amount >= zero)
      || not (Quantity.is_zero row.transaction.fee)
    then Or_error.error_string "settlement requires a fee-free outgoing bank movement"
    else Ok ()
  in
  let* accounts = bank_account_info db in
  let account = row.transaction.account in
  let* reference =
    Bank_account_config.reference
      ~institution:row.institution
      ~bank_id:account.bank_id
      ~account_id:account.account_id
      ~account_type:account.account_type
  in
  let candidates =
    List.filter accounts ~f:(fun card ->
      List.mem
        [ "CREDITCRD"; "credit-card" ]
        card.mapping.account_type
        ~equal:String.equal
      && Option.value_map card.settlement_account ~default:false ~f:(fun r ->
        String.equal r.institution reference.institution
        && String.equal
             (Bank_account_config.source_account r)
             (Bank_account_config.source_account reference))
      && Iso4217.equal card.currency account.currency
      && Option.value_map card_id ~default:true ~f:(fun id ->
        String.equal id (Bank_account_mapping.id card.mapping)))
  in
  let* card =
    match candidates with
    | [ card ] -> Ok card
    | [] ->
      Or_error.error_string
        "no linked card matches this transaction; configure the card settlement account \
         with fin account edit"
    | _ -> Or_error.error_string "multiple linked cards; select one with --card ID"
  in
  if not (String.is_prefix card.mapping.ledger_account ~prefix:"liabilities:")
  then
    Or_error.error_string
      "map the card to a liabilities account with fin account edit first"
  else
    let* source_account =
      List.find accounts ~f:(fun a ->
        String.equal a.mapping.institution reference.institution
        && String.equal
             (Bank_account_mapping.source_account a.mapping)
             (Bank_account_config.source_account reference))
      |> Or_error.of_option ~error:(Error.of_string "missing settlement bank account")
    in
    if String.equal source_account.mapping.ledger_account card.mapping.ledger_account
    then Or_error.error_string "the bank and card must use different ledger accounts"
    else Ok (row, card)
;;

let confirm_card_settlement_in_transaction
      ?card_id
      ?expected_ledger
      ?(reason = "Confirmed card settlement")
      ?validated_card
      ?source_event
      db
      event_id
  =
  let* card =
    match validated_card with
    | Some card -> Ok card
    | None ->
      let* _, card = card_settlement_preview ?card_id db event_id in
      Ok card
  in
  let* () =
    if
      Option.value_map
        expected_ledger
        ~default:true
        ~f:(String.equal card.mapping.ledger_account)
    then Ok ()
    else Or_error.error_string "card account changed since the preview; review it again"
  in
  let* event =
    match source_event with
    | Some event -> Ok event
    | None ->
      let* source = events db in
      List.Assoc.find source event_id ~equal:Int64.equal
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
  in
  let* existing = verify_bank_entry db event_id event in
  let* reason_value = Validation.text "settlement reason" reason in
  let id_value = event_id in
  let open Db_schema.Card_settlement_decisions in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  let* () =
    db_result
      (Db.exec
         db.conn
         (insert
            Col.
              [ Value (event_id, id_value)
              ; Value (card_institution, card.mapping.institution)
              ; Value
                  (card_source_account, Bank_account_mapping.source_account card.mapping)
              ; Value (ledger_account, card.mapping.ledger_account)
              ; Value (reason, reason_value)
              ]))
  in
  let* () =
    match existing with
    | None -> Ok ()
    | Some entry_id ->
      db_result (Db.exec db.conn (update_posting card.mapping.ledger_account entry_id 1))
  in
  let open Db_schema.Card_settlement_changes in
  let audit = Rel_sql.insert_into_cols Db.dialect table in
  db_result
    (Db.exec
       db.conn
       (audit
          Col.
            [ Value (event_id, id_value)
            ; Value (card_institution, card.mapping.institution)
            ; Value (card_source_account, Bank_account_mapping.source_account card.mapping)
            ; Value (ledger_account, card.mapping.ledger_account)
            ; Value (action, "confirmed")
            ]))
;;

let confirm_card_settlement ?card_id ?expected_ledger db event_id =
  transaction db (fun db ->
    confirm_card_settlement_in_transaction ?card_id ?expected_ledger db event_id)
;;

let undo_card_settlement db event_id =
  transaction db (fun db ->
    let open Rel_sql.Stmt in
    let query =
      func
        "SELECT card_institution,card_source_account,ledger_account FROM \
         card_settlement_decisions WHERE event_id=?"
        (int64
         @-> ret
               Row.(
                 t3
                   (text "card_institution")
                   (text "card_source_account")
                   (text "ledger_account")))
    in
    let* info = db_result (Db.first db.conn (query event_id)) in
    let* institution, source_account, account =
      Or_error.of_option
        info
        ~error:(Error.of_string "no confirmed settlement for this transaction")
    in
    let* source = events db in
    let* event =
      List.Assoc.find source event_id ~equal:Int64.equal
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction")
    in
    let* existing = verify_bank_entry db event_id event in
    let delete =
      func "DELETE FROM card_settlement_decisions WHERE event_id=?" (int64 @-> unit)
    in
    let* () = db_result (Db.exec db.conn (delete event_id)) in
    let* category = category_account db event_id in
    let* () =
      match existing with
      | None -> Ok ()
      | Some entry_id ->
        db_result
          (Db.exec
             db.conn
             (update_posting
                (Option.value category ~default:"equity:unclassified")
                entry_id
                1))
    in
    let id_value = event_id in
    let open Db_schema.Card_settlement_changes in
    let audit = Rel_sql.insert_into_cols Db.dialect table in
    db_result
      (Db.exec
         db.conn
         (audit
            Col.
              [ Value (event_id, id_value)
              ; Value (card_institution, institution)
              ; Value (card_source_account, source_account)
              ; Value (ledger_account, account)
              ; Value (action, "undone")
              ])))
;;

type card_settlement_action =
  | Confirmed
  | Undone

type card_settlement_change =
  { id : int64
  ; event_id : int64
  ; card_institution : string
  ; card_source_account : string
  ; ledger_account : string
  ; action : card_settlement_action
  ; changed_at : Ptime.t
  }

let card_settlement_history db =
  let open Db_schema.Card_settlement_changes in
  let row =
    Row.(
      unit
        (fun
            id
             event_id
             card_institution
             card_source_account
             ledger_account
             action
             changed_at
           ->
           ( id
           , event_id
           , card_institution
           , card_source_account
           , ledger_account
           , action
           , changed_at ))
      * read_column id
      * read_column event_id
      * read_column card_institution
      * read_column card_source_account
      * read_column ledger_account
      * read_column action
      * read_column changed_at)
  in
  let* rows =
    collect
      db
      (statement
         "SELECT \
          id,event_id,card_institution,card_source_account,ledger_account,action,changed_at \
          FROM card_settlement_changes ORDER BY id"
         row)
  in
  traverse
    (fun ( id
         , event_id
         , card_institution
         , card_source_account
         , ledger_account
         , action
         , changed_at ) ->
       let* action =
         match action with
         | "confirmed" -> Ok Confirmed
         | "undone" -> Ok Undone
         | _ -> Or_error.error_string "unknown card settlement action"
       in
       Ok
         ({ id
          ; event_id
          ; card_institution
          ; card_source_account
          ; ledger_account
          ; action
          ; changed_at
          }
          : card_settlement_change))
    rows
;;

type classification_rule_match =
  { transaction : bank_review
  ; eligible : bool
  ; apply_type : bool
  ; apply_account : bool
  ; settlement_ledger : string option
  ; explanation : string option
  }

let preview_classification_rule db (rule : Classification_rule.t) =
  let* pattern =
    match rule.pattern with
    | None -> Ok None
    | Some text ->
      let* p = Transaction_pattern.compile ~case_sensitive:rule.case_sensitive text in
      Ok (Some p)
  in
  let* rows = bank_review db in
  let* settlement_card =
    match rule.action with
    | Settlement card_id ->
      let* accounts = bank_account_info db in
      let* card =
        match
          List.filter accounts ~f:(fun info ->
            String.equal (Bank_account_mapping.id info.mapping) card_id)
        with
        | [ card ] -> Ok card
        | [] -> Or_error.error_string "rule references an unknown card ID"
        | _ -> Or_error.error_string "rule card ID is ambiguous"
      in
      if
        not
          (List.mem
             [ "CREDITCRD"; "credit-card" ]
             card.mapping.account_type
             ~equal:String.equal)
      then Or_error.error_string "rule settlement target is not a credit card"
      else if not (String.is_prefix card.mapping.ledger_account ~prefix:"liabilities:")
      then Or_error.error_string "map the rule's card to a liabilities account first"
      else if Option.is_none card.settlement_account
      then Or_error.error_string "link the rule's card to its settlement account first"
      else Ok (Some card)
    | _ -> Ok None
  in
  let* raw_types =
    match rule.mode, rule.action, rule.match_type with
    | Automatic, (Type _ | Type_and_category _), Some _ ->
      let* source = events db in
      source
      |> List.filter ~f:(fun (_, event) -> Registry.is_movement db.registry event)
      |> traverse (fun (id, event) ->
        let profile = Source_typing.create ~institution:event.Source_event.venue [] in
        let* movement = Registry.movement ~source_typing:profile db.registry event in
        Ok (id, movement.movement_type))
    | _ -> Ok []
  in
  let matched =
    List.filter rows ~f:(fun row ->
      let matched_type =
        match rule.mode, rule.action, rule.match_type with
        | Automatic, (Type _ | Type_and_category _), Some _ ->
          Option.value
            (List.Assoc.find raw_types row.event_id ~equal:Int64.equal)
            ~default:None
        | _ -> row.movement_type
      in
      Option.value_map rule.institution ~default:true ~f:(String.equal row.institution)
      && Option.value_map
           rule.account_type
           ~default:true
           ~f:(String.equal row.transaction.account.account_type)
      && Option.value_map rule.match_type ~default:true ~f:(function
        | Classification_rule.Unknown -> Option.is_none matched_type
        | Is kind -> Option.value_map matched_type ~default:false ~f:(Poly.equal kind))
      && Option.value_map pattern ~default:true ~f:(fun p ->
        let display_name, descriptions =
          match rule.mode with
          | Automatic ->
            ( None
            , [ Option.value_map
                  (List.hd row.transaction.descriptions)
                  ~default:""
                  ~f:String.strip
              ] )
          | Review | Disabled -> None, [ row.description ]
        in
        Transaction_pattern.matches p ~display_name ~descriptions))
  in
  Ok
    (List.map matched ~f:(fun row ->
       let type_requested =
         Option.is_some (Classification_rule.action_type rule.action)
       in
       let account_requested =
         match rule.action with
         | Category _ | Settlement _ | Type_and_category _ -> true
         | Type _ | Card_reference | Withdrawal_reversal_hint -> false
       in
       let type_block = type_requested && Option.is_some row.movement_type_override in
       let already_reviewed =
         (not (String.equal row.counter_account "equity:unclassified"))
         || Option.is_some row.reason
       in
       let account_reason =
         if not account_requested
         then None
         else if already_reviewed
         then Some "already reviewed"
         else (
           match settlement_card with
           | None -> None
           | Some card ->
             let movement = row.transaction in
             let linked = Option.value_exn card.settlement_account in
             let account = movement.account in
             if
               not
                 (String.equal row.institution linked.institution
                  && String.equal account.bank_id linked.bank_id
                  && String.equal account.account_id linked.account_id
                  && String.equal account.account_type linked.account_type)
             then Some "not from the linked settlement account"
             else if
               (not (Iso4217.equal card.currency account.currency))
               || Quantity.(movement.amount >= zero)
               || not (Quantity.is_zero movement.fee)
             then Some "not a fee-free debit in the card currency"
             else if
               List.exists rows ~f:(fun other ->
                 let tx = other.transaction in
                 String.equal other.institution card.mapping.institution
                 && String.equal tx.account.bank_id card.mapping.bank_id
                 && String.equal tx.account.account_id card.mapping.account_id
                 && String.equal tx.account.account_type card.mapping.account_type
                 && Iso4217.equal tx.account.currency account.currency
                 && Quantity.equal tx.amount (Quantity.neg movement.amount)
                 && Int.abs (Date.diff tx.booked_on movement.booked_on) <= 7)
             then Some "matching card-side payment exists; review as a transfer"
             else None)
       in
       let type_already_matches =
         Option.value_map
           (Classification_rule.action_type rule.action)
           ~default:false
           ~f:(fun kind ->
             Option.value_map row.movement_type ~default:false ~f:(Poly.equal kind))
       in
       let apply_type = type_requested && (not type_block) && not type_already_matches in
       let apply_account = account_requested && Option.is_none account_reason in
       let reasons =
         List.filter_opt
           [ Option.some_if type_block "type override already reviewed"
           ; Option.some_if
               (type_requested && (not type_block) && type_already_matches)
               "type already matches"
           ; account_reason
           ]
       in
       { transaction = row
       ; eligible = apply_type || apply_account
       ; apply_type
       ; apply_account
       ; settlement_ledger =
           Option.map settlement_card ~f:(fun card -> card.mapping.ledger_account)
       ; explanation =
           Option.some_if (not (List.is_empty reasons)) (String.concat ~sep:"; " reasons)
       }))
;;

let apply_classification_rule db ~expected ~selected =
  let event_ids = List.map selected ~f:(fun m -> m.transaction.event_id) in
  if List.contains_dup event_ids ~compare:Int64.compare
  then Or_error.error_string "duplicate rule match"
  else
    transaction db (fun db ->
      let* rules = classification_rules db in
      let* current =
        List.find rules ~f:(fun r ->
          String.equal r.Classification_rule.name expected.Classification_rule.name)
        |> Or_error.of_option ~error:(Error.of_string "rule no longer exists")
      in
      if not (Poly.equal current expected)
      then Or_error.error_string "rule changed since preview; review it again"
      else
        let* matches = preview_classification_rule db current in
        let* () =
          traverse
            (fun old ->
               match
                 List.find matches ~f:(fun m ->
                   Int64.equal m.transaction.event_id old.transaction.event_id)
               with
               | Some now when old.eligible && Poly.equal old now -> Ok ()
               | _ ->
                 Or_error.error_string
                   "a rule match changed since preview; review it again")
            selected
          |> Result.map ~f:ignore
        in
        let reason =
          Printf.sprintf
            "Rule %s%s"
            current.name
            (Option.value_map current.pattern ~default:"" ~f:(fun p -> ": " ^ p))
        in
        let* () =
          match Classification_rule.action_type current.action with
          | None -> Ok ()
          | Some kind ->
            let* _ =
              traverse
                (fun m ->
                   if not m.apply_type
                   then Ok false
                   else
                     write_movement_type_override
                       db
                       ~event_id:m.transaction.event_id
                       ~previous:m.transaction.movement_type_override
                       ~movement_type:(Some kind)
                       ~reason)
                selected
            in
            Ok ()
        in
        let account_ids =
          List.filter_map selected ~f:(fun m ->
            Option.some_if m.apply_account m.transaction.event_id)
        in
        match current.action with
        | Card_reference | Withdrawal_reversal_hint ->
          Or_error.error_string
            "evidence rules apply during import; refresh stored evidence to update \
             existing transactions"
        | Type _ -> Ok (List.length selected)
        | Category account | Type_and_category (_, account) ->
          let* decisions =
            traverse
              (fun event_id -> Bank_category.create ~event_id ~account ~reason)
              account_ids
          in
          let* _ =
            set_bank_categories_in_transaction ~only_unclassified:true db decisions
          in
          Ok (List.length selected)
        | Settlement card_id ->
          let* source = events db in
          let source = Hashtbl.of_alist_exn (module Int64) source in
          let* card =
            match account_ids with
            | [] -> Ok None
            | first :: _ ->
              let* _, card = card_settlement_preview ~card_id db first in
              Ok (Some card)
          in
          let* () =
            List.fold_result account_ids ~init:() ~f:(fun () event_id ->
              let* event =
                Hashtbl.find source event_id
                |> Or_error.of_option
                     ~error:(Error.of_string "unknown transaction event ID")
              in
              confirm_card_settlement_in_transaction
                ~card_id
                ~reason
                ?validated_card:card
                ~source_event:event
                db
                event_id)
          in
          Ok (List.length selected))
;;

let bank_overview db =
  let* source = movement_source db in
  let* pairs = collect db transfer_pairs in
  let needed = account_representatives source in
  List.iter pairs ~f:(fun (a, b) ->
    Hash_set.add needed a;
    Hash_set.add needed b);
  let* table = selected_movements db needed in
  let* _, transfers, _, _ = review_metadata db in
  let* summaries, movements, unclassified = status_summaries db source in
  let* accounts = bank_account_info_from ~summaries db source table in
  let* candidates = transfer_candidates_from ~pairs db source table transfers in
  Ok (accounts, movements, unclassified, List.length candidates)
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
             let* ta = movement db d.first_event_id in
             let* tb = movement db d.second_event_id in
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
  ; changed_at : Ptime.t
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
      * Col.make "changed_at" Db_schema.instant Col.no_proj)
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

let apply_bank_accounts_in_transaction ?(dry_run = false) db config =
  let* config = Bank_account_config.of_json (Bank_account_config.to_json config) in
  let* () =
    if dry_run then db_result (Db.exec_sql db.conn "SAVEPOINT account_preview") else Ok ()
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
              "%s has no imported statements; import them with fin import %s FOLDER \
               before importing the account settings"
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
        "%d configured account%s ha%s no imported statements: %s. Import each one's \
         statement folder with fin import BANK PATH, then import the account settings \
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
              let* () = db_result (Db.exec db.conn (remove_iban r.institution source)) in
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
         bank_account_settlements(institution,source_account,settlement_account) VALUES \
         (?,?,?) ON CONFLICT(institution,source_account) DO UPDATE SET \
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
               else Ok (Some (Json.encode Bank_account_config.reference_jsont parent))
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
  Ok result
;;

let apply_bank_accounts ?(dry_run = false) db config =
  transaction db (fun db -> apply_bank_accounts_in_transaction ~dry_run db config)
;;

let confirm_card_settlement_with_setup
      db
      ~event_id
      ~card_id
      ~expected_ledger
      ~expected_link
      ~setup
  =
  transaction db (fun db ->
    let* accounts = bank_account_info db in
    let* card =
      match
        List.filter accounts ~f:(fun info ->
          String.equal (Bank_account_mapping.id info.mapping) card_id)
      with
      | [ card ] -> Ok card
      | [] -> Or_error.error_string "card no longer exists"
      | _ -> Or_error.error_string "card ID collision; review is refused"
    in
    let equal_reference
          (a : Bank_account_config.reference)
          (b : Bank_account_config.reference)
      =
      String.equal a.Bank_account_config.institution b.institution
      && String.equal
           (Bank_account_config.source_account a)
           (Bank_account_config.source_account b)
    in
    let* () =
      if
        String.equal
          setup.Bank_account_config.reference.institution
          card.mapping.institution
        && String.equal
             (Bank_account_config.source_account setup.reference)
             (Bank_account_mapping.source_account card.mapping)
      then Ok ()
      else Or_error.error_string "card setup does not match the selected card"
    in
    let* () =
      if
        String.equal card.mapping.ledger_account expected_ledger
        && Option.equal equal_reference card.settlement_account expected_link
      then Ok ()
      else Or_error.error_string "card setup changed since the preview; review it again"
    in
    let* _ = apply_bank_accounts_in_transaction db [ setup ] in
    confirm_card_settlement_in_transaction ~card_id db event_id)
;;

let registry t = t.registry

(* Deletion deliberately removes immutable evidence only within one transaction;
   the original triggers are restored before commit and rollback restores them on
   every error. Refuse relationships that would affect another account. *)
type account_deletion =
  { transactions : int
  ; documents : int
  }

let delete_account ?(dry_run = false) db (mapping : Bank_account_mapping.t) =
  transaction db (fun db ->
    let quote value =
      "'" ^ String.substr_replace_all value ~pattern:"'" ~with_:"''" ^ "'"
    in
    let institution = quote mapping.institution in
    let account = quote (Bank_account_mapping.source_account mapping) in
    let exec sql = db_result (Db.exec_sql db.conn sql) in
    let scalar sql =
      let* value = db_result (Db.first db.conn (statement sql integer)) in
      Or_error.of_option value ~error:(Error.of_string "account deletion count missing")
    in
    let events =
      "SELECT id FROM source_events WHERE venue="
      ^ institution
      ^ " AND account="
      ^ account
    in
    let docs =
      "SELECT DISTINCT document_id FROM import_document_events WHERE event_id IN ("
      ^ events
      ^ ")"
    in
    let* transactions =
      scalar ("SELECT count(*) FROM source_events WHERE id IN (" ^ events ^ ")")
    in
    let* settlements =
      scalar
        ("SELECT count(*) FROM card_settlement_decisions WHERE event_id IN ("
         ^ events
         ^ ") OR (card_institution="
         ^ institution
         ^ " AND card_source_account="
         ^ account
         ^ ")")
    in
    let* reversals =
      scalar
        ("SELECT count(*) FROM reversal_pairs WHERE original IN ("
         ^ events
         ^ ") OR reversal IN ("
         ^ events
         ^ ")")
    in
    let* documents =
      scalar ("SELECT count(*) FROM import_documents WHERE id IN (" ^ docs ^ ")")
    in
    if reversals > 0
    then
      Or_error.error_string
        "account has confirmed reversals; undo them before deleting it"
    else if settlements > 0
    then
      Or_error.error_string "account has confirmed card settlements; deletion is refused"
    else if transactions = 0
    then Or_error.error_string "account has no imported transactions"
    else
      let* shared =
        scalar
          ("SELECT count(*) FROM import_document_events WHERE document_id IN ("
           ^ docs
           ^ ") AND event_id NOT IN ("
           ^ events
           ^ ")")
      in
      let* statements_shared =
        scalar
          ("SELECT count(*) FROM statements WHERE document_id IN ("
           ^ docs
           ^ ") AND (institution<>"
           ^ institution
           ^ " OR source_account<>"
           ^ account
           ^ ")")
      in
      let* companions =
        scalar ("SELECT count(*) FROM companion_rows WHERE document_id IN (" ^ docs ^ ")")
      in
      let* matches =
        scalar
          ("SELECT (SELECT count(*) FROM companion_matches WHERE event_id IN ("
           ^ events
           ^ ") OR EXISTS (SELECT 1 FROM json_each(candidates_json) WHERE CAST(value AS \
              INTEGER) IN ("
           ^ events
           ^ "))) + (SELECT count(*) FROM companion_match_changes WHERE event_id IN ("
           ^ events
           ^ ") OR previous_event_id IN ("
           ^ events
           ^ ") OR EXISTS (SELECT 1 FROM json_each(candidates_json) WHERE CAST(value AS \
              INTEGER) IN ("
           ^ events
           ^ ")))")
      in
      let* transfers =
        scalar
          ("SELECT (SELECT count(*) FROM bank_transfer_decisions WHERE first_event_id IN \
            ("
           ^ events
           ^ ") OR second_event_id IN ("
           ^ events
           ^ ")) + (SELECT count(*) FROM bank_transfer_changes WHERE first_event_id IN ("
           ^ events
           ^ ") OR second_event_id IN ("
           ^ events
           ^ "))")
      in
      let* reference =
        Bank_account_config.reference
          ~institution:mapping.institution
          ~bank_id:mapping.bank_id
          ~account_id:mapping.account_id
          ~account_type:mapping.account_type
      in
      let target = quote (Json.encode Bank_account_config.reference_jsont reference) in
      let* settlement_refs =
        scalar
          ("SELECT (SELECT count(*) FROM bank_account_settlements WHERE \
            settlement_account="
           ^ target
           ^ " AND NOT (institution="
           ^ institution
           ^ " AND source_account="
           ^ account
           ^ ")) + (SELECT count(*) FROM bank_account_settlement_changes WHERE \
              (settlement_account="
           ^ target
           ^ " OR previous_settlement="
           ^ target
           ^ ") AND NOT (institution="
           ^ institution
           ^ " AND source_account="
           ^ account
           ^ "))")
      in
      let* shared_inventories =
        scalar
          ("SELECT count(*) FROM day_inventories WHERE document_id IN ("
           ^ docs
           ^ ") AND (institution<>"
           ^ institution
           ^ " OR source_account<>"
           ^ account
           ^ ")")
      in
      if
        shared
        + statements_shared
        + companions
        + matches
        + transfers
        + settlement_refs
        + shared_inventories
        > 0
      then
        Or_error.error_string
          "account has shared documents, companion matches, transfer decisions or \
           settlement references; deletion cannot safely affect other accounts"
      else if dry_run
      then Ok { transactions; documents }
      else
        let* () = exec "CREATE TEMP TABLE fin_delete_events(id INTEGER PRIMARY KEY)" in
        let* () = exec ("INSERT INTO fin_delete_events " ^ events) in
        let* () = exec "CREATE TEMP TABLE fin_delete_docs(id INTEGER PRIMARY KEY)" in
        let* () = exec ("INSERT INTO fin_delete_docs " ^ docs) in
        let tables =
          [ "source_events"
          ; "movements"
          ; "import_documents"
          ; "statements"
          ; "day_inventories"
          ; "bank_import_attempts"
          ; "document_contexts"
          ; "bank_category_changes"
          ; "bank_account_mapping_changes"
          ; "bank_account_identity_changes"
          ; "bank_account_settlement_changes"
          ; "reversal_changes"
          ; "movement_type_changes"
          ]
        in
        let rec remove_triggers acc = function
          | [] -> Ok acc
          | table :: rest ->
            let name = table ^ "_no_delete" in
            let* sql =
              db_result
                (Db.first
                   db.conn
                   (statement
                      ("SELECT sql FROM sqlite_schema WHERE type='trigger' AND name="
                       ^ quote name)
                      Row.(t1 (text "sql"))))
            in
            (match sql with
             | None -> Or_error.errorf "missing immutable trigger %s" name
             | Some sql ->
               let* () = exec ("DROP TRIGGER " ^ name) in
               remove_triggers (sql :: acc) rest)
        in
        let* triggers = remove_triggers [] tables in
        let event_ids = "SELECT id FROM fin_delete_events"
        and doc_ids = "SELECT id FROM fin_delete_docs" in
        let local = "institution=" ^ institution ^ " AND source_account=" ^ account in
        let statements =
          [ "DELETE FROM postings WHERE entry_id IN (SELECT id FROM journal_entries \
             WHERE event_id IN ("
            ^ event_ids
            ^ "))"
          ; "DELETE FROM journal_entries WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM bank_categories WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM bank_category_changes WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM movement_type_overrides WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM movement_type_changes WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM reversal_changes WHERE original IN ("
            ^ event_ids
            ^ ") OR reversal IN ("
            ^ event_ids
            ^ ")"
          ; "DELETE FROM statement_events WHERE event_id IN ("
            ^ event_ids
            ^ ") OR statement_id IN (SELECT id FROM statements WHERE document_id IN ("
            ^ doc_ids
            ^ "))"
          ; "DELETE FROM statements WHERE document_id IN (" ^ doc_ids ^ ")"
          ; "DELETE FROM import_document_events WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM day_inventories WHERE " ^ local
          ; "DELETE FROM bank_import_attempts WHERE document_id IN (" ^ doc_ids ^ ")"
          ; "DELETE FROM document_contexts WHERE document_id IN (" ^ doc_ids ^ ")"
          ; "DELETE FROM import_documents WHERE id IN (" ^ doc_ids ^ ")"
          ; "DELETE FROM movement_evidence WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM movements WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM source_event_fields WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM source_events WHERE id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM bank_account_mappings WHERE " ^ local
          ; "DELETE FROM bank_account_mapping_changes WHERE " ^ local
          ; "DELETE FROM bank_account_identities WHERE " ^ local
          ; "DELETE FROM bank_account_identity_changes WHERE " ^ local
          ; "DELETE FROM bank_account_settlements WHERE " ^ local
          ; "DELETE FROM bank_account_settlement_changes WHERE " ^ local
          ; "DROP TABLE fin_delete_events"
          ; "DROP TABLE fin_delete_docs"
          ]
        in
        let* () = List.fold_result statements ~init:() ~f:(fun () sql -> exec sql) in
        let* () = List.fold_result triggers ~init:() ~f:(fun () sql -> exec sql) in
        Ok { transactions; documents })
;;

type type_rule_outcome =
  | Newly_typed
  | Changed
  | Unchanged
  | Shadowed of string
  | Overridden

type type_rule_row =
  { event_id : int64
  ; institution : string
  ; description : string
  ; before : Movement_type.t option
  ; after : Movement_type.t option
  ; outcome : type_rule_outcome
  }

type type_rule_test =
  { scanned : int
  ; matches : type_rule_row list
  ; untyped_before : int
  ; untyped_after : int
  }

let typing_rule (rule : Classification_rule.t) kind =
  Source_typing.make_rule
    ?field:rule.field
    (Option.value rule.pattern ~default:".*")
    rule.account_type
    rule.case_sensitive
    (Movement_type.to_string kind)
  |> Or_error.map ~f:(fun typing ->
    Source_typing.with_match_type
      typing
      (Option.map rule.match_type ~f:Classification_rule.condition_name))
;;

let test_type_rule db (draft : Classification_rule.t) =
  let* draft_kind =
    Classification_rule.action_type draft.action
    |> Or_error.of_option
         ~error:(Error.of_string "only rules that set a movement type can be tested")
  in
  let* draft_typing = typing_rule draft draft_kind in
  let* saved = classification_rules db in
  let* saved =
    List.filter saved ~f:(fun (r : Classification_rule.t) ->
      (match r.mode with
       | Automatic -> true
       | Review | Disabled -> false)
      && Option.is_some (Classification_rule.action_type r.action))
    |> List.map ~f:(fun (r : Classification_rule.t) ->
      let* typing =
        typing_rule r (Option.value_exn (Classification_rule.action_type r.action))
      in
      Ok (r, (r.priority, r.name, typing)))
    |> Or_error.combine_errors
  in
  let ordered entries =
    List.sort entries ~compare:(fun (p1, n1, _) (p2, n2, _) ->
      match Int.compare p1 p2 with
      | 0 -> String.compare n1 n2
      | c -> c)
  in
  (* The saved version of the draft's rule is part of "before" and absent from
     "after", so editing a rule shows what the edit changes. *)
  let for_venue ~without_draft venue =
    List.filter_map saved ~f:(fun ((r : Classification_rule.t), entry) ->
      Option.some_if
        (Option.value_map r.institution ~default:true ~f:(String.equal venue)
         && not (without_draft && String.equal r.name draft.name))
        entry)
  in
  let draft_applies venue =
    Option.value_map draft.institution ~default:true ~f:(String.equal venue)
  in
  let* source = movement_source db in
  let source =
    List.filter source ~f:(fun (_, (e : Source_event.t)) -> draft_applies e.venue)
  in
  let* columns = companion_columns db in
  let* override_rows =
    collect
      db
      (statement
         "SELECT event_id,movement_type FROM movement_type_overrides"
         Db_schema.Movement_type_overrides.row)
  in
  let overridden =
    Set.of_list
      (module Int64)
      (List.map override_rows ~f:(fun (r : Db_schema.Movement_type_overrides.t) ->
         r.event_id))
  in
  let winner rules movement =
    List.find rules ~f:(fun (_, _, r) -> Source_typing.matches r movement)
  in
  let type_of (movement : Movement.t) = function
    | None -> movement.movement_type
    | Some (_, _, (r : Source_typing.rule)) -> Some r.kind
  in
  let* results =
    traverse
      (fun (event_id, (event : Source_event.t)) ->
         let base = Source_typing.create ~institution:event.venue [] in
         let* movement = Registry.movement ~source_typing:base db.registry event in
         let movement =
           match Hashtbl.find columns event_id with
           | None -> movement
           | Some more -> Movement.with_fields movement (movement.fields @ more)
         in
         let before_rules = ordered (for_venue ~without_draft:false event.venue) in
         let after_rules =
           ordered
             ((draft.priority, draft.name, draft_typing)
              :: for_venue ~without_draft:true event.venue)
         in
         let before_winner = winner before_rules movement in
         let after_winner = winner after_rules movement in
         let before = type_of movement before_winner in
         let after = type_of movement after_winner in
         let is_overridden = Set.mem overridden event_id in
         let row =
           if not (Source_typing.matches draft_typing movement)
           then None
           else (
             let outcome =
               if is_overridden
               then Overridden
               else (
                 match after_winner with
                 | Some (_, name, _) when not (String.equal name draft.name) ->
                   Shadowed name
                 | _ ->
                   (match before with
                    | None -> Newly_typed
                    | Some _ when Option.equal Poly.equal before after -> Unchanged
                    | Some _ -> Changed))
             in
             Some
               { event_id
               ; institution = event.venue
               ; description = String.concat ~sep:" | " movement.descriptions
               ; before
               ; after
               ; outcome
               })
         in
         Ok
           ( row
           , (not is_overridden) && Option.is_none before
           , (not is_overridden) && Option.is_none after ))
      source
  in
  Ok
    { scanned = List.length source
    ; matches = List.filter_map results ~f:(fun (row, _, _) -> row)
    ; untyped_before = List.count results ~f:(fun (_, b, _) -> b)
    ; untyped_after = List.count results ~f:(fun (_, _, a) -> a)
    }
;;

type description_gap =
  { institution : string
  ; key : string
  ; count : int
  ; first_on : Date.t
  ; last_on : Date.t
  ; debits : int
  ; credits : int
  ; example : string
  ; pattern : string
  }

let description_gaps ?institution db =
  let* reviews = bank_review db in
  let groups = Hashtbl.create (module String) in
  List.iter reviews ~f:(fun (r : bank_review) ->
    if
      Option.is_none r.movement_type
      && Option.value_map institution ~default:true ~f:(String.equal r.institution)
    then (
      let description =
        Option.value
          (List.hd (Movement.field_values r.transaction "description"))
          ~default:""
      in
      let key = Description_key.key description in
      Hashtbl.add_multi
        groups
        ~key:(r.institution ^ "\000" ^ key)
        ~data:(r.institution, key, description, r.transaction)));
  Hashtbl.data groups
  |> List.map ~f:(fun members ->
    let institution, key, example, _ = List.hd_exn members in
    let dates = List.map members ~f:(fun (_, _, _, (m : Movement.t)) -> m.booked_on) in
    let debits =
      List.count members ~f:(fun (_, _, _, (m : Movement.t)) ->
        Quantity.(m.amount < zero))
    in
    { institution
    ; key
    ; count = List.length members
    ; first_on = List.min_elt dates ~compare:Date.compare |> Option.value_exn
    ; last_on = List.max_elt dates ~compare:Date.compare |> Option.value_exn
    ; debits
    ; credits = List.length members - debits
    ; example
    ; pattern = Description_key.pattern key
    })
  |> List.sort ~compare:(fun a b ->
    match Int.compare b.count a.count with
    | 0 ->
      (match String.compare a.institution b.institution with
       | 0 -> String.compare a.key b.key
       | c -> c)
    | c -> c)
  |> fun gaps -> Ok gaps
;;

type institution_summary =
  { movements : int
  ; first_on : string option
  ; last_on : string option
  ; by_type : (Movement_type.t option * int) list
  }

let institution_summary db institution =
  let* rows =
    db_result
      (Db.fold
         db.conn
         (Rel_sql.Stmt.(
            func
              "SELECT m.event_id,m.booked_on,m.movement_type,o.movement_type FROM \
               movements m JOIN source_events e ON e.id=m.event_id LEFT JOIN \
               movement_type_overrides o ON o.event_id=m.event_id WHERE e.venue=?"
              (text
               @-> ret
                     Row.(
                       t4
                         (int64 "event_id")
                         (text "booked_on")
                         (option Rel.Type.text "movement_type")
                         (option Rel.Type.text "override"))))
            institution)
         (fun row acc -> row :: acc)
         [])
  in
  (* The stored type already follows the rules, companion columns included. *)
  let effective (_, _, primary, override) =
    Option.map (Option.first_some override primary) ~f:Movement_type.of_string
  in
  let counts = Hashtbl.create (module String) in
  List.iter rows ~f:(fun row ->
    let kind = effective row in
    Hashtbl.update
      counts
      (Option.value_map kind ~default:"" ~f:Movement_type.to_string)
      ~f:(fun n -> kind, 1 + Option.value_map n ~default:0 ~f:snd));
  let dates = List.map rows ~f:(fun (_, d, _, _) -> d) in
  Ok
    { movements = List.length rows
    ; first_on = List.min_elt dates ~compare:String.compare
    ; last_on = List.max_elt dates ~compare:String.compare
    ; by_type =
        Hashtbl.data counts
        |> List.sort ~compare:(fun (a, x) (b, y) ->
          match Int.compare y x with
          | 0 ->
            String.compare
              (Option.value_map a ~default:"" ~f:Movement_type.to_string)
              (Option.value_map b ~default:"" ~f:Movement_type.to_string)
          | c -> c)
    }
;;

(* The columns of the companion rows matched to one movement. *)
let companion_columns_of_event db event_id =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT r.id,r.document_id,r.row_number,r.institution,r.format,r.fields_json \
            FROM companion_matches c JOIN companion_rows r ON r.id=c.row_id WHERE \
            c.event_id=? AND c.state='matched' ORDER BY r.id"
           (int64
            @-> ret
                  Row.(
                    unit (fun a b c d e f -> a, b, c, d, e, f)
                    * int64 "id"
                    * int64 "document_id"
                    * int "row_number"
                    * text "institution"
                    * text "format"
                    * text "fields_json")))
         event_id)
  in
  traverse
    (fun (row_id, document_id, row_number, institution, format, fields_json) ->
       let* companion =
         List.find (registered_companions db) ~f:(fun (owner, (c : Companion.t)) ->
           String.equal owner institution && String.equal c.format format)
         |> Option.map ~f:snd
         |> Or_error.of_option ~error:(Error.of_string "unregistered companion format")
       in
       let* fields = decode_fields fields_json in
       let* context_rows =
         collect
           db
           (Rel_sql.Stmt.(
              func
                "SELECT context_json FROM document_contexts WHERE document_id=?"
                (int64 @-> ret Row.(t1 (text "context_json"))))
              document_id)
       in
       let* context =
         match context_rows with
         | [] -> Ok []
         | json :: _ -> Json.decode Json.string_map json
       in
       companion.fields
         { Companion.row_id
         ; document_id
         ; institution
         ; format
         ; row_number
         ; fields
         ; context
         })
    rows
  |> Result.map ~f:List.concat
;;

let explain_movement db event_id =
  let* event = event_by_id db event_id in
  let venue = event.Source_event.venue in
  let* empty = Source_typing.empty venue in
  let* parsed = Registry.movement ~source_typing:empty db.registry event in
  let* more = companion_columns_of_event db event_id in
  let parsed = Movement.with_fields parsed (parsed.fields @ more) in
  let* stored = movement db event_id in
  let* rules = classification_rules db in
  let automatic (r : Classification_rule.t) =
    (match r.mode with
     | Automatic -> true
     | Review | Disabled -> false)
    && Option.value_map r.institution ~default:true ~f:(String.equal venue)
  in
  let first_hit = ref false in
  let decide checks_for =
    (* Mark the first match as the decider and the later ones as shadowed. *)
    List.map checks_for ~f:(fun (name, pattern, input, outcome) ->
      let verdict =
        match outcome with
        | Error reason -> Explanation.Does_not_match reason
        | Ok kind ->
          if !first_hit
          then Also_matches
          else (
            first_hit := true;
            Decides kind)
      in
      { Explanation.name; pattern; input; verdict })
  in
  let after_code = parsed in
  (* Type rules over every field of the movement; the first that matches decides. *)
  let* description_entries =
    List.filter rules ~f:(fun r ->
      automatic r && Option.is_some (Classification_rule.action_type r.action))
    |> traverse (fun (r : Classification_rule.t) ->
      let kind = Option.value_exn (Classification_rule.action_type r.action) in
      let* typing = typing_rule r kind in
      let pattern = Option.value r.pattern ~default:"any description" in
      let field = Option.value r.field ~default:"description" in
      let values = Movement.field_values after_code field in
      let input =
        if List.is_empty values
        then Printf.sprintf "%s: this movement has no such field" field
        else Printf.sprintf "%s: %s" field (String.concat ~sep:" | " values)
      in
      let reason =
        if
          not
            (Option.value_map
               r.account_type
               ~default:true
               ~f:(String.equal after_code.account.account_type))
        then
          Some
            (Printf.sprintf
               "account type is %s, not %s"
               after_code.account.account_type
               (Option.value_exn r.account_type))
        else if
          not
            (Option.value_map r.match_type ~default:true ~f:(fun cond ->
               match cond, after_code.movement_type with
               | Classification_rule.Unknown, None -> true
               | Unknown, Some _ -> false
               | Is wanted, Some have -> Poly.equal wanted have
               | Is _, None -> false))
        then
          Some
            (Printf.sprintf
               "parser type is %s, the rule wants %s"
               (Option.value_map
                  after_code.movement_type
                  ~default:"unknown"
                  ~f:Movement_type.to_string)
               (Option.value_map
                  r.match_type
                  ~default:"any"
                  ~f:Classification_rule.condition_name))
        else if not (Source_typing.matches typing after_code)
        then
          Some
            (if List.is_empty values
             then Printf.sprintf "the movement has no field %s" field
             else Printf.sprintf "field %s does not match" field)
        else None
      in
      Ok
        ( r.name
        , pattern
        , input
        , match reason with
          | Some reason -> Error reason
          | None -> Ok (Some kind) ))
  in
  let checks = decide description_entries in
  let rules_type =
    match
      List.find_map checks ~f:(fun c ->
        match c.verdict with
        | Decides kind -> kind
        | _ -> None)
    with
    | Some kind -> Some kind
    | None -> after_code.movement_type
  in
  let* override_rows =
    db_result
      (Db.fold
         db.conn
         (Rel_sql.Stmt.(
            func
              "SELECT movement_type FROM movement_type_overrides WHERE event_id=?"
              (int64 @-> ret Row.(t1 (text "movement_type"))))
            event_id)
         (fun row acc -> row :: acc)
         [])
  in
  let override = List.hd override_rows |> Option.map ~f:Movement_type.of_string in
  let* category = db_result (Db.first db.conn (find_category event_id)) in
  let effective, source =
    match override, rules_type with
    | Some kind, _ -> Some kind, Explanation.Manual_override
    | None, Some kind -> Some kind, Rules
    | None, None -> None, Untyped
  in
  Ok
    { Explanation.event_id
    ; institution = venue
    ; booked_on = Date.to_string stored.booked_on
    ; amount = Quantity.to_string stored.amount
    ; currency = Iso4217.to_alpha_code stored.account.currency
    ; account_type = stored.account.account_type
    ; descriptions = stored.descriptions
    ; override
    ; checks
    ; rules_type
    ; effective
    ; source
    ; category
    }
;;

let document_detail db document_id =
  let* docs =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT sha256,imported_at FROM import_documents WHERE id=?"
           (int64
            @-> ret
                  Row.(
                    t2
                      (text "sha256")
                      (read_column Db_schema.Import_documents.imported_at))))
         document_id)
  in
  let* paths =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT path FROM bank_import_attempts WHERE document_id=? ORDER BY id"
           (int64 @-> ret Row.(t1 (text "path"))))
         document_id)
  in
  match docs with
  | [ (sha256, imported_at) ] ->
    Ok
      { Transaction_detail.id = document_id
      ; sha256
      ; imported_at = Timestamp.to_rfc3339 imported_at
      ; paths = List.dedup_and_sort paths ~compare:String.compare
      }
  | _ -> Or_error.errorf "no stored document %Ld" document_id
;;

let show_movement db event_id =
  let* explanation = explain_movement db event_id in
  let* source =
    collect
      db
      (statement
         (Printf.sprintf
            "SELECT \
             id,venue,account,kind,source_id,occurred_at,occurred_on,retrieved_at,payload \
             FROM source_events WHERE id = %Ld"
            event_id)
         source_row)
  in
  let* (event : Db_schema.Source_events.t) =
    match source with
    | [ event ] -> Ok event
    | _ -> Or_error.error_string "unknown transaction event ID"
  in
  let* document_ids =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT document_id FROM import_document_events WHERE event_id=? ORDER BY \
            document_id"
           (int64 @-> ret Row.(t1 (int64 "document_id"))))
         event_id)
  in
  let* documents = traverse (document_detail db) document_ids in
  let* statement_rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT \
            s.reference,s.source_account,s.currency,s.period_start,s.period_end,s.opening,s.closing \
            FROM statement_events e JOIN statements s ON s.id=e.statement_id WHERE \
            e.event_id=? ORDER BY s.id"
           (int64
            @-> ret
                  Row.(
                    unit (fun a b c d e f g -> a, b, c, d, e, f, g)
                    * option Rel.Type.text "reference"
                    * text "source_account"
                    * text "currency"
                    * read_column Db_schema.Statements.period_start
                    * read_column Db_schema.Statements.period_end
                    * text "opening"
                    * text "closing")))
         event_id)
  in
  let statements =
    List.map
      statement_rows
      ~f:
        (fun
          (reference, source_account, currency, period_start, period_end, opening, closing)
        ->
        { Transaction_detail.reference
        ; source_account
        ; currency
        ; period =
            Temporal.to_string period_start ^ " to " ^ Temporal.to_string period_end
        ; opening
        ; closing
        })
  in
  let* companion_rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT \
            r.id,r.document_id,r.row_number,r.institution,r.format,r.fields_json,c.state,c.reason,c.policy \
            FROM companion_matches c JOIN companion_rows r ON r.id=c.row_id WHERE \
            c.event_id=? ORDER BY r.id"
           (int64
            @-> ret
                  Row.(
                    unit (fun a b c d e f g h i -> a, b, c, d, e, f, g, h, i)
                    * int64 "id"
                    * int64 "document_id"
                    * int "row_number"
                    * text "institution"
                    * text "format"
                    * text "fields_json"
                    * text "state"
                    * text "reason"
                    * text "policy")))
         event_id)
  in
  let* companions =
    traverse
      (fun ( row_id
           , document_id
           , row_number
           , institution
           , format
           , fields_json
           , state
           , reason
           , policy ) ->
         let* companion =
           List.find (registered_companions db) ~f:(fun (owner, (c : Companion.t)) ->
             String.equal owner institution && String.equal c.format format)
           |> Option.map ~f:snd
           |> Or_error.of_option ~error:(Error.of_string "unregistered companion format")
         in
         let* raw = decode_fields fields_json in
         let* context_rows =
           collect
             db
             (Rel_sql.Stmt.(
                func
                  "SELECT context_json FROM document_contexts WHERE document_id=?"
                  (int64 @-> ret Row.(t1 (text "context_json"))))
                document_id)
         in
         let* context =
           match context_rows with
           | [] -> Ok []
           | json :: _ -> Json.decode Json.string_map json
         in
         let row : Companion.row =
           { row_id; document_id; institution; format; row_number; fields = raw; context }
         in
         let* columns = companion.fields row in
         let* annotation = companion.annotate row in
         let* document = document_detail db document_id in
         Ok
           { Transaction_detail.row_id
           ; format
           ; row_number
           ; document
           ; fields = columns
           ; state
           ; reason
           ; policy
           ; display_name = annotation.display_name
           ; tags = annotation.tags
           })
      companion_rows
  in
  let* transfer = linked_transfer db event_id in
  let* stored_fields =
    let* rows =
      collect
        db
        (statement
           (Printf.sprintf
              "SELECT event_id,ordinal,name,value FROM source_event_fields WHERE \
               event_id = %Ld ORDER BY ordinal"
              event_id)
           Db_schema.Source_event_fields.row)
    in
    Ok (List.map rows ~f:(fun (f : Db_schema.Source_event_fields.t) -> f.name, f.value))
  in
  let payload =
    match Json.decode Jsont.json event.payload with
    | Ok value -> Json.encode ~format:Jsont.Indent Jsont.json value
    | Error _ -> event.payload
  in
  Ok
    { Transaction_detail.explanation
    ; venue = event.venue
    ; account = event.account
    ; kind = event.kind
    ; source_id = event.source_id
    ; occurred =
        (match event.occurred_at, event.occurred_on with
         | Some at, _ -> Timestamp.to_rfc3339 at
         | None, Some on -> on
         | None, None -> "")
    ; retrieved_at = Timestamp.to_rfc3339 event.retrieved_at
    ; payload
    ; fields = stored_fields
    ; documents
    ; statements
    ; companions
    ; transfer
    }
;;
