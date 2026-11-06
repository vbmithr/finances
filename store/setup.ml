open Base
open Rel
module Db = Rel_sqlite3
open Handle

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
         With the previous fin version, run fin backup (it keeps a copy) and fin \
         decisions export FILE. Then, with this one, run fin delete and fin init, import \
         your statement folders again and run fin decisions restore FILE"
        Db_schema.version)
;;

external decimal_extension : unit -> Db.Extension.t = "fin_sqlite_decimal_extension"

let with_connection ~mode path f =
  let* conn = db_result (Db.open' ~foreign_keys:true ~mode path) in
  let result =
    try
      let* () = db_result (Db.Extension.apply conn (decimal_extension ())) in
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
      ; in_transaction = false
      ; registry
      ; source_typing_cache = Hashtbl.create (module String)
      ; source_evidence_cache = Hashtbl.create (module String)
      }
    in
    let* () = initialize db in
    let* () = db_result (Db.exec_sql db.conn Sql.views) in
    f db)
;;

let check_queries db =
  let* () =
    List.fold_result Sql.queries ~init:() ~f:(fun () (name, query) ->
      db_result (Db.exec_sql db.conn ("EXPLAIN " ^ query))
      |> Or_error.tag ~tag:(name ^ ".sql"))
  in
  (* EXPLAIN covers only a script's first statement, so a script runs, then is undone. *)
  List.fold_result Sql.scripts ~init:() ~f:(fun () (name, script) ->
    let* () = db_result (Db.exec_sql db.conn "SAVEPOINT check_script") in
    let ran = db_result (Db.exec_sql db.conn script) in
    let* () =
      db_result (Db.exec_sql db.conn "ROLLBACK TO check_script; RELEASE check_script")
    in
    Or_error.tag ran ~tag:(name ^ ".sql"))
;;

let backup source path =
  if Stdlib.Sys.file_exists path
  then Or_error.errorf "%s already exists" path
  else if not (Stdlib.Sys.file_exists source)
  then Or_error.errorf "no database at %s" source
  else
    (* Deliberately independent of the schema version: a backup must be possible
       before rebuilding a database that this build can no longer open. *)
    with_connection ~mode:Read source (fun conn ->
      let quoted = "'" ^ String.substr_replace_all path ~pattern:"'" ~with_:"''" ^ "'" in
      db_result (Db.exec_sql conn ("VACUUM INTO " ^ quoted)))
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
