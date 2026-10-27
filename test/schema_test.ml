open Base
open Stdio
open Rel
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)

let workspace f =
  let path =
    Stdlib.Filename.temp_file
      ~temp_dir:(Sys.getenv_exn "TMPDIR")
      "schema-baseline"
      ".sqlite3"
  in
  Exn.protect ~finally:(fun () -> Unix.unlink path) ~f:(fun () -> f path)
;;

let raw path f =
  let db =
    Rel_sqlite3.open' ~foreign_keys:true ~mode:Read_write_create path
    |> Rel_sqlite3.string_error
    |> Result.ok_or_failwith
  in
  Exn.protect ~finally:(fun () -> ignore (Rel_sqlite3.close db)) ~f:(fun () -> f db)
;;

let sql db s =
  Rel_sqlite3.exec_sql db s |> Rel_sqlite3.string_error |> Result.ok_or_failwith
;;

let scalar db s =
  Rel_sqlite3.first db (Rel_sql.Stmt.v s ~rev_args:[] ~result:Row.(t1 (int "value")))
  |> Rel_sqlite3.string_error
  |> Result.ok_or_failwith
  |> Option.value_exn
;;

let initialize () =
  workspace (fun path ->
    ok
      (Store.with_open ~registry:Builtin.registry path (fun db ->
         check "fresh database empty" 0 (List.length (ok (Store.events db)));
         Ok ()));
    raw path (fun db ->
      check "latest baseline" 11 (scalar db "PRAGMA user_version");
      check
        "all tables are strict"
        24
        (scalar
           db
           "SELECT count(*) FROM pragma_table_list WHERE schema='main' AND name NOT LIKE \
            'sqlite_%' AND strict=1");
      List.iter
        [ "source_events"
        ; "bank_categories"
        ; "statements"
        ; "bank_transfer_decisions"
        ; "bank_account_identities"
        ; "bank_account_settlements"
        ]
        ~f:(fun table ->
          check
            table
            1
            (scalar
               db
               ("SELECT count(*) FROM sqlite_master WHERE type='table' AND name='"
                ^ table
                ^ "'")))))
;;

let sqlite_constraints () =
  workspace (fun path ->
    ok (Store.with_open ~registry:Builtin.registry path (fun _ -> Ok ()));
    raw path (fun db ->
      let rejected s =
        Alcotest.(check bool) s true (Result.is_error (Rel_sqlite3.exec_sql db s))
      in
      let event values =
        "INSERT INTO \
         source_events(venue,account,kind,source_id,occurred_at_ms,occurred_on,payload) \
         VALUES "
        ^ values
      in
      sql
        db
        (event "('bank','main','transaction','valid',9223372036854775807,NULL,X'00')");
      check
        "database supplies retrieval timestamp"
        1
        (scalar
           db
           "SELECT count(*) FROM source_events WHERE retrieved_at GLOB \
            '????-??-??T??:??:??*Z'");
      rejected (event "('bank','main','transaction','neither',NULL,NULL,X'00')");
      rejected (event "('bank','main','transaction','both',1,'2026-10-27',X'00')");
      rejected (event "('bank','main','transaction','negative',-1,NULL,X'00')");
      rejected (event "('bank','main','transaction','wrong-blob',1,NULL,'text')");
      rejected "UPDATE source_events SET account='changed'";
      rejected "DELETE FROM source_events";
      sql
        db
        "INSERT INTO import_documents(institution,sha256,contents) VALUES \
         ('bank','digest',X'00')";
      rejected
        "INSERT INTO \
         companion_rows(document_id,row_number,institution,format,fields_json) VALUES \
         (1,0,'bank','csv','{}')";
      sql
        db
        "INSERT INTO \
         companion_rows(document_id,row_number,institution,format,fields_json) VALUES \
         (1,1,'bank','csv','{}')";
      let matching state event =
        Printf.sprintf
          "INSERT INTO \
           companion_matches(row_id,event_id,state,reason,candidates_json,policy) VALUES \
           (1,%s,'%s','test','[]','test')"
          event
          state
      in
      rejected (matching "invalid" "NULL");
      rejected (matching "matched" "NULL");
      rejected (matching "unmatched" "1");
      rejected (matching "matched" "999");
      sql db (matching "matched" "1");
      sql db (event "('bank','other','transaction','second',1,NULL,X'00')");
      rejected "INSERT INTO bank_transfer_decisions VALUES (2,1,'confirmed','test')";
      rejected "INSERT INTO bank_transfer_decisions VALUES (1,2,'invalid','test')";
      sql db "INSERT INTO bank_transfer_decisions VALUES (1,2,'confirmed','test')";
      rejected "INSERT INTO bank_transfer_memberships VALUES (1,2,1)";
      sql db (event "('bank','third','transaction','third',1,NULL,X'00')");
      rejected "INSERT INTO bank_transfer_memberships VALUES (3,1,2)";
      sql db "INSERT INTO bank_transfer_memberships VALUES (1,1,2)";
      let attempt format outcome document inserted =
        Printf.sprintf
          "INSERT INTO \
           bank_import_attempts(institution,path,document_id,format,outcome,inserted_count,existing_count) \
           VALUES ('bank','input',%s,'%s','%s',%d,0)"
          document
          format
          outcome
          inserted
      in
      (* Formats belong to adapters, so the schema does not list them. *)
      sql db (attempt "adapter-defined" "imported" "1" 0);
      rejected (attempt "csv" "invalid" "1" 0);
      rejected (attempt "csv" "imported" "NULL" 0);
      rejected (attempt "csv" "unreadable" "1" 0);
      rejected (attempt "csv" "imported" "1" (-1));
      sql db (attempt "csv" "unreadable" "NULL" 0);
      check
        "foreign keys remain consistent"
        0
        (scalar db "SELECT count(*) FROM pragma_foreign_key_check")))
;;

let generated_queries () =
  workspace (fun path ->
    let event account kind source_id =
      ok
        (Source_event.create
           ~venue:"test bank"
           ~account
           ~kind
           ~source_id
           ~occurred_at_ms:Int64.max_value
           ~payload:"bytes\000payload")
    in
    let first = event "checking" "deposit" "quoted ' ? identifier" in
    let others =
      [ event "savings" "deposit" first.source_id
      ; event "checking" "withdrawal" first.source_id
      ; event "checking" "deposit" "another identifier"
      ]
    in
    ok
      (Store.with_open ~registry:Builtin.registry path (fun db ->
         let id =
           match ok (Store.ingest db first) with
           | Inserted id -> id
           | Existing _ -> failwith "fresh event expected"
         in
         List.iter others ~f:(fun e -> ignore (ok (Store.ingest db e)));
         (match ok (Store.ingest db first) with
          | Existing existing ->
            Alcotest.(check int64) "four bound keys keep their order" id existing
          | Inserted _ -> failwith "replay inserted a duplicate");
         let events = ok (Store.events db) in
         check "distinct keys preserved" 4 (List.length events);
         List.iter events ~f:(fun (_, e) ->
           Alcotest.(check bool)
             "int64 and blob round-trip"
             true
             (Poly.equal e.Source_event.occurred (Source_event.At_ms Int64.max_value)
              && String.equal e.payload first.payload));
         Ok ())))
;;

let large_rowid () =
  workspace (fun path ->
    ok (Store.with_open ~registry:Builtin.registry path (fun _ -> Ok ()));
    raw path (fun db ->
      sql
        db
        "INSERT INTO \
         source_events(id,venue,account,kind,source_id,occurred_at_ms,payload) VALUES \
         (9223372036854775807,'bank','main','test','large-id',1,X'00')");
    ok
      (Store.with_open ~registry:Builtin.registry path (fun db ->
         let id, event = List.hd_exn (ok (Store.events db)) in
         Alcotest.(check int64) "full SQLite rowid range" Int64.max_value id;
         (match ok (Store.ingest db event) with
          | Existing id ->
            Alcotest.(check int64) "generated lookup keeps rowid" Int64.max_value id
          | Inserted _ -> failwith "large rowid replay inserted a duplicate");
         Ok ())))
;;

let rejects_old () =
  workspace (fun path ->
    raw path (fun db ->
      sql db "CREATE TABLE experimental_marker(value TEXT); PRAGMA user_version=9");
    error (Store.with_open ~registry:Builtin.registry path (fun _ -> Ok ()));
    raw path (fun db ->
      check "old version left intact" 9 (scalar db "PRAGMA user_version");
      check
        "no migration attempted"
        1
        (scalar db "SELECT count(*) FROM sqlite_master WHERE type='table'")))
;;

let initialization_rollback () =
  workspace (fun path ->
    raw path (fun db -> sql db "CREATE TABLE bank_account_settlements(collision TEXT)");
    error (Store.with_open ~registry:Builtin.registry path (fun _ -> Ok ()));
    raw path (fun db ->
      check
        "failed initialization remains unversioned"
        0
        (scalar db "PRAGMA user_version");
      check
        "earlier tables rolled back"
        0
        (scalar db "SELECT count(*) FROM sqlite_master WHERE name='source_events'")))
;;

let persistent_state () =
  workspace (fun path ->
    ok
      (Store.with_open ~registry:Builtin.registry path (fun db ->
         let ofx =
           In_channel.read_all "fixtures/bank.ofx"
           |> fun s ->
           String.substr_replace_all
             s
             ~pattern:"<ACCTTYPE>SAVINGS</ACCTTYPE>"
             ~with_:"<ACCTTYPE>CREDITCRD</ACCTTYPE>"
         in
         ignore
           (ok (Store.import_ofx db ~institution:"boursobank" (ok (Bank_ofx.parse ofx))));
         List.iter
           [ "bunq", "fixtures/bunq.mt940"; "revolut", "fixtures/revolut.csv" ]
           ~f:(fun (institution, file) ->
             ignore
               (ok
                  (Store.import_bank_file
                     ~account_id:"main"
                     db
                     ~institution
                     ~path:file
                     ~contents:(In_channel.read_all file))));
         ignore (ok (Store.project_bank db));
         let rows = ok (Store.export_bank_accounts db) in
         let checking =
           List.find_exn rows ~f:(fun r ->
             String.equal r.Bank_account_config.reference.account_type "CHECKING")
         in
         let rows =
           List.map rows ~f:(fun r ->
             if String.equal r.Bank_account_config.reference.account_type "CREDITCRD"
             then { r with settlement_account = Set checking.reference }
             else if String.equal r.reference.institution "revolut"
             then
               { r with
                 ledger_account = Some "assets:bank:daily"
               ; iban = Set (ok (Iban.of_string "BE68539007547034"))
               }
             else r)
         in
         ignore (ok (Store.apply_bank_accounts db rows));
         let reviewed =
           List.find_exn
             (ok (Store.bank_review db))
             ~f:(fun r ->
               String.equal r.Store.transaction.account.account_type "CHECKING")
         in
         ignore
           (ok
              (Store.set_bank_categories
                 db
                 [ ok
                     (Bank_category.create
                        ~event_id:reviewed.event_id
                        ~account:"expenses:food"
                        ~reason:"Reviewed")
                 ]));
         let pair =
           List.find_exn (ok (Store.transfer_candidates db)) ~f:(fun c -> c.Store.unique)
         in
         ignore
           (ok
              (Store.set_bank_transfers
                 db
                 [ ok
                     (Bank_transfer.create
                        ~first_event_id:pair.first_event_id
                        ~second_event_id:pair.second_event_id
                        ~state:Confirmed
                        ~reason:"Both owned accounts")
                 ]));
         Ok ()));
    ok
      (Store.with_open ~registry:Builtin.registry path (fun db ->
         check "source evidence retained" 11 (List.length (ok (Store.events db)));
         check "entries retained" 11 (List.length (ok (Store.entries db)));
         check "category retained" 1 (List.length (ok (Store.bank_category_history db)));
         check "transfer retained" 1 (List.length (ok (Store.bank_transfer_history db)));
         let accounts = ok (Store.bank_account_info db) in
         let card =
           List.find_exn accounts ~f:(fun a ->
             String.equal a.Store.mapping.account_type "CREDITCRD")
         in
         Alcotest.(check bool)
           "settlement link retained"
           true
           (Option.is_some card.settlement_account);
         let revolut =
           List.find_exn accounts ~f:(fun a ->
             String.equal a.Store.mapping.institution "revolut")
         in
         Alcotest.(check string)
           "IBAN retained"
           "BE68539007547034"
           (Iban.to_string (Option.value_exn revolut.iban));
         check "projection remains idempotent" 0 (ok (Store.project_bank db)).inserted;
         Ok ()));
    raw path (fun db ->
      List.iter
        [ "bank_category_changes"
        ; "bank_transfer_changes"
        ; "bank_account_identity_changes"
        ; "bank_account_settlement_changes"
        ]
        ~f:(fun table ->
          Alcotest.(check bool)
            "audit remains immutable"
            true
            (Result.is_error (Rel_sqlite3.exec_sql db ("DELETE FROM " ^ table))))))
;;

(* Banks have no tables or columns of their own: adding one never adds DDL. *)
let no_bank_schema () =
  workspace (fun path ->
    ok (Store.with_open ~registry:Builtin.registry path (fun _ -> Ok ()));
    raw path (fun db ->
      let names =
        Rel_sqlite3.fold
          db
          (Rel_sql.Stmt.v
             "SELECT m.name || '.' || p.name AS value FROM sqlite_master m JOIN \
              pragma_table_info(m.name) p WHERE m.type='table' UNION SELECT name FROM \
              sqlite_master"
             ~rev_args:[]
             ~result:Row.(t1 (text "value")))
          (fun name acc -> name :: acc)
          []
        |> Rel_sqlite3.string_error
        |> Result.ok_or_failwith
      in
      Alcotest.(check bool) "schema listed" true (List.length names > 24);
      let offending =
        List.filter names ~f:(fun name ->
          List.exists (Registry.institutions Builtin.registry) ~f:(fun institution ->
            String.is_substring (String.lowercase name) ~substring:institution))
      in
      Alcotest.(check (list string)) "no institution-named table or column" [] offending))
;;

let () =
  Alcotest.run
    "Latest experimental schema"
    [ ( "baseline"
      , List.map
          [ "initialize latest", initialize
          ; "SQLite extensions preserve constraints", sqlite_constraints
          ; "generated queries preserve bindings and types", generated_queries
          ; "64-bit primary keys", large_rowid
          ; "reject old without alteration", rejects_old
          ; "atomic initialization rollback", initialization_rollback
          ; "reopen preserves reviewed state and audit", persistent_state
          ; "no bank-specific tables or columns", no_bank_schema
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
