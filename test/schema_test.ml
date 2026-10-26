open Core
open Rel
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)

let workspace f =
  let path =
    Filename_unix.temp_file ~in_dir:(Sys.getenv_exn "TMPDIR") "schema-baseline" ".sqlite3"
  in
  Exn.protect ~finally:(fun () -> Core_unix.unlink path) ~f:(fun () -> f path)
;;

let raw path f =
  let db =
    Rel_sqlite3.open' ~mode:Read_write_create path
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
      (Store.with_open path (fun db ->
         check "fresh database empty" 0 (List.length (ok (Store.events db)));
         Ok ()));
    raw path (fun db ->
      check "latest baseline" 10 (scalar db "PRAGMA user_version");
      List.iter
        [ "source_events"
        ; "bank_categories"
        ; "bunq_statements"
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

let rejects_old () =
  workspace (fun path ->
    raw path (fun db ->
      sql db "CREATE TABLE experimental_marker(value TEXT); PRAGMA user_version=9");
    error (Store.with_open path (fun _ -> Ok ()));
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
    error (Store.with_open path (fun _ -> Ok ()));
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
      (Store.with_open path (fun db ->
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
               ; iban = Set "BE68539007547034"
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
      (Store.with_open path (fun db ->
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
           (Option.value_exn revolut.iban);
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

let () =
  Alcotest.run
    "Latest experimental schema"
    [ ( "baseline"
      , List.map
          [ "initialize latest", initialize
          ; "reject old without alteration", rejects_old
          ; "atomic initialization rollback", initialization_rollback
          ; "reopen preserves reviewed state and audit", persistent_state
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
