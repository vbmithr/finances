open Core
open Rel
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)

let edge first second weight =
  { Transfer_ranking.first = Int64.of_int first; second = Int64.of_int second; weight }
;;

let assignments es = ok (Transfer_ranking.assignments es)

let status rows a b =
  (List.find_exn rows ~f:(fun r ->
     Int64.equal r.Transfer_ranking.edge.first (Int64.of_int a)
     && Int64.equal r.edge.second (Int64.of_int b)))
    .status
;;

let global_matching () =
  let rows = assignments [ edge 1 3 70; edge 1 4 68; edge 2 3 68 ] in
  Alcotest.(check string) "greedy edge excluded" "not_preferred" (status rows 1 3);
  Alcotest.(check string) "global first pair" "supported" (status rows 1 4);
  Alcotest.(check string) "global second pair" "supported" (status rows 2 3);
  let best = (List.hd_exn rows).best_score |> Option.value_exn in
  check "best total" 136 best;
  let rows = assignments [ edge 1 3 100; edge 1 4 40; edge 2 3 40 ] in
  Alcotest.(check string) "unmatched vertices allowed" "supported" (status rows 1 3)
;;

let ties () =
  let rows = assignments [ edge 1 3 60; edge 1 4 60; edge 2 3 60; edge 2 4 60 ] in
  List.iter rows ~f:(fun r ->
    Alcotest.(check string)
      "ties stay alternatives"
      "alternative"
      r.Transfer_ranking.status;
    check "no decisive margin" 0 (Option.value_exn r.exclusion_margin));
  let reversed = assignments [ edge 2 4 60; edge 2 3 60; edge 1 4 60; edge 1 3 60 ] in
  List.iter reversed ~f:(fun r ->
    Alcotest.(check string)
      "order never breaks ties"
      "alternative"
      r.Transfer_ranking.status)
;;

let bounds () =
  let rows = assignments (List.init 18 ~f:(fun i -> edge 1 (i + 2) 10)) in
  List.iter rows ~f:(fun r ->
    Alcotest.(check string)
      "large graph abstains"
      "not_evaluated"
      r.Transfer_ranking.status);
  error (Transfer_ranking.assignments [ edge 1 1 5 ]);
  error (Transfer_ranking.assignments [ edge 1 2 5; edge 2 1 6 ]);
  error (Transfer_ranking.assignments [ edge 1 2 (-1) ]);
  check "empty graph" 0 (List.length (assignments []))
;;

let bunq_iban = "FR1420041010050500013M02606"
let counterpart_iban = "BE68539007547034"
let other_iban = "DE89370400440532013000"
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b

let import db institution path contents =
  ignore (ok (Store.import_bank_file ~account_id:"main" db ~institution ~path ~contents))
;;

let seed db =
  let mt =
    In_channel.read_all "fixtures/bunq.mt940"
    |> fun s ->
    replace (replace s "SYNTHETICBUNQ" bunq_iban) "SYNTHETICOTHER" counterpart_iban
  in
  import db "bunq" "bunq.mt940" mt;
  import db "revolut" "revolut.csv" (In_channel.read_all "fixtures/revolut.csv")
;;

let memory f =
  ok
    (Store.with_open ":memory:" (fun db ->
       seed db;
       f db;
       Ok ()))
;;

let identity iban =
  ok
    (Account_identity.create
       ~institution:"revolut"
       ~bank_id:"revolut"
       ~account_id:"main"
       ~account_type:"current-EUR"
       ~iban)
;;

let unique db =
  List.find_exn (ok (Store.transfer_candidates db)) ~f:(fun c -> c.Store.unique)
;;

let decision c =
  ok
    (Bank_transfer.create
       ~first_event_id:c.Store.first_event_id
       ~second_event_id:c.second_event_id
       ~state:Confirmed
       ~reason:"Checked owned accounts")
;;

let iban_validation () =
  Alcotest.(check string)
    "normalize spaces/case"
    counterpart_iban
    (ok (Account_identity.normalize_iban "be68 5390 0754 7034"));
  error (Account_identity.normalize_iban "BE69539007547034");
  error (Account_identity.normalize_iban "main");
  check
    "null template has no update"
    0
    (List.length
       (ok
          (Account_identity.of_json
             "[{\"institution\":\"revolut\",\"bank_id\":\"revolut\",\"account_id\":\"main\",\"account_type\":\"current-EUR\",\"iban\":null}]")))
;;

let registration () =
  memory (fun db ->
    let c = unique db in
    check
      "registered"
      1
      (ok (Store.set_bank_account_identities db [ identity counterpart_iban ]));
    check
      "idempotent"
      0
      (ok (Store.set_bank_account_identities db [ identity counterpart_iban ]));
    let info =
      List.find_exn
        (ok (Store.bank_account_info db))
        ~f:(fun i -> String.equal i.Store.mapping.institution "revolut")
    in
    Alcotest.(check string) "manual provenance" "registered" info.iban_source;
    Alcotest.(check string) "saved IBAN" counterpart_iban (Option.value_exn info.iban);
    let ranked = ok (Transfer_ranking.analyze (ok (Store.transfer_candidates db))) in
    let r =
      List.find_exn ranked ~f:(fun r ->
        Int64.equal r.Transfer_ranking.candidate.first_event_id c.first_event_id
        && Int64.equal r.candidate.second_event_id c.second_event_id)
    in
    Alcotest.(check bool) "positive IBAN evidence" true r.candidate.iban_match;
    Alcotest.(check bool)
      "explicit points"
      true
      (List.exists r.evidence ~f:(fun e ->
         String.equal e.Transfer_ranking.code "counterparty_iban"));
    ignore (ok (Store.set_bank_transfers db [ decision c ]));
    error (Store.set_bank_account_identities db [ identity other_iban ]);
    let info =
      List.find_exn
        (ok (Store.bank_account_info db))
        ~f:(fun i -> String.equal i.Store.mapping.institution "revolut")
    in
    Alcotest.(check string)
      "identity conflict rolled back"
      counterpart_iban
      (Option.value_exn info.iban))
;;

let known_contradiction () =
  memory (fun db ->
    let c = unique db in
    let wise =
      In_channel.read_all "fixtures/wise.xml"
      |> fun s -> replace s "SYNTHETIC-ACCOUNT" counterpart_iban
    in
    import db "wise" "wise.xml" wise;
    let all = ok (Store.transfer_candidates ~include_reviewed:true db) in
    let conflict =
      List.find_exn all ~f:(fun r ->
        Int64.equal r.Store.first_event_id c.first_event_id
        && Int64.equal r.second_event_id c.second_event_id)
    in
    Alcotest.(check bool)
      "known Wise identity contradicts Revolut match"
      false
      conflict.iban_compatible;
    check "incompatible pair removed" 4 (List.length (ok (Store.transfer_candidates db)));
    error (Store.set_bank_transfers db [ decision c ]);
    check "no invalid decision" 0 (List.length (ok (Store.bank_transfer_decisions db))))
;;

let explicit_contradiction () =
  memory (fun db ->
    let c = unique db in
    ignore (ok (Store.set_bank_account_identities db [ identity other_iban ]));
    check
      "IBAN contradiction outweighs exact amount"
      4
      (List.length (ok (Store.transfer_candidates db)));
    error (Store.set_bank_transfers db [ decision c ]);
    let imported =
      ok
        (Account_identity.create
           ~institution:"bunq"
           ~bank_id:"bunq"
           ~account_id:bunq_iban
           ~account_type:"current-EUR"
           ~iban:other_iban)
    in
    error (Store.set_bank_account_identities db [ imported ]))
;;

let workspace f =
  let path =
    Filename_unix.temp_file ~in_dir:(Sys.getenv_exn "TMPDIR") "identity-tests" ".sqlite3"
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

let persistence_and_migration () =
  workspace (fun path ->
    raw path (fun db -> sql db (In_channel.read_all "fixtures/schema-v8.sql"));
    ok
      (Store.with_open path (fun db ->
         seed db;
         ignore (ok (Store.set_bank_account_identities db [ identity counterpart_iban ]));
         Ok ()));
    ok
      (Store.with_open path (fun db ->
         let info =
           List.find_exn
             (ok (Store.bank_account_info db))
             ~f:(fun i -> String.equal i.Store.mapping.institution "revolut")
         in
         Alcotest.(check string)
           "reopened identity"
           counterpart_iban
           (Option.value_exn info.iban);
         Ok ()));
    raw path (fun db ->
      check "schema ten" 10 (scalar db "PRAGMA user_version");
      check "audit row" 1 (scalar db "SELECT count(*) FROM bank_account_identity_changes");
      Alcotest.(check bool)
        "audit immutable"
        true
        (Result.is_error
           (Rel_sqlite3.exec_sql db "DELETE FROM bank_account_identity_changes"))));
  workspace (fun path ->
    raw path (fun db ->
      sql db (In_channel.read_all "fixtures/schema-v8.sql");
      sql db "CREATE TABLE bank_account_identity_changes(collision TEXT)");
    error (Store.with_open path (fun _ -> Ok ()));
    raw path (fun db ->
      check "migration rollback" 8 (scalar db "PRAGMA user_version");
      check
        "identity table rolled back"
        0
        (scalar
           db
           "SELECT count(*) FROM sqlite_master WHERE name='bank_account_identities'")))
;;

let () =
  Alcotest.run
    "Account identities and ranking"
    [ ( "analysis"
      , List.map
          [ "global matching and unmatched vertices", global_matching
          ; "equal optima remain ambiguous", ties
          ; "bounded exact solver", bounds
          ; "IBAN validation", iban_validation
          ; "identity registration and confirmed transfer guard", registration
          ; "known account contradictions", known_contradiction
          ; "explicit identity contradictions", explicit_contradiction
          ; "identity persistence and migration", persistence_and_migration
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
