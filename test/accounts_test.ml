open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b

let seed db =
  let ofx =
    In_channel.read_all "fixtures/bank.ofx"
    |> fun s ->
    replace
      (replace s "<ACCTTYPE>SAVINGS</ACCTTYPE>" "<ACCTTYPE>CREDITCRD</ACCTTYPE>")
      "<ACCTID>SAVINGS</ACCTID>"
      "<ACCTID>BE68539007547034</ACCTID>"
  in
  ignore (ok (Store.import_ofx db ~institution:"boursobank" (ok (Bank_ofx.parse ofx))));
  ignore (ok (Store.project_bank db))
;;

let memory f =
  ok
    (Store.with_open ~registry:Builtin.registry ":memory:" (fun db ->
       seed db;
       f db;
       Ok ()))
;;

let config db = ok (Store.export_bank_accounts db)

let checking rows =
  List.find_exn rows ~f:(fun r ->
    String.equal r.Bank_account_config.reference.account_type "CHECKING")
;;

let card rows =
  List.find_exn rows ~f:(fun r ->
    String.equal r.Bank_account_config.reference.account_type "CREDITCRD")
;;

let changes db =
  let rows = config db in
  let cash = checking rows
  and credit = card rows in
  [ { cash with
      ledger_account = Some "assets:bank:daily"
    ; iban = Set (ok (Iban.of_string "FR1420041010050500013M02606"))
    }
  ; { credit with settlement_account = Set cash.reference }
  ]
;;

let roundtrip () =
  memory (fun db ->
    let rows = config db in
    let parsed = ok (Bank_account_config.of_json (Bank_account_config.to_json rows)) in
    check "roundtrip rows" 2 (List.length parsed);
    let report = ok (Store.apply_bank_accounts db parsed) in
    check "export/apply does not invent mappings" 0 report.mapping_changes;
    check "export/apply does not invent identities" 0 report.identity_changes;
    check "no audit noise" 0 (List.length (ok (Store.bank_account_history db)));
    error (Bank_account_config.of_json "{\"version\":2,\"accounts\":[]}");
    error
      (Bank_account_config.of_json
         (Bank_account_config.to_json [ checking rows; checking rows ]));
    let unknown = { (checking rows).reference with account_id = "unknown" } in
    error (Store.apply_bank_accounts db [ { (checking rows) with reference = unknown } ]))
;;

let dry_run () =
  memory (fun db ->
    let before = ok (Store.entries db) in
    let report = ok (Store.apply_bank_accounts ~dry_run:true db (changes db)) in
    check "preview mapping" 1 report.mapping_changes;
    check "preview identity" 1 report.identity_changes;
    check "preview settlement" 1 report.settlement_changes;
    check "three concrete proposed changes" 3 (List.length report.changes);
    check "dry run retains no audit" 0 (List.length (ok (Store.bank_account_history db)));
    Alcotest.(check string)
      "ledger unchanged"
      (Option.value_exn (checking (config db)).ledger_account)
      (List.hd_exn (List.hd_exn before |> fun (_, _, e) -> e.Accounting.postings)).account;
    let account =
      List.find_exn
        (ok (Store.bank_account_info db))
        ~f:(fun i -> String.equal i.Store.mapping.account_type "CHECKING")
    in
    Alcotest.(check bool) "IBAN unchanged" true (Option.is_none account.iban);
    let report = ok (Store.apply_bank_accounts db (changes db)) in
    check
      "apply all"
      3
      (report.mapping_changes + report.identity_changes + report.settlement_changes);
    check "three audit kinds" 3 (List.length (ok (Store.bank_account_history db)));
    let repeated = ok (Store.apply_bank_accounts db (config db)) in
    check
      "idempotent canonical config"
      0
      (repeated.mapping_changes + repeated.identity_changes + repeated.settlement_changes);
    let credit =
      List.find_exn
        (ok (Store.bank_account_info db))
        ~f:(fun i -> String.equal i.Store.mapping.account_type "CREDITCRD")
    in
    Alcotest.(check bool) "card has no IBAN" true (Option.is_none credit.iban);
    Alcotest.(check bool)
      "card has settlement relation"
      true
      (Option.is_some credit.settlement_account);
    ignore (ok (Store.project_bank db)))
;;

let rollback () =
  memory (fun db ->
    let rows = changes db in
    let credit = card rows in
    let bad =
      List.map rows ~f:(fun row ->
        if String.equal row.Bank_account_config.reference.account_type "CREDITCRD"
        then { row with settlement_account = Set credit.reference }
        else row)
    in
    error (Store.apply_bank_accounts db bad);
    check "all audit rolled back" 0 (List.length (ok (Store.bank_account_history db)));
    let current = checking (config db) in
    Alcotest.(check bool)
      "identity update rolled back"
      true
      (Poly.equal current.iban Bank_account_config.Clear);
    Alcotest.(check bool)
      "mapping rename rolled back"
      false
      (Option.equal String.equal current.ledger_account (Some "assets:bank:daily")))
;;

let clear_and_patch () =
  memory (fun db ->
    ignore (ok (Store.apply_bank_accounts db (changes db)));
    let rows = config db in
    let cash = checking rows
    and credit = card rows in
    let patch =
      { cash with ledger_account = None; iban = Keep; settlement_account = Keep }
    in
    let report = ok (Store.apply_bank_accounts db [ patch ]) in
    check
      "omitted properties retained"
      0
      (report.mapping_changes + report.identity_changes + report.settlement_changes);
    let report =
      ok
        (Store.apply_bank_accounts
           db
           [ { cash with ledger_account = None; iban = Clear }
           ; { credit with ledger_account = None; settlement_account = Clear }
           ])
    in
    check "clear registered IBAN" 1 report.identity_changes;
    check "unlink settlement" 1 report.settlement_changes;
    check "clears audited" 5 (List.length (ok (Store.bank_account_history db)));
    let info =
      List.find_exn
        (ok (Store.bank_account_info db))
        ~f:(fun i -> String.equal i.Store.mapping.account_type "CHECKING")
    in
    Alcotest.(check bool) "cleared IBAN absent" true (Option.is_none info.iban))
;;

let () =
  Alcotest.run
    "Unified bank accounts"
    [ ( "config"
      , List.map
          [ "canonical roundtrip and validation", roundtrip
          ; "dry run and apply", dry_run
          ; "atomic multi-field rollback", rollback
          ; "clear and patch semantics", clear_and_patch
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
