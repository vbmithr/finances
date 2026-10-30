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
  ignore (ok (Test_support.import_ofx db ~institution:"boursobank" ofx));
  ignore (ok (Store.project_bank db))
;;

let memory f =
  ok
    (Store.with_open ~registry:Test_support.registry ":memory:" (fun db ->
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
    match
      Store.apply_bank_accounts db [ { (checking rows) with reference = unknown } ]
    with
    | Ok _ -> Alcotest.fail "an account with no imported statements was configured"
    | Error e ->
      let message = Error.to_string_hum e in
      List.iter
        [ "1 configured account has no imported statements"
        ; "boursobank CHECKING unknown"
        ; "fin import BANK PATH"
        ]
        ~f:(fun part ->
          Alcotest.(check bool) part true (String.is_substring message ~substring:part)))
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

let settlement_setup_rolls_back () =
  memory (fun db ->
    let rows = config db in
    let cash = checking rows
    and credit = card rows in
    let before = Bank_account_config.to_json rows in
    let info =
      List.find_exn
        (ok (Store.bank_account_info db))
        ~f:(fun a -> String.equal a.Store.mapping.account_type "CREDITCRD")
    in
    let setup =
      { credit with
        ledger_account = Some "liabilities:cards:test"
      ; settlement_account = Set cash.reference
      }
    in
    error
      (Store.confirm_card_settlement_with_setup
         db
         ~event_id:999999L
         ~card_id:(Bank_account_mapping.id info.mapping)
         ~expected_ledger:"liabilities:cards:stale"
         ~expected_link:info.settlement_account
         ~setup);
    error
      (Store.confirm_card_settlement_with_setup
         db
         ~event_id:999999L
         ~card_id:(Bank_account_mapping.id info.mapping)
         ~expected_ledger:info.mapping.ledger_account
         ~expected_link:info.settlement_account
         ~setup);
    Alcotest.(check string)
      "failed settlement rolls back all setup"
      before
      (Bank_account_config.to_json (config db));
    check
      "failed settlement writes no account history"
      0
      (List.length (ok (Store.bank_account_history db)));
    check
      "failed settlement writes no settlement history"
      0
      (List.length (ok (Store.card_settlement_history db))))
;;

let card_settlement () =
  memory (fun db ->
    let rows = config db in
    let cash = checking rows
    and credit = card rows in
    let event =
      List.find_exn
        (ok (Store.bank_review db))
        ~f:(fun r ->
          String.equal r.transaction.account.account_type "CHECKING"
          && Quantity.(r.transaction.amount < zero))
    in
    error (Store.card_settlement_preview db event.event_id);
    ignore
      (ok
         (Store.apply_bank_accounts
            db
            [ { credit with
                ledger_account = Some "liabilities:cards:test"
              ; settlement_account = Set cash.reference
              }
            ]));
    let before = ok (Store.bank_overview db) in
    ignore (ok (Store.card_settlement_preview db event.event_id));
    error
      (Store.confirm_card_settlement
         ~expected_ledger:"liabilities:cards:stale"
         db
         event.event_id);
    ok (Store.confirm_card_settlement db event.event_id);
    let reviewed =
      List.find_exn
        (ok (Store.bank_review db))
        ~f:(fun r -> Int64.equal r.event_id event.event_id)
    in
    Alcotest.(check string)
      "settlement counter account"
      "liabilities:cards:test"
      reviewed.counter_account;
    let _, _, before_count, _ = before in
    let _, _, after_count, _ = ok (Store.bank_overview db) in
    check "no longer unclassified" (before_count - 1) after_count;
    let _, _, entry =
      List.find_exn
        (ok (Store.entries db))
        ~f:(fun (id, _, _) -> Int64.equal id event.event_id)
    in
    Alcotest.(check string)
      "posting uses liability"
      "liabilities:cards:test"
      (List.nth_exn entry.postings 1).account;
    error (Store.confirm_card_settlement db event.event_id);
    let decision =
      ok
        (Bank_category.create
           ~event_id:event.event_id
           ~account:"expenses:food"
           ~reason:"test")
    in
    error (Store.set_bank_categories db [ decision ]);
    error
      (Store.apply_bank_accounts
         db
         [ { credit with ledger_account = Some "liabilities:cards:new" } ]);
    ok (Store.undo_card_settlement db event.event_id);
    let restored =
      List.find_exn
        (ok (Store.bank_review db))
        ~f:(fun r -> Int64.equal r.event_id event.event_id)
    in
    Alcotest.(check string)
      "undo restores unclassified"
      "equity:unclassified"
      restored.counter_account;
    error (Store.undo_card_settlement db event.event_id);
    ignore (ok (Store.project_bank db)))
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
          ; "card settlement confirmation and undo", card_settlement
          ; "settlement setup rolls back on failure", settlement_setup_rolls_back
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
