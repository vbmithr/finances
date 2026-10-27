open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b
let xml () = In_channel.read_all "fixtures/wise.xml"
let csv () = In_channel.read_all "fixtures/wise.csv"

let import db ?(institution = "wise") path contents =
  ok (Store.import_bank_file db ~institution ~path ~contents)
;;

let accepted result =
  Alcotest.(check bool)
    "accepted"
    true
    (Poly.equal result.Bank_import.outcome Imported || Poly.equal result.outcome Unchanged)
;;

let rejected result =
  Alcotest.(check bool) "rejected" true (Poly.equal result.Bank_import.outcome Rejected)
;;

let memory f =
  ok
    (Store.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       f db;
       Ok ()))
;;

let error result = Alcotest.(check bool) "invalid document" true (Result.is_error result)

let parser () =
  let doc = ok (Wise_camt.parse (xml ())) in
  let tx = (List.hd_exn doc.statements).transactions in
  check "reversal preserved" 3 (List.length tx);
  check
    "unique identities"
    3
    (List.length
       (List.dedup_and_sort
          (List.map tx ~f:(fun t -> t.source_id))
          ~compare:String.compare));
  Alcotest.(check string)
    "local calendar"
    "2025-01-02"
    (Date.to_string (List.hd_exn tx).booked_on);
  error
    (Wise_camt.parse
       (replace (xml ()) "<Amt Ccy=\"EUR\">105.00</Amt>" "<Amt Ccy=\"EUR\">106.00</Amt>"));
  error (Wise_camt.parse (replace (xml ()) "<Cd>BOOK</Cd>" "<Cd>PDNG</Cd>"));
  error (Wise_camt.parse (replace (xml ()) "camt.053.001.10" "camt.053.001.08"));
  let original = xml () in
  let start = String.substr_index_exn original ~pattern:"<Ntry>" in
  let finish = String.substr_index_exn original ~pattern:"</Ntry>" + 7 in
  let entry = String.sub original ~pos:start ~len:(finish - start) in
  error (Wise_camt.parse (replace original "</Stmt>" (entry ^ "</Stmt>")));
  error (Wise_csv.parse (replace (csv ()) "02-01-2025" "32-01-2025"))
;;

let multi_bank () =
  memory (fun db ->
    accepted (import db "wise.csv" (csv ()));
    check "CSV creates no movements" 0 (List.length (ok (Store.events db)));
    check "CSV waiting" 3 (ok (Store.match_companions ~institution:"wise" db)).unmatched;
    accepted
      (import
         db
         ~institution:"boursobank"
         "bank.ofx"
         (In_channel.read_all "fixtures/bank.ofx"));
    accepted (import db "wise.xml" (xml ()));
    check "both institutions" 6 (List.length (ok (Store.events db)));
    check
      "all Wise rows matched"
      3
      (ok (Store.match_companions ~institution:"wise" db)).matched;
    check "all projected" 6 (ok (Store.project_bank db)).inserted;
    let accounts = ok (Store.bank_accounts db) in
    Alcotest.(check bool)
      "separate bank accounts"
      true
      (List.exists accounts ~f:(fun a ->
         String.equal a.Bank_account_mapping.institution "wise")
       && List.exists accounts ~f:(fun a ->
         String.equal a.Bank_account_mapping.institution "boursobank"));
    let statements = ok (Store.statement_reconciliations ~institution:"wise" db) in
    Alcotest.(check bool) "statements stored" true (not (List.is_empty statements));
    Alcotest.(check int)
      "every Wise movement belongs to a statement"
      3
      (List.sum (module Int) statements ~f:(fun r -> r.Store.entries));
    List.iter statements ~f:(fun r ->
      Alcotest.(check bool)
        "stored balance reconciled"
        true
        (Quantity.is_zero r.Store.difference));
    let journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "merchant metadata exported"
      true
      (String.is_substring journal ~substring:"wise-merchant: Synthetic shop");
    Alcotest.(check bool)
      "nonzero fees retained as metadata"
      true
      (String.is_substring journal ~substring:"wise-reported-fees: 0.5");
    accepted (import db "replay.xml" (xml ()));
    accepted (import db "overlap.xml" (replace (xml ()) "statement-a" "statement-b"));
    check "overlap no new projections" 0 (ok (Store.project_bank db)).inserted;
    check
      "repeated matching stable"
      0
      (ok (Store.match_companions ~institution:"wise" db)).changed;
    check "replay source count" 6 (List.length (ok (Store.events db)));
    let wise_account =
      List.find_exn accounts ~f:(fun a ->
        String.equal a.Bank_account_mapping.institution "wise")
    in
    let mapped =
      ok
        (Bank_account_mapping.create
           ~institution:wise_account.institution
           ~bank_id:wise_account.bank_id
           ~account_id:wise_account.account_id
           ~account_type:wise_account.account_type
           ~ledger_account:"assets:bank:wise:daily")
    in
    check "Wise mapping applied" 1 (ok (Store.set_bank_account_mappings db [ mapped ]));
    check "mapping replay" 0 (ok (Store.project_bank db)).inserted;
    let mapped_journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "Wise alias exported"
      true
      (String.is_substring mapped_journal ~substring:"assets:bank:wise:daily");
    let changed =
      replace
        (replace (xml ()) "<Amt Ccy=\"EUR\">5.00</Amt>" "<Amt Ccy=\"EUR\">6.00</Amt>")
        "<Amt Ccy=\"EUR\">105.00</Amt>"
        "<Amt Ccy=\"EUR\">106.00</Amt>"
    in
    rejected (import db "conflict.xml" changed);
    check "conflict atomic" 6 (List.length (ok (Store.events db)));
    rejected (import db ~institution:"boursobank" "wrong.xml" (xml ())))
;;

let currencies () =
  memory (fun db ->
    accepted (import db "eur.xml" (xml ()));
    accepted (import db "usd.xml" (replace (xml ()) "EUR" "USD"));
    check "currencies independent" 6 (List.length (ok (Store.events db)));
    accepted (import db "eur.csv" (csv ()));
    check "currency match" 3 (ok (Store.match_companions ~institution:"wise" db)).matched;
    accepted (import db "fees.csv" (replace (csv ()) "0.50" "0.75"));
    check
      "fee conflict audit"
      2
      (ok (Store.match_companions ~institution:"wise" db)).conflicts;
    check "postings only movements" 6 (ok (Store.project_bank db)).inserted)
;;

let duplicate_rows () =
  memory (fun db ->
    accepted (import db "wise.xml" (xml ()));
    let lines = String.split_lines (csv ()) in
    accepted (import db "dup.csv" (csv () ^ List.nth_exn lines 1 ^ "\n"));
    check
      "ambiguous duplicates"
      2
      (ok (Store.match_companions ~institution:"wise" db)).ambiguous)
;;

let () =
  Alcotest.run
    "Wise and multiple banks"
    [ ( "adapter"
      , List.map
          [ "parser and balances", parser
          ; "two banks, replay, conflict", multi_bank
          ; "currency and fee conflicts", currencies
          ; "duplicate CSV claims", duplicate_rows
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
