open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)
let mt () = In_channel.read_all "fixtures/bunq.mt940"
let csv () = In_channel.read_all "fixtures/bunq.csv"
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b

let memory f =
  ok
    (Store.with_open ~registry:Builtin.registry ":memory:" (fun db ->
       f db;
       Ok ()))
;;

let import ?(institution = "bunq") db path contents =
  ok (Store.import_bank_file db ~institution ~path ~contents)
;;

let accepted report =
  Alcotest.(check bool)
    "accepted"
    true
    (Poly.equal report.Bank_import.outcome Imported || Poly.equal report.outcome Unchanged)
;;

let rejected report =
  Alcotest.(check bool)
    "rejected import"
    true
    (Poly.equal report.Bank_import.outcome Rejected)
;;

let parser () =
  let document = ok (Bunq_mt940.parse (mt ())) in
  check "four movements including two identical" 4 (List.length document.transactions);
  check
    "four identities"
    4
    (List.length
       (List.map document.transactions ~f:(fun tx -> tx.source_id)
        |> List.dedup_and_sort ~compare:String.compare));
  let last = List.last_exn document.transactions in
  Alcotest.(check string) "booking date" "2026-01-04" (Date.to_string last.booked_on);
  Alcotest.(check string) "interest date" "2026-01-03" (Date.to_string last.value_on);
  Alcotest.(check string) "multiline memo" "First line Second line" last.description;
  error (Bunq_mt940.parse (replace (mt ()) "EUR175,00" "EUR176,00"));
  error (Bunq_mt940.parse (replace (mt ()) "NMSCNONREF" "NMSCTRANSACTIONID"));
  error (Bunq_mt940.parse (replace (mt ()) "2601030104" "2601031231"));
  error (Bunq_mt940.parse (mt () ^ "extra"));
  let rows = ok (Bunq_csv.parse (replace (csv ()) "100,00" "1\226\128\175100,00")) in
  Alcotest.(check bool)
    "grouped CSV decimal"
    true
    (Quantity.equal (List.nth_exn rows 2).amount (Quantity.of_string "1100"));
  error (Bunq_csv.parse (replace (csv ()) "100,00" "1 1,00"));
  error (Bunq_csv.parse (replace (csv ()) "2026-01-02" "2026-02-30"))
;;

let workflow () =
  memory (fun db ->
    accepted (import db "early.csv" (csv ()));
    check "CSV creates no events" 0 (List.length (ok (Store.events db)));
    check "CSV pending" 4 (ok (Store.match_bunq_csv db)).unmatched;
    accepted (import db "extensionless" (mt ()));
    check "matched all occurrences" 4 (ok (Store.match_bunq_csv db)).matched;
    check "generated movements" 4 (ok (Store.project_bank db)).inserted;
    List.iter
      (ok (Store.bunq_reconciliations db))
      ~f:(fun r ->
        Alcotest.(check bool)
          "persisted statement reconciles"
          true
          (Quantity.is_zero r.Store.difference));
    let journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "CSV provenance"
      true
      (String.is_substring journal ~substring:"bunq-csv-row-id:");
    Alcotest.(check bool)
      "value date available"
      true
      (String.is_substring journal ~substring:"2026-01-04=2026-01-03");
    let again = import db "renamed.mt940" (mt ()) in
    accepted again;
    check "replay existing" 4 again.existing;
    accepted
      (import
         db
         "overlap.mt940"
         (replace (mt ()) "synthetic-statement" "overlapping-statement"));
    check "no duplicate projections" 0 (ok (Store.project_bank db)).inserted;
    check "no match changes" 0 (ok (Store.match_bunq_csv db)).changed;
    check "still four events" 4 (List.length (ok (Store.events db)));
    let category =
      ok
        (Bank_category.create
           ~event_id:(fst (List.hd_exn (ok (Store.bunq_transactions db))))
           ~account:"expenses:food"
           ~reason:"Reviewed")
    in
    ignore (ok (Store.set_bank_categories db [ category ]));
    let a = List.hd_exn (ok (Store.bank_accounts db)) in
    let mapping =
      ok
        (Bank_account_mapping.create
           ~institution:a.institution
           ~bank_id:a.bank_id
           ~account_id:a.account_id
           ~account_type:a.account_type
           ~ledger_account:"assets:bank:bunq:daily")
    in
    ignore (ok (Store.set_bank_account_mappings db [ mapping ]));
    check "replay reviewed mapped entries" 0 (ok (Store.project_bank db)).inserted)
;;

let conflicts () =
  memory (fun db ->
    accepted (import db "original.mt940" (mt ()));
    let changed = replace (replace (mt ()) "D10," "D11,") "EUR175,00" "EUR173,00" in
    rejected (import db "changed.mt940" changed);
    rejected
      (import db "metadata.mt940" (replace (mt ()) "Synthetic shop" "Different shop"));
    let entry =
      ":61:2601020102D10,NMSCNONREF\n:86:/NAME/Synthetic shop/REMI/Purchase\n\n"
    in
    let partial =
      String.substr_replace_first (mt ()) ~pattern:entry ~with_:""
      |> fun s -> replace s "EUR175,00" "EUR185,00"
    in
    rejected (import db "partial.mt940" partial);
    check "atomic source rollback" 4 (List.length (ok (Store.events db)));
    check
      "statement observations unchanged"
      1
      (List.length (ok (Store.bunq_reconciliations db)));
    rejected (import ~institution:"boursobank" db "wrong.mt940" (mt ()));
    rejected (import ~institution:"wise" db "wrong.csv" (csv ())))
;;

let ambiguity () =
  memory (fun db ->
    accepted (import db "original.mt940" (mt ()));
    let rows = ok (Bunq_csv.parse (csv ())) in
    let partial =
      List.tl_exn rows
      |> List.mapi ~f:(fun i row ->
        { Bunq_csv.row_id = Int64.of_int (i + 1); document_id = 1L; row })
    in
    let results = Bunq_csv.match_rows partial (ok (Store.bunq_transactions db)) in
    check
      "partial identical group is ambiguous"
      1
      (List.count results ~f:(fun r -> Poly.equal r.Bank_match.state Ambiguous));
    let other = replace (mt ()) "SYNTHETICBUNQ" "SYNTHETICSECOND" in
    accepted (import db "other.mt940" other);
    accepted (import db "original.csv" (csv ()));
    check "account matching isolates movements" 4 (ok (Store.match_bunq_csv db)).matched;
    accepted (import db "usd.mt940" (replace (mt ()) "EUR" "USD"));
    check
      "currency-free CSV cannot guess currency"
      4
      (ok (Store.match_bunq_csv db)).ambiguous)
;;

let multibank () =
  memory (fun db ->
    accepted (import db "bunq.mt940" (mt ()));
    accepted
      (import ~institution:"wise" db "wise.xml" (In_channel.read_all "fixtures/wise.xml"));
    accepted
      (import
         ~institution:"boursobank"
         db
         "bank.ofx"
         (In_channel.read_all "fixtures/bank.ofx"));
    check "three banks ten movements" 10 (List.length (ok (Store.events db)));
    check "ten projections" 10 (ok (Store.project_bank db)).inserted;
    check
      "three institutions"
      3
      (List.map
         (ok (Store.bank_accounts db))
         ~f:(fun a -> a.Bank_account_mapping.institution)
       |> List.dedup_and_sort ~compare:String.compare
       |> List.length))
;;

let () =
  Alcotest.run
    "Bunq"
    [ ( "adapter"
      , List.map
          [ "parsers and exact balances", parser
          ; "CSV first, duplicates, overlap and review", workflow
          ; "changed day rejects atomically", conflicts
          ; "ambiguity and account scope", ambiguity
          ; "three banks", multibank
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
