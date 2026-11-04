open Base
open Stdio
open Finances
open Finances_store

let bunq_profile = Option.value_exn (Builtin.mt940_profile "bunq")
let bunq_engine = Mt940_transaction.make bunq_profile

module Bunq_transaction = struct
  include Mt940_transaction
end

module Bunq_mt940 = struct
  let parse = Mt940_source.parse bunq_engine
end

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)
let mt () = In_channel.read_all "fixtures/bunq.mt940"
let csv () = In_channel.read_all "fixtures/bunq.csv"
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b

let memory f =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       f db;
       Ok ()))
;;

let import ?(institution = "bunq") db path contents =
  ok (Import.import_bank_file db ~institution ~path ~contents)
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
  error (Bunq_mt940.parse (mt () ^ "extra"))
;;

let workflow () =
  memory (fun db ->
    accepted (import db "early.csv" (csv ()));
    check "CSV creates no events" 0 (List.length (ok (Ledger.events db)));
    check "CSV pending" 4 (ok (Ledger.match_companions ~institution:"bunq" db)).unmatched;
    accepted (import db "extensionless" (mt ()));
    check
      "matched all occurrences"
      4
      (ok (Ledger.match_companions ~institution:"bunq" db)).matched;
    check "generated movements" 4 (ok (Ledger.project_bank db)).inserted;
    let statements = ok (Review.statement_reconciliations ~institution:"bunq" db) in
    Alcotest.(check (list int))
      "one statement listing every movement"
      [ 4 ]
      (List.map statements ~f:(fun r -> r.Review.entries));
    List.iter statements ~f:(fun r ->
      Alcotest.(check bool)
        "persisted statement reconciles"
        true
        (Quantity.is_zero r.Review.difference));
    let journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "CSV provenance"
      true
      (String.is_substring journal ~substring:"companion-row-ids:");
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
    check "no duplicate projections" 0 (ok (Ledger.project_bank db)).inserted;
    check
      "no match changes"
      0
      (ok (Ledger.match_companions ~institution:"bunq" db)).changed;
    check "still four events" 4 (List.length (ok (Ledger.events db)));
    let category =
      ok
        (Bank_category.create
           ~event_id:
             (let id, _, _ = List.hd_exn (ok (Ledger.movements db)) in
              id)
           ~account:"expenses:food"
           ~reason:"Reviewed")
    in
    ignore (ok (Review.set_bank_categories db [ category ]));
    let a = List.hd_exn (ok (Ledger.bank_accounts db)) in
    let mapping =
      ok
        (Bank_account_mapping.create
           ~institution:a.institution
           ~bank_id:a.bank_id
           ~account_id:a.account_id
           ~account_type:a.account_type
           ~ledger_account:"assets:bank:bunq:daily")
    in
    ignore (ok (Ledger.set_bank_account_mappings db [ mapping ]));
    check "replay reviewed mapped entries" 0 (ok (Ledger.project_bank db)).inserted)
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
    check "atomic source rollback" 4 (List.length (ok (Ledger.events db)));
    check
      "statement observations unchanged"
      1
      (List.length (ok (Review.statement_reconciliations ~institution:"bunq" db)));
    rejected (import ~institution:"boursobank" db "wrong.mt940" (mt ()));
    rejected (import ~institution:"wise" db "wrong.csv" (csv ())))
;;

let ambiguity () =
  memory (fun db ->
    accepted (import db "original.mt940" (mt ()));
    let without_first =
      match String.split_lines (csv ()) with
      | header :: _ :: rows -> String.concat ~sep:"\n" (header :: rows) ^ "\n"
      | _ -> Alcotest.fail "the fixture has rows"
    in
    accepted (import db "partial.csv" without_first);
    check
      "partial identical group is ambiguous"
      1
      (ok (Ledger.match_companions ~institution:"bunq" db)).ambiguous);
  memory (fun db ->
    accepted (import db "original.mt940" (mt ()));
    let other = replace (mt ()) "SYNTHETICBUNQ" "SYNTHETICSECOND" in
    accepted (import db "other.mt940" other);
    accepted (import db "original.csv" (csv ()));
    check
      "account matching isolates movements"
      4
      (ok (Ledger.match_companions ~institution:"bunq" db)).matched;
    (* The CSV states EUR by definition, so a statement in another currency is not its pair. *)
    accepted (import db "usd.mt940" (replace (mt ()) "EUR" "USD"));
    check
      "another currency is left alone"
      4
      (ok (Ledger.match_companions ~institution:"bunq" db)).matched)
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
    check "three banks ten movements" 10 (List.length (ok (Ledger.events db)));
    check "ten projections" 10 (ok (Ledger.project_bank db)).inserted;
    check
      "three institutions"
      3
      (List.map
         (ok (Ledger.bank_accounts db))
         ~f:(fun a -> a.Bank_account_mapping.institution)
       |> List.dedup_and_sort ~compare:String.compare
       |> List.length))
;;

let mt940_identification () =
  memory (fun db ->
    let wise_style =
      "{1:F01WISESYNTHETIC}\n\
       :20:synthetic\n\
       :25:account\n\
       :28C:1\n\
       :60F:C260101EUR0,00\n\
       :62F:C260131EUR0,00\n\
       -\n"
    in
    let other = import ~institution:"wise" db "wise.mt940" wise_style in
    Alcotest.(check bool)
      "non-Bunq MT940 is unsupported rather than claimed by Bunq"
      true
      (Poly.equal other.outcome Unsupported);
    let bunq = import ~institution:"wise" db "bunq.mt940" (mt ()) in
    Alcotest.(check bool)
      "actual Bunq export is rejected"
      true
      (Poly.equal bunq.outcome Rejected);
    accepted (import db "bunq.mt940" (mt ())))
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
          ; "MT940 identification", mt940_identification
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
