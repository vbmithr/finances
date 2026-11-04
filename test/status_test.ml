open Base
open Stdio
open Finances
open Finances_store
module Status = Finances_import.Bank_status

let ok = Or_error.ok_exn
let check = Alcotest.(check int)

let memory f =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       f db;
       Ok ()))
;;

(* Compare SQL summaries against detailed reports through state changes. *)
let status db =
  let report = ok (Status.read db) in
  let detailed = ok (Review.bank_review db) in
  check "SQL movement count" (List.length detailed) report.movements;
  check
    "SQL unclassified count"
    (List.count detailed ~f:(fun row ->
       String.equal row.Review.counter_account "equity:unclassified"))
    report.unclassified_movements;
  check
    "selective candidate count"
    (List.length (ok (Transfers.transfer_candidates db)))
    report.transfer_candidate_pairs;
  List.iter report.accounts ~f:(fun account ->
    let m = account.Transfers.mapping in
    let rows =
      List.filter detailed ~f:(fun row ->
        let a = row.Review.transaction.Movement.account in
        String.equal row.institution m.institution
        && String.equal a.bank_id m.bank_id
        && String.equal a.account_id m.account_id
        && String.equal a.account_type m.account_type)
    in
    check "SQL per-account count" (List.length rows) account.movements;
    let dates = List.map rows ~f:(fun r -> r.Review.transaction.Movement.booked_on) in
    Alcotest.(check string)
      "SQL first booking date"
      (Date.to_string (List.min_elt dates ~compare:Date.compare |> Option.value_exn))
      (Date.to_string account.first_on);
    Alcotest.(check string)
      "SQL last booking date"
      (Date.to_string (List.max_elt dates ~compare:Date.compare |> Option.value_exn))
      (Date.to_string account.last_on));
  Alcotest.(check bool)
    "SQL account ranges, identities and counts"
    true
    (Poly.equal (ok (Transfers.bank_account_info db)) report.accounts);
  report
;;

let empty () =
  memory (fun db ->
    let report = status db in
    check "accounts" 0 (List.length report.accounts);
    check "movements" 0 report.movements;
    check "unclassified" 0 report.unclassified_movements;
    check "pairs" 0 report.transfer_candidate_pairs;
    Alcotest.(check bool) "no import attempts" true (Option.is_none report.latest_import))
;;

let import db institution path =
  ignore
    (ok
       (Import.import_bank_file
          db
          ~institution
          ~path
          ~contents:(In_channel.read_all path)))
;;

let progress () =
  memory (fun db ->
    import db "boursobank" "fixtures/bank.ofx";
    let before = status db in
    check "discovered accounts" 2 (List.length before.accounts);
    check "primary movements" 3 before.movements;
    check "initially unclassified" 3 before.unclassified_movements;
    let account = List.hd_exn before.accounts in
    Alcotest.(check string) "observed date" "2026-01-04" (Date.to_string account.last_on);
    let event_id, _ = List.hd_exn (ok (Ledger.events db)) in
    ignore
      (ok
         (Review.set_bank_categories
            db
            [ ok
                (Bank_category.create
                   ~event_id
                   ~account:"expenses:food"
                   ~reason:"Reviewed")
            ]));
    ignore
      (ok
         (Import.record_bank_read_error
            db
            ~institution:"wise"
            ~path:"missing.xml"
            ~message:"not readable"));
    let after = status db in
    check "categorization keeps movement count" 3 after.movements;
    check "categorization reduces review work" 2 after.unclassified_movements;
    let latest = Option.value_exn after.latest_import in
    Alcotest.(check string)
      "latest failed attempt visible"
      "unreadable"
      (Bank_import.outcome_name latest.outcome);
    Alcotest.(check string)
      "failure detail"
      "not readable"
      (Option.value_exn latest.message))
;;

let transfers () =
  memory (fun db ->
    import db "bunq" "fixtures/bunq.mt940";
    import db "revolut" "fixtures/revolut.csv";
    let before = status db in
    let candidate =
      List.find_exn
        (ok (Transfers.transfer_candidates db))
        ~f:(fun c -> c.Transfers.unique)
    in
    ignore
      (ok
         (Transfers.set_bank_transfers
            db
            [ ok
                (Bank_transfer.create
                   ~first_event_id:candidate.first_event_id
                   ~second_event_id:candidate.second_event_id
                   ~state:Confirmed
                   ~reason:"Reviewed owned accounts")
            ]));
    let after = status db in
    check "confirmation keeps imported records" before.movements after.movements;
    check
      "confirmed legs leave category review"
      (before.unclassified_movements - 2)
      after.unclassified_movements;
    Alcotest.(check bool)
      "confirmed pair leaves candidates"
      true
      (after.transfer_candidate_pairs < before.transfer_candidate_pairs);
    ignore
      (ok
         (Transfers.set_bank_transfers
            db
            [ ok
                (Bank_transfer.create
                   ~first_event_id:candidate.first_event_id
                   ~second_event_id:candidate.second_event_id
                   ~state:Rejected
                   ~reason:"Corrected review")
            ]));
    let rejected = status db in
    check
      "rejected transfer restores category review"
      before.unclassified_movements
      rejected.unclassified_movements;
    Alcotest.(check bool)
      "rejected pair stays reviewed"
      true
      (rejected.transfer_candidate_pairs < before.transfer_candidate_pairs))
;;

let () =
  Alcotest.run
    "Bank status"
    [ ( "progress"
      , List.map
          [ "empty", empty
          ; "classification and failed import", progress
          ; "confirmed transfers", transfers
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
