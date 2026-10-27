open Core
open Finances
module Status = Finances_bank.Bank_status

let ok = Or_error.ok_exn
let check = Alcotest.(check int)

let memory f =
  ok
    (Store.with_open ":memory:" (fun db ->
       f db;
       Ok ()))
;;

let empty () =
  memory (fun db ->
    let report = ok (Status.read db) in
    check "accounts" 0 (List.length report.accounts);
    check "movements" 0 report.movements;
    check "unclassified" 0 report.unclassified_movements;
    check "pairs" 0 report.transfer_candidate_pairs;
    Alcotest.(check bool) "no import attempts" true (Option.is_none report.latest_import))
;;

let import db institution path =
  ignore
    (ok
       (Store.import_bank_file
          ~account_id:"main"
          db
          ~institution
          ~path
          ~contents:(In_channel.read_all path)))
;;

let progress () =
  memory (fun db ->
    import db "boursobank" "fixtures/bank.ofx";
    let before = ok (Status.read db) in
    check "discovered accounts" 2 (List.length before.accounts);
    check "primary movements" 3 before.movements;
    check "initially unclassified" 3 before.unclassified_movements;
    let account = List.hd_exn before.accounts in
    Alcotest.(check string) "observed date" "2026-01-04" (Date.to_string account.last_on);
    let event_id, _ = List.hd_exn (ok (Store.events db)) in
    ignore
      (ok
         (Store.set_bank_categories
            db
            [ ok
                (Bank_category.create
                   ~event_id
                   ~account:"expenses:food"
                   ~reason:"Reviewed")
            ]));
    ignore
      (ok
         (Store.record_bank_read_error
            db
            ~institution:"wise"
            ~path:"missing.xml"
            ~message:"not readable"));
    let after = ok (Status.read db) in
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
    let before = ok (Status.read db) in
    let candidate =
      List.find_exn (ok (Store.transfer_candidates db)) ~f:(fun c -> c.Store.unique)
    in
    ignore
      (ok
         (Store.set_bank_transfers
            db
            [ ok
                (Bank_transfer.create
                   ~first_event_id:candidate.first_event_id
                   ~second_event_id:candidate.second_event_id
                   ~state:Confirmed
                   ~reason:"Reviewed owned accounts")
            ]));
    let after = ok (Status.read db) in
    check "confirmation keeps imported records" before.movements after.movements;
    check
      "confirmed legs leave category review"
      (before.unclassified_movements - 2)
      after.unclassified_movements;
    Alcotest.(check bool)
      "confirmed pair leaves candidates"
      true
      (after.transfer_candidate_pairs < before.transfer_candidate_pairs))
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
