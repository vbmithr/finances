open Base
open Stdio

let ok = Or_error.ok_exn
let fixture () = In_channel.read_all "fixtures/bank.ofx"
let rejected name result = Alcotest.(check bool) name true (Result.is_error result)

let replace s old replacement =
  String.substr_replace_first s ~pattern:old ~with_:replacement
;;

let statements () =
  let statements = ok (Ofx.parse (fixture ())) in
  Alcotest.(check int) "statements" 2 (List.length statements);
  let checking = List.hd_exn statements in
  Alcotest.(check string) "account" "CHECKING" checking.account.account_id;
  Alcotest.(check string) "currency" "EUR" checking.account.currency;
  Alcotest.(check (option string))
    "available balance as written"
    (Some "100.01")
    (Option.map checking.available_balance ~f:(fun b -> b.amount));
  Alcotest.(check bool) "no ledger balance" true (Option.is_none checking.ledger_balance);
  let first, second =
    match checking.transactions with
    | [ a; b ] -> a, b
    | _ -> Alcotest.fail "expected two transactions"
  in
  Alcotest.(check string) "FITID" "shared-id" first.fitid;
  Alcotest.(check string) "amount as written" "-0.10" first.amount;
  Alcotest.(check (option string)) "entity decoded" (Some "CAFÉ & CO") first.name;
  Alcotest.(check (option string))
    "invalid DTUSER kept verbatim"
    (Some "00009999")
    first.user_date;
  Alcotest.(check string)
    "precision kept"
    "12345678901234567890.1234567890123456789"
    second.amount;
  Alcotest.(check bool) "no correction" false first.corrects;
  let again = ok (Ofx.transaction_of_tree first.tree) in
  Alcotest.(check string) "tree round trip" first.fitid again.fitid
;;

let dates () =
  let date s = Ofx.calendar_date s |> Result.ok in
  let check name expected s =
    Alcotest.(check (option (triple int int int))) name expected (date s)
  in
  check "date" (Some (2026, 1, 3)) "20260103";
  check "datetime" (Some (2026, 1, 3)) "20260103235959";
  (* The offset does not move the bank's calendar date. *)
  check "zoned" (Some (2026, 1, 3)) "20260103235959.123[-5:EST]";
  check "invalid day" None "20260231";
  check "bad syntax" None "2026-01-03";
  check "DTUSER placeholder" None "00009999"
;;

let rejections () =
  let contents = fixture () in
  rejected
    "failed status"
    (Ofx.parse (replace contents "<CODE>0</CODE>" "<CODE>2000</CODE>"));
  rejected
    "credit-card message set"
    (Ofx.parse (replace contents "<BANKMSGSRSV1>" "<CREDITCARDMSGSRSV1><BANKMSGSRSV1>"));
  rejected "missing FITID" (Ofx.parse (replace contents "<FITID>credit-id</FITID>" ""));
  rejected
    "bad posting date"
    (Ofx.parse
       (replace contents "<DTPOSTED>20260103</DTPOSTED>" "<DTPOSTED>3 Jan</DTPOSTED>"));
  rejected "not OFX" (Ofx.parse "<html/>");
  rejected "SGML" (Ofx.parse "OFXHEADER:100\nDATA:OFXSGML\n<OFX>")
;;

(* Corrections are part of the standard: reported, not refused. *)
let corrections () =
  let contents =
    replace
      (fixture ())
      "<FITID>credit-id</FITID>"
      "<FITID>credit-id</FITID><CORRECTFITID>shared-id</CORRECTFITID><CORRECTACTION>REPLACE</CORRECTACTION>"
  in
  let statements = ok (Ofx.parse contents) in
  let corrected =
    List.concat_map statements ~f:(fun (s : Ofx.statement) -> s.transactions)
    |> List.filter ~f:(fun (t : Ofx.transaction) -> t.corrects)
    |> List.map ~f:(fun (t : Ofx.transaction) -> t.fitid)
  in
  Alcotest.(check (list string)) "flagged" [ "credit-id" ] corrected
;;

let () =
  Alcotest.run
    "OFX"
    [ ( "ofx"
      , [ Alcotest.test_case "statements" `Quick statements
        ; Alcotest.test_case "dates" `Quick dates
        ; Alcotest.test_case "rejections" `Quick rejections
        ; Alcotest.test_case "corrections" `Quick corrections
        ] )
    ]
;;
