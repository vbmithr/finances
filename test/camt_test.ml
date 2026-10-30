open Base
open Stdio

let ok = Or_error.ok_exn
let contents () = In_channel.read_all "fixtures/wise.xml"
let replace text pattern with_ = String.substr_replace_all text ~pattern ~with_
let check = Alcotest.(check bool)

let standard_structure () =
  let document = ok (Camt.parse (contents ())) in
  Alcotest.(check int) "statement count" 1 (List.length document.statements);
  Alcotest.(check string)
    "namespace"
    "urn:iso:std:iso:20022:tech:xsd:camt.053.001.10"
    document.namespace;
  let statement = List.hd_exn document.statements in
  Alcotest.(check int)
    "entries"
    3
    (List.length (Camt.children ~namespace:document.namespace "Ntry" statement));
  let entry =
    List.hd_exn (Camt.children ~namespace:document.namespace "Ntry" statement)
  in
  let amount = List.hd_exn (Camt.children ~namespace:document.namespace "Amt" entry) in
  check
    "currency attribute preserved"
    true
    (match amount with
     | Camt.Element (_, attributes, _) ->
       Option.equal
         String.equal
         (List.Assoc.find attributes ("", "Ccy") ~equal:Poly.equal)
         (Some "EUR")
     | Text _ -> false)
;;

let typed_statement () =
  let document = ok (Camt.parse (contents ())) in
  let statement =
    ok
      (Camt.statement_view
         ~namespace:document.namespace
         (List.hd_exn document.statements))
  in
  Alcotest.(check (option string)) "statement ID" (Some "statement-a") statement.id;
  let account = Option.value_exn statement.account in
  check
    "account identifier"
    true
    (Poly.equal account.id (Some (Camt.Iban "SYNTHETIC-ACCOUNT")));
  Alcotest.(check (option string)) "currency" (Some "EUR") account.currency;
  Alcotest.(check (option string)) "servicer" (Some "Wise") account.servicer_name;
  Alcotest.(check int) "balances" 2 (List.length statement.balances);
  Alcotest.(check int) "entries" 3 (List.length statement.entries);
  let first = List.hd_exn statement.entries in
  let amount = Option.value_exn first.amount in
  Alcotest.(check string) "raw amount" "10.00" amount.value;
  Alcotest.(check (option string)) "raw currency" (Some "EUR") amount.currency;
  check "debit direction" true (Poly.equal first.direction (Some Camt.Debit));
  check "booked status" true (Poly.equal first.status (Some Camt.Booked));
  check
    "source booking datetime"
    true
    (Poly.equal
       first.booking_date
       (Some (Camt.Date_time "2025-01-02T00:00:00.123456+01:00")));
  Alcotest.(check (option string))
    "proprietary code"
    (Some "CARD-1")
    first.proprietary_code;
  Alcotest.(check (option string)) "description" (Some "Purchase") first.additional_info
;;

let other_institutions_and_versions () =
  let text =
    contents ()
    |> fun s ->
    replace s "camt.053.001.10" "camt.053.001.08"
    |> fun s -> replace s ">Wise<" ">Other bank<"
  in
  let document = ok (Camt.parse text) in
  check
    "other version accepted structurally"
    true
    (String.is_suffix document.namespace ~suffix:"camt.053.001.08");
  check "bank-specific servicer not required" true (List.length document.statements = 1)
;;

let invalid_documents () =
  let text = contents () in
  List.iter
    [ replace text "camt.053.001.10" "camt.054.001.10"
    ; (replace text "<BkToCstmrStmt>" "<OtherMessage>"
       |> fun s -> replace s "</BkToCstmrStmt>" "</OtherMessage>")
    ; "<!DOCTYPE Document>" ^ text
    ; text ^ text
    ]
    ~f:(fun invalid ->
      check "invalid document rejected" true (Result.is_error (Camt.parse invalid)))
;;

let () =
  Alcotest.run
    "CAMT reader"
    [ ( "structure"
      , [ Alcotest.test_case "standard structure" `Quick standard_structure
        ; Alcotest.test_case "typed statement" `Quick typed_statement
        ; Alcotest.test_case
            "other institution and version"
            `Quick
            other_institutions_and_versions
        ; Alcotest.test_case "invalid documents" `Quick invalid_documents
        ] )
    ]
;;
