open Base
open Finances_banks

let profile : Csv_profile.t =
  { separator = ','
  ; header = [ "Merchant"; "Date"; "USD" ]
  ; date_column = "Date"
  ; description_column = "Merchant"
  ; amount_column = "USD"
  ; currency = Iso4217.of_alpha_code "USD" |> Option.value_exn
  ; account_type = "prepaid-card"
  ; invert_amount = true
  ; skip_when = Some ("Merchant", "Total")
  }
;;

let rows () =
  let input =
    "Merchant,Date,USD\nShop,2026-10-01,12.50\nRefund,2026-10-02,-2.00\nTotal,,10.50\n"
  in
  let rows = Csv_profile.parse profile input |> Or_error.ok_exn in
  Alcotest.(check int) "only transactions" 2 (List.length rows);
  Alcotest.(check string)
    "purchase"
    "-12.5"
    (Quantity.to_string (List.hd_exn rows).amount);
  Alcotest.(check string) "refund" "2" (Quantity.to_string (List.nth_exn rows 1).amount);
  Alcotest.(check (list string))
    "raw fields"
    [ "Shop"; "2026-10-01"; "12.50" ]
    (List.hd_exn rows).fields;
  Alcotest.(check bool)
    "invalid date rejected"
    true
    (Result.is_error (Csv_profile.parse profile "Merchant,Date,USD\nShop,no,2\n"));
  Alcotest.(check bool)
    "changed header rejected"
    true
    (Result.is_error (Csv_profile.parse profile "Merchant,Date,EUR\n"))
;;

let identities () =
  let rows =
    Csv_profile.parse
      profile
      "Merchant,Date,USD\nShop,2026-10-01,12.50\nShop,2026-10-01,12.50\n"
    |> Or_error.ok_exn
  in
  let first = Csv_movement.events ~account_id:"first" rows |> Or_error.ok_exn in
  let second = Csv_movement.events ~account_id:"second" rows |> Or_error.ok_exn in
  let a = List.hd_exn first
  and b = List.nth_exn first 1
  and c = List.hd_exn second in
  Alcotest.(check bool)
    "repeated rows have distinct IDs"
    true
    (not (String.equal a.source_id b.source_id));
  Alcotest.(check bool)
    "cards have distinct accounts"
    true
    (not (String.equal a.account c.account));
  let m = Csv_movement.decode a |> Or_error.ok_exn in
  Alcotest.(check string) "prepaid card" "prepaid-card" m.account.account_type;
  Alcotest.(check string) "USD" "USD" (Iso4217.to_alpha_code m.account.currency)
;;

let profile_roundtrip () =
  let encoded = Json.encode Csv_profile.codec profile in
  let decoded = Json.decode Csv_profile.codec encoded |> Or_error.ok_exn in
  let input = "Merchant,Date,USD\nTotal,2026-10-01,3.00\nTotal,,3.00\n" in
  let rows = Csv_profile.parse decoded input |> Or_error.ok_exn in
  Alcotest.(check int) "merchant named Total is retained" 1 (List.length rows);
  Alcotest.(check string) "account type survives" "prepaid-card" decoded.account_type
;;

let integration () =
  let registry = Builtin.registry_with_csv_profile profile in
  let csv =
    "Merchant,Date,USD\nShop,2026-10-01,12.50\nShop,2026-10-01,12.50\nTotal,,25.00\n"
  in
  Or_error.ok_exn
    (Finances.Store.with_open ~registry ":memory:" (fun db ->
       let import account content =
         Finances.Store.import_bank_file
           ~account_label:account
           db
           ~institution:"csv"
           ~path:"test.csv"
           ~contents:content
         |> Or_error.ok_exn
       in
       let first = import "card-a" csv in
       Alcotest.(check int) "two occurrences imported" 2 first.inserted;
       let again = import "card-a" csv in
       Alcotest.(check int) "reimport inserts none" 0 again.inserted;
       let changed =
         import "card-a" "Merchant,Date,USD\nShop,2026-10-01,12.50\nTotal,,12.50\n"
       in
       Alcotest.(check bool)
         "partial day rejected"
         true
         (Poly.equal changed.outcome Rejected);
       let other =
         import "card-b" "Merchant,Date,USD\nShop,2026-10-01,12.50\nTotal,,12.50\n"
       in
       Alcotest.(check int) "second card distinct" 1 other.inserted;
       Ok ()))
;;

let provider_identity () =
  let rows =
    Csv_profile.parse profile "Merchant,Date,USD\nShop,2026-10-01,12.50\n"
    |> Or_error.ok_exn
  in
  let one =
    Csv_movement.events ~institution:"card-provider" ~account_id:"main" rows
    |> Or_error.ok_exn
    |> List.hd_exn
  in
  let two =
    Csv_movement.events ~institution:"other-provider" ~account_id:"main" rows
    |> Or_error.ok_exn
    |> List.hd_exn
  in
  Alcotest.(check bool)
    "provider scopes account"
    true
    (not (String.equal one.account two.account));
  let decoded = Registry.movement Builtin.registry one |> Or_error.ok_exn in
  Alcotest.(check string) "bank is provider" "card-provider" decoded.account.bank_id;
  Alcotest.(check string) "event venue is provider" "card-provider" one.venue
;;

let account_names () =
  let row =
    Csv_profile.parse profile "Merchant,Date,USD\nShop,2026-10-01,12.50\n"
    |> Or_error.ok_exn
    |> List.hd_exn
  in
  let source =
    Csv_movement.events ~institution:"examplecard" ~account_id:"old" [ row ]
    |> Or_error.ok_exn
    |> List.hd_exn
  in
  let movement = Registry.movement Builtin.registry source |> Or_error.ok_exn in
  let ledger =
    Finances.Bank_projection.ledger_account ~institution:"examplecard" movement.account
    |> Or_error.ok_exn
  in
  Alcotest.(check string) "prepaid card asset" "assets:cards:examplecard:old" ledger;
  let credit = { movement.account with account_type = "credit-card" } in
  Alcotest.(check string)
    "credit card liability"
    "liabilities:cards:examplecard:old"
    (Finances.Bank_projection.ledger_account ~institution:"examplecard" credit
     |> Or_error.ok_exn);
  let check name =
    Finances.Bank_account_mapping.create
      ~institution:"examplecard"
      ~bank_id:"examplecard"
      ~account_id:"old"
      ~account_type:"credit-card"
      ~ledger_account:name
  in
  Alcotest.(check bool)
    "custom liability accepted"
    true
    (Result.is_ok (check "liabilities:cards:custom"));
  Alcotest.(check bool)
    "expense mapping rejected"
    true
    (Result.is_error (check "expenses:shopping"))
;;

let () =
  Alcotest.run
    "CSV profile"
    [ ( "rows"
      , [ Alcotest.test_case "decode" `Quick rows
        ; Alcotest.test_case "identities" `Quick identities
        ; Alcotest.test_case "import and overlap" `Quick integration
        ; Alcotest.test_case "profile roundtrip" `Quick profile_roundtrip
        ; Alcotest.test_case "provider identity" `Quick provider_identity
        ; Alcotest.test_case "account names" `Quick account_names
        ] )
    ]
;;
