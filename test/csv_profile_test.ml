open Base
open Finances_institutions

let profile : Csv_profile.t =
  { separator = ','
  ; header = [ "Merchant"; "Date"; "USD" ]
  ; date_column = "Date"
  ; date_format = Csv_profile.Iso
  ; amount_format = Csv_profile.Dot_decimal
  ; description_column = "Merchant"
  ; amount_column = "USD"
  ; currency = Iso4217.of_alpha_code "USD" |> Option.value_exn
  ; account_type = Csv_profile.Prepaid_card
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
  Alcotest.(check string)
    "account type survives"
    "prepaid-card"
    (Csv_profile.account_type_name decoded.account_type)
;;

let localized_formats () =
  let profile =
    { profile with
      date_format = Csv_profile.Month_first
    ; amount_format = Csv_profile.Comma_decimal
    ; invert_amount = false
    ; skip_when = None
    }
  in
  let profile =
    Json.decode Csv_profile.codec (Json.encode Csv_profile.codec profile)
    |> Or_error.ok_exn
  in
  let rows =
    Csv_profile.parse profile "Merchant,Date,USD\nShop,04/03/2026,\"1 234,50\"\n"
    |> Or_error.ok_exn
  in
  let row = List.hd_exn rows in
  Alcotest.(check string) "month-first date" "2026-04-03" (Date.to_string row.date);
  Alcotest.(check string) "exact amount" "1234.5" (Quantity.to_string row.amount);
  Alcotest.(check bool)
    "invalid dates rejected"
    true
    (Result.is_error (Csv_profile.parse profile "Merchant,Date,USD\nShop,02/31/2026,5\n"));
  let legacy =
    {|{"separator":",","header":["Merchant","Date","USD"],"date":"Date","description":"Merchant","amount":"USD","currency":"USD","account_type":"prepaid-card","invert_amount":true,"skip_column":null,"skip_value":null}|}
  in
  let legacy = Json.decode Csv_profile.codec legacy |> Or_error.ok_exn in
  Alcotest.(check bool)
    "legacy profile defaults"
    true
    (Poly.equal legacy.date_format Csv_profile.Iso
     && Poly.equal legacy.amount_format Csv_profile.Dot_decimal)
;;

let datetime_format () =
  let profile = { profile with date_format = Csv_profile.Iso_datetime } in
  let profile =
    Json.decode Csv_profile.codec (Json.encode Csv_profile.codec profile)
    |> Or_error.ok_exn
  in
  let row =
    Csv_profile.parse profile "Merchant,Date,USD\nShop,2025-06-09 17:56:16,4.3\n"
    |> Or_error.ok_exn
    |> List.hd_exn
  in
  Alcotest.(check string) "booking date" "2025-06-09" (Date.to_string row.date);
  Alcotest.(check (list string))
    "original timestamp"
    [ "Shop"; "2025-06-09 17:56:16"; "4.3" ]
    row.fields;
  Alcotest.(check bool)
    "invalid clock rejected"
    true
    (Result.is_error
       (Csv_profile.parse profile "Merchant,Date,USD\nShop,2025-06-09 25:00:00,4.3\n"))
;;

let bank_account () =
  let profile =
    { profile with account_type = Csv_profile.Savings; invert_amount = false }
  in
  let encoded = Json.encode Csv_profile.codec profile in
  let profile = Json.decode Csv_profile.codec encoded |> Or_error.ok_exn in
  let rows =
    Csv_profile.parse profile "Merchant,Date,USD\nDeposit,2026-10-01,12.50\n"
    |> Or_error.ok_exn
  in
  let source =
    Csv_movement.events ~institution:"mybank" ~account_id:"reserve" rows
    |> Or_error.ok_exn
    |> List.hd_exn
  in
  let movement = Csv_movement.decode source |> Or_error.ok_exn in
  Alcotest.(check string) "savings" "savings" movement.account.account_type;
  Alcotest.(check bool)
    "IBAN may be applicable"
    true
    (Poly.equal movement.own_iban Movement.Unknown);
  Alcotest.(check string)
    "bank ledger"
    "assets:bank:mybank:savings:reserve"
    (Finances.Bank_projection.ledger_account ~institution:"mybank" movement.account
     |> Or_error.ok_exn)
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
        ; Alcotest.test_case "bank account" `Quick bank_account
        ; Alcotest.test_case "localized formats" `Quick localized_formats
        ; Alcotest.test_case "date and time" `Quick datetime_format
        ; Alcotest.test_case "profile roundtrip" `Quick profile_roundtrip
        ; Alcotest.test_case "provider identity" `Quick provider_identity
        ; Alcotest.test_case "account names" `Quick account_names
        ] )
    ]
;;
