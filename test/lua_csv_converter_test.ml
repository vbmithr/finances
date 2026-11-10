open Base
open Finances_core
open Finances_institutions

let ok = Or_error.ok_exn
let bundled name = List.Assoc.find_exn Bundled_definitions.csv name ~equal:String.equal

let header =
  "Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native \
   Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n"
;;

let compare_converters () =
  let json =
    Csv_converter.of_string (Stdio.In_channel.read_all "fixtures/cdc-converter.json")
    |> ok
  in
  let lua = Lua_csv_converter.create ~institution:"cdc" (bundled "lua/cdc.lua") |> ok in
  let kinds =
    [ "referral_card_cashback"
    ; "mco_stake_reward"
    ; "gift_card_reward"
    ; "referral_bonus"
    ; "referral_gift"
    ; "crypto_deposit"
    ; "exchange_to_crypto_transfer"
    ; "reimbursement"
    ; "supercharger_withdrawal"
    ; "lockup_unlock"
    ; "card_top_up"
    ; "card_cashback_reverted"
    ; "supercharger_deposit"
    ; "lockup_lock"
    ; "crypto_viban_exchange"
    ; "crypto_exchange"
    ; "trading.crypto_conversion.cash"
    ; "viban_purchase"
    ; "unknown_kind"
    ]
  in
  let rows =
    List.concat_map kinds ~f:(fun kind ->
      List.concat_map [ "1.1234567890123456789"; "-2"; "0" ] ~f:(fun amount ->
        List.map
          [ "", ""; "EUR", "0.2"; "EUR", "-1" ]
          ~f:(fun (dest, value) ->
            Printf.sprintf
              "2025-06-09 17:56:16,%s-%s-%s,CRO,%s,%s,%s,EUR,0.10,0.12,%s,\n"
              kind
              amount
              value
              amount
              dest
              value
              kind)))
  in
  let contents = header ^ String.concat rows in
  let old = Csv_converter.convert json ~name:"sample.csv" contents |> ok in
  let new_ = Lua_csv_converter.convert lua ~name:"sample.csv" contents |> ok in
  Alcotest.(check string)
    "identical records"
    (Records.to_string old)
    (Records.to_string new_);
  Alcotest.(check bool)
    "recognizes header"
    true
    (Lua_csv_converter.recognizes lua contents);
  Alcotest.(check bool)
    "rejects wrong header"
    false
    (Lua_csv_converter.recognizes lua "wrong,header\n");
  let invalid = header ^ "bad-date,Oops,CRO,1,,,EUR,1,1,crypto_deposit,\n" in
  Alcotest.(check bool)
    "rejects bad date"
    true
    (Result.is_error (Lua_csv_converter.convert lua ~name:"sample.csv" invalid));
  let duplicate_header =
    String.substr_replace_first
      (bundled "lua/cdc.lua")
      ~pattern:"\"Transaction Hash\""
      ~with_:"\"Transaction Kind\""
  in
  Alcotest.(check bool)
    "rejects duplicate headers"
    true
    (Result.is_error (Lua_csv_converter.create ~institution:"cdc" duplicate_header));
  let mixed =
    "version = 1\ninstitution = \"cdc\"\ntype = \"crypto-exchange\"\n"
    ^ "[[statements]]\nformat = \"csv\"\ndefinition = \"old.json\"\n"
    ^ "[[statements]]\nformat = \"csv\"\nscript = \"new.lua\"\n"
  in
  let load = function
    | "old.json" -> Ok (Stdio.In_channel.read_all "fixtures/cdc-converter.json")
    | "new.lua" -> Ok (bundled "lua/cdc.lua")
    | _ -> Or_error.error_string "unexpected path"
  in
  Alcotest.(check bool)
    "rejects mixing JSON and Lua layouts"
    true
    (Result.is_error (Institution_toml.decode mixed ~load));
  Alcotest.(check bool)
    "rejects institution mismatch"
    true
    (Result.is_error
       (Lua_csv_converter.create ~institution:"other" (bundled "lua/cdc.lua")))
;;

let fdm_equivalence () =
  let old =
    Csv_converter.of_string (Stdio.In_channel.read_all "fixtures/fdm-converter.json")
    |> ok
  in
  let lua = Lua_csv_converter.create ~institution:"fdm" (bundled "lua/fdm.lua") |> ok in
  let invalid_link =
    String.substr_replace_first
      (bundled "lua/fdm.lua")
      ~pattern:"link_statements = yes"
      ~with_:"link_statements = \"bad\""
  in
  Alcotest.(check bool)
    "rejects invalid statement-link option"
    true
    (Result.is_error (Lua_csv_converter.create ~institution:"fdm" invalid_link));
  let header = "Merchant,Category,Date,Local currency,Local amount,Exchange rate,USD\n" in
  let valid name contents =
    let old = Csv_converter.convert old ~name contents |> ok in
    let actual = Lua_csv_converter.convert lua ~name contents |> ok in
    Alcotest.(check string) name (Records.to_string old) (Records.to_string actual)
  in
  valid
    "sample"
    (header
     ^ "Shop,Shopping,2025-01-02,EUR,10.00,1.1,11.00\n\
        Shop,Shopping,2025-01-03,EUR,-1.00,1.1,-1.10\n\
        Total,,,,,,9.90\n");
  valid
    "total first and repeated dates"
    (header
     ^ "Total,,,,,,20\n\
       \ Shop ,Shopping,2025-01-02,EUR,10,1,10\n\
       \ Shop ,Shopping,2025-01-02,EUR,10,1,10\n");
  valid
    "no total or currency"
    (header ^ "Shop,Shopping,2025-01-02,,,,0.000000000000000000001\n");
  let generated =
    List.concat_map [ "1.234567890123456789"; "-2.5"; "0" ] ~f:(fun amount ->
      List.concat_map [ ""; "EUR" ] ~f:(fun currency ->
        List.concat_map [ ""; "0"; "-1"; "1.23456789" ] ~f:(fun rate ->
          List.map [ ""; "5"; "-0.5" ] ~f:(fun local ->
            Printf.sprintf
              "Merchant,Category,2025-01-02,%s,%s,%s,%s\n"
              currency
              local
              rate
              amount))))
  in
  valid "decimal and exchange combinations" (header ^ String.concat generated);
  valid "header only" header;
  valid "total only" (header ^ "Total,,,,,,not-a-number\n");
  valid
    "total mismatch is kept as stated"
    (header ^ "Shop,Shopping,2025-01-02,EUR,10,1,10\nTotal,,,,,,11\n");
  let invalid =
    [ header ^ "Shop,Shopping,2025-01-02,EUR,10,1,bad\n"
    ; header ^ "Shop,Shopping,not-a-date,EUR,10,1,10\n"
    ; header ^ "Shop,Shopping,2025-01-02,EUR,10,bad,10\n"
    ; header ^ "Shop,Shopping,2025-01-02,EUR,10,1,10\nTotal,,,,,,10\nTotal,,,,,,10\n"
    ]
  in
  List.iteri invalid ~f:(fun i contents ->
    let old =
      Csv_converter.convert old ~name:"invalid.csv" contents
      |> Or_error.bind ~f:(fun d -> Records.of_string (Records.to_string d))
    in
    let actual = Lua_csv_converter.convert lua ~name:"invalid.csv" contents in
    Alcotest.(check bool)
      (Printf.sprintf "old rejects invalid %d" i)
      true
      (Result.is_error old);
    Alcotest.(check bool)
      (Printf.sprintf "Lua rejects invalid %d" i)
      true
      (Result.is_error actual))
;;

let other_institution () =
  let source = Stdio.In_channel.read_all "fixtures/lua-example-bank.lua" in
  let converter = Lua_csv_converter.create ~institution:"example-bank" source |> ok in
  let document =
    Lua_csv_converter.convert
      converter
      ~name:"bank.csv"
      "Date,Description,Amount\n2026-01-02,Coffee,-3.50\n"
    |> ok
  in
  Alcotest.(check string) "institution" "example-bank" document.institution;
  let row = List.hd_exn document.records in
  Alcotest.(check string)
    "amount"
    "-3.5"
    (Quantity.to_string (List.hd_exn row.movements).quantity)
;;

let () =
  Alcotest.run
    "Lua CSV"
    [ ( "converter"
      , [ Alcotest.test_case "CDC equivalence" `Quick compare_converters
        ; Alcotest.test_case "FDM equivalence" `Quick fdm_equivalence
        ; Alcotest.test_case "another institution" `Quick other_institution
        ] )
    ]
;;
