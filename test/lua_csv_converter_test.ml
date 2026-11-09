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
        ; Alcotest.test_case "another institution" `Quick other_institution
        ] )
    ]
;;
