open Base
open Finances_core
open Finances_institutions

let ok = Or_error.ok_exn
let bundled name = List.Assoc.find_exn Bundled_definitions.csv name ~equal:String.equal

let check_converter institution script fixture =
  let converter = Lua_csv_converter.create ~institution (bundled script) |> ok in
  let contents = Stdio.In_channel.read_all fixture in
  Alcotest.(check bool)
    "recognizes source"
    true
    (Lua_csv_converter.recognizes converter contents);
  let records = Lua_csv_converter.convert converter ~name:fixture contents |> ok in
  ignore (Records.validate_document records |> ok : Records.t);
  Alcotest.(check bool)
    "records emitted"
    true
    (String.is_substring (Records.to_string records) ~substring:"\"kind\":\"record\"");
  Alcotest.(check bool)
    "rejects wrong header"
    false
    (Lua_csv_converter.recognizes converter "wrong,header\n")
;;

let invalid_rows () =
  let converter =
    Lua_csv_converter.create ~institution:"revolut" (bundled "lua/revolut.lua") |> ok
  in
  let header =
    "Type,Product,Started Date,Completed \
     Date,Description,Amount,Fee,Currency,State,Balance\n"
  in
  let row =
    "Card,Current,2026-01-01 09:00:00,2026-01-01 08:00:00,Shop,1,0,EUR,COMPLETED,1\n"
  in
  Alcotest.(check bool)
    "completion before start"
    true
    (Result.is_error (Lua_csv_converter.convert converter ~name:"bad.csv" (header ^ row)))
;;

let () =
  Alcotest.run
    "Lua CSV"
    [ ( "converter"
      , [ Alcotest.test_case "Wise" `Quick (fun () ->
            check_converter "wise" "lua/wise.lua" "fixtures/wise.csv")
        ; Alcotest.test_case "Bunq" `Quick (fun () ->
            check_converter "bunq" "lua/bunq.lua" "fixtures/bunq.csv")
        ; Alcotest.test_case "BoursoBank" `Quick (fun () ->
            check_converter "boursobank" "lua/boursobank.lua" "fixtures/bank.csv")
        ; Alcotest.test_case "Revolut" `Quick (fun () ->
            check_converter "revolut" "lua/revolut.lua" "fixtures/revolut.csv")
        ; Alcotest.test_case "invalid rows" `Quick invalid_rows
        ] )
    ]
;;
