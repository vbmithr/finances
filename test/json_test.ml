open Base
open Finances

let ok = Or_error.ok_exn
let rejected value = Alcotest.(check bool) "rejected" true (Result.is_error value)

let snapshot () =
  let reference =
    ok
      (Bank_account_config.reference
         ~institution:"bunq"
         ~bank_id:"bunq"
         ~account_id:"daily"
         ~account_type:"current-EUR")
  in
  let row : Bank_account_config.t =
    { reference
    ; ledger_account = Some "assets:bank:daily"
    ; iban = Keep
    ; settlement_account = Clear
    }
  in
  let encoded = Json.encode Bank_account_config.snapshot_jsont [ row ] in
  let decoded =
    Json.decode Bank_account_config.snapshot_jsont encoded |> ok |> List.hd_exn
  in
  Alcotest.(check bool)
    "absent is keep"
    true
    (match decoded.iban with
     | Keep -> true
     | _ -> false);
  Alcotest.(check bool)
    "null is clear"
    true
    (match decoded.settlement_account with
     | Clear -> true
     | _ -> false);
  let set =
    { row with
      iban = Set (ok (Iban.of_string "FR1420041010050500013M02606"))
    ; settlement_account = Set reference
    }
  in
  let decoded =
    Json.encode Bank_account_config.jsont set
    |> Json.decode Bank_account_config.jsont
    |> ok
  in
  Alcotest.(check bool)
    "IBAN retained"
    true
    (match decoded.iban with
     | Set iban -> Iban.equal iban (ok (Iban.of_string "FR1420041010050500013M02606"))
     | _ -> false);
  Alcotest.(check bool)
    "reference retained"
    true
    (match decoded.settlement_account with
     | Set r -> String.equal r.account_id reference.account_id
     | _ -> false);
  List.iter
    [ {|{"version":1,"version":1,"accounts":[]}|}
    ; {|{"version":1,"accounts":[],"extra":true}|}
    ; {|{"version":1.5,"accounts":[]}|}
    ; {|{"version":"1","accounts":[]}|}
    ; {|{"version":1,"accounts":[{"institution":"bunq","bank_id":"bunq","account_id":"a","account_id":"b","account_type":"current-EUR"}]}|}
    ]
    ~f:(fun text -> rejected (Bank_account_config.of_json text))
;;

let decisions () =
  let original =
    ok
      (Bank_category.create
         ~event_id:Int64.max_value
         ~account:"expenses:food"
         ~reason:"Reviewed")
  in
  let encoded = Json.encode Bank_category.jsont original in
  let decoded =
    Jsont_bytesrw.decode_string Bank_category.jsont encoded |> Result.ok_or_failwith
  in
  Alcotest.(check int64) "large ID exact" Int64.max_value decoded.event_id;
  List.iter [ "1.5"; "9007199254740993"; "null"; "true"; "-1" ] ~f:(fun id ->
    rejected
      (Bank_category.of_json
         ("[{\"event_id\":"
          ^ id
          ^ ",\"account\":\"expenses:food\",\"reason\":\"Reviewed\"}]")));
  rejected
    (Jsont_bytesrw.decode_string
       Bank_category.jsont
       {|{"event_id":1,"account":"invalid","reason":"Reviewed"}|});
  let transfer =
    ok
      (Bank_transfer.create
         ~first_event_id:7L
         ~second_event_id:3L
         ~state:Confirmed
         ~reason:"Owned accounts")
  in
  let decoded =
    Json.encode Bank_transfer.decision_jsont transfer
    |> Json.decode Bank_transfer.decision_jsont
    |> ok
  in
  Alcotest.(check int64) "normalized first" 3L decoded.first_event_id;
  Alcotest.(check int64) "normalized second" 7L decoded.second_event_id
;;

let exact_scalars () =
  Alcotest.(check int)
    "large machine integer exact"
    Int.max_value
    (Json.encode Json.integer Int.max_value |> Json.decode Json.integer |> ok);
  let source = "123456789012345678901234567890.1234567890123456789" in
  let value = Quantity.of_string source in
  let decoded = Json.encode Json.quantity value |> Json.decode Json.quantity |> ok in
  Alcotest.(check string) "exact quantity" source (Quantity.to_string decoded);
  List.iter [ {|"NaN"|}; {|"Infinity"|}; "0.1" ] ~f:(fun text ->
    rejected (Json.decode Json.quantity text));
  rejected (Jsont_bytesrw.decode_string Json.date {|"2026-02-30"|});
  rejected (Jsont_bytesrw.decode_string Json.currency {|"INVALID"|});
  rejected (Json.decode (Jsont.list Jsont.string) {|["a",null]|})
;;

let () =
  Alcotest.run
    "Typed JSON codecs"
    [ ( "codec"
      , [ Alcotest.test_case "snapshot patch semantics and strict fields" `Quick snapshot
        ; Alcotest.test_case "review decisions and exact IDs" `Quick decisions
        ; Alcotest.test_case "exact validated scalars" `Quick exact_scalars
        ] )
    ]
;;
