open Base

(* The examples are the format's contract: valid/ is accepted, and invalid/ and
   semantic/ are refused. invalid/ is what the schema refuses, so fin and the schema
   must agree on every file there. *)
let root = "../formats/records/examples"

let examples kind =
  let dir = root ^ "/" ^ kind in
  Stdlib.Sys.readdir dir
  |> Array.to_list
  |> List.filter ~f:(String.is_suffix ~suffix:".jsonl")
  |> List.sort ~compare:String.compare
  |> List.map ~f:(fun name -> name, Stdio.In_channel.read_all (dir ^ "/" ^ name))
;;

let accepted () =
  let found = examples "valid" in
  Alcotest.(check bool) "examples found" true (not (List.is_empty found));
  List.iter found ~f:(fun (name, text) ->
    match Records.of_string text with
    | Ok _ -> ()
    | Error error -> Alcotest.failf "%s: %s" name (Error.to_string_hum error))
;;

let refused kind () =
  let found = examples kind in
  Alcotest.(check bool) "examples found" true (not (List.is_empty found));
  List.iter found ~f:(fun (name, text) ->
    Alcotest.(check bool) name true (Result.is_error (Records.of_string text)))
;;

(* What each semantic example is refused for, so that one cannot pass by failing
   on something else. *)
let reasons () =
  List.iter
    [ "balance-unknown-account", "unknown account"
    ; "balance-does-not-follow", "states a balance of 123.45 after 96.5"
    ; "balance-with-two-movements", "a balance after the row needs exactly one movement"
    ; "duplicate-id", "repeated id"
    ; "end-miscounts", "the end line counts 99 records, found 2"
    ; "fee-with-two-movements", "a fee needs exactly one movement"
    ; "header-before-end", "a header before the previous section's end line"
    ; "impossible-date", "occurred"
    ; "invalid-currency-code", "ISO 4217"
    ; "no-header", "line 1: expected a header"
    ; "no-source", "names no source"
    ; "observation-without-movement", "an observation needs exactly one movement"
    ; "repeated-role", "repeated role"
    ; "truncated", "no end line"
    ; "unknown-account", "unknown account"
    ; "unknown-statement", "unknown statement"
    ]
    ~f:(fun (name, reason) ->
      let text = Stdio.In_channel.read_all (root ^ "/semantic/" ^ name ^ ".jsonl") in
      match Records.of_string text with
      | Ok _ -> Alcotest.failf "%s was accepted" name
      | Error error ->
        let message = Error.to_string_hum error in
        if not (String.is_substring message ~substring:reason)
        then Alcotest.failf "%s: expected %S in %s" name reason message);
  let names = examples "semantic" |> List.length in
  Alcotest.(check int) "every semantic example has a reason" 16 names
;;

let balance_after () =
  let t =
    Records.of_string (Stdio.In_channel.read_all (root ^ "/valid/running-balance.jsonl"))
    |> Or_error.ok_exn
    |> List.hd_exn
  in
  let balances =
    List.map t.records ~f:(fun r ->
      Option.map r.details.balance_after ~f:Quantity.to_string)
  in
  Alcotest.(check (list (option string)))
    "stated"
    [ Some "96.5"; Some "-1"; None ]
    balances
;;

let print documents = List.map documents ~f:Records.to_string |> String.concat

let round_trip () =
  List.iter (examples "valid") ~f:(fun (name, text) ->
    let once = Records.of_string text |> Or_error.ok_exn |> print in
    let twice = Records.of_string once |> Or_error.ok_exn |> print in
    Alcotest.(check string) name once twice)
;;

let read name =
  List.Assoc.find_exn (examples "valid") ~equal:String.equal (name ^ ".jsonl")
;;

(* A quantity must reach the model digit for digit, whatever its size. *)
let quantities_are_exact () =
  let large = "-12345678901234567890123456789012345678.123456789012345678" in
  let group =
    Re.exec (Re.compile (Re.Perl.re {|"quantity":"[^"]*"|})) (read "wise-conversion")
  in
  let text = read "wise-conversion" in
  let text =
    String.prefix text (Re.Group.start group 0)
    ^ Printf.sprintf {|"quantity":"%s"|} large
    ^ String.drop_prefix text (Re.Group.stop group 0)
  in
  let encoded = Records.of_string text |> Or_error.ok_exn |> print in
  Alcotest.(check bool) "digits kept" true (String.is_substring encoded ~substring:large)
;;

let errors_name_the_place () =
  let message file =
    let text = Stdio.In_channel.read_all (root ^ "/" ^ file ^ ".jsonl") in
    Records.of_string ~file:"f.jsonl" text
    |> Result.error
    |> Option.value_exn
    |> Error.to_string_hum
  in
  Alcotest.(check string)
    "line and path"
    {|f.jsonl: line 4: occurred: invalid date or instant: "2025-02-31"|}
    (message "semantic/impossible-date");
  (* Several problems are reported together, one per line. *)
  let invalid =
    Records.of_string
      ({|{"kind":"header","format":"fin-records","version":1,"institution":"x","identity":"source"}
{"kind":"source","name":"s","sha256":"|}
       ^ String.make 64 '0'
       ^ {|"}
{"kind":"account","id":"a"}
{"kind":"account","id":"a"}
{"kind":"record","id":"1","account":"b","occurred":"2025-01-02","movements":[]}
{"kind":"end","records":1}
|}
      )
  in
  Alcotest.(check string)
    "all problems"
    {|account "a" is listed twice
record "1" of account "b": unknown account|}
    (Result.error invalid |> Option.value_exn |> Error.to_string_hum);
  Alcotest.(check bool) "syntax" true (Result.is_error (Records.of_string "{"));
  Alcotest.(check bool) "empty" true (Result.is_error (Records.of_string ""))
;;

let merging () =
  let document name = Records.of_string (read name) |> Or_error.ok_exn |> List.hd_exn in
  let wise = document "wise-conversion" in
  let twice = Records.merge [ wise; wise ] |> Or_error.ok_exn in
  Alcotest.(check string)
    "an overlap is kept once"
    (Records.to_string wise)
    (Records.to_string twice);
  let changed =
    { wise with
      records = List.map wise.records ~f:(fun r -> { r with group = Some "another" })
    }
  in
  Alcotest.(check bool)
    "a disagreement is refused"
    true
    (Result.is_error (Records.merge [ wise; changed ]));
  Alcotest.(check bool)
    "another institution is refused"
    true
    (Result.is_error (Records.merge [ wise; { wise with institution = "other" } ]));
  Alcotest.(check bool) "nothing to merge" true (Result.is_error (Records.merge []));
  (* One file knows an account's IBAN and another only its number. *)
  let account_of id : Records.account =
    { id
    ; bank = None
    ; number = None
    ; account_type = None
    ; currency = None
    ; iban = Unstated
    }
  in
  let accounts_of accounts =
    { wise with accounts; records = []; statements = []; balances = [] }
  in
  let filled =
    Records.merge
      [ accounts_of [ account_of "a" ]
      ; accounts_of [ { (account_of "a") with number = Some "1"; iban = Iban "X" } ]
      ]
    |> Or_error.ok_exn
  in
  Alcotest.(check int) "one account" 1 (List.length filled.accounts);
  Alcotest.(check (option string))
    "filled in"
    (Some "1")
    (List.hd_exn filled.accounts).number;
  Alcotest.(check bool)
    "an account's number can disagree"
    true
    (Result.is_error
       (Records.merge
          [ accounts_of [ { (account_of "a") with number = Some "1" } ]
          ; accounts_of [ { (account_of "a") with number = Some "2" } ]
          ]));
  (* Each export names its statement differently, and a row can be in both. *)
  let renamed (t : Records.t) =
    { t with
      statements =
        List.map t.statements ~f:(fun (s : Records.statement) ->
          { s with id = "other-" ^ s.id })
    ; records =
        List.map t.records ~f:(fun (r : Records.record) ->
          { r with statements = List.map r.statements ~f:(fun id -> "other-" ^ id) })
    }
  in
  let both = Records.merge [ wise; renamed wise ] |> Or_error.ok_exn in
  Alcotest.(check int)
    "a row in two statements is one record"
    (List.length wise.records)
    (List.length both.records);
  Alcotest.(check bool)
    "listed by both"
    true
    (List.for_all both.records ~f:(fun (r : Records.record) ->
       List.is_empty r.statements || List.length r.statements = 2));
  Alcotest.(check int)
    "balances stated by both are kept once"
    (List.length wise.balances)
    (List.length both.balances);
  Alcotest.(check int)
    "both statements are kept"
    (2 * List.length wise.statements)
    (List.length both.statements);
  Records.check both |> Or_error.ok_exn;
  (* A row one file states twice is refused, not merged away. *)
  let repeated = { wise with records = wise.records @ wise.records } in
  Alcotest.(check bool)
    "a repeat within one document is refused"
    true
    (Result.is_error (Records.merge_all [ repeated ]));
  Alcotest.(check int)
    "the same row in two documents is one"
    (List.length wise.records)
    (List.length
       (List.hd_exn (Records.merge_all [ wise; wise ] |> Or_error.ok_exn)).records);
  (* Files can be concatenated, and fin compares them as it does when merging. *)
  let text = read "wise-conversion" in
  Alcotest.(check int)
    "cat of one institution is one document"
    1
    (List.length (Records.of_string (text ^ text) |> Or_error.ok_exn));
  Alcotest.(check bool)
    "cat of a disagreement is refused"
    true
    (Result.is_error (Records.of_string (text ^ Records.to_string changed)));
  Alcotest.(check int)
    "cat of two institutions is two documents"
    2
    (List.length (Records.of_string (read "two-institutions") |> Or_error.ok_exn))
;;

let compression () =
  let text = read "wise-conversion" in
  let compressed = Records.compress text |> Or_error.ok_exn in
  Alcotest.(check string)
    "reads the same"
    (Records.of_string text |> Or_error.ok_exn |> print)
    (Records.of_string compressed |> Or_error.ok_exn |> print);
  let cut = String.prefix compressed (String.length compressed - 8) in
  Alcotest.(check bool)
    "a cut frame is refused"
    true
    (Result.is_error (Records.of_string cut));
  let corrupt =
    String.mapi compressed ~f:(fun i c ->
      if i = String.length compressed / 2
      then Char.of_int_exn (Char.to_int c lxor 0xff)
      else c)
  in
  Alcotest.(check bool)
    "a corrupt frame is refused"
    true
    (Result.is_error (Records.of_string corrupt))
;;

let () =
  Alcotest.run
    "records"
    [ ( "examples"
      , [ Alcotest.test_case "valid are accepted" `Quick accepted
        ; Alcotest.test_case "invalid are refused" `Quick (refused "invalid")
        ; Alcotest.test_case "semantic are refused" `Quick (refused "semantic")
        ; Alcotest.test_case "semantic reasons" `Quick reasons
        ; Alcotest.test_case "encoding round trips" `Quick round_trip
        ; Alcotest.test_case "quantities are exact" `Quick quantities_are_exact
        ; Alcotest.test_case "errors name the place" `Quick errors_name_the_place
        ; Alcotest.test_case "merging documents" `Quick merging
        ; Alcotest.test_case "running balance" `Quick balance_after
        ; Alcotest.test_case "compression" `Quick compression
        ] )
    ]
;;
