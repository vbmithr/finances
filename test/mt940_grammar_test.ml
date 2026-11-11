open Base

let ok = Or_error.ok_exn
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)
let replace_first s ~pattern ~with_ = String.substr_replace_first s ~pattern ~with_

let standard =
  {|:20:REFERENCE
:25:DE89370400440532013000 EUR
:28C:12/1
:60F:C260101EUR1000,50
:61:2601020102C500,25NTRFNONREF//BANKREF
:86:Transfer from Alice
:61:2601030104D25,00NCHKNONREF
:86:Card payment
:62F:C260131EUR1475,75
-
|}
;;

let statement () =
  let statements = ok (Mt940_grammar.parse standard) in
  Alcotest.(check int) "one statement" 1 (List.length statements);
  let statement = List.hd_exn statements in
  Alcotest.(check string) "reference" "REFERENCE" statement.reference;
  Alcotest.(check string) "account" "DE89370400440532013000 EUR" statement.account;
  Alcotest.(check string) "statement number" "12/1" statement.number;
  Alcotest.(check int) "two entries" 2 (List.length statement.entries);
  Alcotest.(check (list string))
    "memos"
    [ "Transfer from Alice"; "Card payment" ]
    (List.filter_map statement.entries ~f:(fun entry -> entry.memo))
;;

let balances () =
  let statement = List.hd_exn (ok (Mt940_grammar.parse standard)) in
  let opening = Option.value_exn statement.opening in
  Alcotest.(check string) "opening code" "60F" opening.tag;
  Alcotest.(check string) "opening date" "2026-01-01" (Date.to_string opening.date);
  Alcotest.(check string)
    "opening currency"
    "EUR"
    (Iso4217.to_alpha_code opening.currency);
  Alcotest.(check string) "opening amount" "1000.5" (Quantity.to_string opening.amount);
  let closing = Option.value_exn statement.closing in
  Alcotest.(check string) "closing code" "62F" closing.tag;
  Alcotest.(check string) "closing amount" "1475.75" (Quantity.to_string closing.amount)
;;

let entries () =
  let statement = List.hd_exn (ok (Mt940_grammar.parse standard)) in
  let credit = List.nth_exn statement.entries 0 in
  Alcotest.(check string) "value date" "2026-01-02" (Date.to_string credit.value_on);
  Alcotest.(check string)
    "entry date"
    "2026-01-02"
    (match credit.entry_on with
     | Some (m, d) -> Printf.sprintf "2026-%02d-%02d" m d
     | None -> "");
  Alcotest.(check string) "amount" "500.25" (Quantity.to_string credit.amount);
  Alcotest.(check (option char)) "funds code" None credit.funds_code;
  Alcotest.(check string) "type" "NTRF" credit.transaction_type;
  Alcotest.(check string) "reference" "NONREF" credit.reference;
  Alcotest.(check (option string)) "bank reference" (Some "BANKREF") credit.bank_reference;
  Alcotest.(check (option string)) "no supplementary" None credit.supplementary;
  let debit = List.nth_exn statement.entries 1 in
  Alcotest.(check string)
    "debit"
    "-25"
    (Quantity.to_string (Mt940_grammar.signed debit.direction debit.amount))
;;

let multi_line_memo () =
  let contents =
    replace_first
      standard
      ~pattern:":86:Card payment"
      ~with_:":86:Card payment\nSecond line\nThird line"
  in
  let statement = List.hd_exn (ok (Mt940_grammar.parse contents)) in
  Alcotest.(check (list string))
    "memos"
    [ "Transfer from Alice"; "Card payment\nSecond line\nThird line" ]
    (List.filter_map statement.entries ~f:(fun entry -> entry.memo))
;;

let multiple_statements () =
  let contents =
    "{1:F01BANKDEFFAXXX0000000000}\n"
    ^ String.concat ~sep:"\n" [ standard; standard ]
    ^ "\n"
  in
  Alcotest.(check int)
    "two statements"
    2
    (List.length (ok (Mt940_grammar.parse contents)))
;;

let memos_do_not_swallow_fields () =
  let contents =
    replace_first
      standard
      ~pattern:":86:Card payment"
      ~with_:":86:Card payment\n:99:NOTAFIELD"
  in
  let statement = List.hd_exn (ok (Mt940_grammar.parse contents)) in
  Alcotest.(check (list string))
    "memo"
    [ "Transfer from Alice"; "Card payment" ]
    (List.filter_map statement.entries ~f:(fun entry -> entry.memo));
  Alcotest.(check bool)
    "field kept"
    true
    (List.exists statement.fields ~f:(fun field -> String.equal field.tag "99"))
;;

(* A malformed file must fail, not spin: the parser never fails after consuming
   inside a repetition. *)
let rejections () =
  let replace pattern with_ = replace_first standard ~pattern ~with_ in
  error (Mt940_grammar.parse "");
  error (Mt940_grammar.parse (replace "\n-\n" "\n"));
  error (Mt940_grammar.parse (standard ^ "trailing"));
  error (Mt940_grammar.parse (replace ":28C:12/1" ""));
  error (Mt940_grammar.parse (replace "EUR1000,50" "XYZ1000,50"));
  error (Mt940_grammar.parse (replace "260101EUR" "260231EUR"));
  ignore
    (ok (Mt940_grammar.parse (replace ":86:Card payment" ":86:Card payment\n:86:Orphan"))
     : Mt940_grammar.t)
;;

let generic_fields () =
  let contents =
    standard
    |> fun s ->
    replace_first
      s
      ~pattern:"2601020102C500,25NTRFNONREF//BANKREF"
      ~with_:"260102RDK500,25NTRFOWNER/REF//BANKREF\nExtra detail"
    |> fun s ->
    replace_first s ~pattern:":86:Transfer from Alice\n" ~with_:""
    |> fun s -> replace_first s ~pattern:":62F:" ~with_:":99:Unknown value\n:62F:"
  in
  let statement = List.hd_exn (ok (Mt940_grammar.parse contents)) in
  let entry = List.hd_exn statement.entries in
  Alcotest.(check (option (pair int int))) "optional booking date" None entry.entry_on;
  Alcotest.(check (option string)) "optional memo" None entry.memo;
  Alcotest.(check (option char)) "funds code" (Some 'K') entry.funds_code;
  Alcotest.(check string)
    "reversal debit is credit"
    "500.25"
    (Quantity.to_string (Mt940_grammar.signed entry.direction entry.amount));
  Alcotest.(check string) "owner reference keeps slash" "OWNER/REF" entry.reference;
  Alcotest.(check (option string)) "bank reference" (Some "BANKREF") entry.bank_reference;
  Alcotest.(check (option string))
    "supplementary line"
    (Some "Extra detail")
    entry.supplementary;
  Alcotest.(check bool)
    "unknown field preserved"
    true
    (List.exists statement.fields ~f:(fun field ->
       String.equal field.tag "99" && String.equal field.text "Unknown value"));
  let crlf = String.substr_replace_all standard ~pattern:"\n" ~with_:"\r\n" in
  Alcotest.(check int) "CRLF accepted" 1 (List.length (ok (Mt940_grammar.parse crlf)))
;;

let () =
  Alcotest.run
    "MT940 grammar"
    [ ( "grammar"
      , List.map
          [ "statement fields", statement
          ; "balances", balances
          ; "entries", entries
          ; "multi-line memo", multi_line_memo
          ; "several statements", multiple_statements
          ; "memos stop at fields", memos_do_not_swallow_fields
          ; "rejections", rejections
          ; "generic optional fields", generic_fields
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
