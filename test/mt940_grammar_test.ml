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

(* Synthetic values, following the field shapes in the published Citi, RBS,
   and RBC export guides linked from docs/mt940-grammar.md. *)
let bank_export_shapes () =
  let citi =
    "{1:F01EXAMPLE}{2:O940EXAMPLE}{4:\n"
    ^ ":20:EXAMPLE\n:25:123456789\n:28C:42/1\n:60F:C260101DKK1000,00\n"
    ^ ":61:2601020102DK12,34NTRFNONREF//BANK123\n/BAI/195/DETAIL\n"
    ^ ":86:/NAME/Example\n/REMI/Payment\n:62F:C260131DKK987,66\n"
    ^ ":64:C260131DKK987,66\n:65:C260201DKK987,66\n-\n}\n"
  in
  let citi_statement = List.hd_exn (ok (Mt940_grammar.parse citi)) in
  let citi_entry = List.hd_exn citi_statement.entries in
  Alcotest.(check (option char)) "Citi funds code" (Some 'K') citi_entry.funds_code;
  Alcotest.(check (option string))
    "Citi bank reference"
    (Some "BANK123")
    citi_entry.bank_reference;
  Alcotest.(check (option string))
    "Citi supplementary text"
    (Some "/BAI/195/DETAIL")
    citi_entry.supplementary;
  Alcotest.(check bool)
    "Citi available balance retained"
    true
    (List.exists citi_statement.fields ~f:(fun field -> String.equal field.tag "65"));
  let citi_compact = String.substr_replace_first citi ~pattern:"-\n}\n" ~with_:"-}\n" in
  Alcotest.(check int)
    "compact SWIFT trailer"
    1
    (List.length (ok (Mt940_grammar.parse citi_compact)));
  Alcotest.(check int)
    "concatenated SWIFT blocks"
    2
    (List.length (ok (Mt940_grammar.parse (citi ^ citi_compact))));
  error
    (Mt940_grammar.parse
       (String.substr_replace_first citi ~pattern:"-\n}\n" ~with_:"-\n"));
  error (Mt940_grammar.parse (standard ^ "}\n"));
  let rbs =
    ":20:REFERENCE 01\n:25:01234567\n:28C:2/1\n:60F:C260101GBP50,00\n"
    ^ ":61:2601020102C10,25NMSCPAYMENT REF//BANK987\n:86:Transfer\nFrom customer\n"
    ^ ":62F:C260131GBP60,25\n:64:C260131GBP60,25\n"
  in
  error (Mt940_grammar.parse rbs);
  let rbs_statement =
    List.hd_exn (ok (Mt940_grammar.parse ~allow_unterminated:true rbs))
  in
  let rbs_entry = List.hd_exn rbs_statement.entries in
  Alcotest.(check string) "RBS reference" "PAYMENT REF" rbs_entry.reference;
  Alcotest.(check (option string))
    "RBS multiline memo"
    (Some "Transfer\nFrom customer")
    rbs_entry.memo;
  error
    (Mt940_grammar.parse
       ~allow_unterminated:true
       (String.substr_replace_first rbs ~pattern:":62F:C260131GBP60,25\n" ~with_:""));
  let rbc =
    ":20:RBC-EXAMPLE\n:25:123456\n:28C:1/1\n:60F:C260101EUR100,00\n"
    ^ ":61:260102D5,25NMSCREF1//BANK1\n"
    ^ ":61:260103D10,00S100REF2//BANK2\n:86:Payment details\n"
    ^ ":62F:C260131EUR84,75\n-\n"
  in
  let rbc_statement = List.hd_exn (ok (Mt940_grammar.parse rbc)) in
  let first = List.hd_exn rbc_statement.entries in
  Alcotest.(check (option (pair int int))) "RBC optional entry date" None first.entry_on;
  Alcotest.(check (option string)) "RBC optional memo" None first.memo;
  Alcotest.(check int) "RBC entries" 2 (List.length rbc_statement.entries)
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
          ; "published bank export shapes", bank_export_shapes
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
