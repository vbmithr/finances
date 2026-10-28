open Base
open Finances_banks

let reader () =
  let header = [ "Solde"; "Solde"; "Description" ] in
  let input = "\239\187\191  Solde  ;Solde;Description\n1;2;\"one\ntwo\"\n" in
  Alcotest.(check bool)
    "trimmed header with BOM"
    true
    (Csv_reader.recognizes ~separator:';' ~trim:true ~header input);
  Alcotest.(check bool)
    "strict header rejects spaces"
    false
    (Csv_reader.recognizes ~separator:';' ~trim:false ~header input);
  let channel = Csv_reader.channel ~separator:';' input in
  Csv_reader.check_header ~trim:true ~header ~message:"bad header" channel;
  Alcotest.(check (list string))
    "duplicate columns and quoted newline"
    [ "1"; "2"; "one\ntwo" ]
    (Csv.next channel)
;;

let malformed () =
  Alcotest.(check bool)
    "unknown header not recognized"
    false
    (Csv_reader.recognizes
       ~separator:','
       ~trim:false
       ~header:[ "Date"; "Amount" ]
       "Date,Other\n2026-01-01,1\n");
  Alcotest.(check bool)
    "invalid UTF-8 rejected"
    true
    (Result.is_error
       (Or_error.try_with (fun () -> Csv_reader.check_utf8 ~name:"Test" "\255")))
;;

type sample_row =
  { row_number : int
  ; date : Date.t
  ; amount : Quantity.t
  ; balance : Quantity.t option
  ; currency : Iso4217.t
  ; description : string
  ; fields : string list
  }

let schema_decoding () =
  let header = [ "Date"; "Solde"; "Currency"; "Solde"; "Description" ] in
  let schema =
    let open Csv_schema in
    make (fun row_number date amount currency balance description fields ->
      { row_number; date; amount; balance; currency; description; fields })
    |> row_number
    |> col_name "Date" (Cell.iso_date ())
    |> col ~pos:1 ~name:"Solde" Cell.french_amount
    |> col_name "Currency" (Cell.currency ())
    |> col ~pos:3 ~name:"Solde" (Cell.opt Cell.decimal_balance)
    |> col_name "Description" (Cell.required Cell.string)
    |> raw_fields
    |> create
         ~name:"Sample"
         ~separator:';'
         ~header
         ~trim_header:true
         ~trim_cells:true
         ~skip_blank_rows:true
  in
  let input =
    "\239\187\191 Date ; Solde ; Currency ; Solde ; Description \n\
     2026-01-03 ; -1\194\160234,56 ; EUR ; 100.50 ; \"Coffee\n\
     shop\" \n\
     ; ; ; ; \n\
     2026-01-04 ; 10,00 ; USD ; ; Book \n"
  in
  Alcotest.(check bool)
    "recognizes valid header"
    true
    (Csv_schema.recognizes schema input);
  let rows = Or_error.ok_exn (Csv_schema.parse schema input) in
  Alcotest.(check int) "blank row skipped" 2 (List.length rows);
  let first = List.hd_exn rows in
  let second = List.nth_exn rows 1 in
  Alcotest.(check int) "first row number" 1 first.row_number;
  Alcotest.(check int) "row after skipped blank keeps number 3" 3 second.row_number;
  Alcotest.(check string) "date decoded" "2026-01-03" (Date.to_string first.date);
  Alcotest.(check bool)
    "French amount decoded"
    true
    (Quantity.equal first.amount (Quantity.of_string "-1234.56"));
  Alcotest.(check string) "currency decoded" "EUR" (Iso4217.to_alpha_code first.currency);
  Alcotest.(check bool)
    "optional balance present"
    true
    (Option.value_map
       first.balance
       ~default:false
       ~f:(Quantity.equal (Quantity.of_string "100.50")));
  Alcotest.(check bool) "optional balance absent" true (Option.is_none second.balance);
  Alcotest.(check string) "trimmed description" "Coffee\nshop" first.description;
  Alcotest.(check string)
    "raw field retains surrounding space"
    "2026-01-03 "
    (List.hd_exn first.fields)
;;

let schema_validation_and_diagnostics () =
  let header = [ "Solde"; "Solde"; "Amount" ] in
  Alcotest.(check bool)
    "duplicate column selected by name rejected at compile time"
    true
    (Result.is_error
       (Or_error.try_with (fun () ->
          let open Csv_schema in
          make Fn.id
          |> col_name "Solde" Cell.string
          |> create ~name:"Dup" ~separator:',' ~header)));
  Alcotest.(check bool)
    "mismatched expected column name at position rejected"
    true
    (Result.is_error
       (Or_error.try_with (fun () ->
          let open Csv_schema in
          make Fn.id
          |> col ~pos:0 ~name:"Other" Cell.string
          |> create ~name:"Mismatch" ~separator:',' ~header)));
  let schema =
    let open Csv_schema in
    make (fun d q -> d, q)
    |> col_name "Date" (Cell.dmy_date ())
    |> col_name "Amount" Cell.bunq_amount
    |> create
         ~name:"Diag"
         ~separator:','
         ~header:[ "Date"; "Amount" ]
         ~validate_doc:(fun rows ->
           if List.is_empty rows then failwith "document must not be empty")
  in
  let bad_cell = Csv_schema.parse schema "Date,Amount\n31-01-2026,bad\n" in
  Alcotest.(check bool) "bad cell rejected" true (Result.is_error bad_cell);
  let message = Error.to_string_hum (Result.error bad_cell |> Option.value_exn) in
  Alcotest.(check bool)
    "diagnostic identifies format"
    true
    (String.is_substring message ~substring:"Diag CSV");
  Alcotest.(check bool)
    "diagnostic identifies row"
    true
    (String.is_substring message ~substring:"CSV row 1");
  Alcotest.(check bool)
    "diagnostic identifies column"
    true
    (String.is_substring message ~substring:"column 2 (Amount)");
  let empty_doc = Csv_schema.parse schema "Date,Amount\n" in
  Alcotest.(check bool)
    "document validation callback runs"
    true
    (Result.is_error empty_doc)
;;

let () =
  Alcotest.run
    "Shared CSV reader"
    [ ( "transport"
      , [ Alcotest.test_case "headers and raw rows" `Quick reader
        ; Alcotest.test_case "malformed input" `Quick malformed
        ] )
    ; ( "schema"
      , [ Alcotest.test_case "typed cells and row construction" `Quick schema_decoding
        ; Alcotest.test_case
            "selectors and diagnostics"
            `Quick
            schema_validation_and_diagnostics
        ] )
    ]
;;
