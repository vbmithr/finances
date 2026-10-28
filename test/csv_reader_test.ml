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

let () =
  Alcotest.run
    "Shared CSV reader"
    [ ( "transport"
      , [ Alcotest.test_case "headers and raw rows" `Quick reader
        ; Alcotest.test_case "malformed input" `Quick malformed
        ] )
    ]
;;
