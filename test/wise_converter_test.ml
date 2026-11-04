open Base

let read name = Stdio.In_channel.read_all ("fixtures/" ^ name)

let convert name =
  Wise_converter.convert ~name (read name)
  |> Or_error.ok_exn
  |> Records.to_string
  |> Records.of_string
  |> Or_error.ok_exn
  |> List.hd_exn
;;

let find (records : Records.t) id =
  List.find_exn records.records ~f:(fun r -> String.is_prefix r.id ~prefix:id)
;;

let servicer (r : Records.record) =
  Option.bind r.details.references ~f:(fun references -> references.servicer)
;;

let quantity (r : Records.record) =
  match r.movements with
  | [ m ] -> Quantity.to_string m.quantity
  | _ -> Alcotest.failf "record %s has no single movement" r.id
;;

let camt () =
  let t = convert "wise.xml" in
  Alcotest.(check int) "every entry is a record" 3 (List.length t.records);
  Alcotest.(check int) "statement" 1 (List.length t.statements);
  let purchase = find t "CARD-1/" in
  Alcotest.(check bool) "counted" false purchase.observation;
  Alcotest.(check string) "signed" "-10" (quantity purchase);
  Alcotest.(check string)
    "instant kept as stated"
    "2025-01-02T00:00:00.123456+01:00"
    purchase.occurred.stated;
  Alcotest.(check string)
    "booked on the stated date, not the UTC one"
    "2025-01-02"
    (Date.to_string (Records.local_date purchase.occurred));
  let refund =
    List.filter t.records ~f:(fun (r : Records.record) ->
      String.is_prefix r.id ~prefix:"CARD-1/")
    |> List.map ~f:(fun (r : Records.record) -> r.id)
    |> List.dedup_and_sort ~compare:String.compare
  in
  Alcotest.(check int)
    "a payment and its refund have their own IDs"
    2
    (List.length refund)
;;

let csv () =
  let t = convert "wise.csv" in
  Alcotest.(check bool)
    "all are observations"
    true
    (List.for_all t.records ~f:(fun r -> r.observation));
  let row = find t "CARD-1/" in
  Alcotest.(check string)
    "a date, the clock has no offset"
    "2025-01-02"
    row.occurred.stated;
  Alcotest.(check bool)
    "the clock stays in the fields"
    true
    (List.exists row.details.fields ~f:(fun (name, _) -> String.equal name "Date Time"));
  Alcotest.(check bool) "no fee: Total fees repeats a row" true (Option.is_none row.fee);
  Alcotest.(check bool)
    "empty columns are left out"
    false
    (List.exists row.details.fields ~f:(fun (_, value) -> String.is_empty value))
;;

(* The pairing the format promises must be possible: each CSV row finds a CAMT record
   with its reference, quantity and date. *)
let observations_pair () =
  let camt = convert "wise.xml"
  and csv = convert "wise.csv" in
  List.iter csv.records ~f:(fun observation ->
    let same (r : Records.record) =
      Option.equal String.equal (servicer r) (servicer observation)
      && String.equal (quantity r) (quantity observation)
      && Date.equal
           (Records.local_date r.occurred)
           (Records.local_date observation.occurred)
    in
    match List.filter camt.records ~f:same with
    | [ _ ] -> ()
    | found ->
      Alcotest.failf "%s pairs with %d records" observation.id (List.length found))
;;

let statement_entry ~credit ~time ~reference ~amount =
  Printf.sprintf
    {|<Ntry><Amt Ccy="EUR">%s</Amt><CdtDbtInd>%s</CdtDbtInd><Sts><Cd>BOOK</Cd></Sts><BookgDt><DtTm>2025-01-02T%s+01:00</DtTm></BookgDt><BkTxCd><Prtry><Cd>%s</Cd></Prtry></BkTxCd><AddtlNtryInf>Text</AddtlNtryInf></Ntry>|}
    amount
    (if credit then "CRDT" else "DBIT")
    time
    reference
;;

let statement entries ~closing =
  Printf.sprintf
    {|<Document xmlns="urn:iso:std:iso:20022:tech:xsd:camt.053.001.10"><BkToCstmrStmt><Stmt><Id>s</Id><FrToDt><FrDtTm>2025-01-01T00:00:00+01:00</FrDtTm><ToDtTm>2025-01-31T23:59:59+01:00</ToDtTm></FrToDt><Acct><Id><IBAN>SYNTHETIC-ACCOUNT</IBAN></Id><Ccy>EUR</Ccy><Svcr><FinInstnId><Nm>Wise</Nm></FinInstnId></Svcr></Acct><Bal><Tp><CdOrPrtry><Cd>OPBD</Cd></CdOrPrtry></Tp><Amt Ccy="EUR">100.00</Amt><CdtDbtInd>CRDT</CdtDbtInd><Dt><DtTm>2025-01-01T00:00:00+01:00</DtTm></Dt></Bal><Bal><Tp><CdOrPrtry><Cd>CLBD</Cd></CdOrPrtry></Tp><Amt Ccy="EUR">%s</Amt><CdtDbtInd>CRDT</CdtDbtInd><Dt><DtTm>2025-01-31T00:00:00+01:00</DtTm></Dt></Bal>%s</Stmt></BkToCstmrStmt></Document>|}
    closing
    (String.concat entries)
;;

let conversion =
  [ statement_entry
      ~credit:false
      ~time:"10:00:00.100000"
      ~reference:"FEE-BALANCE-77"
      ~amount:"1.00"
  ; statement_entry
      ~credit:false
      ~time:"10:00:00.200000"
      ~reference:"BALANCE-77"
      ~amount:"10.00"
  ]
;;

let groups () =
  let t =
    Wise_converter.convert ~name:"s.xml" (statement conversion ~closing:"89.00")
    |> Or_error.ok_exn
  in
  let fee = find t "FEE-BALANCE-77/"
  and debit = find t "BALANCE-77/" in
  Alcotest.(check (option string)) "the fee's group" (Some "BALANCE-77") fee.group;
  Alcotest.(check (option string)) "the debit's group" (Some "BALANCE-77") debit.group;
  Alcotest.(check bool) "the fee only supports" true fee.supporting;
  Alcotest.(check bool) "the debit represents it" false debit.supporting
;;

(* A record is the same record whichever download states it. *)
let ids_do_not_depend_on_the_download () =
  let ids contents =
    (Wise_converter.convert ~name:"s.xml" contents |> Or_error.ok_exn).records
    |> List.map ~f:(fun (r : Records.record) -> r.id)
  in
  let short = ids (statement conversion ~closing:"89.00") in
  let long =
    ids
      (statement
         (statement_entry
            ~credit:false
            ~time:"09:00:00.000000"
            ~reference:"CARD-9"
            ~amount:"5.00"
          :: conversion)
         ~closing:"84.00")
  in
  Alcotest.(check (list string)) "same IDs" short (List.tl_exn long)
;;

(* A balance with no activity exports a header. It states nothing, and that is not
   an error. *)
let empty_statement () =
  let header = List.hd_exn (String.split_lines (read "wise.csv")) in
  let t =
    Wise_converter.convert ~name:"empty.csv" (header ^ "\n")
    |> Or_error.ok_exn
    |> Records.to_string
    |> Records.of_string
    |> Or_error.ok_exn
    |> List.hd_exn
  in
  Alcotest.(check int) "no records" 0 (List.length t.records);
  Alcotest.(check int) "no accounts" 0 (List.length t.accounts);
  Alcotest.(check int) "no statements" 0 (List.length t.statements);
  Alcotest.(check int) "the source is still named" 1 (List.length t.sources)
;;

let other_input () =
  Alcotest.(check bool)
    "not Wise"
    true
    (Result.is_error (Wise_converter.convert ~name:"f" "a,b\n1,2\n"))
;;

let () =
  Alcotest.run
    "wise converter"
    [ ( "convert"
      , [ Alcotest.test_case "CAMT entries are counted records" `Quick camt
        ; Alcotest.test_case "CSV rows are observations" `Quick csv
        ; Alcotest.test_case "observations can pair" `Quick observations_pair
        ; Alcotest.test_case "Wise's references group" `Quick groups
        ; Alcotest.test_case "IDs are stable" `Quick ids_do_not_depend_on_the_download
        ; Alcotest.test_case "an empty statement converts" `Quick empty_statement
        ; Alcotest.test_case "other input is refused" `Quick other_input
        ] )
    ]
;;
