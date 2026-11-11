open Base

let read name = Stdio.In_channel.read_all ("fixtures/" ^ name)

(* What `fin convert wise` does with a file: the first source that recognizes it. *)
let wise_convert ~name contents =
  let records = Option.value_exn (Builtin.records_of "wise") in
  match
    List.find (Institution_definition.sources records) ~f:(fun source ->
      source.recognizes ~filename:name contents)
  with
  | Some source -> source.convert ~name contents
  | None -> Or_error.error_string "not a Wise file"
;;

let convert name =
  wise_convert ~name (read name)
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
  Alcotest.(check (option string))
    "the running balance is kept"
    (Some "90")
    (Option.map row.details.balance_after ~f:Quantity.to_string);
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
    wise_convert ~name:"s.xml" (statement conversion ~closing:"89.00") |> Or_error.ok_exn
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
    (wise_convert ~name:"s.xml" contents |> Or_error.ok_exn).records
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
    wise_convert ~name:"empty.csv" (header ^ "\n")
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

let shared_lua_prototype () =
  let source = read "wise-shared-prototype.lua" in
  let interpret = Lua_camt_interpreter.create source |> Or_error.ok_exn in
  let original = Option.value_exn (Builtin.camt_profile "wise") in
  let prototype = original in
  let compare name contents =
    let baseline = Camt_converter.convert original ~name contents |> Or_error.ok_exn in
    let actual =
      Camt_converter.convert ~interpret_entry:interpret prototype ~name contents
      |> Or_error.ok_exn
    in
    Alcotest.(check string) name (Records.to_string baseline) (Records.to_string actual)
  in
  compare "wise.xml" (read "wise.xml");
  compare "groups.xml" (statement conversion ~closing:"89.00");
  let xml = read "wise.xml" in
  compare
    "prefixed.xml"
    (String.substr_replace_all
       xml
       ~pattern:"abcdef0123456789abcdef0123456789"
       ~with_:"BALANCE_CASHBACK-ABCDEF01-2345-6789-ABCD-EF0123456789");
  compare
    "precise.xml"
    (String.substr_replace_all
       xml
       ~pattern:"2025-01-02T00:00:00.123456+01:00"
       ~with_:"2025-01-02T00:00:00.123456789123+01:00");
  let bad =
    String.substr_replace_all
      xml
      ~pattern:"abcdef0123456789abcdef0123456789"
      ~with_:"BALANCE_CASHBACK-not-hex"
  in
  Alcotest.(check bool)
    "legacy rejects malformed reference"
    true
    (Result.is_error (Camt_converter.convert original ~name:"bad" bad));
  Alcotest.(check bool)
    "Lua rejects malformed reference"
    true
    (Result.is_error
       (Camt_converter.convert ~interpret_entry:interpret prototype ~name:"bad" bad));
  let failing =
    String.substr_replace_first
      source
      ~pattern:
        "local reference = wise_reference(entry.proprietary_code, entry.description)"
      ~with_:"reject(\"prototype failed\")"
  in
  let failing_interpret = Lua_camt_interpreter.create failing |> Or_error.ok_exn in
  Alcotest.(check bool)
    "script failure is returned"
    true
    (Result.is_error
       (Camt_converter.convert
          ~interpret_entry:failing_interpret
          prototype
          ~name:"bad"
          xml));
  let bad_group =
    String.substr_replace_first
      source
      ~pattern:"local key = drop_prefix(reference, \"FEE-\")"
      ~with_:"reject(\"group failed\")"
  in
  let failing_group = Lua_camt_interpreter.create bad_group |> Or_error.ok_exn in
  Alcotest.(check bool)
    "group script failure is returned"
    true
    (Result.is_error
       (Camt_converter.convert ~interpret_entry:failing_group prototype ~name:"bad" xml));
  (* CAMT-only scripts need no CSV config, and can choose either raw reference. *)
  let standalone =
    "function interpret_camt(entry, statement)\n"
    ^ "  if statement.id ~= \"statement-a\" or statement.currency ~= \"EUR\" then \
       reject(\"bad statement\") end\n"
    ^ "  if entry.amount == nil or entry.booked_at == nil then reject(\"bad entry\") end\n"
    ^ "  return { reference = entry.account_servicer_reference }\nend\n"
  in
  let standalone = Lua_camt_interpreter.create standalone |> Or_error.ok_exn in
  let with_alternate =
    String.substr_replace_all
      xml
      ~pattern:"<Ntry>"
      ~with_:"<Ntry><AcctSvcrRef>ALTERNATE</AcctSvcrRef>"
  in
  let chosen =
    Camt_converter.convert
      ~interpret_entry:standalone
      prototype
      ~name:"alt"
      with_alternate
    |> Or_error.ok_exn
  in
  Alcotest.(check bool)
    "script chooses account-servicer reference"
    true
    (List.for_all chosen.records ~f:(fun record ->
       String.is_prefix record.id ~prefix:"ALTERNATE/"));
  let only_alternate =
    [ "<BkTxCd><Prtry><Cd>CARD-1</Cd></Prtry></BkTxCd>"
    ; "<BkTxCd><Prtry><Cd>abcdef0123456789abcdef0123456789</Cd></Prtry></BkTxCd>"
    ]
    |> List.fold ~init:with_alternate ~f:(fun text pattern ->
      String.substr_replace_all text ~pattern ~with_:"")
  in
  Alcotest.(check bool)
    "legacy profile requires proprietary code"
    true
    (Result.is_error (Camt_converter.convert original ~name:"alt" only_alternate));
  Alcotest.(check bool)
    "script can use the only available reference"
    true
    (Result.is_ok
       (Camt_converter.convert
          ~interpret_entry:standalone
          original
          ~name:"alt"
          only_alternate));
  (* Interpreted references are not normalized again when source payloads decode. *)
  let literal =
    Lua_camt_interpreter.create
      "function interpret_camt(entry, statement) return { reference = \
       \"BALANCE_CASHBACK-nothex\" } end"
    |> Or_error.ok_exn
  in
  let engine = Camt_source.create ~interpret_entry:literal original in
  let document = Camt_source.parse engine xml |> Or_error.ok_exn in
  let transaction = List.hd_exn (List.hd_exn document.statements).transactions in
  let decoded =
    Json.decode (Camt_source.jsont engine) transaction.payload |> Or_error.ok_exn
  in
  Alcotest.(check string)
    "interpreted reference survives payload decode"
    transaction.source_id
    decoded.source_id;
  let typo =
    Lua_camt_interpreter.create
      "function interpret_camt(entry, statement) return { reference = \
       entry.proprietary_code, gruop = \"x\" } end"
    |> Or_error.ok_exn
  in
  let typo_result =
    Camt_converter.convert ~interpret_entry:typo original ~name:"bad" xml
  in
  (match typo_result with
   | Ok _ -> Alcotest.fail "unknown interpretation field accepted"
   | Error error ->
     Alcotest.(check bool)
       "error identifies entry and unknown field"
       true
       (String.is_substring (Error.to_string_hum error) ~substring:"entry 1"
        && String.is_substring (Error.to_string_hum error) ~substring:"gruop"));
  let no_group =
    Lua_camt_interpreter.create
      "function interpret_camt(entry, statement) return { reference = \
       entry.proprietary_code, supporting = yes } end"
    |> Or_error.ok_exn
  in
  Alcotest.(check bool)
    "supporting requires a group"
    true
    (Result.is_error
       (Camt_converter.convert ~interpret_entry:no_group original ~name:"bad" xml));
  let csv_proto =
    Lua_csv_converter.create ~institution:"wise" source |> Or_error.ok_exn
  in
  let csv_old = wise_convert ~name:"wise.csv" (read "wise.csv") |> Or_error.ok_exn in
  let csv_new =
    Lua_csv_converter.convert csv_proto ~name:"wise.csv" (read "wise.csv")
    |> Or_error.ok_exn
  in
  Alcotest.(check string)
    "CSV unchanged with shared function"
    (Records.to_string csv_old)
    (Records.to_string csv_new)
;;

let other_input () =
  Alcotest.(check bool)
    "not Wise"
    true
    (Result.is_error (wise_convert ~name:"f" "a,b\n1,2\n"))
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
        ; Alcotest.test_case "shared Lua CAMT prototype" `Quick shared_lua_prototype
        ; Alcotest.test_case "other input is refused" `Quick other_input
        ] )
    ]
;;
