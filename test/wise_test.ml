open Base
open Stdio
open Finances
open Finances_store

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b

module Wise_camt = struct
  let profile = Option.value_exn (Builtin.camt_profile "wise")
  let source = Camt_source.create profile
  let parse = Camt_source.parse source
end

let xml () = In_channel.read_all "fixtures/wise.xml"
let csv () = In_channel.read_all "fixtures/wise.csv"

let import db ?(institution = "wise") path contents =
  ok (Import.import_bank_file db ~institution ~path ~contents)
;;

let accepted result =
  Alcotest.(check bool)
    "accepted"
    true
    (Poly.equal result.Bank_import.outcome Imported || Poly.equal result.outcome Unchanged)
;;

let rejected result =
  Alcotest.(check bool) "rejected" true (Poly.equal result.Bank_import.outcome Rejected)
;;

(* These tests are about the bank's type code, so drop the bundled rules that
   type by TransferWise ID; the typing_rules suite covers those. *)
let memory f =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       List.iter [ "wise-id-fee"; "wise-id-cashback" ] ~f:(fun name ->
         ok (Ledger.delete_classification_rule db name));
       f db;
       Ok ()))
;;

let error result = Alcotest.(check bool) "invalid document" true (Result.is_error result)

let parser () =
  let doc = ok (Wise_camt.parse (xml ())) in
  let tx = (List.hd_exn doc.statements).transactions in
  check "reversal preserved" 3 (List.length tx);
  check
    "unique identities"
    3
    (List.length
       (List.dedup_and_sort
          (List.map tx ~f:(fun t -> t.source_id))
          ~compare:String.compare));
  Alcotest.(check string)
    "local calendar"
    "2025-01-02"
    (Date.to_string (List.hd_exn tx).booked_on);
  error
    (Wise_camt.parse
       (replace (xml ()) "<Amt Ccy=\"EUR\">105.00</Amt>" "<Amt Ccy=\"EUR\">106.00</Amt>"));
  error (Wise_camt.parse (replace (xml ()) "<Cd>BOOK</Cd>" "<Cd>PDNG</Cd>"));
  error (Wise_camt.parse (replace (xml ()) "camt.053.001.10" "camt.053.001.08"));
  let original = xml () in
  let start = String.substr_index_exn original ~pattern:"<Ntry>" in
  let finish = String.substr_index_exn original ~pattern:"</Ntry>" + 7 in
  let entry = String.sub original ~pos:start ~len:(finish - start) in
  error (Wise_camt.parse (replace original "</Stmt>" (entry ^ "</Stmt>")))
;;

let statement ~id ~currency ~opening ~closing entries =
  let balance code amount =
    Printf.sprintf
      {|<Bal><Tp><CdOrPrtry><Cd>%s</Cd></CdOrPrtry></Tp><Amt Ccy="%s">%s</Amt><CdtDbtInd>CRDT</CdtDbtInd><Dt><DtTm>2025-01-01T00:00:00+01:00</DtTm></Dt></Bal>|}
      code
      currency
      amount
  in
  let entry (amount, direction, time, code) =
    Printf.sprintf
      {|<Ntry><Amt Ccy="%s">%s</Amt><CdtDbtInd>%s</CdtDbtInd><Sts><Cd>BOOK</Cd></Sts><BookgDt><DtTm>2025-01-02T%s+01:00</DtTm></BookgDt><BkTxCd><Prtry><Cd>%s</Cd></Prtry></BkTxCd><AddtlNtryInf>Synthetic</AddtlNtryInf></Ntry>|}
      currency
      amount
      direction
      time
      code
  in
  Printf.sprintf
    {|<Document xmlns="urn:iso:std:iso:20022:tech:xsd:camt.053.001.10"><BkToCstmrStmt><Stmt><Id>%s</Id><FrToDt><FrDtTm>2025-01-01T00:00:00+01:00</FrDtTm><ToDtTm>2025-01-31T23:59:59+01:00</ToDtTm></FrToDt><Acct><Id><IBAN>SYNTHETIC-ACCOUNT</IBAN></Id><Ccy>%s</Ccy><Svcr><FinInstnId><Nm>Wise</Nm></FinInstnId></Svcr></Acct>%s%s%s</Stmt></BkToCstmrStmt></Document>|}
    id
    currency
    (balance "OPBD" opening)
    (balance "CLBD" closing)
    (String.concat (List.map entries ~f:entry))
;;

(* One conversion: net and fee leave the source balance, the target amount enters the
   other. A card payment and its reversal share a reference and stay apart. *)
let source_statement =
  statement
    ~id:"statement-eur"
    ~currency:"EUR"
    ~opening:"100.00"
    ~closing:"89.00"
    [ "1.00", "DBIT", "10:00:00.100000", "FEE-BALANCE-77"
    ; "10.00", "DBIT", "10:00:00.200000", "BALANCE-77"
    ; "3.00", "DBIT", "11:00:00.000000", "CARD-5"
    ; "3.00", "CRDT", "12:00:00.000000", "CARD-5"
    ]
;;

let target_statement =
  statement
    ~id:"statement-sgd"
    ~currency:"SGD"
    ~opening:"0.00"
    ~closing:"15.00"
    [ "15.00", "CRDT", "10:00:00.200000", "BALANCE-77" ]
;;

let conversion_rows db =
  List.map
    (ok (Review.operation_review db))
    ~f:(fun row ->
      let amounts =
        List.map row.movements ~f:(fun m ->
          Quantity.to_string m.quantity ^ " " ^ Commodity.display m.commodity)
        |> List.sort ~compare:String.compare
      in
      row.operation_id, amounts)
;;

let source_stated_groups () =
  let shapes db =
    List.map (conversion_rows db) ~f:snd
    |> List.sort ~compare:(List.compare String.compare)
  in
  let same = Alcotest.(check (list (list string))) in
  let expected = [ [ "-1 EUR"; "-10 EUR"; "15 SGD" ]; [ "-3 EUR" ]; [ "3 EUR" ] ] in
  memory (fun db ->
    accepted (import db "source.xml" source_statement);
    same
      "the source balance alone groups the fee with the net"
      [ [ "-1 EUR"; "-10 EUR" ]; [ "-3 EUR" ]; [ "3 EUR" ] ]
      (shapes db);
    accepted (import db "target.xml" target_statement);
    same "all three rows" expected (shapes db);
    (* Grouping is a read: each row keeps its own source event. *)
    check "events stay per row" 5 (List.length (ok (Ledger.events db)));
    let id, _ = List.find_exn (conversion_rows db) ~f:(fun (_, a) -> List.length a = 3) in
    let row =
      List.find_exn
        (ok (Review.operation_review db))
        ~f:(fun r -> Int64.equal r.operation_id id)
    in
    Alcotest.(check string)
      "the main row represents the group, not the fee"
      "-10"
      (Quantity.to_string (List.hd_exn row.movements).quantity);
    List.iter
      (ok (Ledger.events db))
      ~f:(fun (event_id, (e : Source_event.t)) ->
        if String.is_substring e.payload ~substring:"BALANCE-77"
        then (
          let operations = ok (Operations.operations_for_event db event_id) in
          Alcotest.(check (list int64))
            "each row finds the group"
            [ id ]
            (List.map operations ~f:fst);
          check
            "with all its movements"
            3
            (List.length (snd (List.hd_exn operations)).movements)));
    check
      "the group reads the same by its ID"
      3
      (List.length (ok (Operations.get_operation db id)).movements));
  memory (fun db ->
    accepted (import db "target.xml" target_statement);
    accepted (import db "source.xml" source_statement);
    same "either import order" expected (shapes db))
;;

let reusable_camt_source () =
  let wise = Option.value_exn (Builtin.camt_profile "wise") in
  let profile =
    { wise with
      institution = "example"
    ; servicer_contains = Some "other bank"
    ; account_type_prefix = "account-"
    ; event_kind = "example-camt"
    ; payload_tag = "example-camt-v1"
    ; reference_field = Camt_profile.Account_servicer_reference
    ; reference_normalization = Camt_profile.Preserve
    }
  in
  let profile =
    Json.decode Camt_profile.codec (Json.encode Camt_profile.codec profile) |> ok
  in
  let contents =
    xml ()
    |> fun s ->
    replace s ">Wise<" ">Other Bank<"
    |> fun s -> replace s "<BkTxCd>" "<AcctSvcrRef>EXAMPLE-REF</AcctSvcrRef><BkTxCd>"
  in
  let engine = Camt_source.create profile in
  let document = ok (Camt_source.parse engine contents) in
  let transaction = List.hd_exn (List.hd_exn document.statements).transactions in
  Alcotest.(check string) "configured bank" "example" transaction.account.bank_id;
  Alcotest.(check string)
    "configured account type"
    "account-EUR"
    transaction.account.account_type;
  Alcotest.(check string) "configured reference field" "EXAMPLE-REF" transaction.reference;
  let event =
    ok
      (Source_event.create
         ~venue:"example"
         ~account:(Camt_source.account_identity transaction.account)
         ~kind:profile.event_kind
         ~source_id:transaction.source_id
         ~occurred_at:transaction.booked_at
         ~payload:transaction.payload
         ())
  in
  let decoded = ok (Camt_source.source_transaction engine event) in
  Alcotest.(check string) "payload replay" transaction.payload decoded.payload;
  let wrong =
    ok
      (Source_event.create
         ~venue:"another"
         ~account:event.account
         ~kind:event.kind
         ~source_id:event.source_id
         ~occurred_at:transaction.booked_at
         ~payload:event.payload
         ())
  in
  error (Camt_source.source_transaction engine wrong)
;;

let multi_bank () =
  memory (fun db ->
    accepted (import db "wise.csv" (csv ()));
    check "CSV creates no movements" 0 (List.length (ok (Ledger.events db)));
    check "CSV waiting" 3 (ok (Ledger.match_companions ~institution:"wise" db)).unmatched;
    accepted
      (import
         db
         ~institution:"boursobank"
         "bank.ofx"
         (In_channel.read_all "fixtures/bank.ofx"));
    accepted (import db "wise.xml" (xml ()));
    check "both institutions" 6 (List.length (ok (Ledger.events db)));
    check
      "all Wise rows matched"
      3
      (ok (Ledger.match_companions ~institution:"wise" db)).matched;
    check "all projected" 6 (ok (Ledger.project_bank db)).inserted;
    let accounts = ok (Ledger.bank_accounts db) in
    Alcotest.(check bool)
      "separate bank accounts"
      true
      (List.exists accounts ~f:(fun a ->
         String.equal a.Bank_account_mapping.institution "wise")
       && List.exists accounts ~f:(fun a ->
         String.equal a.Bank_account_mapping.institution "boursobank"));
    let statements = ok (Review.statement_reconciliations ~institution:"wise" db) in
    Alcotest.(check bool) "statements stored" true (not (List.is_empty statements));
    Alcotest.(check int)
      "every Wise movement belongs to a statement"
      3
      (List.sum (module Int) statements ~f:(fun r -> r.Review.entries));
    List.iter statements ~f:(fun r ->
      Alcotest.(check bool)
        "stored balance reconciled"
        true
        (Quantity.is_zero r.Review.difference));
    let journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "merchant metadata exported"
      true
      (String.is_substring journal ~substring:"wise-merchant: Synthetic shop");
    Alcotest.(check bool)
      "nonzero fees retained as metadata"
      true
      (String.is_substring journal ~substring:"wise-reported-fees: 0.5");
    accepted (import db "replay.xml" (xml ()));
    accepted (import db "overlap.xml" (replace (xml ()) "statement-a" "statement-b"));
    check "overlap no new projections" 0 (ok (Ledger.project_bank db)).inserted;
    check
      "repeated matching stable"
      0
      (ok (Ledger.match_companions ~institution:"wise" db)).changed;
    check "replay source count" 6 (List.length (ok (Ledger.events db)));
    let wise_account =
      List.find_exn accounts ~f:(fun a ->
        String.equal a.Bank_account_mapping.institution "wise")
    in
    let mapped =
      ok
        (Bank_account_mapping.create
           ~institution:wise_account.institution
           ~bank_id:wise_account.bank_id
           ~account_id:wise_account.account_id
           ~account_type:wise_account.account_type
           ~ledger_account:"assets:bank:wise:daily")
    in
    check "Wise mapping applied" 1 (ok (Ledger.set_bank_account_mappings db [ mapped ]));
    check "mapping replay" 0 (ok (Ledger.project_bank db)).inserted;
    let mapped_journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "Wise alias exported"
      true
      (String.is_substring mapped_journal ~substring:"assets:bank:wise:daily");
    let changed =
      replace
        (replace (xml ()) "<Amt Ccy=\"EUR\">5.00</Amt>" "<Amt Ccy=\"EUR\">6.00</Amt>")
        "<Amt Ccy=\"EUR\">105.00</Amt>"
        "<Amt Ccy=\"EUR\">106.00</Amt>"
    in
    rejected (import db "conflict.xml" changed);
    check "conflict atomic" 6 (List.length (ok (Ledger.events db)));
    rejected (import db ~institution:"boursobank" "wrong.xml" (xml ())))
;;

let converted_reimport () =
  let definition =
    Csv_converter.of_string (In_channel.read_all "fixtures/wise-converter.json") |> ok
  in
  let converted =
    Csv_converter.convert definition ~name:"wise.csv" (csv ()) |> ok |> Records.to_string
  in
  List.iter [ true; false ] ~f:(fun records_first ->
    memory (fun db ->
      accepted (import db "wise.xml" (xml ()));
      let first, second =
        if records_first then converted, csv () else csv (), converted
      in
      accepted (import db "first" first);
      accepted (import db "second" second);
      check
        "both representations match"
        6
        (ok (Ledger.match_companions ~institution:"wise" db)).matched;
      check "counted events are not duplicated" 3 (List.length (ok (Ledger.events db)));
      ignore (ok (Ledger.project_bank db));
      Alcotest.(check bool)
        "merchant survives reimport"
        true
        (String.is_substring
           (ok (Finances_hledger.export db))
           ~substring:"wise-merchant: Synthetic shop")))
;;

let shared_lua_camt_prototype () =
  let source = In_channel.read_all "fixtures/wise-shared-prototype.lua" in
  let interpret = Lua_camt_interpreter.create source |> ok in
  let profile = Option.value_exn (Builtin.camt_profile "wise") in
  let records =
    Camt_converter.convert ~interpret_entry:interpret profile ~name:"wise.xml" (xml ())
    |> ok
    |> Records.to_string
  in
  List.iter [ true; false ] ~f:(fun camt_first ->
    memory (fun db ->
      let first, second = if camt_first then records, csv () else csv (), records in
      accepted (import db "first" first);
      accepted (import db "second" second);
      check
        "prototype observations matched"
        3
        (ok (Ledger.match_companions ~institution:"wise" db)).matched;
      check "prototype counted events" 3 (List.length (ok (Ledger.events db)));
      ignore (ok (Ledger.project_bank db));
      Alcotest.(check bool)
        "prototype preserves journal metadata"
        true
        (String.is_substring
           (ok (Finances_hledger.export db))
           ~substring:"wise-merchant: Synthetic shop");
      let replay = import db "wise.xml" (xml ()) in
      accepted replay;
      check "legacy CAMT rows reimport" 3 replay.existing;
      check "no duplicate events" 3 (List.length (ok (Ledger.events db)))))
;;

let currencies () =
  memory (fun db ->
    accepted (import db "eur.xml" (xml ()));
    accepted (import db "usd.xml" (replace (xml ()) "EUR" "USD"));
    check "currencies independent" 6 (List.length (ok (Ledger.events db)));
    accepted (import db "eur.csv" (csv ()));
    check "currency match" 3 (ok (Ledger.match_companions ~institution:"wise" db)).matched;
    accepted (import db "fees.csv" (replace (csv ()) "0.50" "0.75"));
    check
      "fee conflict audit"
      2
      (ok (Ledger.match_companions ~institution:"wise" db)).conflicts;
    check "postings only movements" 6 (ok (Ledger.project_bank db)).inserted)
;;

let duplicate_rows () =
  memory (fun db ->
    accepted (import db "wise.xml" (xml ()));
    let lines = String.split_lines (csv ()) in
    (* One row stated twice in a file cannot be told from itself, so the file is refused. *)
    rejected (import db "dup.csv" (csv () ^ List.nth_exn lines 1 ^ "\n"));
    accepted (import db "wise.csv" (csv ()));
    check
      "and the clean file pairs"
      3
      (ok (Ledger.match_companions ~institution:"wise" db)).matched)
;;

let timestamp_precision () =
  let source = "2025-01-02T00:00:00.123456789123+01:00" in
  let contents = replace (xml ()) "2025-01-02T00:00:00.123456+01:00" source in
  let tx =
    (List.hd_exn (ok (Wise_camt.parse contents)).statements).transactions |> List.hd_exn
  in
  Alcotest.(check string) "raw booking text retained" source tx.booked_at_source;
  Alcotest.(check string)
    "full Ptime precision"
    "2025-01-01T23:00:00.123456789123Z"
    (Timestamp.to_rfc3339 tx.booked_at);
  Alcotest.(check string)
    "local date is not UTC date"
    "2025-01-02"
    (Date.to_string tx.booked_on);
  Alcotest.(check string)
    "source identity retains legacy nanosecond encoding"
    "[\"CARD-1\",\"1735772400123456789\",\"DBIT\"]"
    tx.source_id;
  memory (fun db ->
    accepted (import db "precise.xml" contents);
    let rows = ok (Ledger.events db) in
    let event =
      List.find_exn rows ~f:(fun (_, e) ->
        String.equal e.Source_event.source_id "CARD-1/1735772400123456789/DBIT")
      |> snd
    in
    (match event.occurred with
     | Temporal.Instant instant ->
       Alcotest.(check bool)
         "SQL keeps all picoseconds"
         true
         (Ptime.equal instant tx.booked_at)
     | Temporal.Date _ -> Alcotest.fail "Wise instant lost");
    let _, record = ok (Records.row_of_string event.payload) in
    Alcotest.(check string)
      "the record keeps the instant as stated"
      source
      record.occurred.stated;
    let replay = import db "precise.xml" contents in
    Alcotest.(check bool) "replay unchanged" true (Poly.equal replay.outcome Unchanged))
;;

let review_enrichment () =
  memory (fun db ->
    accepted (import db "wise.xml" (xml ()));
    accepted (import db "wise.csv" (csv ()));
    let check_companions () =
      let all = ok (Ledger.companion_observations db) in
      let journal = ok (Ledger.journal_companions db) in
      let matched =
        List.filter_map all ~f:(fun (observation : Ledger.companion_observation) ->
          match observation.matching with
          | Some { state = Matched; event_id = Some event_id; _ } ->
            Some (event_id, observation.row)
          | _ -> None)
      in
      let review =
        List.count all ~f:(fun observation ->
          not
            (Option.exists observation.Ledger.matching ~f:(fun result ->
               Poly.equal result.state Bank_match.Matched)))
      in
      check "all companion rows" (List.length all) journal.total;
      check "companion rows needing review" review journal.review;
      Alcotest.(check bool)
        "matched companion rows"
        true
        (Poly.equal matched journal.rows)
    in
    check_companions ();
    ignore (ok (Ledger.match_companions ~institution:"wise" db) : Ledger.companion_report);
    ignore (ok (Ledger.project_bank db));
    let check_journal_descriptions rows =
      let journal = ok (Finances_hledger.export db) in
      let transactions =
        Hledger.Parse.journal_exn journal |> Hledger.Journal.transactions
      in
      List.iter rows ~f:(fun (row : Review.bank_review) ->
        let code = Printf.sprintf "event-%Ld" row.event_id in
        let tx =
          List.find_exn transactions ~f:(fun tx ->
            Option.equal String.equal tx.code (Some code))
        in
        Alcotest.(check string)
          "journal uses reviewed description"
          row.description
          tx.description)
    in
    check_companions ();
    let rows = ok (Review.bank_review db) in
    check_journal_descriptions rows;
    List.iter rows ~f:(fun row ->
      Alcotest.(check string) "effective description" "Synthetic shop" row.description;
      Alcotest.(check (option string))
        "type"
        (Some "card")
        (Option.map row.movement_type ~f:Movement_type.to_string);
      check "source row recorded" 1 (List.length row.detail_sources));
    accepted (import db "other.csv" (replace (csv ()) "Synthetic shop" "Other shop"));
    ignore (ok (Ledger.match_companions ~institution:"wise" db) : Ledger.companion_report);
    check_companions ();
    let rows = ok (Review.bank_review db) in
    check_journal_descriptions rows;
    List.iter rows ~f:(fun row ->
      Alcotest.(check string)
        "disagreeing companion descriptions fall back to primary"
        (List.nth_exn row.transaction.descriptions row.transaction.main_description)
        row.description;
      Alcotest.(check (option string))
        "agreeing type retained"
        (Some "card")
        (Option.map row.movement_type ~f:Movement_type.to_string)))
;;

let reported_type_rules () =
  memory (fun db ->
    accepted (import db "first.csv" (csv ()));
    accepted (import db "later.xml" (xml ()));
    ignore (ok (Ledger.match_companions ~institution:"wise" db) : Ledger.companion_report);
    let rows = ok (Review.bank_review db) in
    check "all transactions imported" 3 (List.length rows);
    List.iter rows ~f:(fun row ->
      Alcotest.(check (option string))
        "default from the imported column"
        (Some "card")
        (Option.map row.movement_type ~f:Movement_type.to_string));
    let event_id = (List.hd_exn rows).event_id in
    ignore
      (ok
         (Restore.set_movement_type_override
            db
            ~event_id
            ~movement_type:(Some Movement_type.Fee))
       : bool);
    let rule =
      ok
        (Classification_rule.create
           ~mode:Automatic
           ~account_type:None
           ~priority:(-1000)
           ~name:"card-as-bonus"
           ~pattern:(Some "^CARD$")
           ~institution:(Some "wise")
           ~match_type:None
           ~case_sensitive:true
           ~action:(Type Bonus)
           ~field:"Transaction Details Type"
           ())
    in
    (* Types are derived: the rule applies as soon as it is saved. *)
    ok (Ledger.add_classification_rule db rule);
    let types () =
      List.map
        (ok (Review.bank_review db))
        ~f:(fun r ->
          r.event_id, Option.map r.imported_movement_type ~f:Movement_type.to_string)
    in
    List.iter (types ()) ~f:(fun (_, t) ->
      Alcotest.(check (option string)) "retyped at once" (Some "bonus") t);
    (* Reimporting a file does not change what the rules give. *)
    accepted (import db "first.csv" (csv ()));
    List.iter (types ()) ~f:(fun (_, t) ->
      Alcotest.(check (option string)) "unchanged by a reimport" (Some "bonus") t);
    (* Disabling the rule retypes again, enabling it restores. *)
    ok (Ledger.set_classification_rule_mode db "card-as-bonus" Disabled);
    List.iter (types ()) ~f:(fun (_, t) ->
      Alcotest.(check (option string)) "back to the column's rule" (Some "card") t);
    ok (Ledger.set_classification_rule_mode db "card-as-bonus" Automatic);
    List.iter (types ()) ~f:(fun (_, t) ->
      Alcotest.(check (option string)) "enabled again" (Some "bonus") t);
    let overridden =
      List.find_exn
        (ok (Review.bank_review db))
        ~f:(fun r -> Int64.equal r.event_id event_id)
    in
    Alcotest.(check (option string))
      "manual override preserved"
      (Some "fee")
      (Option.map overridden.movement_type ~f:Movement_type.to_string))
;;

let reported_type_fallbacks () =
  memory (fun db ->
    let csv_unknown = replace (csv ()) ",CARD\n" ",UNKNOWN\n" in
    accepted (import db "unknown.csv" csv_unknown);
    accepted (import db "statement.xml" (xml ()));
    ignore (ok (Ledger.match_companions ~institution:"wise" db) : Ledger.companion_report);
    List.iter
      (ok (Review.bank_review db))
      ~f:(fun row ->
        Alcotest.(check (option string))
          "an UNKNOWN code is kept as a provider-specific type"
          (Some "wise:UNKNOWN")
          (Option.map row.movement_type ~f:Movement_type.to_string)));
  memory (fun db ->
    let csv_other = replace (csv ()) ",CARD\n" ",NEW_CODE\n" in
    accepted (import db "other.csv" csv_other);
    accepted (import db "statement.xml" (xml ()));
    ignore (ok (Ledger.match_companions ~institution:"wise" db) : Ledger.companion_report);
    List.iter
      (ok (Review.bank_review db))
      ~f:(fun row ->
        Alcotest.(check (option string))
          "unrecognized code retained"
          (Some "wise:NEW_CODE")
          (Option.map row.movement_type ~f:Movement_type.to_string)))
;;

let () =
  Alcotest.run
    "Wise and multiple banks"
    [ ( "adapter"
      , List.map
          [ "parser and balances", parser
          ; "source-stated operation groups", source_stated_groups
          ; "reusable CAMT source profile", reusable_camt_source
          ; "reported type rules, replay and refresh", reported_type_rules
          ; "unknown and provider-specific codes", reported_type_fallbacks
          ; "Ptime instants, raw text and stable identity", timestamp_precision
          ; "two banks, replay, conflict", multi_bank
          ; "old records and Lua CSV reimport", converted_reimport
          ; "shared Lua CAMT prototype", shared_lua_camt_prototype
          ; "currency and fee conflicts", currencies
          ; "duplicate CSV claims", duplicate_rows
          ; "review enrichment and conflicting observations", review_enrichment
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
