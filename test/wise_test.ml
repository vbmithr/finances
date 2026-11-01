open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b

module Wise_csv = struct
  include Csv_clock_companion

  let profile = (Option.value_exn (Builtin.camt_preset "wise")).csv_profile
  let camt = (Option.value_exn (Builtin.camt_preset "wise")).camt_profile
  let parse = parse profile camt
  let match_rows = match_rows profile
end

module Wise_camt = struct
  let profile = (Option.value_exn (Builtin.camt_preset "wise")).camt_profile
  let source = Camt_source.create profile
  let parse = Camt_source.parse source
  let source_transaction = Camt_source.source_transaction source
  let matching_time = Camt_source.matching_time
end

let xml () = In_channel.read_all "fixtures/wise.xml"
let csv () = In_channel.read_all "fixtures/wise.csv"

let import db ?(institution = "wise") path contents =
  ok (Store.import_bank_file db ~institution ~path ~contents)
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
    (Store.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       List.iter [ "wise-id-fee"; "wise-id-cashback" ] ~f:(fun name ->
         ok (Store.delete_classification_rule db name));
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
  error (Wise_camt.parse (replace original "</Stmt>" (entry ^ "</Stmt>")));
  error (Wise_csv.parse (replace (csv ()) "02-01-2025" "32-01-2025"));
  error
    (Wise_csv.parse
       (replace (csv ()) "02-01-2025 00:00:00.123" "03-01-2025 00:00:00.123"));
  error (Wise_csv.parse (replace (csv ()) "DEBIT" "CREDIT"));
  error
    (Wise_csv.parse (replace (csv ()) "02-01-2025 00:00:00.123" "02-01-2025 00:00:00"))
;;

let reusable_clock_csv () =
  let wise = (Option.value_exn (Builtin.camt_preset "wise")).csv_profile in
  let camt = (Option.value_exn (Builtin.camt_preset "wise")).camt_profile in
  let header =
    List.map wise.header ~f:(fun s ->
      if String.equal s "TransferWise ID" then "Reference" else s)
  in
  let profile =
    { wise with
      institution = "example"
    ; name = "Example"
    ; header
    ; columns = { wise.columns with reference = "Reference" }
    }
  in
  let profile =
    Json.decode Csv_clock_profile.codec (Json.encode Csv_clock_profile.codec profile)
    |> ok
  in
  let contents = replace (csv ()) "TransferWise ID" "Reference" in
  let rows = ok (Csv_clock_companion.parse profile camt contents) in
  check "other profile parses all rows" 3 (List.length rows);
  Alcotest.(check bool)
    "recognizes configured header"
    true
    (Csv_clock_companion.recognizes profile camt contents);
  let bad = { profile with columns = { profile.columns with reference = "missing" } } in
  error (Json.decode Csv_clock_profile.codec (Json.encode Csv_clock_profile.codec bad));
  let module A =
    (val Camt_csv_adapter.make
           ~profile:camt
           ~csv_profile:profile
           ~csv:"example_csv"
           ~policy:"example-v1"
           ~metadata_prefix:"example"
           ()
      : Adapter.S)
  in
  Alcotest.(check (option string))
    "configured importer recognizes CSV"
    (Some "example_csv")
    (A.probe ~filename:"statement.csv" contents);
  let document =
    ok (A.parse ~context:{ account_label = None } ~format:"example_csv" contents)
  in
  check "configured importer retains rows" 3 (List.length document.companion_rows)
;;

let reusable_camt_source () =
  let wise = (Option.value_exn (Builtin.camt_preset "wise")).camt_profile in
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
    check "CSV creates no movements" 0 (List.length (ok (Store.events db)));
    check "CSV waiting" 3 (ok (Store.match_companions ~institution:"wise" db)).unmatched;
    accepted
      (import
         db
         ~institution:"boursobank"
         "bank.ofx"
         (In_channel.read_all "fixtures/bank.ofx"));
    accepted (import db "wise.xml" (xml ()));
    check "both institutions" 6 (List.length (ok (Store.events db)));
    check
      "all Wise rows matched"
      3
      (ok (Store.match_companions ~institution:"wise" db)).matched;
    check "all projected" 6 (ok (Store.project_bank db)).inserted;
    let accounts = ok (Store.bank_accounts db) in
    Alcotest.(check bool)
      "separate bank accounts"
      true
      (List.exists accounts ~f:(fun a ->
         String.equal a.Bank_account_mapping.institution "wise")
       && List.exists accounts ~f:(fun a ->
         String.equal a.Bank_account_mapping.institution "boursobank"));
    let statements = ok (Store.statement_reconciliations ~institution:"wise" db) in
    Alcotest.(check bool) "statements stored" true (not (List.is_empty statements));
    Alcotest.(check int)
      "every Wise movement belongs to a statement"
      3
      (List.sum (module Int) statements ~f:(fun r -> r.Store.entries));
    List.iter statements ~f:(fun r ->
      Alcotest.(check bool)
        "stored balance reconciled"
        true
        (Quantity.is_zero r.Store.difference));
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
    check "overlap no new projections" 0 (ok (Store.project_bank db)).inserted;
    check
      "repeated matching stable"
      0
      (ok (Store.match_companions ~institution:"wise" db)).changed;
    check "replay source count" 6 (List.length (ok (Store.events db)));
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
    check "Wise mapping applied" 1 (ok (Store.set_bank_account_mappings db [ mapped ]));
    check "mapping replay" 0 (ok (Store.project_bank db)).inserted;
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
    check "conflict atomic" 6 (List.length (ok (Store.events db)));
    rejected (import db ~institution:"boursobank" "wrong.xml" (xml ())))
;;

let currencies () =
  memory (fun db ->
    accepted (import db "eur.xml" (xml ()));
    accepted (import db "usd.xml" (replace (xml ()) "EUR" "USD"));
    check "currencies independent" 6 (List.length (ok (Store.events db)));
    accepted (import db "eur.csv" (csv ()));
    check "currency match" 3 (ok (Store.match_companions ~institution:"wise" db)).matched;
    accepted (import db "fees.csv" (replace (csv ()) "0.50" "0.75"));
    check
      "fee conflict audit"
      2
      (ok (Store.match_companions ~institution:"wise" db)).conflicts;
    check "postings only movements" 6 (ok (Store.project_bank db)).inserted)
;;

let duplicate_rows () =
  memory (fun db ->
    accepted (import db "wise.xml" (xml ()));
    let lines = String.split_lines (csv ()) in
    accepted (import db "dup.csv" (csv () ^ List.nth_exn lines 1 ^ "\n"));
    check
      "ambiguous duplicates"
      2
      (ok (Store.match_companions ~institution:"wise" db)).ambiguous)
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
    let rows = ok (Store.events db) in
    let event =
      List.find_exn rows ~f:(fun (_, e) ->
        String.equal e.Source_event.source_id tx.source_id)
      |> snd
    in
    (match event.occurred with
     | Temporal.Instant instant ->
       Alcotest.(check bool)
         "SQL keeps all picoseconds"
         true
         (Ptime.equal instant tx.booked_at)
     | Temporal.Date _ -> Alcotest.fail "Wise instant lost");
    let decoded = ok (Wise_camt.source_transaction event) in
    Alcotest.(check string) "payload remains byte-identical" tx.payload decoded.payload;
    let replay = import db "precise.xml" contents in
    Alcotest.(check bool) "replay unchanged" true (Poly.equal replay.outcome Unchanged))
;;

let csv_times () =
  let rows = ok (Wise_csv.parse (csv ())) in
  let first = List.hd_exn rows in
  Alcotest.(check string)
    "CSV clock assumed UTC"
    "2025-01-02T00:00:00.123000000000Z"
    (Timestamp.to_rfc3339 first.booked_at);
  List.iter [ "+01:00"; "-05:00"; "+05:30" ] ~f:(fun offset ->
    let document = ok (Wise_camt.parse (replace (xml ()) "+01:00" offset)) in
    let transactions = (List.hd_exn document.statements).transactions in
    let tx = List.hd_exn transactions in
    Alcotest.(check bool)
      "matching key uses source-local clock"
      true
      (Ptime.equal first.booked_at (Wise_camt.matching_time tx));
    Alcotest.(check bool)
      "matching clock is distinct from actual CAMT instant"
      false
      (Ptime.equal first.booked_at tx.booked_at);
    let input =
      List.mapi rows ~f:(fun i row ->
        { Wise_csv.row_id = Int64.of_int (i + 1); document_id = 1L; row })
    in
    let candidates = List.mapi transactions ~f:(fun i tx -> Int64.of_int (i + 100), tx) in
    let results = Wise_csv.match_rows input candidates in
    check
      "all local-clock matches preserved"
      3
      (List.count results ~f:(fun row -> Poly.equal row.Bank_match.state Matched)))
;;

let review_enrichment () =
  memory (fun db ->
    accepted (import db "wise.xml" (xml ()));
    accepted (import db "wise.csv" (csv ()));
    let check_companions () =
      let all = ok (Store.companion_observations db) in
      let journal = ok (Store.journal_companions db) in
      let matched =
        List.filter_map all ~f:(fun (observation : Store.companion_observation) ->
          match observation.matching with
          | Some { state = Matched; event_id = Some event_id; _ } ->
            Some (event_id, observation.row)
          | _ -> None)
      in
      let review =
        List.count all ~f:(fun observation ->
          not
            (Option.exists observation.Store.matching ~f:(fun result ->
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
    ignore (ok (Store.match_companions ~institution:"wise" db) : Store.companion_report);
    ignore (ok (Store.project_bank db));
    let check_journal_descriptions rows =
      let journal = ok (Finances_hledger.export db) in
      let transactions =
        Hledger.Parse.journal_exn journal |> Hledger.Journal.transactions
      in
      List.iter rows ~f:(fun (row : Store.bank_review) ->
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
    let rows = ok (Store.bank_review db) in
    check_journal_descriptions rows;
    List.iter rows ~f:(fun row ->
      Alcotest.(check string) "effective description" "Synthetic shop" row.description;
      Alcotest.(check (option string))
        "type"
        (Some "card")
        (Option.map row.movement_type ~f:Movement_type.to_string);
      check "source row recorded" 1 (List.length row.detail_sources));
    accepted (import db "other.csv" (replace (csv ()) "Synthetic shop" "Other shop"));
    ignore (ok (Store.match_companions ~institution:"wise" db) : Store.companion_report);
    check_companions ();
    let rows = ok (Store.bank_review db) in
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
    ignore (ok (Store.match_companions ~institution:"wise" db) : Store.companion_report);
    let rows = ok (Store.bank_review db) in
    check "all transactions imported" 3 (List.length rows);
    List.iter rows ~f:(fun row ->
      Alcotest.(check (option string))
        "default from the imported column"
        (Some "card")
        (Option.map row.movement_type ~f:Movement_type.to_string));
    let event_id = (List.hd_exn rows).event_id in
    ignore
      (ok
         (Store.set_movement_type_override
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
    ok (Store.add_classification_rule db rule);
    let types () =
      List.map
        (ok (Store.bank_review db))
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
    ok (Store.set_classification_rule_mode db "card-as-bonus" Disabled);
    List.iter (types ()) ~f:(fun (_, t) ->
      Alcotest.(check (option string)) "back to the column's rule" (Some "card") t);
    ok (Store.set_classification_rule_mode db "card-as-bonus" Automatic);
    List.iter (types ()) ~f:(fun (_, t) ->
      Alcotest.(check (option string)) "enabled again" (Some "bonus") t);
    let overridden =
      List.find_exn
        (ok (Store.bank_review db))
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
    ignore (ok (Store.match_companions ~institution:"wise" db) : Store.companion_report);
    List.iter
      (ok (Store.bank_review db))
      ~f:(fun row ->
        Alcotest.(check (option string))
          "an UNKNOWN code is kept as a provider-specific type"
          (Some "wise:UNKNOWN")
          (Option.map row.movement_type ~f:Movement_type.to_string)));
  memory (fun db ->
    let csv_other = replace (csv ()) ",CARD\n" ",NEW_CODE\n" in
    accepted (import db "other.csv" csv_other);
    accepted (import db "statement.xml" (xml ()));
    ignore (ok (Store.match_companions ~institution:"wise" db) : Store.companion_report);
    List.iter
      (ok (Store.bank_review db))
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
          ; "reusable CAMT source profile", reusable_camt_source
          ; "reusable clock CSV profile and importer", reusable_clock_csv
          ; "reported type rules, replay and refresh", reported_type_rules
          ; "unknown and provider-specific codes", reported_type_fallbacks
          ; "Ptime instants, raw text and stable identity", timestamp_precision
          ; "CSV UTC assumption and source-local matching", csv_times
          ; "two banks, replay, conflict", multi_bank
          ; "currency and fee conflicts", currencies
          ; "duplicate CSV claims", duplicate_rows
          ; "review enrichment and conflicting observations", review_enrichment
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
