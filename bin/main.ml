open Core
open Finances
open Cmdliner
open Or_error.Let_syntax

let read_file path = Or_error.try_with (fun () -> In_channel.read_all path)

let ingest database venue account kind source_id at path =
  let%bind payload = read_file path in
  let%bind occurred_at_ms = Or_error.try_with (fun () -> Int64.of_string at) in
  let%bind event =
    Source_event.create ~venue ~account ~kind ~source_id ~occurred_at_ms ~payload
  in
  Store.with_open database (fun db ->
    let%bind result = Store.ingest db event in
    let status, id =
      match result with
      | Store.Inserted id -> "inserted", id
      | Existing id -> "existing", id
    in
    Printf.printf "%s %Ld\n" status id;
    Ok ())
;;

let list_events database =
  Store.with_open database (fun db ->
    let%bind events = Store.events db in
    let json =
      `List
        (List.map events ~f:(fun (id, (e : Source_event.t)) ->
           `Assoc
             [ "id", `Intlit (Int64.to_string id)
             ; "venue", `String e.venue
             ; "account", `String e.account
             ; "kind", `String e.kind
             ; "source_id", `String e.source_id
             ; ( "occurred_at_ms"
               , match e.occurred with
                 | At_ms ms -> `Intlit (Int64.to_string ms)
                 | On_date _ -> `Null )
             ; ( "occurred_on"
               , match e.occurred with
                 | On_date date -> `String (Date.to_string date)
                 | At_ms _ -> `Null )
             ]))
    in
    Out_channel.print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let database =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"DATABASE" ~doc:"SQLite database path.")
;;

let required name doc = Arg.(required & opt (some string) None & info [ name ] ~doc)

let command name doc term =
  Cmd.v
    (Cmd.info name ~doc)
    Term.(
      const (fun result ->
        Result.map_error result ~f:(fun e -> `Msg (Error.to_string_hum e)))
      $ term
      |> term_result)
;;

let init =
  command
    "init"
    "Create or migrate a database."
    Term.(const (fun path -> Store.with_open path (fun _ -> Ok ())) $ database)
;;

let events =
  command
    "events"
    "List imported source event metadata as JSON."
    Term.(const list_events $ database)
;;

let ingest_cmd =
  command
    "ingest"
    "Store an immutable source payload; identical retries are safe."
    Term.(
      const ingest
      $ database
      $ required "venue" "Source venue, for example paradex."
      $ required "account" "Source account identifier."
      $ required "kind" "Source record kind, for example fill."
      $ required "id" "Stable source record identifier."
      $ required "at-ms" "Event timestamp in Unix milliseconds."
      $ Arg.(
          required
          & pos 1 (some string) None
          & info [] ~docv:"PAYLOAD" ~doc:"File containing the unmodified source payload."))
;;

let import_ofx database institution path =
  let%bind contents = read_file path in
  let%bind document = Bank_ofx.parse contents in
  Store.with_open database (fun db ->
    let%bind report = Store.import_ofx db ~institution document in
    printf
      "statements=%d inserted=%d existing=%d document=%Ld\n"
      report.statements
      report.inserted
      report.existing
      report.document_id;
    Ok ())
;;

let import_ofx_cmd =
  command
    "import-ofx"
    "Import all bank statements from an OFX 2.x XML export."
    Term.(
      const import_ofx
      $ database
      $ Arg.(
          value
          & opt string "boursobank"
          & info
              [ "institution" ]
              ~doc:"Stable institution identifier used to scope source identities.")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"OFX_FILE"))
;;

let project_bank database =
  Store.with_open database (fun db ->
    let%bind report = Store.project_bank db in
    printf "inserted=%d existing=%d\n" report.inserted report.existing;
    Ok ())
;;

let project_bank_cmd =
  command
    "project-bank"
    "Create balanced bank movement entries from stored events."
    Term.(const project_bank $ database)
;;

let export_hledger database =
  Store.with_open database (fun db ->
    let%bind journal = Finances_hledger.export db in
    Out_channel.output_string stdout journal;
    Ok ())
;;

let export_hledger_cmd =
  command
    "export-hledger"
    "Write a reproducible hledger journal to standard output."
    Term.(const export_hledger $ database)
;;

let account_info_json (info : Store.bank_account_info) =
  let a = info.mapping in
  [ "institution", `String a.institution
  ; "bank_id", `String a.bank_id
  ; "account_id", `String a.account_id
  ; "account_type", `String a.account_type
  ; "iban", Option.value_map info.iban ~default:`Null ~f:(fun s -> `String s)
  ]
;;

let list_bank_accounts database details =
  Store.with_open database (fun db ->
    if not details
    then (
      let%bind accounts = Store.bank_accounts db in
      print_endline (Bank_account_mapping.to_json accounts);
      Ok ())
    else (
      let%bind accounts = Store.bank_account_info db in
      let json =
        `List
          (List.map accounts ~f:(fun info ->
             `Assoc
               (account_info_json info
                @ [ "iban_source", `String info.Store.iban_source
                  ; "ledger_account", `String info.mapping.ledger_account
                  ; "currency", `String info.currency
                  ; "movements", `Int info.movements
                  ; "first_on", `String (Date.to_string info.first_on)
                  ; "last_on", `String (Date.to_string info.last_on)
                  ; ( "settlement_account"
                    , Option.value_map
                        info.settlement_account
                        ~default:`Null
                        ~f:Bank_account_config.reference_to_json )
                  ])))
      in
      print_endline (Yojson.Safe.pretty_to_string json);
      Ok ()))
;;

let bank_accounts_cmd =
  command
    "bank-accounts"
    "List source accounts and ledger names; --details includes IBAN provenance and \
     record coverage."
    Term.(
      const list_bank_accounts
      $ database
      $ Arg.(
          value
          & flag
          & info
              [ "details" ]
              ~doc:"Include IBAN, its source, currency and imported record coverage."))
;;

let bank_identities database =
  Store.with_open database (fun db ->
    let%bind rows = Store.bank_account_info db in
    print_endline
      (Yojson.Safe.pretty_to_string
         (`List (List.map rows ~f:(fun r -> `Assoc (account_info_json r)))));
    Ok ())
;;

let bank_identities_cmd =
  command
    "bank-identities"
    "Export an editable IBAN identity template; null means missing or not applicable."
    Term.(const bank_identities $ database)
;;

let map_bank_identities database path =
  let%bind contents = read_file path in
  let%bind identities = Account_identity.of_json contents in
  Store.with_open database (fun db ->
    let%bind changed = Store.set_bank_account_identities db identities in
    printf "identity_changes=%d\n" changed;
    Ok ())
;;

let map_bank_identities_cmd =
  command
    "map-bank-identities"
    "Persist and audit IBAN identities from a private JSON file."
    Term.(
      const map_bank_identities
      $ database
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"IDENTITIES"))
;;

let map_bank_accounts database path =
  let%bind contents = read_file path in
  let%bind mappings = Bank_account_mapping.of_json contents in
  Store.with_open database (fun db ->
    let%bind changed = Store.set_bank_account_mappings db mappings in
    printf "mapping_changes=%d\n" changed;
    Ok ())
;;

let map_bank_accounts_cmd =
  command
    "map-bank-accounts"
    "Store account mappings and rename verified generated bank postings."
    Term.(
      const map_bank_accounts
      $ database
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"ACCOUNTS_JSON"))
;;

let import_history database limit =
  Store.with_open database (fun db ->
    let%bind attempts = Store.bank_import_history ~limit db in
    let optional f value = Option.value_map value ~default:`Null ~f in
    let json =
      `List
        (List.map attempts ~f:(fun (row : Bank_import.file_report) ->
           `Assoc
             [ "attempt_id", `Intlit (Int64.to_string row.attempt_id)
             ; "institution", `String row.institution
             ; "path", `String row.path
             ; "format", `String (Bank_import.format_name row.format)
             ; "outcome", `String (Bank_import.outcome_name row.outcome)
             ; ( "document_id"
               , optional (fun id -> `Intlit (Int64.to_string id)) row.document_id )
             ; "sha256", optional (fun s -> `String s) row.sha256
             ; "inserted", `Int row.inserted
             ; "existing", `Int row.existing
             ; "message", optional (fun s -> `String s) row.message
             ; "attempted_at", `String row.attempted_at
             ]))
    in
    Out_channel.print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let import_history_cmd =
  command
    "import-history"
    "List recent inbox import attempts, newest first, as JSON."
    Term.(
      const import_history
      $ database
      $ Arg.(value & opt int 100 & info [ "limit" ] ~doc:"Maximum attempts (1-10000)."))
;;

let sync_bank database directory institution journal accounts max_mib account_id =
  if max_mib < 1 || max_mib > 1024
  then Or_error.error_string "max-file-mib must be between 1 and 1024"
  else (
    let journal =
      Option.value
        journal
        ~default:(Filename.concat (Filename.dirname database) "bank.journal")
    in
    let%bind report =
      Finances_bank.Sync.run
        ?account_id
        ?accounts
        ~max_file_bytes:Int64.(of_int max_mib * 1048576L)
        ~database
        ~institution
        ~directory
        ~journal
        ()
    in
    List.iter report.inbox.files ~f:(fun row ->
      printf
        "%s\t%s\tinserted=%d existing=%d%s\n"
        (Bank_import.outcome_name row.outcome)
        row.path
        row.inserted
        row.existing
        (Option.value_map row.message ~default:"" ~f:(fun message -> "\t" ^ message)));
    List.iter report.inbox.ignored ~f:(fun row ->
      printf "ignored\t%s\t%s\n" row.path row.reason);
    let count outcome =
      List.count report.inbox.files ~f:(fun row -> Poly.equal row.outcome outcome)
    in
    printf
      "imported=%d unchanged=%d rejected=%d unsupported=%d unreadable=%d ignored=%d\n"
      (count Imported)
      (count Unchanged)
      (count Rejected)
      (count Unsupported)
      (count Unreadable)
      (List.length report.inbox.ignored);
    printf
      "mapping_changes=%d entries_inserted=%d entries_existing=%d\njournal=%s\n"
      report.mapping_changes
      report.projection.inserted
      report.projection.existing
      report.journal;
    printf
      "identity_changes=%d settlement_changes=%d\n"
      report.identity_changes
      report.settlement_changes;
    printf
      "csv_matched=%d csv_unmatched=%d csv_ambiguous=%d csv_conflicts=%d csv_changes=%d\n"
      report.csv.matched
      report.csv.unmatched
      report.csv.ambiguous
      report.csv.conflicts
      report.csv.changed;
    printf
      "wise_csv_matched=%d wise_csv_unmatched=%d wise_csv_ambiguous=%d \
       wise_csv_conflicts=%d wise_csv_changes=%d\n"
      report.wise_csv.matched
      report.wise_csv.unmatched
      report.wise_csv.ambiguous
      report.wise_csv.conflicts
      report.wise_csv.changed;
    printf
      "bunq_csv_matched=%d bunq_csv_unmatched=%d bunq_csv_ambiguous=%d \
       bunq_csv_conflicts=%d bunq_csv_changes=%d\n"
      report.bunq_csv.matched
      report.bunq_csv.unmatched
      report.bunq_csv.ambiguous
      report.bunq_csv.conflicts
      report.bunq_csv.changed;
    if count Rejected + count Unreadable > 0
    then
      Or_error.error_string
        "some inputs need review; successful inputs were imported and the journal was \
         generated"
    else Ok ())
;;

let sync_bank_cmd =
  command
    "sync-bank"
    "Import an inbox, apply mappings, and publish a bank journal."
    Term.(
      const sync_bank
      $ database
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"INBOX")
      $ Arg.(
          value
          & opt string "boursobank"
          & info [ "institution" ] ~doc:"Stable source institution identifier.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "journal" ]
              ~doc:"Output path (default bank.journal beside the database).")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "accounts" ]
              ~doc:"Versioned account configuration or legacy ledger mapping JSON input.")
      $ Arg.(
          value
          & opt int 64
          & info
              [ "max-file-mib" ]
              ~doc:"Per-file read limit in MiB (1-1024, default 64).")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "account-id" ]
              ~doc:"Stable Revolut account label, required for its account-free CSV."))
;;

let csv_enrichments database review =
  Store.with_open database (fun db ->
    let%bind observations = Store.bank_csv_observations db in
    let optional f value = Option.value_map value ~default:`Null ~f in
    let string s = `String s in
    let id n = `Intlit (Int64.to_string n) in
    let observations =
      if not review
      then observations
      else
        List.filter observations ~f:(fun row ->
          not
            (Option.exists row.Store.matching ~f:(fun matching ->
               Poly.equal matching.state Bank_csv_match.Matched)))
    in
    let rows =
      List.map observations ~f:(fun observation ->
        let row = observation.Store.row in
        `Assoc
          [ "row_id", id observation.row_id
          ; "document_id", id observation.document_id
          ; "row_number", `Int row.row_number
          ; "institution", string observation.institution
          ; "account_id", string row.account_id
          ; "booked_on", string (Date.to_string row.booked_on)
          ; "value_on", optional (fun date -> string (Date.to_string date)) row.value_on
          ; "amount", string (Bigdecimal.to_string_no_sn row.amount)
          ; "description", string row.description
          ; "suggested_description", optional string row.suggested_description
          ; "category", optional string row.category
          ; "parent_category", optional string row.parent_category
          ; "comment", optional string row.comment
          ; "account_label", optional string row.account_label
          ; ( "reported_balance"
            , optional
                (fun amount -> string (Bigdecimal.to_string_no_sn amount))
                row.reported_balance )
          ; "pointage", optional string row.pointage
          ; ( "event_id"
            , optional id (Option.bind observation.matching ~f:(fun m -> m.event_id)) )
          ; ( "state"
            , string
                (Option.value_map observation.matching ~default:"pending" ~f:(fun m ->
                   Bank_csv_match.state_name m.state)) )
          ; ( "reason"
            , optional string (Option.map observation.matching ~f:(fun m -> m.reason)) )
          ; ( "candidate_event_ids"
            , `List
                (List.map
                   (Option.value_map observation.matching ~default:[] ~f:(fun m ->
                      m.candidates))
                   ~f:id) )
          ])
    in
    Out_channel.print_endline (Yojson.Safe.pretty_to_string (`List rows));
    Ok ())
;;

let csv_enrichments_cmd =
  command
    "csv-enrichments"
    "List CSV observations, enrichment links, and unresolved matches as JSON."
    Term.(
      const csv_enrichments
      $ database
      $ Arg.(
          value
          & flag
          & info
              [ "review" ]
              ~doc:"Only unmatched, ambiguous, conflicting or pending observations."))
;;

let match_csv database =
  Store.with_open database (fun db ->
    let%bind result = Store.match_bank_csv db in
    printf
      "matched=%d unmatched=%d ambiguous=%d conflicts=%d changed=%d\n"
      result.matched
      result.unmatched
      result.ambiguous
      result.conflicts
      result.changed;
    Ok ())
;;

let match_csv_cmd =
  command
    "match-bank-csv"
    "Recompute and audit CSV enrichment matches against stored OFX evidence."
    Term.(const match_csv $ database)
;;

let wise_status database =
  Store.with_open database (fun db ->
    let%bind statements = Store.wise_reconciliations db in
    let%bind rows = Store.wise_csv_observations db in
    let string s = `String s in
    let json =
      `Assoc
        [ ( "statements"
          , `List
              (List.map statements ~f:(fun (s : Store.wise_reconciliation) ->
                 `Assoc
                   [ "statement_id", `Intlit (Int64.to_string s.statement_id)
                   ; "document_id", `Intlit (Int64.to_string s.document_id)
                   ; "currency", string s.currency
                   ; "source_account", string s.source_account
                   ; "opening", string (Bigdecimal.to_string_no_sn s.opening)
                   ; "closing", string (Bigdecimal.to_string_no_sn s.closing)
                   ; "movements", string (Bigdecimal.to_string_no_sn s.movements)
                   ; "entries", `Int s.entries
                   ; "difference", string (Bigdecimal.to_string_no_sn s.difference)
                   ; "reconciled", `Bool (Bigdecimal.is_zero s.difference)
                   ])) )
        ; ( "csv_rows"
          , `List
              (List.map rows ~f:(fun (r : Store.wise_csv_observation) ->
                 `Assoc
                   [ "row_id", `Intlit (Int64.to_string r.row_id)
                   ; "document_id", `Intlit (Int64.to_string r.document_id)
                   ; "reference", string r.row.reference
                   ; ( "state"
                     , string
                         (Option.value_map r.matching ~default:"pending" ~f:(fun m ->
                            Bank_csv_match.state_name m.state)) )
                   ; ( "reason"
                     , Option.value_map r.matching ~default:`Null ~f:(fun m ->
                         string m.reason) )
                   ])) )
        ]
    in
    print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let wise_status_cmd =
  command
    "wise-status"
    "Inspect Wise statement reconciliation and CSV matching as JSON."
    Term.(const wise_status $ database)
;;

let categorize_bank database path =
  let%bind contents = read_file path in
  let%bind decisions = Bank_category.of_json contents in
  Store.with_open database (fun db ->
    let%bind changed = Store.set_bank_categories db decisions in
    printf "category_changes=%d\n" changed;
    Ok ())
;;

let categorize_bank_cmd =
  command
    "categorize-bank"
    "Apply a JSON batch of reviewed bank counter-accounts atomically."
    Term.(
      const categorize_bank
      $ database
      $ Arg.(
          required
          & pos 1 (some string) None
          & info [] ~docv:"DECISIONS" ~doc:"JSON decisions: event_id, account, reason."))
;;

let bank_review database all =
  Store.with_open database (fun db ->
    let%bind rows = Store.bank_review db in
    let rows =
      List.filter rows ~f:(fun r ->
        all || String.equal r.Store.counter_account "equity:unclassified")
    in
    let json =
      `List
        (List.map rows ~f:(fun r ->
           let tx = r.Store.transaction in
           `Assoc
             [ "event_id", `Intlit (Int64.to_string r.event_id)
             ; "institution", `String r.institution
             ; "source_account", `String tx.account.account_id
             ; "booked_on", `String (Date.to_string tx.booked_on)
             ; "amount", `String (Bigdecimal.to_string_no_sn tx.amount)
             ; "fee", `String (Bigdecimal.to_string_no_sn tx.fee)
             ; "currency", `String tx.account.currency
             ; ( "description"
               , `String (String.concat ~sep:" | " (List.filter_opt [ tx.name; tx.memo ]))
               )
             ; "account", `String r.counter_account
             ; "reason", Option.value_map r.reason ~default:`Null ~f:(fun s -> `String s)
             ]))
    in
    print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let bank_review_cmd =
  command
    "bank-review"
    "List unclassified bank movements as JSON for review."
    Term.(
      const bank_review
      $ database
      $ Arg.(
          value & flag & info [ "all" ] ~doc:"Include movements with reviewed categories."))
;;

let category_history database =
  Store.with_open database (fun db ->
    let%bind rows = Store.bank_category_history db in
    let json =
      `List
        (List.map rows ~f:(fun (r : Store.category_change) ->
           `Assoc
             [ "id", `Intlit (Int64.to_string r.id)
             ; "event_id", `Intlit (Int64.to_string r.event_id)
             ; ( "previous_account"
               , Option.value_map r.previous_account ~default:`Null ~f:(fun s ->
                   `String s) )
             ; "account", `String r.account
             ; "reason", `String r.reason
             ; "changed_at", `String r.changed_at
             ]))
    in
    print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let category_history_cmd =
  command
    "category-history"
    "List the append-only history of reviewed bank categories."
    Term.(const category_history $ database)
;;

let bunq_status database =
  Store.with_open database (fun db ->
    let%bind statements = Store.bunq_reconciliations db in
    let%bind rows = Store.bunq_csv_observations db in
    let json =
      `Assoc
        [ ( "statements"
          , `List
              (List.map statements ~f:(fun (s : Store.bunq_reconciliation) ->
                 `Assoc
                   [ "document_id", `Intlit (Int64.to_string s.document_id)
                   ; "account_id", `String s.account_id
                   ; "currency", `String s.currency
                   ; "entries", `Int s.entries
                   ; "difference", `String (Bigdecimal.to_string_no_sn s.difference)
                   ; "reconciled", `Bool (Bigdecimal.is_zero s.difference)
                   ])) )
        ; ( "csv_rows"
          , `List
              (List.map rows ~f:(fun (r : Store.bunq_csv_observation) ->
                 `Assoc
                   [ "row_id", `Intlit (Int64.to_string r.row_id)
                   ; "document_id", `Intlit (Int64.to_string r.document_id)
                   ; "row_number", `Int r.row.row_number
                   ; ( "state"
                     , `String
                         (Option.value_map r.matching ~default:"pending" ~f:(fun m ->
                            Bank_match.state_name m.state)) )
                   ; ( "reason"
                     , Option.value_map r.matching ~default:`Null ~f:(fun m ->
                         `String m.reason) )
                   ])) )
        ]
    in
    print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let bunq_status_cmd =
  command
    "bunq-status"
    "Inspect Bunq statement reconciliation and companion CSV matches as JSON."
    Term.(const bunq_status $ database)
;;

let transfer_leg event_id (event : Source_event.t) (tx : Bank_transaction.t) =
  `Assoc
    [ "event_id", `Intlit (Int64.to_string event_id)
    ; "institution", `String event.venue
    ; "source_account", `String event.account
    ; "booked_on", `String (Date.to_string tx.booked_on)
    ; "currency", `String tx.account.currency
    ; "bank_amount", `String (Bigdecimal.to_string_no_sn tx.amount)
    ; "principal", `String (Bigdecimal.to_string_no_sn Bigdecimal.(tx.amount + tx.fee))
    ; "fee", `String (Bigdecimal.to_string_no_sn tx.fee)
    ; ( "description"
      , `String (String.concat ~sep:" | " (List.filter_opt [ tx.name; tx.memo ])) )
    ]
;;

let transfer_candidates database all =
  Store.with_open database (fun db ->
    let%bind rows = Store.transfer_candidates ~include_reviewed:all db in
    let json =
      `List
        (List.map rows ~f:(fun (r : Store.transfer_candidate) ->
           `Assoc
             [ "first", transfer_leg r.first_event_id r.first r.first_transaction
             ; "second", transfer_leg r.second_event_id r.second r.second_transaction
             ; "days_apart", `Int r.days_apart
             ; "unique_both_directions", `Bool r.unique
             ; "iban_compatible", `Bool r.iban_compatible
             ; "iban_match", `Bool r.iban_match
             ; "iban_evidence", `List (List.map r.iban_evidence ~f:(fun s -> `String s))
             ; ( "state"
               , Option.value_map r.state ~default:(`String "unreviewed") ~f:(fun s ->
                   `String (Bank_transfer.state_name s)) )
             ; "reason", Option.value_map r.reason ~default:`Null ~f:(fun s -> `String s)
             ]))
    in
    print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let transfer_candidates_cmd =
  command
    "transfer-candidates"
    "Review possible same-currency transfers; amount/date matches do not establish \
     ownership."
    Term.(
      const transfer_candidates
      $ database
      $ Arg.(
          value
          & flag
          & info
              [ "all" ]
              ~doc:"Include reviewed pairs and movements already assigned to transfers."))
;;

let review_transfers database path =
  let%bind contents = read_file path in
  let%bind decisions = Bank_transfer.of_json contents in
  Store.with_open database (fun db ->
    let%bind changed = Store.set_bank_transfers db decisions in
    printf "transfer_changes=%d\n" changed;
    Ok ())
;;

let review_transfers_cmd =
  command
    "review-transfers"
    "Apply explicit JSON confirm/reject decisions; confirmation asserts both accounts \
     are yours."
    Term.(
      const review_transfers
      $ database
      $ Arg.(
          required
          & pos 1 (some string) None
          & info [] ~docv:"DECISIONS" ~doc:"JSON transfer review decisions."))
;;

let transfer_history database =
  Store.with_open database (fun db ->
    let%bind rows = Store.bank_transfer_history db in
    let json =
      `List
        (List.map rows ~f:(fun (r : Store.transfer_change) ->
           `Assoc
             [ "id", `Intlit (Int64.to_string r.id)
             ; "first_event_id", `Intlit (Int64.to_string r.first_event_id)
             ; "second_event_id", `Intlit (Int64.to_string r.second_event_id)
             ; ( "previous_state"
               , Option.value_map r.previous_state ~default:`Null ~f:(fun s -> `String s)
               )
             ; "state", `String (Bank_transfer.state_name r.state)
             ; "reason", `String r.reason
             ; "changed_at", `String r.changed_at
             ]))
    in
    print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let transfer_history_cmd =
  command
    "transfer-history"
    "Inspect the append-only transfer review history."
    Term.(const transfer_history $ database)
;;

let transfer_preview database =
  Store.with_open database (fun db ->
    let%bind candidates = Store.transfer_candidates db in
    let%bind all_candidates = Store.transfer_candidates ~include_reviewed:true db in
    let%bind ranked = Transfer_ranking.analyze candidates in
    let id x = `Intlit (Int64.to_string x) in
    let optional_int = Option.value_map ~default:`Null ~f:(fun x -> `Int x) in
    let json =
      `Assoc
        [ "policy", `String Transfer_ranking.policy
        ; ( "notice"
          , `String
              "Heuristic evidence, not calibrated probabilities. Conditional on imported \
               records, registered identities and the seven-day candidate policy. Review \
               is required; nothing is confirmed automatically." )
        ; "component_vertex_limit", `Int Transfer_ranking.max_component_vertices
        ; ( "excluded_pairs"
          , `List
              (List.filter_map all_candidates ~f:(fun c ->
                 if c.Store.iban_compatible
                 then None
                 else
                   Some
                     (`Assoc
                         [ "first_event_id", id c.first_event_id
                         ; "second_event_id", id c.second_event_id
                         ; ( "reasons"
                           , `List (List.map c.iban_evidence ~f:(fun s -> `String s)) )
                         ]))) )
        ; ( "proposed_pairs"
          , `List
              (List.filter_map ranked ~f:(fun r ->
                 if String.equal r.Transfer_ranking.assignment.status "supported"
                 then
                   Some
                     (`Assoc
                         [ "first_event_id", id r.candidate.first_event_id
                         ; "second_event_id", id r.candidate.second_event_id
                         ; "confidence", `String r.confidence
                         ; "review_required", `Bool true
                         ])
                 else None)) )
        ; ( "candidates"
          , `List
              (List.map ranked ~f:(fun r ->
                 let c = r.Transfer_ranking.candidate in
                 `Assoc
                   [ "first", transfer_leg c.first_event_id c.first c.first_transaction
                   ; ( "second"
                     , transfer_leg c.second_event_id c.second c.second_transaction )
                   ; "score", `Int r.score
                   ; "confidence", `String r.confidence
                   ; "iban_match", `Bool c.iban_match
                   ; ( "evidence"
                     , `List
                         (List.map r.evidence ~f:(fun e ->
                            `Assoc
                              [ "code", `String e.Transfer_ranking.code
                              ; "points", `Int e.points
                              ; "detail", `String e.detail
                              ])) )
                   ; ( "iban_evidence"
                     , `List (List.map c.iban_evidence ~f:(fun s -> `String s)) )
                   ; "component", `Int r.assignment.component
                   ; "assignment_status", `String r.assignment.status
                   ; "component_best_score", optional_int r.assignment.best_score
                   ; "exclusion_margin", optional_int r.assignment.exclusion_margin
                   ; "inclusion_penalty", optional_int r.assignment.inclusion_penalty
                   ])) )
        ]
    in
    print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let transfer_preview_cmd =
  command
    "transfer-preview"
    "Rank compatible transfer candidates and preview globally consistent pairs; scores \
     are not probabilities."
    Term.(const transfer_preview $ database)
;;

let accounts_export database =
  Store.with_open database (fun db ->
    let%bind config = Store.export_bank_accounts db in
    print_endline (Bank_account_config.to_json config);
    Ok ())
;;

let account_change_json (r : Store.account_change) =
  let value =
    Option.value_map ~default:`Null ~f:(fun text ->
      if String.equal r.kind "settlement_account"
      then Yojson.Safe.from_string text
      else `String text)
  in
  `Assoc
    [ "id", `Intlit (Int64.to_string r.id)
    ; "kind", `String r.kind
    ; "institution", `String r.institution
    ; "source_account", `String r.source_account
    ; "previous_value", value r.previous_value
    ; "value", value r.value
    ; "changed_at", `String r.changed_at
    ]
;;

let accounts_apply database path dry_run =
  let%bind contents = read_file path in
  let%bind config = Bank_account_config.of_json contents in
  Store.with_open database (fun db ->
    let%bind result = Store.apply_bank_accounts ~dry_run db config in
    print_endline
      (Yojson.Safe.pretty_to_string
         (`Assoc
             [ "dry_run", `Bool dry_run
             ; "mapping_changes", `Int result.mapping_changes
             ; "identity_changes", `Int result.identity_changes
             ; "settlement_changes", `Int result.settlement_changes
             ; "changes", `List (List.map result.changes ~f:account_change_json)
             ]));
    Ok ())
;;

let accounts_history database =
  Store.with_open database (fun db ->
    let%bind rows = Store.bank_account_history db in
    print_endline
      (Yojson.Safe.pretty_to_string (`List (List.map rows ~f:account_change_json)));
    Ok ())
;;

let accounts_cmd =
  Cmd.group
    (Cmd.info
       "accounts"
       ~doc:"Manage discovered bank accounts through SQLite and versioned configuration.")
    [ command
        "list"
        "List account details, IBAN provenance, coverage and card settlement links."
        Term.(const (fun db -> list_bank_accounts db true) $ database)
    ; command
        "export"
        "Export version 1 account configuration for editing."
        Term.(const accounts_export $ database)
    ; command
        "apply"
        "Apply the whole account configuration atomically, or preview it with --dry-run."
        Term.(
          const accounts_apply
          $ database
          $ Arg.(required & pos 1 (some string) None & info [] ~docv:"CONFIG")
          $ Arg.(
              value
              & flag
              & info
                  [ "dry-run" ]
                  ~doc:
                    "Validate and report changes without committing updates or audit \
                     rows."))
    ; command
        "history"
        "Inspect audited ledger name, IBAN and settlement relationship changes."
        Term.(const accounts_history $ database)
    ]
;;

let () =
  exit
    (Cmd.eval
       (Cmd.group
          (Cmd.info "finances" ~doc:"Personal finance accounting and position tracking.")
          [ accounts_cmd
          ; init
          ; ingest_cmd
          ; events
          ; import_ofx_cmd
          ; project_bank_cmd
          ; export_hledger_cmd
          ; bank_accounts_cmd
          ; bank_identities_cmd
          ; map_bank_identities_cmd
          ; map_bank_accounts_cmd
          ; import_history_cmd
          ; sync_bank_cmd
          ; csv_enrichments_cmd
          ; match_csv_cmd
          ; wise_status_cmd
          ; bunq_status_cmd
          ; categorize_bank_cmd
          ; bank_review_cmd
          ; category_history_cmd
          ; transfer_candidates_cmd
          ; transfer_preview_cmd
          ; review_transfers_cmd
          ; transfer_history_cmd
          ]))
;;
