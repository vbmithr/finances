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

let list_bank_accounts database =
  Store.with_open database (fun db ->
    let%bind accounts = Store.bank_accounts db in
    Out_channel.print_endline (Bank_account_mapping.to_json accounts);
    Ok ())
;;

let bank_accounts_cmd =
  command
    "bank-accounts"
    "List source bank accounts and current ledger names as mapping JSON."
    Term.(const list_bank_accounts $ database)
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

let sync_bank database directory institution journal accounts max_mib =
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
          & info [ "accounts" ] ~doc:"Optional account mapping JSON input.")
      $ Arg.(
          value
          & opt int 64
          & info
              [ "max-file-mib" ]
              ~doc:"Per-file read limit in MiB (1-1024, default 64)."))
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

let () =
  exit
    (Cmd.eval
       (Cmd.group
          (Cmd.info "finances" ~doc:"Personal finance accounting and position tracking.")
          [ init
          ; ingest_cmd
          ; events
          ; import_ofx_cmd
          ; project_bank_cmd
          ; export_hledger_cmd
          ; bank_accounts_cmd
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
          ]))
;;
