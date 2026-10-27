open Base
open Stdio
open Finances
open Cmdliner
open Or_error.Let_syntax

let paths =
  let home =
    match Sys.getenv "HOME" with
    | Some home when not (Stdlib.Filename.is_relative home) -> home
    | _ -> (Unix.getpwuid (Unix.getuid ())).pw_dir
  in
  App_paths.resolve ~home ~getenv:Sys.getenv
;;

let ensure_database_parent path =
  if String.equal path ":memory:"
  then Ok ()
  else (
    let%bind parent =
      Or_error.try_with (fun () -> Fpath.v (Stdlib.Filename.dirname path))
    in
    Bos.OS.Dir.create ~mode:0o700 parent
    |> Result.map_error ~f:(fun (`Msg message) -> Error.of_string message)
    |> Result.map ~f:(fun _ -> ()))
;;

let with_database path f =
  let%bind () = ensure_database_parent path in
  Store.with_open ~registry:Builtin.registry path f
;;

let write_file path contents =
  let%bind () = ensure_database_parent path in
  let%bind path =
    Or_error.try_with (fun () -> Fpath.v (if String.equal path "-" then "./-" else path))
  in
  Bos.OS.File.write ~mode:0o600 path contents
  |> Result.map_error ~f:(fun (`Msg message) -> Error.of_string message)
;;

let read_file path =
  let%bind path =
    Or_error.try_with (fun () -> Fpath.v (if String.equal path "-" then "./-" else path))
  in
  Bos.OS.File.read path
  |> Result.map_error ~f:(fun (`Msg message) -> Error.of_string message)
;;

let ingest database venue account kind source_id at path =
  let%bind payload = read_file path in
  let%bind occurred_at_ms = Or_error.try_with (fun () -> Int64.of_string at) in
  let%bind event =
    Source_event.create ~venue ~account ~kind ~source_id ~occurred_at_ms ~payload
  in
  with_database database (fun db ->
    let%bind result = Store.ingest db event in
    let status, id =
      match result with
      | Store.Inserted id -> "inserted", id
      | Existing id -> "existing", id
    in
    Stdio.printf "%s %Ld\n" status id;
    Ok ())
;;

let list_events database =
  with_database database (fun db ->
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
    value
    & opt string paths.database
    & info
        [ "d"; "database" ]
        ~docv:"DATABASE"
        ~doc:"SQLite database (default: XDG data directory; see fin paths).")
;;

let required name doc = Arg.(required & opt (some string) None & info [ name ] ~doc)

let json_output =
  Arg.(
    value & flag & info [ "json" ] ~doc:"Write JSON for scripts instead of readable text.")
;;

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
    "Initialize the latest experimental database schema."
    Term.(const (fun path -> with_database path (fun _ -> Ok ())) $ database)
;;

let welcome () =
  print_endline
    "fin - your personal finances\n\n\
     Get started:\n\
     \032 fin init                Set up your database (optional)\n\
     \032 fin bank sync --help    Learn how to import bank statements\n\n\
     Explore your finances:\n\
     \032 fin status              See import coverage and review progress\n\
     \032 fin accounts list       See your bank accounts\n\
     \032 fin bank review         Review imported transactions\n\
     \032 fin journal export      Produce an hledger journal\n\n\
     More:\n\
     \032 fin paths               Show data and config locations\n\
     \032 fin --help              Explore all commands"
;;

let setup database =
  let%bind () = with_database database (fun _ -> Ok ()) in
  let database_option =
    if String.equal database paths.database
    then ""
    else " --database " ^ Stdlib.Filename.quote database
  in
  Stdio.printf
    "fin is ready.\n\n\
     Database: %s\n\
     Account config (optional): %s\n\n\
     Import your first statements, for example:\n\
     \032 fin bank sync ~/Statements/boursobank --institution boursobank%s\n\n\
     Accounts appear when you import statements. Then explore:\n\
     \032 fin accounts list%s\n\
     \032 fin bank review%s\n\n\
     See fin bank sync --help for import options.\n\
     You can repeat fin init safely; existing data and config are preserved.\n"
    database
    paths.accounts_config
    database_option
    database_option
    database_option;
  Ok ()
;;

let setup_cmd =
  command
    "init"
    "Set up the database and show how to import your first statements."
    Term.(const setup $ database)
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
          & pos 0 (some string) None
          & info [] ~docv:"PAYLOAD" ~doc:"File containing the unmodified source payload."))
;;

let project_bank database =
  with_database database (fun db ->
    let%bind report = Store.project_bank db in
    printf "inserted=%d existing=%d\n" report.inserted report.existing;
    Ok ())
;;

let project_bank_cmd =
  command
    "project"
    "Create balanced bank movement entries from stored events."
    Term.(const project_bank $ database)
;;

let export_hledger database =
  with_database database (fun db ->
    let%bind journal = Finances_hledger.export db in
    Out_channel.output_string stdout journal;
    Ok ())
;;

let export_hledger_cmd =
  command
    "export"
    "Write a reproducible hledger journal to standard output."
    Term.(const export_hledger $ database)
;;

let account_info_json (info : Store.bank_account_info) =
  let a = info.mapping in
  [ "institution", `String a.institution
  ; "bank_id", `String a.bank_id
  ; "account_id", `String a.account_id
  ; "account_type", `String a.account_type
  ; ( "iban"
    , Option.value_map info.iban ~default:`Null ~f:(fun iban ->
        `String (Iban.to_string iban)) )
  ]
;;

let account_details_json info =
  `Assoc
    (account_info_json info
     @ [ "iban_source", `String info.Store.iban_source
       ; "ledger_account", `String info.mapping.ledger_account
       ; "currency", `String (Iso4217.to_alpha_code info.currency)
       ; "movements", `Int info.movements
       ; "first_on", `String (Date.to_string info.first_on)
       ; "last_on", `String (Date.to_string info.last_on)
       ; ( "settlement_account"
         , Option.value_map
             info.settlement_account
             ~default:`Null
             ~f:Bank_account_config.reference_to_json )
       ])
;;

let list_bank_accounts database json_output =
  with_database database (fun db ->
    let%bind accounts = Store.bank_account_info db in
    let json = `List (List.map accounts ~f:account_details_json) in
    if json_output
    then print_endline (Yojson.Safe.pretty_to_string json)
    else if List.is_empty accounts
    then
      print_endline
        "No bank accounts yet. Import statements with fin bank sync; see fin bank sync \
         --help."
    else List.iter accounts ~f:Terminal.account;
    Ok ())
;;

let status database json_output =
  let module Status = Finances_bank.Bank_status in
  let initialized = String.equal database ":memory:" || Stdlib.Sys.file_exists database in
  let%bind report =
    if initialized then with_database database Status.read else Ok Status.empty
  in
  let banks =
    List.map report.accounts ~f:(fun a -> a.Store.mapping.institution)
    |> List.dedup_and_sort ~compare:String.compare
    |> List.length
  in
  if json_output
  then
    print_endline
      (Yojson.Safe.pretty_to_string
         (`Assoc
             [ "database", `String database
             ; "initialized", `Bool initialized
             ; "banks", `Int banks
             ; "accounts", `List (List.map report.accounts ~f:account_details_json)
             ; "movements", `Int report.movements
             ; "unclassified_movements", `Int report.unclassified_movements
             ; "transfer_candidate_pairs", `Int report.transfer_candidate_pairs
             ; ( "latest_import"
               , Option.value_map report.latest_import ~default:`Null ~f:(fun a ->
                   `Assoc
                     [ "attempted_at", `String a.attempted_at
                     ; "institution", `String a.institution
                     ; "path", `String a.path
                     ; "outcome", `String (Bank_import.outcome_name a.outcome)
                     ; ( "message"
                       , Option.value_map a.message ~default:`Null ~f:(fun s -> `String s)
                       )
                     ]) )
             ]))
  else (
    printf "Database: %s\n\n" (Terminal.text database);
    if not initialized
    then
      print_endline
        "No database yet. Run fin init, or import statements with fin bank sync."
    else (
      printf
        "%d banks, %d accounts, %d imported movements\n\n"
        banks
        (List.length report.accounts)
        report.movements;
      if List.is_empty report.accounts
      then print_endline "No bank records yet. See fin bank sync --help."
      else
        Terminal.table
          [ "Bank"
          ; "Account"
          ; "Currency"
          ; "Movements"
          ; "First observed"
          ; "Last observed"
          ]
          (List.map report.accounts ~f:(fun a ->
             [ a.Store.mapping.institution
             ; a.mapping.ledger_account
             ; Iso4217.to_alpha_code a.currency
             ; Int.to_string a.movements
             ; Date.to_string a.first_on
             ; Date.to_string a.last_on
             ]));
      printf
        "\nReview: %d unclassified movements; %d unreviewed transfer candidate pairs\n"
        report.unclassified_movements
        report.transfer_candidate_pairs;
      (match report.latest_import with
       | None -> print_endline "Latest inbox import: none recorded."
       | Some a ->
         printf
           "Latest inbox import: %s  %s  %s\n  %s\n"
           a.attempted_at
           (Terminal.text a.institution)
           (Bank_import.outcome_name a.outcome)
           (Terminal.text a.path);
         Option.iter a.message ~f:(fun message -> printf "  %s\n" (Terminal.text message)));
      print_endline
        "\n\
         Date ranges show observed records, not complete statement coverage. Financial \
         reports: fin journal export, then query with hledger.";
      print_endline "Next: fin bank review; fin transfers preview; fin bank history."));
  Ok ()
;;

let status_cmd =
  command
    "status"
    "Show observed account coverage, import history and review progress; no balances are \
     inferred."
    Term.(const status $ database $ json_output)
;;

let import_history database limit json_output =
  with_database database (fun db ->
    let%bind attempts = Store.bank_import_history ~limit db in
    let optional f value = Option.value_map value ~default:`Null ~f in
    let json =
      `List
        (List.map attempts ~f:(fun (row : Bank_import.file_report) ->
           `Assoc
             [ "attempt_id", `Intlit (Int64.to_string row.attempt_id)
             ; "institution", `String row.institution
             ; "path", `String row.path
             ; "format", `String row.format
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
    if json_output
    then Out_channel.print_endline (Yojson.Safe.pretty_to_string json)
    else Terminal.import_history attempts;
    Ok ())
;;

let import_history_cmd =
  command
    "history"
    "List recent file import attempts, newest first; use --json for scripts."
    Term.(
      const import_history
      $ database
      $ Arg.(value & opt int 100 & info [ "limit" ] ~doc:"Maximum attempts (1-10000).")
      $ json_output)
;;

let sync_bank database directory institution journal accounts max_mib account_id =
  if max_mib < 1 || max_mib > 1024
  then Or_error.error_string "max-file-mib must be between 1 and 1024"
  else (
    let%bind () = ensure_database_parent database in
    let journal =
      Option.value
        journal
        ~default:
          (Stdlib.Filename.concat (Stdlib.Filename.dirname database) "bank.journal")
    in
    let%bind report =
      Finances_bank.Sync.run
        ~registry:Builtin.registry
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
      "companions_matched=%d companions_unmatched=%d companions_ambiguous=%d \
       companions_conflicts=%d companions_changes=%d\n"
      report.companions.matched
      report.companions.unmatched
      report.companions.ambiguous
      report.companions.conflicts
      report.companions.changed;
    if count Rejected + count Unreadable > 0
    then
      Or_error.error_string
        "some inputs need review; successful inputs were imported and the journal was \
         generated"
    else Ok ())
;;

let sync_bank_cmd =
  command
    "sync"
    "Import an inbox, apply mappings, and publish a bank journal."
    Term.(
      const sync_bank
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INBOX")
      $ Arg.(
          value
          & opt string "boursobank"
          & info
              [ "institution" ]
              ~doc:
                ("Source institution: one of "
                 ^ String.concat ~sep:", " (Registry.institutions Builtin.registry)
                 ^ "."))
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "journal" ]
              ~doc:"Output path (default bank.journal beside the database).")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "accounts" ] ~doc:"Version 1 bank account configuration JSON input.")
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

let statement_json (s : Store.statement_reconciliation) =
  `Assoc
    [ "statement_id", `Intlit (Int64.to_string s.statement_id)
    ; "document_id", `Intlit (Int64.to_string s.document_id)
    ; "institution", `String s.institution
    ; "source_account", `String s.source_account
    ; "reference", Option.value_map s.reference ~default:`Null ~f:(fun r -> `String r)
    ; "currency", `String (Iso4217.to_alpha_code s.currency)
    ; "period_start", `String s.period_start
    ; "period_end", `String s.period_end
    ; "opening", `String (Quantity.to_string s.opening)
    ; "closing", `String (Quantity.to_string s.closing)
    ; "movements", `String (Quantity.to_string s.movements)
    ; "entries", `Int s.entries
    ; "difference", `String (Quantity.to_string s.difference)
    ; "reconciled", `Bool (Quantity.is_zero s.difference)
    ]
;;

let institution_filter =
  Arg.(
    value
    & opt (some string) None
    & info
        [ "institution" ]
        ~docv:"INSTITUTION"
        ~doc:"Only this institution; all registered institutions by default.")
;;

let statements database institution =
  with_database database (fun db ->
    let%bind statements = Store.statement_reconciliations ?institution db in
    print_endline
      (Yojson.Safe.pretty_to_string
         (`Assoc [ "statements", `List (List.map statements ~f:statement_json) ]));
    Ok ())
;;

let statements_cmd =
  command
    "statements"
    "Show bank-stated balances against the movements each statement lists, as JSON."
    Term.(const statements $ database $ institution_filter)
;;

let companions database institution review =
  with_database database (fun db ->
    let%bind observations = Store.companion_observations ?institution db in
    let observations =
      if not review
      then observations
      else
        List.filter observations ~f:(fun o ->
          not
            (Option.exists o.Store.matching ~f:(fun m ->
               Poly.equal m.state Bank_match.Matched)))
    in
    let describers = Registry.companions (Store.registry db) in
    let id n = `Intlit (Int64.to_string n) in
    let optional f value = Option.value_map value ~default:`Null ~f in
    let%map rows =
      List.map observations ~f:(fun (o : Store.companion_observation) ->
        let row = o.row in
        let%bind (companion : Companion.t) =
          Or_error.of_option
            (List.find_map describers ~f:(fun (owner, c) ->
               Option.some_if
                 (String.equal owner row.institution
                  && String.equal c.Companion.format row.format)
                 c))
            ~error:(Error.of_string "companion row of an unregistered format")
        in
        let%map fields = companion.describe row in
        `Assoc
          ([ "row_id", id row.row_id
           ; "document_id", id row.document_id
           ; "row_number", `Int row.row_number
           ; "institution", `String row.institution
           ; "format", `String row.format
           ]
           @ List.map fields ~f:(fun (name, value) ->
             name, optional (fun s -> `String s) value)
           @ [ "event_id", optional id (Option.bind o.matching ~f:(fun m -> m.event_id))
             ; ( "state"
               , `String
                   (Option.value_map o.matching ~default:"pending" ~f:(fun m ->
                      Bank_match.state_name m.state)) )
             ; ( "reason"
               , optional
                   (fun s -> `String s)
                   (Option.map o.matching ~f:(fun m -> m.reason)) )
             ; ( "candidate_event_ids"
               , `List
                   (List.map
                      (Option.value_map o.matching ~default:[] ~f:(fun m -> m.candidates))
                      ~f:id) )
             ]))
      |> Or_error.combine_errors
    in
    Out_channel.print_endline (Yojson.Safe.pretty_to_string (`List rows)))
;;

let companions_cmd =
  command
    "companions"
    "List companion observations (CSV enrichment exports), their fields and matches, as \
     JSON."
    Term.(
      const companions
      $ database
      $ institution_filter
      $ Arg.(
          value
          & flag
          & info
              [ "review" ]
              ~doc:"Only unmatched, ambiguous, conflicting or pending observations."))
;;

let match_companions database institution =
  with_database database (fun db ->
    let%bind result = Store.match_companions ?institution db in
    printf
      "matched=%d unmatched=%d ambiguous=%d conflicts=%d changed=%d\n"
      result.matched
      result.unmatched
      result.ambiguous
      result.conflicts
      result.changed;
    Ok ())
;;

let match_companions_cmd =
  command
    "match"
    "Recompute and audit companion matches against stored movements."
    Term.(const match_companions $ database $ institution_filter)
;;

let categorize_bank database path =
  let%bind contents = read_file path in
  let%bind decisions = Bank_category.of_json contents in
  with_database database (fun db ->
    let%bind changed = Store.set_bank_categories db decisions in
    printf "category_changes=%d\n" changed;
    Ok ())
;;

let categorize_bank_cmd =
  command
    "apply"
    "Apply a JSON batch of reviewed bank counter-accounts atomically."
    Term.(
      const categorize_bank
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"DECISIONS" ~doc:"JSON decisions: event_id, account, reason."))
;;

let bank_review database all json_output =
  with_database database (fun db ->
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
             ; "amount", `String (Quantity.to_string tx.amount)
             ; "fee", `String (Quantity.to_string tx.fee)
             ; "currency", `String (Iso4217.to_alpha_code tx.account.currency)
             ; "description", `String (String.concat ~sep:" | " tx.descriptions)
             ; "account", `String r.counter_account
             ; "reason", Option.value_map r.reason ~default:`Null ~f:(fun s -> `String s)
             ]))
    in
    if json_output
    then print_endline (Yojson.Safe.pretty_to_string json)
    else Terminal.review rows;
    Ok ())
;;

let bank_review_cmd =
  command
    "review"
    "List unclassified bank movements for review; use --json for scripts."
    Term.(
      const bank_review
      $ database
      $ Arg.(
          value & flag & info [ "all" ] ~doc:"Include movements with reviewed categories.")
      $ json_output)
;;

let category_history database =
  with_database database (fun db ->
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
    "history"
    "List the append-only history of reviewed bank categories."
    Term.(const category_history $ database)
;;

let transfer_leg event_id (event : Source_event.t) (tx : Movement.t) =
  `Assoc
    [ "event_id", `Intlit (Int64.to_string event_id)
    ; "institution", `String event.venue
    ; "source_account", `String event.account
    ; "booked_on", `String (Date.to_string tx.booked_on)
    ; "currency", `String (Iso4217.to_alpha_code tx.account.currency)
    ; "bank_amount", `String (Quantity.to_string tx.amount)
    ; "principal", `String (Quantity.to_string (Movement.principal tx))
    ; "fee", `String (Quantity.to_string tx.fee)
    ; "description", `String (String.concat ~sep:" | " tx.descriptions)
    ]
;;

let transfer_candidates database all json_output =
  with_database database (fun db ->
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
    if json_output
    then print_endline (Yojson.Safe.pretty_to_string json)
    else if List.is_empty rows
    then print_endline "No transfer candidates."
    else
      List.iter rows ~f:(fun r ->
        Terminal.transfer r;
        printf
          "  State: %s%s\n\n"
          (Option.value_map r.state ~default:"unreviewed" ~f:Bank_transfer.state_name)
          (if r.iban_compatible then "" else " (excluded: conflicting IBAN evidence)");
        Option.iter r.reason ~f:(fun reason ->
          printf "  Reason: %s\n" (Terminal.text reason)));
    Ok ())
;;

let transfer_candidates_cmd =
  command
    "candidates"
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
              ~doc:"Include reviewed pairs and movements already assigned to transfers.")
      $ json_output)
;;

let review_transfers database path =
  let%bind contents = read_file path in
  let%bind decisions = Bank_transfer.of_json contents in
  with_database database (fun db ->
    let%bind changed = Store.set_bank_transfers db decisions in
    printf "transfer_changes=%d\n" changed;
    Ok ())
;;

let review_transfers_cmd =
  command
    "review"
    "Apply explicit JSON confirm/reject decisions; confirmation asserts both accounts \
     are yours."
    Term.(
      const review_transfers
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"DECISIONS" ~doc:"JSON transfer review decisions."))
;;

let transfer_history database =
  with_database database (fun db ->
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
    "history"
    "Inspect the append-only transfer review history."
    Term.(const transfer_history $ database)
;;

let transfer_preview database json_output =
  with_database database (fun db ->
    let%bind candidates = Store.transfer_candidates db in
    let%bind all_candidates = Store.transfer_candidates ~include_reviewed:true db in
    let%bind ranked = Transfer_ranking.analyze (Store.registry db) candidates in
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
    if json_output
    then print_endline (Yojson.Safe.pretty_to_string json)
    else (
      print_endline
        "Transfer preview - evidence scores, not probabilities. Nothing is confirmed \
         automatically.\n";
      if List.is_empty ranked
      then print_endline "No compatible unreviewed transfer candidates."
      else
        List.iter ranked ~f:(fun r ->
          Terminal.transfer r.Transfer_ranking.candidate;
          printf
            "  Score: %d; confidence: %s; assignment: %s\n"
            r.score
            r.confidence
            r.assignment.status;
          List.iter r.evidence ~f:(fun e ->
            printf "  %s (%+d)\n" (Terminal.text e.Transfer_ranking.detail) e.points);
          print_endline "");
      let excluded =
        List.count all_candidates ~f:(fun c -> not c.Store.iban_compatible)
      in
      if excluded > 0
      then
        printf
          "%d pairs excluded by IBAN evidence; inspect fin transfers candidates --all.\n"
          excluded);
    Ok ())
;;

let transfer_preview_cmd =
  command
    "preview"
    "Rank compatible transfer candidates and preview globally consistent pairs; scores \
     are not probabilities."
    Term.(const transfer_preview $ database $ json_output)
;;

let accounts_export database output =
  with_database database (fun db ->
    let%bind () =
      match output with
      | None -> Ok ()
      | Some output ->
        let same =
          Or_error.try_with (fun () ->
            let a = Unix.LargeFile.stat database
            and b = Unix.LargeFile.stat output in
            a.st_dev = b.st_dev && a.st_ino = b.st_ino)
        in
        if Result.ok same |> Option.value ~default:false
        then Or_error.error_string "configuration output must not overwrite the database"
        else Ok ()
    in
    let%bind config = Store.export_bank_accounts db in
    let contents = Bank_account_config.to_json config ^ "\n" in
    match output with
    | None ->
      print_string contents;
      Ok ()
    | Some path -> write_file path contents)
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
  with_database database (fun db ->
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
  with_database database (fun db ->
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
        Term.(const list_bank_accounts $ database $ json_output)
    ; command
        "export"
        "Export version 1 account configuration for editing."
        Term.(
          const accounts_export
          $ database
          $ Arg.(
              value
              & opt (some string) None
              & info
                  [ "o"; "output" ]
                  ~docv:"FILE"
                  ~doc:
                    "Write configuration atomically to FILE instead of standard output."))
    ; command
        "apply"
        "Apply the whole account configuration atomically, or preview it with --dry-run."
        Term.(
          const accounts_apply
          $ database
          $ Arg.(
              value
              & pos 0 string paths.accounts_config
              & info
                  []
                  ~docv:"CONFIG"
                  ~doc:
                    "Account configuration (default: XDG config directory; see fin \
                     paths).")
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

let paths_cmd =
  command
    "paths"
    "Display resolved XDG data and configuration locations."
    Term.(
      const (fun () ->
        print_endline
          (Yojson.Safe.pretty_to_string
             (`Assoc
                 [ "data_dir", `String paths.data_dir
                 ; "config_dir", `String paths.config_dir
                 ; "database", `String paths.database
                 ; "accounts_config", `String paths.accounts_config
                 ]));
        Ok ())
      $ const ())
;;

let group name doc commands = Cmd.group (Cmd.info name ~doc) commands

let () =
  Stdlib.exit
    (Cmd.eval
       (Cmd.group
          ~default:Term.(const welcome $ const ())
          (Cmd.info
             "fin"
             ~version:"0.1.0"
             ~doc:"Personal finance accounting and position tracking."
             ~man:
               [ `S Manpage.s_description
               ; `P
                   "Import bank statements into a local database, review accounts and \
                    transactions, and export an hledger journal. Run $(b,fin) without \
                    arguments for a short welcome."
               ; `P
                   "Setup is optional: $(b,fin init) initializes the database and shows \
                    how to import statements. The first import can also initialize it. \
                    Accounts are discovered from imported records."
               ; `S Manpage.s_examples
               ; `Pre
                   "fin init\n\
                    fin bank sync ~/Statements/boursobank --institution boursobank\n\
                    fin status\n\
                    fin accounts list\n\
                    fin bank review\n\
                    fin journal export > my-finances.journal"
               ; `P
                   "Use $(b,fin paths) to inspect storage locations, or append \
                    $(b,--help) to any subcommand for its options."
               ])
          [ setup_cmd
          ; status_cmd
          ; paths_cmd
          ; accounts_cmd
          ; group
              "db"
              "Initialize and inspect local source evidence."
              [ init; events; ingest_cmd ]
          ; group
              "bank"
              "Import and review banking records."
              [ sync_bank_cmd
              ; project_bank_cmd
              ; bank_review_cmd
              ; import_history_cmd
              ; group
                  "categories"
                  "Manage reviewed bank categories."
                  [ categorize_bank_cmd; category_history_cmd ]
              ; companions_cmd
              ; match_companions_cmd
              ; statements_cmd
              ]
          ; group
              "transfers"
              "Review transfer candidates and decisions."
              [ transfer_candidates_cmd
              ; transfer_preview_cmd
              ; review_transfers_cmd
              ; transfer_history_cmd
              ]
          ; group "journal" "Render persisted accounting entries." [ export_hledger_cmd ]
          ]))
;;
