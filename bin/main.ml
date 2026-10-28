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

(* Only init and restore create a database. Other commands require one so a
   mistyped --database fails instead of starting an empty database. *)
let require_database path =
  if String.equal path ":memory:" || Stdlib.Sys.file_exists path
  then Ok ()
  else
    Or_error.errorf
      "fin is not initialized: no database at %s. Run fin init%s first."
      path
      (if String.equal path paths.database
       then ""
       else " --database " ^ Stdlib.Filename.quote path)
;;

let create_database path f =
  let%bind () = ensure_database_parent path in
  Store.with_open ~registry:Builtin.registry path f
;;

let with_database path f =
  let%bind () = require_database path in
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
  let%bind occurred_at =
    Or_error.try_with (fun () -> Timestamp.of_unix_ms_exn (Int64.of_string at))
  in
  let%bind event =
    Source_event.create ~venue ~account ~kind ~source_id ~occurred_at ~payload
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
    Report_json.print (Jsont.list Report_json.event) events;
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
    Term.(const (fun path -> create_database path (fun _ -> Ok ())) $ database)
;;

let stamp () =
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time (Ptime_clock.now ()) in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ" y m d hh mm ss
;;

let backup database directory =
  with_database database (fun db ->
    let%bind _ =
      Bos.OS.Dir.create ~mode:0o700 (Fpath.v directory)
      |> Result.map_error ~f:(fun (`Msg m) -> Error.of_string m)
    in
    let target = Stdlib.Filename.concat directory ("fin-backup-" ^ stamp ()) in
    let%bind () =
      Bos.OS.Dir.create ~path:false ~mode:0o700 (Fpath.v target)
      |> Result.map_error ~f:(fun (`Msg m) -> Error.of_string m)
      |> Result.bind ~f:(function
        | true -> Ok ()
        | false -> Or_error.errorf "%s already exists" target)
    in
    let copy = Stdlib.Filename.concat target in
    let%bind () = Store.backup db (copy "fin.sqlite3") in
    let%map () = Or_error.try_with (fun () -> Unix.chmod (copy "fin.sqlite3") 0o600) in
    printf "Backed up to %s:\n  fin.sqlite3    from %s\n" target database;
    printf
      "To restore: fin restore %s%s\n"
      (Stdlib.Filename.quote target)
      (if String.equal database paths.database
       then ""
       else " --database " ^ Stdlib.Filename.quote database))
;;

let backup_cmd =
  command
    "backup"
    "Save a consistent SQLite database snapshot in a new dated folder."
    Term.(
      const backup
      $ database
      $ Arg.(
          value
          & pos 0 string Stdlib.Filename.current_dir_name
          & info
              []
              ~docv:"DIR"
              ~doc:
                "Where to create the fin-backup-YYYYMMDDTHHMMSSZ folder (default: the \
                 current directory). Missing parent directories are created privately. \
                 The copy is consistent even while fin is running."))
;;

let restore database backup yes =
  let%bind source, stale, existing =
    Or_error.try_with (fun () ->
      let source =
        if Stdlib.Sys.is_directory backup
        then Stdlib.Filename.concat backup "fin.sqlite3"
        else backup
      in
      let input = Unix.stat source in
      let stale =
        [ database ^ ".edit"
        ; Stdlib.Filename.concat (Stdlib.Filename.dirname database) "bank.journal"
        ]
      in
      List.iter stale ~f:(fun path ->
        try
          let file = Unix.stat path in
          if input.st_dev = file.st_dev && input.st_ino = file.st_ino
          then
            failwith
              "backup must not be a generated journal or pending account edit draft"
        with
        | Unix.Unix_error (Unix.ENOENT, _, _) -> ());
      let existing =
        List.filter
          (database
           :: (database ^ "-wal")
           :: (database ^ "-shm")
           :: (database ^ "-journal")
           :: stale)
          ~f:Stdlib.Sys.file_exists
      in
      source, stale, existing)
  in
  let%bind confirmed =
    if List.is_empty existing || yes
    then Ok true
    else (
      printf "Restore %s to %s.\n" source database;
      print_endline
        "This replaces the database and discards its generated journal and pending \
         account edit draft:";
      List.iter existing ~f:(fun path -> printf "  %s\n" path);
      if Unix.isatty Unix.stdin
      then (
        Out_channel.output_string stdout "Type yes to restore: ";
        Out_channel.flush stdout;
        Ok
          (Option.value_map
             (In_channel.input_line In_channel.stdin)
             ~default:false
             ~f:(fun answer -> String.equal (String.strip answer) "yes")))
      else
        Or_error.error_string
          "refusing to replace existing state without confirmation: run fin restore in a \
           terminal, or pass --yes")
  in
  if not confirmed
  then (
    print_endline "Nothing restored.";
    Ok ())
  else (
    let%bind () = Store.restore ~source ~destination:database in
    let%bind () =
      Fin_state.delete stale
      |> Or_error.tag ~tag:"database restored; clearing stale generated files"
    in
    printf "Restored %s to %s.\n" source database;
    print_endline
      "Run fin journal export to generate a journal from the restored entries.";
    Ok ())
;;

let restore_cmd =
  command
    "restore"
    "Restore a SQLite snapshot, including all account settings and review decisions."
    Term.(
      const restore
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"BACKUP"
              ~doc:
                "A fin backup folder or SQLite database file. The snapshot must use the \
                 current schema and pass integrity checks. A missing destination is \
                 created; replacing existing state requires confirmation. The generated \
                 bank.journal and pending account edit draft are removed after success. \
                 Exported account snapshots and statement files are kept.")
      $ Arg.(
          value
          & flag
          & info [ "yes" ] ~doc:"Replace existing state without asking, for scripts."))
;;

let delete database yes =
  let%bind files = Fin_state.files ~database in
  match files with
  | [] ->
    print_endline "fin has no state to delete.";
    Ok ()
  | files ->
    print_endline "This permanently deletes:";
    List.iter files ~f:(fun f -> printf "  %s\n" f);
    print_endline
      "Your statement folders are not touched. Run fin backup first if you may need this \
       data.";
    let%bind confirmed =
      if yes
      then Ok true
      else if Unix.isatty Unix.stdin
      then (
        Out_channel.output_string stdout "Type yes to delete these files: ";
        Out_channel.flush stdout;
        Ok
          (Option.value_map
             (In_channel.input_line In_channel.stdin)
             ~default:false
             ~f:(fun answer -> String.equal (String.strip answer) "yes")))
      else
        Or_error.error_string
          "refusing to delete without confirmation: run fin delete in a terminal, or \
           pass --yes"
    in
    if confirmed
    then (
      let%map () = Fin_state.delete files in
      print_endline "Deleted. Run fin init to start again.")
    else (
      print_endline "Nothing deleted.";
      Ok ())
;;

let delete_cmd =
  command
    "delete"
    "Delete fin state: the database, its journal and any pending account edit draft."
    Term.(
      const delete
      $ database
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Delete without asking, for scripts."))
;;

let welcome () =
  print_endline
    "fin - your personal finances\n\n\
     Get started:\n\
     \032 fin init                Set up your database\n\
     \032 fin import --help       Learn how to import bank statements\n\n\
     Explore your finances:\n\
     \032 fin status              See import coverage and review progress\n\
     \032 fin accounts list       See your bank accounts\n\
     \032 fin bank review         Review imported transactions\n\
     \032 fin journal export      Produce an hledger journal\n\n\
     More:\n\
     \032 fin paths               Show database and data locations\n\
     \032 fin backup              Save a SQLite database snapshot\n\
     \032 fin restore BACKUP      Restore a SQLite database snapshot\n\
     \032 fin delete              Delete all fin data, after confirmation\n\
     \032 fin --help              Explore all commands"
;;

let setup database =
  let%bind () = create_database database (fun _ -> Ok ()) in
  let database_option =
    if String.equal database paths.database
    then ""
    else " --database " ^ Stdlib.Filename.quote database
  in
  Stdio.printf
    "fin is ready.\n\n\
     Database: %s\n\
     Account settings are stored in the database.\n\n\
     Import your first statements, for example:\n\
     \032 fin import boursobank ~/Statements/boursobank%s\n\n\
     Accounts appear when you import statements. Then explore:\n\
     \032 fin accounts list%s\n\
     \032 fin bank review%s\n\n\
     See fin import --help for import options.\n\
     You can repeat fin init safely; existing data and account settings are preserved.\n"
    database
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
      $ required
          "at-ms"
          "Event timestamp in Unix milliseconds, converted exactly to Ptime. Event \
           output uses RFC 3339 occurred_at."
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

let list_bank_accounts database json_output =
  with_database database (fun db ->
    let%bind accounts = Store.bank_account_info db in
    if json_output
    then Report_json.print (Jsont.list Report_json.account) accounts
    else if List.is_empty accounts
    then
      print_endline
        "No bank accounts yet. Import statements with fin import; see fin import --help."
    else List.iter accounts ~f:Terminal.account;
    Ok ())
;;

let status database json_output =
  let module Status = Finances_bank.Bank_status in
  let%bind report = with_database database Status.read in
  let banks =
    List.map report.accounts ~f:(fun a -> a.Store.mapping.institution)
    |> List.dedup_and_sort ~compare:String.compare
    |> List.length
  in
  if json_output
  then Report_json.print Report_json.status (database, banks, report)
  else (
    printf "Database: %s\n\n" (Terminal.text database);
    printf
      "%d banks, %d accounts, %d imported movements\n\n"
      banks
      (List.length report.accounts)
      report.movements;
    if List.is_empty report.accounts
    then print_endline "No bank records yet. See fin import --help."
    else
      Terminal.table
        [ "Bank"; "Account"; "Currency"; "Movements"; "First observed"; "Last observed" ]
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
         (Timestamp.to_rfc3339 a.attempted_at)
         (Terminal.text a.institution)
         (Bank_import.outcome_name a.outcome)
         (Terminal.text a.path);
       Option.iter a.message ~f:(fun message -> printf "  %s\n" (Terminal.text message)));
    print_endline
      "\n\
       Date ranges show observed records, not complete statement coverage. Financial \
       reports: fin journal export, then query with hledger.";
    print_endline "Next: fin bank review; fin transfers preview; fin bank history.");
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
    if json_output
    then Report_json.print (Jsont.list Report_json.import) attempts
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

(* Prompts live at the CLI boundary; library callers and redirected stdin never
   need a terminal. EOF has the same meaning as skipping an optional IBAN. *)
let prompt text =
  Out_channel.output_string stderr text;
  Out_channel.flush stderr;
  In_channel.input_line In_channel.stdin
;;

let import_account_label institution account_id =
  let validate = Validation.token "account label" in
  match Registry.find Builtin.registry institution with
  | Some (module A : Adapter.S) when A.takes_account_label ->
    let message =
      Printf.sprintf
        "%s files do not say which account they come from: name it with --account-id \
         LABEL, for example main, and use the same label every time."
        institution
    in
    (match account_id with
     | Some label -> Result.map (validate label) ~f:Option.some
     | None when Unix.isatty Unix.stdin ->
       eprintf "%s files do not say which account they come from.\n" institution;
       let rec read () =
         match
           prompt "Label for this account (e.g. main; use the same one every time): "
         with
         | None -> Or_error.error_string message
         | Some label ->
           (match validate label with
            | Ok label -> Ok (Some label)
            | Error error ->
              eprintf "%s\n" (Error.to_string_hum error);
              read ())
       in
       read ()
     | None -> Or_error.error_string message)
  | _ -> Ok account_id
;;

let configure_new_accounts db fresh =
  let same_group (a : Bank_account_mapping.t) (b : Bank_account_mapping.t) =
    String.equal a.institution b.institution
    && String.equal a.bank_id b.bank_id
    && String.equal a.account_id b.account_id
  in
  let rec groups remaining =
    match remaining with
    | [] -> Ok 0
    | (first : Store.bank_account_info) :: rest ->
      let siblings, rest =
        List.partition_tf rest ~f:(fun info ->
          same_group first.mapping info.Store.mapping)
      in
      let group = first :: siblings in
      let missing =
        List.filter group ~f:(fun info -> String.equal info.Store.iban_source "missing")
      in
      let%bind count =
        match missing with
        | [] -> Ok 0
        | first :: _ ->
          eprintf
            "New account: %s %s (%s), %d movements, %s to %s.\n"
            first.mapping.institution
            first.mapping.account_id
            (String.concat
               ~sep:", "
               (List.map missing ~f:(fun i -> i.Store.mapping.account_type)))
            (List.sum (module Int) missing ~f:(fun i -> i.Store.movements))
            (Date.to_string
               (List.reduce_exn
                  (List.map missing ~f:(fun i -> i.Store.first_on))
                  ~f:(fun a b -> if Date.compare a b <= 0 then a else b)))
            (Date.to_string
               (List.reduce_exn
                  (List.map missing ~f:(fun i -> i.Store.last_on))
                  ~f:(fun a b -> if Date.compare a b >= 0 then a else b)));
          let rec read () =
            match
              prompt "Its statements do not include an IBAN. IBAN (Enter to skip): "
            with
            | None | Some "" -> Ok None
            | Some value ->
              (match Iban.of_string value with
               | Ok iban -> Ok (Some iban)
               | Error error ->
                 eprintf "%s\n" (Error.to_string_hum error);
                 read ())
          in
          let%bind iban = read () in
          (match iban with
           | None -> Ok 0
           | Some iban ->
             let%bind current = Store.export_bank_accounts db in
             let updates =
               List.filter_map current ~f:(fun row ->
                 if
                   List.exists missing ~f:(fun info ->
                     String.equal
                       row.Bank_account_config.reference.institution
                       info.Store.mapping.institution
                     && String.equal
                          (Bank_account_config.source_account row.reference)
                          (Bank_account_mapping.source_account info.mapping))
                 then Some { row with iban = Set iban }
                 else None)
             in
             let%map result = Store.apply_bank_accounts db updates in
             result.identity_changes)
      in
      let%map remaining = groups rest in
      count + remaining
  in
  if Unix.isatty Unix.stdin then groups fresh else Ok 0
;;

let import_bank database institution paths journal max_mib account_id =
  if max_mib < 1 || max_mib > 1024
  then Or_error.error_string "max-file-mib must be between 1 and 1024"
  else (
    let%bind () = require_database database in
    let%bind account_id = import_account_label institution account_id in
    let journal =
      Option.value
        journal
        ~default:
          (Stdlib.Filename.concat (Stdlib.Filename.dirname database) "bank.journal")
    in
    let%bind report =
      Finances_bank.Sync.run
        ~registry:Builtin.registry
        ~configure_new_accounts
        ?account_id
        ~max_file_bytes:Int64.(of_int max_mib * 1048576L)
        ~database
        ~institution
        ~paths
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
      "entries_inserted=%d entries_existing=%d\njournal=%s\n"
      report.projection.inserted
      report.projection.existing
      report.journal;
    printf "identity_changes=%d\n" report.identity_changes;
    printf
      "companions_matched=%d companions_unmatched=%d companions_ambiguous=%d \
       companions_conflicts=%d companions_changes=%d\n"
      report.companions.matched
      report.companions.unmatched
      report.companions.ambiguous
      report.companions.conflicts
      report.companions.changed;
    (* A file named on the command line that no adapter recognizes was not
       imported, unlike an unrelated file found inside a directory. *)
    let unsupported_explicit =
      List.count report.inbox.files ~f:(fun row ->
        Poly.equal row.outcome Unsupported
        && List.mem paths row.path ~equal:String.equal
        && Stdlib.Sys.file_exists row.path
        && not (Stdlib.Sys.is_directory row.path))
    in
    if count Rejected + count Unreadable + unsupported_explicit > 0
    then
      Or_error.error_string
        "some inputs need review; successful inputs were imported and the journal was \
         generated"
    else Ok ())
;;

let import_bank_cmd =
  command
    "import"
    "Import a bank's statement files, apply mappings, and publish a bank journal."
    Term.(
      const import_bank
      $ database
      $ Arg.(
          required
          & pos
              0
              (some
                 (enum
                    (List.map (Registry.institutions Builtin.registry) ~f:(fun i -> i, i))))
              None
          & info
              []
              ~docv:"BANK"
              ~doc:
                ("The bank that produced the files: "
                 ^ String.concat ~sep:", " (Registry.institutions Builtin.registry)
                 ^ "."))
      $ Arg.(
          non_empty
          & pos_right 0 string []
          & info
              []
              ~docv:"PATH"
              ~doc:
                "Statement files exported from $(i,BANK), or directories holding them. \
                 Directories are searched recursively; hidden and partially downloaded \
                 files inside them are skipped. Files are only read, never modified, and \
                 importing them again imports only what is new. ZIP files are opened \
                 automatically in memory; each member follows the same import rules. \
                 Nested ZIPs and encrypted archives are unsupported. Revolut and Wise \
                 CSV clocks are assumed UTC; Wise matching retains CAMT source-local \
                 clocks.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "journal" ]
              ~doc:"Output path (default bank.journal beside the database).")
      $ Arg.(
          value
          & opt int 64
          & info
              [ "max-file-mib" ]
              ~doc:
                "Per-file read limit in MiB (1-1024, default 64). For ZIPs, also limits \
                 the total expanded size; at most 1024 members per archive.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "account-id" ]
              ~doc:
                "Stable account label for account-free exports (Revolut). Asked on a \
                 terminal if omitted; required in scripts. New accounts missing an IBAN \
                 prompt once on a terminal; Enter skips, and accounts edit can add it \
                 later."))
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
    Report_json.print Report_json.statements statements;
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
        o, fields)
      |> Or_error.combine_errors
    in
    Report_json.print (Jsont.list Report_json.companion) rows)
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
          & info
              []
              ~docv:"DECISIONS"
              ~doc:
                "JSON decisions: event_id, account, reason. Large IDs use decimal \
                 strings."))
;;

let bank_review database all json_output =
  with_database database (fun db ->
    let%bind rows = Store.bank_review db in
    let rows =
      List.filter rows ~f:(fun r ->
        all || String.equal r.Store.counter_account "equity:unclassified")
    in
    if json_output
    then Report_json.print (Jsont.list Report_json.review) rows
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
    Report_json.print (Jsont.list Report_json.category_change) rows;
    Ok ())
;;

let category_history_cmd =
  command
    "history"
    "List the append-only history of reviewed bank categories."
    Term.(const category_history $ database)
;;

let transfer_candidates database all json_output =
  with_database database (fun db ->
    let%bind rows = Store.transfer_candidates ~include_reviewed:all db in
    if json_output
    then Report_json.print (Jsont.list Report_json.candidate) rows
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
          & info
              []
              ~docv:"DECISIONS"
              ~doc:"JSON transfer review decisions. Large IDs use decimal strings."))
;;

let transfer_history database =
  with_database database (fun db ->
    let%bind rows = Store.bank_transfer_history db in
    Report_json.print (Jsont.list Report_json.transfer_change) rows;
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
    if json_output
    then Report_json.print Report_json.preview (ranked, all_candidates)
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
        then
          Or_error.error_string "account snapshot output must not overwrite the database"
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

let accounts_import database path dry_run =
  let%bind contents = read_file path in
  let%bind config = Bank_account_config.of_json contents in
  with_database database (fun db ->
    let%bind result = Store.apply_bank_accounts ~dry_run db config in
    Report_json.print Report_json.account_config (dry_run, result);
    Ok ())
;;

let editor () =
  List.find_map [ "VISUAL"; "EDITOR" ] ~f:(fun name ->
    Option.filter (Sys.getenv name) ~f:(Fn.non String.is_empty))
  |> Option.value ~default:"vi"
;;

let ask question =
  Out_channel.output_string stdout question;
  Out_channel.flush stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> false
  | Some answer ->
    (match String.lowercase (String.strip answer) with
     | "" | "y" | "yes" -> true
     | _ -> false)
;;

(* The edit draft is recoverable, but SQLite is the sole persistent settings store. *)
let accounts_edit database =
  with_database database (fun db ->
    let%bind current = Store.export_bank_accounts db in
    let original = Bank_account_config.to_json current ^ "\n" in
    let draft = database ^ ".edit" in
    let%bind () =
      if Stdlib.Sys.file_exists draft
      then (
        printf "Continuing the draft that was not applied: %s\n" draft;
        Ok ())
      else write_file draft original
    in
    let discard () = ignore (Stdlib.Sys.remove draft : unit) in
    let rec edit () =
      match Stdlib.Sys.command (editor () ^ " " ^ Stdlib.Filename.quote draft) with
      | status when status <> 0 ->
        Or_error.errorf
          "the editor exited with status %d; your draft is in %s"
          status
          draft
      | _ ->
        let%bind contents = read_file draft in
        if String.equal contents original
        then (
          discard ();
          print_endline "No changes.";
          Ok ())
        else (
          match
            Result.bind (Bank_account_config.of_json contents) ~f:(fun edited ->
              Store.apply_bank_accounts db edited)
          with
          | Ok result ->
            discard ();
            printf
              "Applied %d change%s.\n"
              (List.length result.changes)
              (if List.length result.changes = 1 then "" else "s");
            List.iter result.changes ~f:(fun c ->
              printf
                "  %s %s %s: %s -> %s\n"
                c.institution
                c.source_account
                c.kind
                (Option.value c.previous_value ~default:"none")
                (Option.value c.value ~default:"none"));
            Ok ()
          | Error error ->
            eprintf "fin: %s\n" (Error.to_string_hum error);
            Out_channel.flush stderr;
            if Unix.isatty Unix.stdin && ask "Nothing was applied. Edit again? [Y/n] "
            then edit ()
            else Or_error.errorf "nothing was applied; your draft is in %s" draft)
    in
    edit ())
;;

let accounts_history database =
  with_database database (fun db ->
    let%bind rows = Store.bank_account_history db in
    Report_json.print (Jsont.list Report_json.account_change) rows;
    Ok ())
;;

let accounts_cmd =
  Cmd.group
    (Cmd.info
       "accounts"
       ~doc:"Manage bank account settings stored in SQLite; export and import snapshots.")
    [ command
        "list"
        "List account details, IBAN provenance, coverage and card settlement links."
        Term.(const list_bank_accounts $ database $ json_output)
    ; command
        "export"
        "Export an account settings snapshot as version 1 JSON."
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
                    "Write a private snapshot atomically to FILE instead of standard \
                     output."))
    ; command
        "edit"
        "Edit account names, IBANs and card settlement links in your editor, and apply \
         them on save."
        Term.(const accounts_edit $ database)
    ; command
        "import"
        "Restore an explicit account settings snapshot atomically, or preview with \
         --dry-run."
        Term.(
          const accounts_import
          $ database
          $ Arg.(
              required
              & pos 0 (some string) None
              & info
                  []
                  ~docv:"FILE"
                  ~doc:
                    "Account settings JSON snapshot to import; there is no default file.")
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
    "Display resolved data directory and authoritative SQLite database path."
    Term.(
      const (fun () ->
        Report_json.print Report_json.paths paths;
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
                   "Start with $(b,fin init): it creates the database and shows how to \
                    import statements. Imports and review commands need that database \
                    and say so when it is missing. Accounts are discovered from imported \
                    records."
               ; `S Manpage.s_examples
               ; `Pre
                   "fin init\n\
                    fin import boursobank ~/Statements/boursobank\n\
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
          ; backup_cmd
          ; restore_cmd
          ; delete_cmd
          ; accounts_cmd
          ; import_bank_cmd
          ; group
              "db"
              "Initialize and inspect local source evidence."
              [ init; events; ingest_cmd ]
          ; group
              "bank"
              "Review banking records and reconcile statements."
              [ project_bank_cmd
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
