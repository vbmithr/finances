open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax

let confirm_default_yes value =
  List.mem [ ""; "y"; "yes" ] (String.lowercase (String.strip value)) ~equal:String.equal
;;

(* For destructive actions: only an explicit yes confirms; Enter declines. *)
let confirm_default_no value =
  List.mem [ "y"; "yes" ] (String.lowercase (String.strip value)) ~equal:String.equal
;;

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
  Setup.with_open ~registry:Builtin.registry path f
;;

let companion_profiles path =
  let%bind profiles =
    Setup.with_open ~registry:Builtin.registry path Setup.institution_companion_profiles
  in
  profiles
  |> List.map ~f:(fun (institution, json) ->
    let%map profile = Json.decode Csv_companion_layout.codec json in
    institution, profile)
  |> Or_error.combine_errors
;;

let institution_definitions path =
  let%bind rows =
    Setup.with_open ~registry:Builtin.registry path Setup.institution_definitions
  in
  rows
  |> List.map ~f:(fun (name, json) ->
    let%bind definition = Json.decode Institution_definition.codec json in
    if String.equal name (Institution_definition.institution definition)
    then Ok definition
    else Or_error.error_string "stored institution definition name does not match")
  |> Or_error.combine_errors
;;

let registry_for path =
  let%bind profiles = companion_profiles path in
  let%map definitions = institution_definitions path in
  Builtin.registry_with_companions ~definitions profiles
;;

let with_database path f =
  let%bind () = require_database path in
  let%bind registry = registry_for path in
  Setup.with_open ~registry path f
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
    Source_event.create ~venue ~account ~kind ~source_id ~occurred_at ~payload ()
  in
  with_database database (fun db ->
    let%bind result = Sources.ingest db event in
    let status, id =
      match result with
      | Handle.Inserted id -> "inserted", id
      | Existing id -> "existing", id
    in
    Stdio.printf "%s %Ld\n" status id;
    Ok ())
;;

let list_events database =
  with_database database (fun db ->
    let%bind events = Ledger.events db in
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

let command ?(man = []) name doc term =
  Cmd.v
    (Cmd.info name ~doc ~man)
    Term.(
      const (fun result ->
        Result.map_error result ~f:(fun e -> `Msg (Error.to_string_hum e)))
      $ term
      |> term_result)
;;

let inspect_movement_csv (profile : Movement_csv.profile) files summary_only json_output =
  let%bind rows =
    List.map files ~f:(fun file ->
      let%bind contents = read_file file in
      Movement_csv.parse profile ~file contents)
    |> Or_error.combine_errors
    |> Result.map ~f:List.concat
  in
  if summary_only
  then (
    let summary = Movement_csv.summarize rows in
    if json_output
    then Report_json.print Report_json.movement_csv_summary summary
    else (
      printf
        "%s: %d rows; proposed effects are not reconciled.\n"
        profile.institution
        summary.total;
      List.iter summary.by_kind ~f:(fun (kind : Movement_csv.kind_summary) ->
        printf
          "  %s: %d (single %d, exchange %d, review %d)\n"
          (Terminal.text
             (if String.is_empty kind.kind then "(empty kind)" else kind.kind))
          kind.rows
          kind.single
          kind.exchange
          kind.review);
      printf
        "Identical-row groups (review, not automatically deduplicated): %d\n"
        (List.length summary.identical_rows);
      List.iter summary.identical_rows ~f:(fun refs ->
        List.iter refs ~f:(fun (r : Movement_csv.row_reference) ->
          printf "  %s:%d\n" (Terminal.text r.file) r.row_number))))
  else if json_output
  then Report_json.print (Jsont.list Report_json.movement_csv_row) rows
  else (
    let review =
      List.count rows ~f:(fun (row : Movement_csv.row) ->
        match row.interpretation with
        | Review _ -> true
        | _ -> false)
    in
    printf
      "%s: %d rows; %d need review. Proposed legs are not posted.\n"
      profile.institution
      (List.length rows)
      review;
    List.iter rows ~f:(fun (row : Movement_csv.row) ->
      let kind = if String.is_empty row.kind then "(empty kind)" else row.kind in
      let legs =
        List.map row.proposed_legs ~f:(fun (leg : Movement_csv.leg) ->
          Quantity.to_string leg.amount ^ " " ^ Commodity.display leg.commodity)
        |> String.concat ~sep:", "
      in
      let result =
        match row.interpretation with
        | Review reason -> "review: " ^ reason
        | Single | Exchange -> legs
      in
      printf
        "%s:%d  %s  %s\n"
        (Terminal.text row.file)
        row.row_number
        (Terminal.text kind)
        (Terminal.text result)));
  Ok ()
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
  let%bind () = require_database database in
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
  let%bind () = Setup.backup database (copy "fin.sqlite3") in
  let%map () = Or_error.try_with (fun () -> Unix.chmod (copy "fin.sqlite3") 0o600) in
  printf "Backed up to %s:\n  fin.sqlite3    from %s\n" target database;
  printf
    "To restore: fin restore %s%s\n"
    (Stdlib.Filename.quote target)
    (if String.equal database paths.database
     then ""
     else " --database " ^ Stdlib.Filename.quote database)
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
        Out_channel.output_string stdout "Restore? [Y/n]: ";
        Out_channel.flush stdout;
        Ok
          (Option.value_map
             (In_channel.input_line In_channel.stdin)
             ~default:false
             ~f:confirm_default_yes))
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
    let%bind () = Setup.restore ~source ~destination:database in
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
        Out_channel.output_string stdout "Delete these files? [y/N]: ";
        Out_channel.flush stdout;
        Ok
          (Option.value_map
             (In_channel.input_line In_channel.stdin)
             ~default:false
             ~f:confirm_default_no))
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
     \032 fin institution list    List institutions\n\
     \032 fin institution add     Add an institution for CSV imports\n\
     \032 fin import --help       Learn how to import statements\n\n\
     Explore your finances:\n\
     \032 fin status              See import coverage and review progress\n\
     \032 fin account list        See your accounts\n\
     \032 fin category list       See your category catalog\n\
     \032 fin operation list    List imported operations\n\
     \032 fin operation review  Review unclassified movements\n\
     \032 fin statement list       Inspect statement reconciliation\n\
     \032 fin settlement review    Review card settlements\n\
     \032 fin reversal candidates  Find possible reversals\n\
     \032 fin rule --help          Manage classification rules\n\
     \032 fin transfer candidates  Find possible transfers\n\
     \032 fin journal export       Produce an hledger journal\n\
     \032 fin ledger               Run hledger reports\n\n\
     More:\n\
     \032 fin paths               Show database and data locations\n\
     \032 fin db --help           Inspect source evidence commands\n\
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
     Choose an institution and import statements:\n\
     \032 fin institution list%s\n\
     \032 fin import boursobank ~/Statements/boursobank%s\n\n\
     For another provider, add an institution for its CSV files:\n\
     \032 fin institution add%s\n\
     \032 fin import INSTITUTION ~/Statements/INSTITUTION%s\n\n\
     Accounts appear when you import statements. Then explore:\n\
     \032 fin account list%s\n\
     \032 fin operation list%s\n\
     \032 fin operation review%s\n\
     \032 fin settlement review%s\n\n\
     See fin import --help for import options.\n\
     You can repeat fin init safely; existing data and account settings are preserved.\n"
    database
    database_option
    database_option
    database_option
    database_option
    database_option
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
    let%bind report = Ledger.project_bank db in
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

let ledger database args =
  let%bind journal = with_database database Finances_hledger.export in
  let run () =
    let read_fd, write_fd = Unix.pipe () in
    Unix.set_close_on_exec write_fd;
    let pid =
      try
        Unix.create_process
          "hledger"
          (Array.of_list ("hledger" :: "-f" :: "-" :: args))
          read_fd
          Unix.stdout
          Unix.stderr
      with
      | exn ->
        Unix.close read_fd;
        Unix.close write_fd;
        raise exn
    in
    Unix.close read_fd;
    let previous = Stdlib.Sys.signal Stdlib.Sys.sigpipe Stdlib.Sys.Signal_ignore in
    let write_error =
      try
        let rec write offset =
          if offset < String.length journal
          then (
            let count =
              Unix.write_substring write_fd journal offset (String.length journal - offset)
            in
            write (offset + count))
        in
        write 0;
        None
      with
      | Unix.Unix_error (Unix.EPIPE, _, _) -> None
      | exn -> Some exn
    in
    Unix.close write_fd;
    ignore (Stdlib.Sys.signal Stdlib.Sys.sigpipe previous : Stdlib.Sys.signal_behavior);
    let _, status = Unix.waitpid [] pid in
    match write_error, status with
    | Some exn, Unix.WEXITED 0 -> raise exn
    | _, Unix.WEXITED code -> code
    | _, (Unix.WSIGNALED _ | Unix.WSTOPPED _) -> 128
  in
  match Or_error.try_with run with
  | Error error ->
    Or_error.errorf
      "could not run hledger (is it installed?): %s"
      (Error.to_string_hum error)
  | Ok code -> Stdlib.exit code
;;

let ledger_cmd =
  command
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Generate a fresh journal and pass arguments to hledger. Place Fin's \
           --database option before the hledger command. The journal uses standard \
           input; use this command for reports, not edits."
      ]
    "ledger"
    "Run an hledger report using the current Fin journal."
    Term.(
      const ledger
      $ database
      $ Arg.(value & pos_all string [] & info [] ~docv:"HLEDGER-ARGS"))
;;

let list_bank_accounts database json_output =
  with_database database (fun db ->
    let%bind accounts = Transfers.bank_account_info db in
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
  let module Status = Finances_import.Bank_status in
  let%bind report = with_database database Status.read in
  let banks =
    List.map report.accounts ~f:(fun a -> a.Transfers.mapping.institution)
    |> List.dedup_and_sort ~compare:String.compare
    |> List.length
  in
  if json_output
  then Report_json.print Report_json.status (database, banks, report)
  else (
    printf "Database: %s\n\n" (Terminal.text database);
    printf
      "%d banks, %d accounts, %d imported movements\n"
      banks
      (List.length report.accounts)
      report.movements;
    if List.is_empty report.accounts && List.is_empty report.operations
    then print_endline "\nNo records yet. See fin import --help.";
    if not (List.is_empty report.accounts)
    then (
      print_endline "";
      Terminal.table
        [ "Bank"; "Account"; "Currency"; "Movements"; "First observed"; "Last observed" ]
        (List.map report.accounts ~f:(fun a ->
           [ a.Transfers.mapping.institution
           ; a.mapping.ledger_account
           ; Iso4217.to_alpha_code a.currency
           ; Int.to_string a.movements
           ; Date.to_string a.first_on
           ; Date.to_string a.last_on
           ])));
    if not (List.is_empty report.operations)
    then (
      print_endline "\nOperations without a bank account";
      Terminal.table
        [ "Institution"; "Operations"; "Unresolved"; "First observed"; "Last observed" ]
        (List.map
           report.operations
           ~f:(fun (institution, (s : Explain.operation_summary)) ->
             [ institution
             ; Int.to_string s.operations
             ; Int.to_string s.unresolved
             ; Option.value s.first_on ~default:"-"
             ; Option.value s.last_on ~default:"-"
             ])));
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
    print_endline
      "Next: fin operation review; fin transfer candidates; fin institution history.");
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
    let%bind attempts = Import.bank_import_history ~limit db in
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
  Out_channel.flush stdout;
  Out_channel.output_string stderr text;
  Out_channel.flush stderr;
  In_channel.input_line In_channel.stdin
;;

let import_account_label registry institution account_id =
  let validate = Validation.token "account label" in
  match Registry.find registry institution with
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
    | (first : Transfers.bank_account_info) :: rest ->
      let siblings, rest =
        List.partition_tf rest ~f:(fun info ->
          same_group first.mapping info.Transfers.mapping)
      in
      let group = first :: siblings in
      let missing =
        List.filter group ~f:(fun info ->
          String.equal info.Transfers.iban_source "missing")
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
               (List.map missing ~f:(fun i -> i.Transfers.mapping.account_type)))
            (List.sum (module Int) missing ~f:(fun i -> i.Transfers.movements))
            (Date.to_string
               (List.reduce_exn
                  (List.map missing ~f:(fun i -> i.Transfers.first_on))
                  ~f:(fun a b -> if Date.compare a b <= 0 then a else b)))
            (Date.to_string
               (List.reduce_exn
                  (List.map missing ~f:(fun i -> i.Transfers.last_on))
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
             let%bind current = Accounts.export_bank_accounts db in
             let updates =
               List.filter_map current ~f:(fun row ->
                 if
                   List.exists missing ~f:(fun info ->
                     String.equal
                       row.Bank_account_config.reference.institution
                       info.Transfers.mapping.institution
                     && String.equal
                          (Bank_account_config.source_account row.reference)
                          (Bank_account_mapping.source_account info.mapping))
                 then Some { row with iban = Set iban }
                 else None)
             in
             let%map result = Accounts.apply_bank_accounts db updates in
             result.identity_changes)
      in
      let%map remaining = groups rest in
      count + remaining
  in
  if Unix.isatty Unix.stdin then groups fresh else Ok 0
;;

let csv_import_profile ?(setup_only = false) database path max_mib ~institution =
  let%bind contents =
    Or_error.try_with (fun () ->
      let stat = Unix.stat path in
      if not (Poly.equal stat.st_kind Unix.S_REG)
      then failwith "CSV setup needs a regular file";
      if stat.st_size > max_mib * 1024 * 1024
      then failwith "CSV file exceeds --max-file-mib";
      In_channel.read_all path)
  in
  let%bind contexts =
    with_database database (fun db -> Import.document_contexts db ~institution)
  in
  let%bind profiles =
    List.map contexts ~f:(fun context ->
      let%bind fields = Json.decode Json.string_map context in
      match List.Assoc.find fields "csv_profile" ~equal:String.equal with
      | None -> Ok None
      | Some value ->
        let%map p = Json.decode Csv_profile.codec value in
        Some p)
    |> Or_error.combine_errors
  in
  let%bind configured =
    with_database database (fun db -> Setup.institution_profile db institution)
  in
  let%bind configured =
    match configured with
    | None -> Ok []
    | Some encoded ->
      let%map profile = Json.decode Csv_profile.codec encoded in
      [ profile ]
  in
  let matching =
    configured @ List.filter_opt profiles
    |> List.filter ~f:(fun (p : Csv_profile.t) ->
      Csv_reader.recognizes ~separator:p.separator ~trim:false ~header:p.header contents)
    |> List.dedup_and_sort ~compare:Poly.compare
  in
  let read message =
    match prompt message with
    | Some s when not (String.is_empty (String.strip s)) -> String.strip s
    | _ -> failwith "CSV setup cancelled"
  in
  let choose question options =
    eprintf "%s\n" question;
    List.iteri options ~f:(fun i (label, _, _) -> eprintf "  %d. %s\n" (i + 1) label);
    let rec loop () =
      match prompt "Choice: " with
      | None -> failwith "CSV setup cancelled"
      | Some input ->
        let input = String.strip input in
        let by_number =
          Option.try_with (fun () -> Int.of_string input)
          |> Option.bind ~f:(fun n -> List.nth options (n - 1))
        in
        let selected =
          match by_number with
          | Some choice -> Some choice
          | None ->
            List.find options ~f:(fun (label, aliases, _) ->
              String.Caseless.equal input label
              || List.mem aliases input ~equal:String.Caseless.equal)
        in
        (match selected with
         | Some (_, _, value) when not (String.is_empty input) -> value
         | _ ->
           eprintf "Choose one of the numbered options.\n";
           loop ())
    in
    loop ()
  in
  let yes_no question =
    choose question [ "Yes", [ "y"; "yes" ], true; "No", [ "n"; "no" ], false ]
  in
  let%bind profile =
    match matching with
    | [ p ] -> Ok p
    | _ :: _ ->
      Or_error.error_string "multiple saved CSV layouts match; choose a distinct header"
    | [] when not (Unix.isatty Unix.stdin) ->
      Or_error.error_string
        "unknown CSV layout; run this command in a terminal to set it up"
    | [] ->
      Or_error.try_with (fun () ->
        eprintf "Unknown CSV layout. Set up the columns below.\n";
        let separator =
          match Csv_reader.detect_separator contents with
          | Some separator ->
            eprintf
              "Detected separator: %s.\n"
              (if Char.equal separator ',' then "comma" else "semicolon");
            separator
          | None ->
            choose "Separator:" [ "Comma", [ "," ], ','; "Semicolon", [ ";" ], ';' ]
        in
        let header = Csv.next (Csv_reader.channel ~separator contents) in
        eprintf "Columns: %s\n" (String.concat ~sep:", " header);
        let find names =
          List.find header ~f:(fun h ->
            List.mem names (String.lowercase (String.strip h)) ~equal:String.equal)
        in
        let date_guess = find [ "date"; "transaction date"; "booking date" ] in
        let description_guess = find [ "merchant"; "description"; "payee" ] in
        let currencies =
          List.filter_map header ~f:(fun h ->
            Option.map (Iso4217.of_alpha_code h) ~f:(fun currency -> h, currency))
        in
        let amount_guess =
          match currencies with
          | [ pair ] -> Some pair
          | _ -> None
        in
        let proposed =
          match date_guess, description_guess, amount_guess with
          | Some date, Some description, Some (amount, currency) ->
            eprintf
              "\n\
               Suggested mapping:\n\
              \  Transaction date: %s\n\
              \  Description: %s\n\
              \  Account amount: %s (%s)\n"
              date
              description
              amount
              (Iso4217.to_alpha_code currency);
            let other =
              List.filter header ~f:(fun h ->
                not (List.mem [ date; description; amount ] h ~equal:String.equal))
            in
            if not (List.is_empty other)
            then eprintf "  Other source details: %s\n" (String.concat ~sep:", " other);
            eprintf
              "These columns describe the transaction date, description, and amount in \
               the account currency.\n";
            if yes_no "Use these columns?"
            then Some (date, description, amount, currency)
            else None
          | _ ->
            eprintf "I could not identify all transaction columns; choose them below.\n";
            None
        in
        let date_column, description_column, amount_column, currency =
          match proposed with
          | Some mapping -> mapping
          | None ->
            let column question =
              choose question (List.map header ~f:(fun h -> h, [], h))
            in
            let date = column "Column containing the transaction date:" in
            let description = column "Column containing the merchant or description:" in
            let amount = column "Column containing the amount charged to the account:" in
            let currency =
              read "Account currency (e.g. USD): "
              |> Iso4217.of_alpha_code
              |> Option.value_exn
            in
            date, description, amount, currency
        in
        let sample_rows = Csv_reader.channel ~separator contents in
        ignore (Csv.next sample_rows : string list);
        let sample_rows = Csv.input_all sample_rows in
        let values column =
          match List.findi header ~f:(fun _ value -> String.equal value column) with
          | None -> []
          | Some (index, _) ->
            List.filter_map sample_rows ~f:(fun row ->
              Option.bind (List.nth row index) ~f:(fun value ->
                let value = String.strip value in
                Option.some_if (not (String.is_empty value)) value))
        in
        let date_values = values date_column in
        let iso_date = Re.Perl.compile_pat "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" in
        let iso_datetime =
          Re.Perl.compile_pat "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$"
        in
        let date_format =
          if
            (not (List.is_empty date_values))
            && List.for_all date_values ~f:(Re.execp iso_date)
          then Csv_profile.Iso
          else if
            (not (List.is_empty date_values))
            && List.for_all date_values ~f:(Re.execp iso_datetime)
          then (
            eprintf
              "Detected date and time; the calendar date will be used for booking.\n";
            Csv_profile.Iso_datetime)
          else (
            eprintf
              "Dates must be interpreted consistently; examples: %s\n"
              (String.concat ~sep:", " (List.take date_values 3));
            choose
              "Date format:"
              [ "YYYY-MM-DD", [], Csv_profile.Iso
              ; "DD/MM/YYYY or DD-MM-YYYY", [ "day-first" ], Csv_profile.Day_first
              ; "MM/DD/YYYY or MM-DD-YYYY", [ "month-first" ], Csv_profile.Month_first
              ; ( "YYYY-MM-DD HH:MM:SS (date only)"
                , [ "iso-datetime" ]
                , Csv_profile.Iso_datetime )
              ])
        in
        let amount_values = values amount_column in
        let amount_format =
          if List.exists amount_values ~f:(fun value -> String.contains value ',')
          then (
            eprintf
              "Check the decimal mark in amounts; examples: %s\n"
              (String.concat ~sep:", " (List.take amount_values 3));
            choose
              "Amount decimal mark:"
              [ "Dot (e.g. 1234.56)", [ "dot" ], Csv_profile.Dot_decimal
              ; "Comma (e.g. 1234,56)", [ "comma" ], Csv_profile.Comma_decimal
              ])
          else Csv_profile.Dot_decimal
        in
        eprintf "A prepaid card holds money you add; a credit card is paid later.\n";
        let account_type =
          choose
            "Account type:"
            [ "Prepaid card", [ "prepaid-card" ], Csv_profile.Prepaid_card
            ; "Credit card", [ "credit-card" ], Csv_profile.Credit_card
            ; "Current account", [ "current"; "checking" ], Csv_profile.Current
            ; "Savings account", [ "savings" ], Csv_profile.Savings
            ]
        in
        let invert_amount =
          match account_type with
          | Csv_profile.Prepaid_card | Credit_card ->
            eprintf
              "A purchase should reduce the card balance. Check how your file writes \
               purchases.\n";
            yes_no "Are purchases positive amounts in the file?"
          | Current | Savings ->
            eprintf
              "Money leaving the account should be negative. Check how your file writes \
               withdrawals or payments.\n";
            yes_no "Are outgoing amounts positive in the file?"
        in
        let raw = Csv_reader.channel ~separator contents in
        ignore (Csv.next raw : string list);
        let records = Csv.input_all raw in
        let summary =
          match List.findi header ~f:(fun _ h -> String.equal h date_column) with
          | None -> None
          | Some (date_pos, _) ->
            List.filter records ~f:(fun row ->
              List.length row = List.length header
              && String.is_empty (String.strip (List.nth_exn row date_pos)))
            |> List.filter_map ~f:(fun row ->
              List.find_mapi row ~f:(fun i value ->
                Option.some_if
                  (String.Caseless.equal (String.strip value) "total")
                  (List.nth_exn header i, String.strip value)))
            |> List.dedup_and_sort ~compare:[%compare: string * string]
            |> (function
             | [ result ] -> Some result
             | _ -> None)
        in
        let skip_when =
          match summary with
          | Some (column, value) ->
            eprintf "Found a summary row with an empty date: %s = %s.\n" column value;
            if yes_no "Skip this summary row?" then Some (column, value) else None
          | None ->
            if yes_no "Skip a summary row with an empty date?"
            then (
              let column =
                choose "Summary label column:" (List.map header ~f:(fun h -> h, [], h))
              in
              let value = read "Summary label value: " in
              Some (column, value))
            else None
        in
        Csv_profile.
          { separator
          ; header
          ; date_column
          ; date_format
          ; amount_format
          ; description_column
          ; amount_column
          ; currency
          ; account_type
          ; invert_amount
          ; skip_when
          })
  in
  let%bind rows = Csv_profile.parse profile contents in
  if Unix.isatty Unix.stdin
  then (
    eprintf
      "Preview: %d transactions; %s account.\n"
      (List.length rows)
      (Csv_profile.account_type_name profile.account_type);
    List.take rows 5
    |> List.iter ~f:(fun (r : Csv_profile.row) ->
      eprintf "  %s  %s\n" (Date.to_string r.date) r.description;
      let currency = Iso4217.to_alpha_code r.currency in
      let source_amount =
        List.zip_exn profile.header r.fields
        |> fun fields ->
        List.Assoc.find fields profile.amount_column ~equal:String.equal
        |> Option.value_exn
      in
      eprintf
        "    CSV amount: %s %s; account movement: %s %s\n"
        source_amount
        currency
        (Quantity.to_string r.amount)
        currency;
      let other =
        List.zip_exn profile.header r.fields
        |> List.filter ~f:(fun (name, value) ->
          (not
             (List.mem
                [ profile.date_column; profile.description_column; profile.amount_column ]
                name
                ~equal:String.equal))
          && not (String.is_empty (String.strip value)))
        |> List.map ~f:(fun (name, value) -> name ^ ": " ^ value)
      in
      if not (List.is_empty other)
      then eprintf "    CSV details: %s\n" (String.concat ~sep:"; " other)));
  let%bind () =
    if not (Unix.isatty Unix.stdin)
    then Ok ()
    else (
      let%bind confirmed =
        Or_error.try_with (fun () ->
          yes_no
            (if setup_only then "Use this CSV layout?" else "Import these transactions?"))
      in
      if confirmed then Ok () else Or_error.error_string "CSV import cancelled")
  in
  Ok profile
;;

let builtin_institution_name registry name =
  List.exists (Registry.institutions registry) ~f:(fun institution ->
    List.mem (Registry.aliases registry institution) name ~equal:String.equal)
;;

let ensure_generic_source database institution expected_kind =
  with_database database (fun db ->
    let%bind companion = Setup.institution_companion_profile db institution in
    let%bind () =
      if String.equal expected_kind "csv-movement" && Option.is_some companion
      then
        Or_error.error_string
          "this institution uses a CSV companion; import it with fin import INSTITUTION \
           PATH"
      else Ok ()
    in
    let%bind events = Ledger.events db in
    if
      List.exists events ~f:(fun (_, event) ->
        String.equal event.Source_event.venue institution
        && List.mem [ "csv-movement"; "bank-transaction" ] event.kind ~equal:String.equal
        && not (String.equal event.kind expected_kind))
    then
      Or_error.error_string
        "this institution already has imports in another generic format; CSV and OFX are \
         separate movement sources and may duplicate transactions"
    else Ok ())
;;

let import_bank database institution paths journal max_mib account_id provider =
  if max_mib < 1 || max_mib > 1024
  then Or_error.error_string "max-file-mib must be between 1 and 1024"
  else (
    let%bind () = require_database database in
    let%bind runtime_registry = registry_for database in
    let%bind definitions = institution_definitions database in
    let%bind account_id = import_account_label runtime_registry institution account_id in
    let journal =
      Option.value
        journal
        ~default:
          (Stdlib.Filename.concat (Stdlib.Filename.dirname database) "bank.journal")
    in
    let selected_profile = ref None in
    let%bind companion_profiles = companion_profiles database in
    let%bind institution, registry =
      if String.equal institution "csv"
      then (
        let%bind provider =
          match provider with
          | Some name -> Validation.token "CSV provider" name
          | None when Unix.isatty Unix.stdin ->
            let rec read_provider () =
              match prompt "Provider name (required): " with
              | None -> Or_error.error_string "CSV provider is required"
              | Some name when String.is_empty (String.strip name) ->
                eprintf "A provider name is required.\n";
                read_provider ()
              | Some name ->
                (match Validation.token "CSV provider" (String.strip name) with
                 | Ok name when not (builtin_institution_name runtime_registry name) ->
                   Ok name
                 | Ok _ ->
                   eprintf "Choose a provider name that is not a built-in institution.\n";
                   read_provider ()
                 | Error error ->
                   eprintf "%s\n" (Error.to_string_hum error);
                   read_provider ())
            in
            read_provider ()
          | None -> Or_error.error_string "generic CSV imports require --provider NAME"
        in
        if builtin_institution_name runtime_registry provider
        then Or_error.error_string "CSV provider must not be a built-in institution name"
        else (
          let%bind () = ensure_generic_source database provider "csv-movement" in
          match paths with
          | [ path ] ->
            let%map profile =
              csv_import_profile database path max_mib ~institution:provider
            in
            selected_profile := Some (Json.encode Csv_profile.codec profile);
            ( provider
            , Builtin.registry_with_csv_profile
                ~definitions
                ~companions:companion_profiles
                ~institution:provider
                profile )
          | _ ->
            Or_error.error_string
              "generic CSV import currently accepts one file at a time"))
      else if Option.is_some provider
      then Or_error.error_string "--provider applies only to generic CSV imports"
      else if Registry.is_registered runtime_registry institution
      then Ok (institution, runtime_registry)
      else
        Ok
          ( institution
          , Builtin.registry_with_ofx
              ~definitions
              ~companions:companion_profiles
              ~institution
              () )
    in
    let%bind report =
      Finances_import.Workflow.run
        ~registry
        ~configure_new_accounts
        ?account_id
        ~max_file_bytes:Int64.(of_int max_mib * 1048576L)
        ~database
        ~institution
        ~paths
        ~journal
        ()
    in
    let%bind () =
      if
        Registry.is_registered runtime_registry institution
        || not
             (List.exists report.inputs.files ~f:(fun row ->
                Poly.equal row.outcome Imported || Poly.equal row.outcome Unchanged))
      then Ok ()
      else
        with_database database (fun db ->
          let%bind existing = Setup.institutions db in
          if List.mem existing institution ~equal:String.equal
          then (
            match !selected_profile with
            | None -> Ok ()
            | Some profile ->
              Setup.set_institution_profile_if_missing db institution profile)
          else Setup.add_institution ?profile:!selected_profile db institution)
    in
    List.iter report.inputs.files ~f:(fun row ->
      printf
        "%s\t%s\tinserted=%d existing=%d%s\n"
        (Bank_import.outcome_name row.outcome)
        row.path
        row.inserted
        row.existing
        (Option.value_map row.message ~default:"" ~f:(fun message -> "\t" ^ message)));
    List.iter report.inputs.ignored ~f:(fun row ->
      printf "ignored\t%s\t%s\n" row.path row.reason);
    let count outcome =
      List.count report.inputs.files ~f:(fun row -> Poly.equal row.outcome outcome)
    in
    printf
      "imported=%d unchanged=%d rejected=%d unsupported=%d unreadable=%d ignored=%d\n"
      (count Imported)
      (count Unchanged)
      (count Rejected)
      (count Unsupported)
      (count Unreadable)
      (List.length report.inputs.ignored);
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
      List.count report.inputs.files ~f:(fun row ->
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

let statement_files ?(extensions = [ ".csv"; ".ofx"; ".zip" ]) paths =
  let rec walk path =
    Or_error.try_with (fun () -> Unix.lstat path)
    |> Result.bind ~f:(fun stat ->
      match stat.Unix.st_kind with
      | Unix.S_REG ->
        if
          List.mem
            [ ".csv"; ".ofx"; ".zip" ]
            (String.lowercase (Stdlib.Filename.extension path))
            ~equal:String.equal
        then Ok [ path ]
        else Or_error.errorf "unsupported statement file: %s" path
      | Unix.S_DIR ->
        let%bind children =
          Bos.OS.Dir.contents (Fpath.v path)
          |> Result.map_error ~f:(fun (`Msg m) -> Error.of_string m)
        in
        let children =
          List.filter children ~f:(fun child ->
            not (String.is_prefix (Fpath.basename child) ~prefix:"."))
          |> List.sort ~compare:Fpath.compare
        in
        List.map children ~f:(fun child ->
          let child = Fpath.to_string child in
          match Unix.lstat child with
          | { st_kind = S_DIR; _ } -> walk child
          | { st_kind = S_REG; _ }
            when List.mem
                   extensions
                   (String.lowercase (Stdlib.Filename.extension child))
                   ~equal:String.equal -> Ok [ child ]
          | _ -> Ok [])
        |> Or_error.combine_errors
        |> Result.map ~f:List.concat
      | _ -> Or_error.errorf "not a regular statement file or directory: %s" path)
  in
  let%bind files =
    List.map paths ~f:walk |> Or_error.combine_errors |> Result.map ~f:List.concat
  in
  let files = List.dedup_and_sort files ~compare:String.compare in
  if List.is_empty files
  then
    Or_error.errorf
      "no %s files found"
      (String.concat
         ~sep:", "
         (List.map extensions ~f:(fun e -> String.uppercase (String.drop_prefix e 1))))
  else Ok files
;;

let generic_statement_files paths = statement_files paths

let import_bank_cmd =
  command
    ~man:
      [ `S Manpage.s_synopsis
      ; `P "$(b,fin import) [$(i,OPTION)]… $(i,INSTITUTION) $(i,PATH)…"
      ; `P "$(b,fin import) [$(i,OPTION)]… $(i,CSV-FILE)"
      ; `S Manpage.s_description
      ; `P
          "Import an institution's files or directories by naming it, for example \
           $(b,fin import wise ~/statements/wise). Run $(b,fin institution list) to see \
           institutions. User-added institutions support OFX 2.x or generic CSV; these \
           are separate primary sources and cannot be mixed. To configure a generic CSV, \
           pass its filename directly."
      ; `P
          "Movement CSV institutions (bundled: cdc) use the same command and the same \
           import pipeline as every other venue: $(b,fin import cdc FILE-OR-DIRECTORY) \
           stores source evidence and proposed operations, and $(b,--inspect) reads a \
           CSV without a database. Review wallets, commodities and kinds afterwards with \
           $(b,fin db sources) and $(b,fin operation list)."
      ]
    "import"
    "Import a bank's statement files, apply mappings, and publish a bank journal."
    Term.(
      const
        (fun
            database
             first
             paths
             journal
             max_mib
             account_id
             provider
             inspect
             summary
             json
           ->
           let movement_profile = Builtin.movement_profile (String.lowercase first) in
           match movement_profile with
           | Some profile when inspect ->
             (match paths with
              | [] -> Or_error.error_string "provide at least one CSV file or directory"
              | _ ->
                let%bind files = statement_files ~extensions:[ ".csv" ] paths in
                inspect_movement_csv profile files summary json)
           | _ ->
             if inspect || summary || json
             then
               Or_error.error_string
                 (match movement_profile with
                  | Some _ -> "--summary and --json require --inspect"
                  | None ->
                    "--inspect, --summary and --json apply only to movement CSV \
                     institutions (for example cdc)")
             else (
               let%bind () = require_database database in
               let%bind runtime_registry = registry_for database in
               let institutions =
                 List.filter
                   (Registry.institutions runtime_registry)
                   ~f:(fun institution ->
                     (not (String.equal institution "csv"))
                     && List.mem
                          (Registry.aliases runtime_registry institution)
                          first
                          ~equal:String.equal)
               in
               let%bind user_institutions =
                 if
                   List.is_empty institutions
                   && ((not (Stdlib.Sys.file_exists first)) || not (List.is_empty paths))
                 then with_database database Setup.institutions
                 else Ok []
               in
               match institutions with
               | [ institution ] ->
                 if List.is_empty paths
                 then Or_error.error_string "provide at least one statement path"
                 else
                   import_bank
                     database
                     institution
                     paths
                     journal
                     max_mib
                     account_id
                     provider
               | _ :: _ :: _ -> Or_error.errorf "ambiguous institution alias: %s" first
               | [] when List.mem user_institutions first ~equal:String.equal ->
                 if List.is_empty paths
                 then
                   Or_error.error_string
                     "provide at least one statement file or directory"
                 else if Option.is_some provider
                 then
                   Or_error.error_string
                     "--provider is unnecessary when the institution is named"
                 else (
                   let%bind files = generic_statement_files paths in
                   let%bind companion =
                     with_database database (fun db ->
                       Setup.institution_companion_profile db first)
                   in
                   if Option.is_some companion
                   then import_bank database first files journal max_mib account_id None
                   else (
                     let csv, ofx =
                       List.partition_tf files ~f:(fun path ->
                         String.Caseless.equal (Stdlib.Filename.extension path) ".csv")
                     in
                     if (not (List.is_empty csv)) && not (List.is_empty ofx)
                     then
                       Or_error.error_string
                         "generic CSV and OFX are separate movement sources; import one \
                          format to avoid duplicate transactions"
                     else if not (List.is_empty ofx)
                     then (
                       let%bind () =
                         ensure_generic_source database first "bank-transaction"
                       in
                       import_bank database first ofx journal max_mib account_id None)
                     else (
                       let%bind () =
                         ensure_generic_source database first "csv-movement"
                       in
                       List.fold_result csv ~init:() ~f:(fun () path ->
                         printf "CSV file: %s\n" path;
                         Out_channel.flush stdout;
                         import_bank
                           database
                           "csv"
                           [ path ]
                           journal
                           max_mib
                           account_id
                           (Some first)))))
               | [] when Stdlib.Sys.file_exists first && Stdlib.Sys.is_directory first ->
                 Or_error.error_string
                   "name the bank before a directory, for example: fin import wise \
                    DIRECTORY"
               | [] when Stdlib.Sys.file_exists first ->
                 import_bank
                   database
                   "csv"
                   (first :: paths)
                   journal
                   max_mib
                   account_id
                   provider
               | [] -> Or_error.errorf "unknown institution or file: %s" first))
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"SOURCE"
              ~doc:
                ("Institution name or generic CSV filename. Built-in institutions: "
                 ^ String.concat
                     ~sep:", "
                     (List.filter (Registry.institutions Builtin.registry) ~f:(fun name ->
                        not (String.equal name "csv")))
                 ^ ". Registered aliases are accepted (for example bourso and boursorama \
                    for boursobank). Unknown CSV layouts are configured interactively."))
      $ Arg.(
          value
          & pos_right 0 string []
          & info
              []
              ~docv:"PATH"
              ~doc:
                "Statement files or directories for the named institution. Directories \
                 are searched recursively; hidden files are skipped. Generic \
                 institutions accept CSV or OFX files and OFX ZIPs, without mixing CSV \
                 and OFX. ZIPs are read in memory; nested ZIPs and encrypted archives \
                 are unsupported. Files are only read, never modified; repeated imports \
                 add only new records. Revolut and Wise CSV clocks are assumed UTC; Wise \
                 matching retains CAMT source-local clocks.")
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
                "Stable account label for account-free exports (Revolut and generic \
                 CSV). Asked on a terminal if omitted; required in scripts. New accounts \
                 missing an IBAN prompt once on a terminal; Enter skips, and account \
                 edit can add it later.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "provider" ]
              ~doc:"Institution name for a generic CSV; prompted on a terminal.")
      $ Arg.(
          value
          & flag
          & info
              [ "inspect" ]
              ~doc:
                "Movement CSV institutions such as cdc: read-only interpretation of the \
                 CSV rows; needs no database.")
      $ Arg.(
          value
          & flag
          & info
              [ "summary" ]
              ~doc:"With --inspect, summarize kinds and identical row candidates.")
      $ json_output)
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
    let%bind statements = Review.statement_reconciliations ?institution db in
    Report_json.print Report_json.statements statements;
    Ok ())
;;

let statements_cmd =
  command
    "list"
    "Show bank-stated balances against the movements each statement lists, as JSON."
    Term.(const statements $ database $ institution_filter)
;;

let companions database institution review =
  with_database database (fun db ->
    let%bind observations = Ledger.companion_observations ?institution db in
    let observations =
      if not review
      then observations
      else
        List.filter observations ~f:(fun o ->
          not
            (Option.exists o.Ledger.matching ~f:(fun m ->
               Poly.equal m.state Bank_match.Matched)))
    in
    let describers = Registry.companions (Accounts.registry db) in
    let%map rows =
      List.map observations ~f:(fun (o : Ledger.companion_observation) ->
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
    let%bind result = Ledger.match_companions ?institution db in
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
    let%bind changed = Review.set_bank_categories db decisions in
    printf "category_changes=%d\n" changed;
    Ok ())
;;

let bank_event_ids_for_operations ?source_event_id db operation_ids =
  let%bind operations = Review.operation_review db in
  let%bind () =
    if Option.is_some source_event_id && List.length operation_ids <> 1
    then Or_error.error_string "--source-event requires exactly one operation ID"
    else Ok ()
  in
  List.map operation_ids ~f:(fun operation_id ->
    match
      List.find operations ~f:(fun (o : Review.operation_review) ->
        Int64.equal o.operation_id operation_id)
    with
    | None -> Or_error.errorf "unknown operation ID: %Ld" operation_id
    | Some operation ->
      (match source_event_id, operation.movements with
       | Some id, movements ->
         (match
            List.find movements ~f:(fun m -> Int64.equal m.Review.anchor_event_id id)
          with
          | Some m when Option.is_some m.review -> Ok id
          | _ ->
            Or_error.errorf
              "source event %Ld is not a bank movement of operation %Ld"
              id
              operation_id)
       | None, [ movement ] when Option.is_some movement.review ->
         Ok movement.anchor_event_id
       | None, [ _ ] ->
         Or_error.errorf "operation %Ld has no reviewable bank movement" operation_id
       | None, [] ->
         Or_error.errorf "operation %Ld has no movement to review" operation_id
       | None, _ ->
         Or_error.errorf
           "operation %Ld has multiple movements; select a side with --source-event \
            where supported"
           operation_id))
  |> Or_error.combine_errors
;;

let category_set database account inputs =
  let stdin_count = List.count inputs ~f:(String.equal "-") in
  if stdin_count > 1
  then Or_error.error_string "standard input can only be specified once"
  else (
    let transaction_id =
      Jsont.Object.map Fn.id
      |> Jsont.Object.mem "operation_id" Json.int64_codec
      |> Jsont.Object.finish
    in
    let%bind id_lists =
      List.map inputs ~f:(fun input ->
        if String.equal input "-"
        then
          if Unix.isatty Unix.stdin
          then
            Or_error.error_string
              "pipe fin operation list --json output into standard input"
          else (
            let%bind contents =
              Or_error.try_with (fun () -> In_channel.input_all In_channel.stdin)
            in
            Json.decode (Jsont.list transaction_id) contents
            |> Or_error.tag ~tag:"invalid transaction list on standard input")
        else
          Or_error.try_with (fun () -> [ Int64.of_string input ])
          |> Or_error.tag ~tag:("invalid operation ID: " ^ input))
      |> Or_error.combine_errors
    in
    let operation_ids = List.concat id_lists in
    let%bind _ =
      Bank_category.create
        ~event_id:1L
        ~account
        ~reason:"Set with fin operation category set"
    in
    with_database database (fun db ->
      let%bind event_ids = bank_event_ids_for_operations db operation_ids in
      let%bind decisions =
        List.map event_ids ~f:(fun event_id ->
          Bank_category.create
            ~event_id
            ~account
            ~reason:"Set with fin operation category set")
        |> Or_error.combine_errors
      in
      let%bind changed = Review.set_bank_categories db decisions in
      printf "category_changes=%d\n" changed;
      Ok ()))
;;

let category_set_cmd =
  command
    "set"
    "Assign or change the category of one or more transactions atomically."
    Term.(
      const category_set
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"ACCOUNT"
              ~doc:"Income or expense account, or equity:unclassified to reset.")
      $ Arg.(
          non_empty
          & pos_right 0 string []
          & info
              []
              ~docv:"OPERATION-ID|-"
              ~doc:
                "Operation IDs, or - to read fin operation list --json from standard \
                 input."))
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

type movement_type_filter =
  | Unknown_type
  | Known_type of Movement_type.t

let parse_movement_type_filter = function
  | "unknown" -> Ok Unknown_type
  | value when String.is_empty (String.strip value) ->
    Or_error.error_string "--type must not be empty"
  | value -> Ok (Known_type (Movement_type.of_string value))
;;

let matches_movement_type_filter filter movement_type =
  match filter with
  | Unknown_type -> Option.is_none movement_type
  | Known_type kind -> Option.value_map movement_type ~default:false ~f:(Poly.equal kind)
;;

type transaction_sort_column =
  | Sort_occurrence
  | Sort_date
  | Sort_amount
  | Sort_currency
  | Sort_institution
  | Sort_account
  | Sort_type
  | Sort_counter_account
  | Sort_description
  | Sort_event

let transaction_sort_columns =
  [ "date", Sort_date
  ; "booked_on", Sort_date
  ; "amount", Sort_amount
  ; "currency", Sort_currency
  ; "institution", Sort_institution
  ; "account", Sort_account
  ; "account-id", Sort_account
  ; "account_id", Sort_account
  ; "type", Sort_type
  ; "movement_type", Sort_type
  ; "counter-account", Sort_counter_account
  ; "counter_account", Sort_counter_account
  ; "description", Sort_description
  ; "event", Sort_event
  ; "event-id", Sort_event
  ; "event_id", Sort_event
  ]
;;

let parse_transaction_sort = function
  | None -> Ok [ Sort_occurrence ]
  | Some value ->
    let%bind columns =
      String.split value ~on:','
      |> List.map ~f:(fun name ->
        let name = String.strip name in
        List.Assoc.find transaction_sort_columns name ~equal:String.equal
        |> Or_error.of_option ~error:(Error.createf "unknown --order-by column: %s" name))
      |> Or_error.combine_errors
    in
    if List.contains_dup columns ~compare:Poly.compare
    then Or_error.error_string "--order-by columns must not be repeated"
    else Ok columns
;;

let parse_list_date label = function
  | None -> Ok None
  | Some text ->
    Or_error.try_with (fun () ->
      let date = Date.of_string text in
      if not (String.equal text (Date.to_string date)) then failwith "expected YYYY-MM-DD";
      Some date)
    |> Or_error.tag ~tag:label
;;

let operation_matches
      ~account_id
      ~institution
      ~movement_type
      ~classification
      ~counter_account
      ~from_date
      ~to_date
      ~search
      (row : Report_json.operation_row)
  =
  let any_movement f =
    List.exists
      row.operation.Review.movements
      ~f:(fun (m : Review.operation_movement_review) ->
        Option.value_map m.review ~default:false ~f)
  in
  let occurred_date =
    Date.of_string (String.prefix (Temporal.to_string row.operation.occurred) 10)
  in
  Option.value_map account_id ~default:true ~f:(fun id ->
    List.exists row.movement_rows ~f:(fun m -> String.equal m.account_id id))
  && Option.value_map institution ~default:true ~f:(fun name ->
    String.equal name row.operation.institution
    || any_movement (fun (r : Review.bank_review) -> String.equal name r.institution))
  && Option.value_map movement_type ~default:true ~f:(fun filter ->
    any_movement (fun (r : Review.bank_review) ->
      matches_movement_type_filter filter r.movement_type))
  && Option.value_map classification ~default:true ~f:(fun wanted ->
    any_movement (fun (r : Review.bank_review) -> Poly.equal r.classification wanted))
  && Option.value_map counter_account ~default:true ~f:(fun account ->
    any_movement (fun (r : Review.bank_review) -> String.equal r.counter_account account))
  && Option.value_map from_date ~default:true ~f:(fun date ->
    Date.compare occurred_date date >= 0)
  && Option.value_map to_date ~default:true ~f:(fun date ->
    Date.compare occurred_date date <= 0)
  && Option.value_map search ~default:true ~f:(fun text ->
    String.Caseless.is_substring row.operation.description ~substring:text)
;;

let compare_operation_column
      column
      (a : Report_json.operation_row)
      (b : Report_json.operation_row)
  =
  let compare_reviews f =
    match a.operation.Review.review, b.operation.Review.review with
    | Some ar, Some br -> f ar br
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> 0
  in
  match column with
  | Sort_occurrence ->
    String.compare
      (Temporal.to_string a.operation.Review.occurred)
      (Temporal.to_string b.operation.Review.occurred)
  | Sort_date ->
    compare_reviews (fun ar br ->
      Date.compare ar.Review.transaction.booked_on br.Review.transaction.booked_on)
  | Sort_amount ->
    compare_reviews (fun ar br ->
      Quantity.compare ar.Review.transaction.amount br.Review.transaction.amount)
  | Sort_currency ->
    compare_reviews (fun ar br ->
      String.compare
        (Iso4217.to_alpha_code ar.Review.transaction.account.currency)
        (Iso4217.to_alpha_code br.Review.transaction.account.currency))
  | Sort_institution -> String.compare a.operation.institution b.operation.institution
  | Sort_account ->
    String.compare
      (Option.value a.account_id ~default:"")
      (Option.value b.account_id ~default:"")
  | Sort_type ->
    let name (row : Report_json.operation_row) =
      Option.bind row.operation.Review.review ~f:(fun r ->
        Option.map r.Review.movement_type ~f:Movement_type.to_string)
      |> Option.value ~default:"-"
    in
    String.compare (name a) (name b)
  | Sort_counter_account ->
    let name (row : Report_json.operation_row) =
      Option.bind row.operation.Review.review ~f:(fun r ->
        if String.equal r.Review.counter_account "equity:unclassified"
        then None
        else Some r.Review.counter_account)
      |> Option.value ~default:"-"
    in
    String.compare (name a) (name b)
  | Sort_description ->
    compare_reviews (fun ar br ->
      String.compare ar.Review.description br.Review.description)
  | Sort_event ->
    let event (row : Report_json.operation_row) =
      Option.map row.operation.Review.review ~f:(fun r -> r.Review.event_id)
      |> Option.value ~default:Int64.max_value
    in
    Int64.compare (event a) (event b)
;;

let list_transactions
      database
      account_id
      institution
      from_date
      to_date
      search
      movement_type
      classification
      counter_account
      limit
      order_by
      order
      json_output
  =
  let%bind sort_columns = parse_transaction_sort order_by in
  let%bind from_date = parse_list_date "--from" from_date in
  let%bind to_date = parse_list_date "--to" to_date in
  let%bind movement_type =
    match movement_type with
    | None -> Ok None
    | Some value -> Result.map (parse_movement_type_filter value) ~f:Option.some
  in
  let%bind () =
    match from_date, to_date, limit with
    | Some first, Some last, _ when Date.compare first last > 0 ->
      Or_error.error_string "--from must not be after --to"
    | _, _, Some n when n <= 0 -> Or_error.error_string "--limit must be positive"
    | _ -> Ok ()
  in
  with_database database (fun db ->
    let%bind accounts = Transfers.bank_account_info db in
    let%bind () =
      match account_id with
      | None -> Ok ()
      | Some id ->
        (match
           List.count accounts ~f:(fun a ->
             String.equal (Bank_account_mapping.id a.Transfers.mapping) id)
         with
         | 1 -> Ok ()
         | 0 -> Or_error.error_string "unknown account ID; see fin account list"
         | _ -> Or_error.error_string "account ID collision; filtering is refused")
    in
    let ids = Hashtbl.Poly.create () in
    List.iter accounts ~f:(fun a ->
      Hashtbl.set
        ids
        ~key:
          (a.Transfers.mapping.institution, Bank_account_mapping.source_account a.mapping)
        ~data:(Bank_account_mapping.id a.mapping));
    let%bind operations = Review.operation_review db in
    let%bind rows =
      List.map operations ~f:(fun (operation : Review.operation_review) ->
        let%map account_id =
          match operation.review with
          | None -> Ok None
          | Some review ->
            let account = review.transaction.account in
            let source =
              Json.encode
                (Jsont.list Jsont.string)
                [ account.bank_id; account.account_id; account.account_type ]
            in
            (match Hashtbl.find ids (review.institution, source) with
             | None -> Or_error.error_string "transaction references an unknown account"
             | Some account_id -> Ok (Some account_id))
        in
        let movement_rows =
          List.filter_map operation.movements ~f:(fun m ->
            Option.map m.Review.review ~f:(fun review ->
              let account = review.Review.transaction.account in
              let source =
                Json.encode
                  (Jsont.list Jsont.string)
                  [ account.bank_id; account.account_id; account.account_type ]
              in
              let account_id =
                Option.value (Hashtbl.find ids (review.institution, source)) ~default:""
              in
              { Report_json.account_id; review }))
        in
        { Report_json.account_id; operation; movement_rows })
      |> Or_error.combine_errors
    in
    let rows =
      List.filter
        rows
        ~f:
          (operation_matches
             ~account_id
             ~institution
             ~movement_type
             ~classification
             ~counter_account
             ~from_date
             ~to_date
             ~search)
      |> fun filtered ->
      List.sort filtered ~compare:(fun a b ->
        let rec compare = function
          | [] ->
            Int64.compare
              a.Report_json.operation.operation_id
              b.Report_json.operation.operation_id
          | column :: rest ->
            (match compare_operation_column column a b with
             | 0 -> compare rest
             | result -> result)
        in
        if order then compare sort_columns else -compare sort_columns)
    in
    let rows = Option.value_map limit ~default:rows ~f:(List.take rows) in
    let print_rows ?out () =
      if json_output
      then Report_json.print (Jsont.list Report_json.operation_row) rows
      else Terminal.operations ?out rows
    in
    if json_output
    then (
      print_rows ();
      Ok ())
    else if Unix.isatty Unix.stdout && not (List.is_empty rows)
    then (
      let pager = Option.value (Sys.getenv "PAGER") ~default:"less -FRX" in
      if String.is_empty (String.strip pager)
      then (
        print_rows ();
        Ok ())
      else (
        let out = Unix.open_process_out pager in
        Exn.protect
          ~f:(fun () ->
            try print_rows ~out () with
            | Sys_error _ -> ())
          ~finally:(fun () ->
            try ignore (Unix.close_process_out out) with
            | Sys_error _ -> ());
        Ok ()))
    else (
      print_rows ();
      Ok ()))
;;

let transaction_list_cmd =
  command
    "list"
    "List imported operations, newest first. Terminal tables use PAGER (default: less \
     -FRX); redirected and JSON output is not paged."
    Term.(
      const list_transactions
      $ database
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account" ] ~docv:"ID" ~doc:"Only this ID from fin account list.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "institution" ] ~docv:"NAME" ~doc:"Only operations from this provider.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "from" ]
              ~docv:"DATE"
              ~doc:"Include booking dates on or after YYYY-MM-DD.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "to" ]
              ~docv:"DATE"
              ~doc:"Include booking dates on or before YYYY-MM-DD.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "search" ]
              ~docv:"TEXT"
              ~doc:"ASCII case-insensitive operation description substring.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "type" ]
              ~docv:"TYPE"
              ~doc:
                "Movement type as displayed, or unknown for movements without a type. \
                 Provider-specific types are accepted.")
      $ Arg.(
          value
          & opt
              (some
                 (enum
                    [ "unclassified", Review.Unclassified
                    ; "category", Category
                    ; "transfer", Transfer
                    ; "settlement", Settlement
                    ; "reversal", Reversal
                    ]))
              None
          & info
              [ "decision" ]
              ~docv:"DECISION"
              ~doc:
                "Accounting decision kind: unclassified, category, transfer, settlement \
                 or reversal.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "category"; "counter-account" ]
              ~docv:"ACCOUNT"
              ~doc:
                "Only operations with a movement carrying this exact counter-account, \
                 including categories.")
      $ Arg.(
          value
          & opt (some int) None
          & info [ "limit" ] ~docv:"COUNT" ~doc:"Maximum number of operations.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "order-by" ]
              ~docv:"COLUMNS"
              ~doc:
                "Comma-separated sort columns: date, amount, currency, institution, \
                 account, type, counter-account, description, event. Default: source \
                 event date/time.")
      $ Arg.(
          value
          & vflag
              false
              [ true, info [ "asc" ] ~doc:"Sort in ascending order."
              ; false, info [ "desc" ] ~doc:"Sort in descending order (default)."
              ])
      $ json_output)
;;

let refresh_transaction_evidence database institution yes =
  with_database database (fun db ->
    let%bind changes = Ledger.preview_source_evidence ?institution db in
    let describe (e : Source_evidence.t) =
      Printf.sprintf
        "card=%s reversal=%s"
        (Option.value_map e.card_reference ~default:"-" ~f:Terminal.text)
        (if e.withdrawal_reversal then "yes" else "no")
    in
    if List.is_empty changes
    then (
      print_endline "No source evidence changes.";
      Ok ())
    else (
      printf "Proposed source evidence changes: %d\n" (List.length changes);
      List.iter changes ~f:(fun (change : Ledger.source_evidence_change) ->
        printf
          "  #%Ld  %s  %s -> %s  %s\n"
          change.event_id
          (Terminal.text change.institution)
          (describe change.before)
          (describe change.after)
          (Terminal.text change.description));
      let%bind confirmed =
        if yes
        then Ok true
        else if Unix.isatty Unix.stdin
        then
          Ok
            (Option.value_map
               (prompt "Apply these source evidence changes? [Y/n]: ")
               ~default:false
               ~f:confirm_default_yes)
        else Or_error.error_string "evidence refresh requires a terminal or --yes"
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%map count =
          Ledger.refresh_source_evidence ?institution db ~expected:changes
        in
        printf "source_evidence_changed=%d\n" count)))
;;

let refresh_transaction_evidence_cmd =
  command
    "refresh-evidence"
    "Preview and refresh imported settlement and reversal evidence."
    Term.(
      const refresh_transaction_evidence
      $ database
      $ institution_filter
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Apply the preview without prompting."))
;;

let transaction_type_list database =
  with_database database (fun db ->
    let%map provider_types = Restore.observed_provider_movement_types db in
    print_endline "Common movement types:";
    List.iter Movement_type.common ~f:(fun (kind, description) ->
      printf "  %-18s %s\n" (Movement_type.to_string kind) description);
    print_endline "\nProvider-specific types in imported data:";
    if List.is_empty provider_types
    then print_endline "  None."
    else List.iter provider_types ~f:(fun name -> printf "  %s\n" (Terminal.text name));
    print_endline
      "\nUse --type unknown with fin operation list to find movements without a type.")
;;

let transaction_type_list_cmd =
  command
    "list"
    "List common and observed provider-specific movement types."
    Term.(const transaction_type_list $ database)
;;

let source_event_selector =
  Arg.(
    value
    & opt (some int64) None
    & info
        [ "source-event" ]
        ~docv:"EVENT-ID"
        ~doc:"Select this source event when the operation has multiple movements.")
;;

let transaction_type_set database value operation_ids source_event_id =
  let%bind kind =
    if String.is_empty (String.strip value) || String.equal value "unknown"
    then
      Or_error.error_string
        "choose a type from fin operation type list; use fin operation type reset to \
         remove an override"
    else Ok (Movement_type.of_string value)
  in
  with_database database (fun db ->
    let%bind event_ids =
      bank_event_ids_for_operations ?source_event_id db operation_ids
    in
    let%map changed =
      Restore.set_movement_type_overrides db ~event_ids ~movement_type:(Some kind)
    in
    printf "type_changes=%d\n" changed)
;;

let transaction_type_set_cmd =
  command
    "set"
    "Set a manual movement type on one or more transactions atomically."
    Term.(
      const transaction_type_set
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"TYPE" ~doc:"Type from fin operation type list.")
      $ Arg.(
          non_empty
          & pos_right 0 int64 []
          & info [] ~docv:"OPERATION-ID" ~doc:"One or more operation IDs.")
      $ source_event_selector)
;;

let transaction_type_reset database operation_id source_event_id =
  with_database database (fun db ->
    let%bind event_id =
      bank_event_ids_for_operations ?source_event_id db [ operation_id ]
      |> Result.map ~f:List.hd_exn
    in
    let%map changed =
      Restore.set_movement_type_override db ~event_id ~movement_type:None
    in
    print_endline
      (if changed then "Movement type override removed." else "No movement type override."))
;;

let transaction_type_reset_cmd =
  command
    "reset"
    "Remove a manual type override and use the imported type."
    Term.(
      const transaction_type_reset
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"OPERATION-ID" ~doc:"Operation ID.")
      $ source_event_selector)
;;

let transaction_type_show database operation_id source_event_id =
  with_database database (fun db ->
    let%bind event_id =
      bank_event_ids_for_operations ?source_event_id db [ operation_id ]
      |> Result.map ~f:List.hd_exn
    in
    let%bind rows = Review.bank_review db in
    let%map row =
      List.find rows ~f:(fun r -> Int64.equal r.Review.event_id event_id)
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
    in
    Terminal.review [ row ];
    if Option.is_none row.movement_type_override
    then print_endline "Type source: imported evidence (no manual override).")
;;

let transaction_type_show_cmd =
  command
    "show"
    "Show a transaction's movement type and its source."
    Term.(
      const transaction_type_show
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"OPERATION-ID" ~doc:"Operation ID.")
      $ source_event_selector)
;;

let transaction_type_history database json_output =
  with_database database (fun db ->
    let%map rows = Restore.movement_type_history db in
    if json_output
    then Report_json.print (Jsont.list Report_json.movement_type_change) rows
    else if List.is_empty rows
    then print_endline "No movement type changes."
    else
      List.iter rows ~f:(fun (r : Restore.movement_type_history_entry) ->
        let name = Option.value_map ~default:"no override" ~f:Movement_type.to_string in
        printf
          "#%Ld  event #%Ld  %s -> %s  %s  %s\n"
          r.id
          r.event_id
          (name r.previous_type)
          (name r.movement_type)
          (Timestamp.to_rfc3339 r.changed_at)
          (Terminal.text r.reason)))
;;

let transaction_type_history_cmd =
  command
    "history"
    "Show audited manual movement type changes."
    Term.(const transaction_type_history $ database $ json_output)
;;

type institution_origin =
  | Built_in of string list
  | Definition
  | Generic

(* Every institution fin knows: bundled, installed from a definition, or added
   as a generic CSV layout. A missing database only lacks the last two. *)
let institution_entries database =
  let present = Stdlib.Sys.file_exists database in
  let%bind definitions = if present then institution_definitions database else Ok [] in
  let built_in =
    List.filter_map (Registry.institutions Builtin.registry) ~f:(fun institution ->
      if String.equal institution "csv"
      then None
      else
        Some
          ( institution
          , Builtin.institution_type institution
          , Built_in (List.tl_exn (Registry.aliases Builtin.registry institution)) ))
  in
  let defined =
    List.filter_map definitions ~f:(fun definition ->
      let name = Institution_definition.institution definition in
      Option.some_if
        (not (Registry.is_registered Builtin.registry name))
        (name, Institution_definition.institution_type definition, Definition))
  in
  let%map generic =
    if not present
    then Ok []
    else
      with_database database (fun db ->
        let%map names = Setup.institutions db in
        List.filter_map names ~f:(fun name ->
          Option.some_if
            (not
               (List.exists definitions ~f:(fun definition ->
                  String.equal name (Institution_definition.institution definition))))
            (name, Institution_type.Bank, Generic)))
  in
  built_in @ defined @ generic
;;

let institution_list database =
  let%bind () = require_database database in
  let%map entries = institution_entries database in
  List.iter entries ~f:(fun (name, institution_type, origin) ->
    let kind = Institution_type.label institution_type in
    match origin with
    | Built_in [] -> printf "%s (%s, built-in)\n" name kind
    | Built_in aliases ->
      printf
        "%s (%s, built-in; aliases: %s)\n"
        name
        kind
        (String.concat ~sep:", " aliases)
    | Definition -> printf "%s (%s, definition)\n" (Terminal.text name) kind
    | Generic -> printf "%s (%s, generic)\n" (Terminal.text name) kind)
;;

let institution_list_cmd =
  command
    "list"
    "List built-in and user-added institutions."
    Term.(const institution_list $ database)
;;

let print_institution_description
      ~institution_type
      (d : Institution_definition.description)
  =
  printf "%s\n" (Terminal.text d.institution);
  printf "  Type: %s\n" (Institution_type.label institution_type);
  printf "  Kind: %s\n" d.kind;
  printf
    "  Aliases: %s\n"
    (if List.is_empty d.aliases
     then "none"
     else String.concat ~sep:", " (List.map d.aliases ~f:Terminal.text));
  print_endline "\n  What fin reads, by role:";
  List.iter d.fields ~f:(fun (role, name) ->
    printf "    %-24s %s\n" role (Terminal.text name));
  if not (List.is_empty d.unread)
  then (
    print_endline "\n  Columns fin does not read:";
    printf "    %s\n" (String.concat ~sep:", " (List.map d.unread ~f:Terminal.text)));
  print_endline "\n  Rules can match:";
  List.iter d.rule_inputs ~f:(fun input -> printf "    %s\n" input)
;;

let print_institution_rules (definition : Institution_definition.t) =
  printf "\n  Default rules (%d):\n" (List.length definition.rules);
  List.iter definition.rules ~f:(fun r ->
    let action =
      match r.action with
      | Type -> "type"
      | Card_reference -> "card-reference"
      | Withdrawal_reversal -> "withdrawal-reversal"
    in
    printf
      "    %3d  %-18s %-40s /%s/%s -> %s\n"
      r.priority
      action
      (Terminal.text r.name)
      (Terminal.text r.pattern)
      (Option.value_map r.field ~default:"" ~f:(fun column ->
         Printf.sprintf " on column %s" (Terminal.text column)))
      (Terminal.text r.target))
;;

let print_institution_data db institution =
  let%bind summary = Explain.institution_summary db institution in
  let%map operations = Explain.operation_summary db institution in
  (match summary.first_on, summary.last_on with
   | Some first, Some last ->
     printf
       "\n  Imported data: %d movements, %s to %s.\n  By type:\n"
       summary.movements
       first
       last;
     List.iter summary.by_type ~f:(fun (kind, n) ->
       printf
         "    %6d  %s\n"
         n
         (Option.value_map kind ~default:"(no type)" ~f:(fun kind ->
            Terminal.text (Movement_type.to_string kind))))
   | _ -> if operations.operations = 0 then print_endline "\n  Imported data: none.");
  match operations.first_on, operations.last_on with
  | Some first, Some last ->
    printf
      "\n  Imported data: %d operations, %s to %s; %d unresolved.\n"
      operations.operations
      first
      last
      operations.unresolved
  | _ -> ()
;;

let print_movement_profile (profile : Movement_csv.profile) =
  printf "%s\n" (Terminal.text profile.institution);
  printf "  Type: %s\n" (Institution_type.label profile.institution_type);
  print_endline "  Kind: movement-csv";
  printf
    "  Aliases: %s\n"
    (if List.is_empty profile.aliases
     then "none"
     else String.concat ~sep:", " (List.map profile.aliases ~f:Terminal.text));
  let c = profile.columns in
  print_endline "\n  What fin reads, by role:";
  List.iter
    [ "timestamp", c.timestamp_column
    ; "currency", c.currency_column
    ; "amount", c.amount_column
    ; "to currency", c.to_currency_column
    ; "to amount", c.to_amount_column
    ; "transaction kind", c.kind_column
    ]
    ~f:(fun (role, name) -> printf "    %-24s %s\n" role (Terminal.text name));
  let read =
    [ c.timestamp_column
    ; c.currency_column
    ; c.amount_column
    ; c.to_currency_column
    ; c.to_amount_column
    ; c.kind_column
    ]
  in
  (match
     List.filter profile.header ~f:(fun h -> not (List.mem read h ~equal:String.equal))
   with
   | [] -> ()
   | unread ->
     print_endline "\n  Columns fin does not read:";
     printf "    %s\n" (String.concat ~sep:", " (List.map unread ~f:Terminal.text)));
  print_endline "\n  How each reported kind becomes an operation:";
  List.iter profile.rules ~f:(fun (rule : Movement_csv.rule) ->
    let shape =
      match rule.legs with
      | Single_in -> "one movement in"
      | Single_out -> "one movement out"
      | Exchange -> "an exchange"
    in
    printf
      "    %-18s %s\n"
      shape
      (String.concat ~sep:", " (List.map rule.kinds ~f:Terminal.text)))
;;

let institution_overview database =
  let%bind entries = institution_entries database in
  let%map rows =
    if Stdlib.Sys.file_exists database
    then
      with_database database (fun db ->
        List.map entries ~f:(fun (name, institution_type, origin) ->
          let%bind movements = Explain.institution_summary db name in
          let%map operations = Explain.operation_summary db name in
          let dates =
            List.filter_opt
              [ movements.first_on
              ; operations.first_on
              ; movements.last_on
              ; operations.last_on
              ]
          in
          let count n = if n = 0 then "-" else Int.to_string n in
          [ name
          ; Institution_type.label institution_type
          ; (match origin with
             | Built_in _ -> "built-in"
             | Definition -> "definition"
             | Generic -> "generic")
          ; count movements.movements
          ; count operations.operations
          ; count operations.unresolved
          ; Option.value (List.min_elt dates ~compare:String.compare) ~default:"-"
          ; Option.value (List.max_elt dates ~compare:String.compare) ~default:"-"
          ])
        |> Or_error.combine_errors)
    else
      Ok
        (List.map entries ~f:(fun (name, institution_type, _) ->
           [ name
           ; Institution_type.label institution_type
           ; "built-in"
           ; "-"
           ; "-"
           ; "-"
           ; "-"
           ; "-"
           ]))
  in
  Terminal.table
    [ "Institution"
    ; "Type"
    ; "Origin"
    ; "Movements"
    ; "Operations"
    ; "Unresolved"
    ; "First"
    ; "Last"
    ]
    rows;
  print_endline "\nDetails and rules: fin institution show INSTITUTION."
;;

let institution_show database name =
  match name with
  | None -> institution_overview database
  | Some name ->
    let%bind installed =
      if Stdlib.Sys.file_exists database then institution_definitions database else Ok []
    in
    let%bind () =
      match
        List.find installed ~f:(fun d ->
          List.mem (Institution_definition.names d) name ~equal:String.equal)
      with
      | Some d ->
        print_institution_description
          ~institution_type:(Institution_definition.institution_type d)
          (Institution_definition.describe d);
        print_institution_rules d;
        Ok ()
      | None ->
        (match Builtin.definition name, Builtin.movement_profile name with
         | Some d, _ ->
           print_institution_description
             ~institution_type:(Institution_definition.institution_type d)
             (Institution_definition.describe d);
           print_institution_rules d;
           Ok ()
         | None, Some profile ->
           print_movement_profile profile;
           Ok ()
         | None, None ->
           Or_error.error_string
             "unknown institution, or one without a definition; see fin institution list")
    in
    let canonical =
      match Builtin.movement_profile name, Builtin.definition name with
      | Some p, _ -> p.institution
      | None, Some d -> Institution_definition.institution d
      | None, None ->
        (match
           List.find installed ~f:(fun d ->
             List.mem (Institution_definition.names d) name ~equal:String.equal)
         with
         | Some d -> Institution_definition.institution d
         | None -> name)
    in
    if Stdlib.Sys.file_exists database
    then with_database database (fun db -> print_institution_data db canonical)
    else (
      print_endline "\n  Imported data: no database yet.";
      Ok ())
;;

let institution_show_cmd =
  command
    "show"
    "Without an argument, give an overview of every institution and what is imported; \
     with one, show what fin reads for it, its default rules and what is imported."
    Term.(
      const institution_show
      $ database
      $ Arg.(value & pos 0 (some string) None & info [] ~docv:"INSTITUTION"))
;;

let institution_add database name sample =
  let%bind name =
    match name with
    | Some name -> Ok name
    | None when Unix.isatty Unix.stdin ->
      (match prompt "Institution name (required): " with
       | Some name -> Ok (String.strip name)
       | None -> Or_error.error_string "institution name is required")
    | None -> Or_error.error_string "institution name is required"
  in
  let%bind name = Validation.token "institution name" name in
  let%bind () = require_database database in
  let%bind runtime_registry = registry_for database in
  let reserved = builtin_institution_name runtime_registry name in
  if reserved
  then Or_error.error_string "institution name is reserved for a built-in importer"
  else (
    let%bind existing = with_database database Setup.institutions in
    if List.mem existing name ~equal:String.equal
    then Or_error.errorf "institution already exists: %s" name
    else (
      let sample =
        match sample with
        | Some _ -> sample
        | None when Unix.isatty Unix.stdin ->
          Option.bind
            (prompt "Sample CSV file (Enter to configure later): ")
            ~f:(fun value ->
              Option.some_if
                (not (String.is_empty (String.strip value)))
                (String.strip value))
        | None -> None
      in
      let%bind profile =
        match sample with
        | None -> Ok None
        | Some path ->
          let%map profile =
            csv_import_profile ~setup_only:true database path 64 ~institution:name
          in
          Some (Json.encode Csv_profile.codec profile)
      in
      with_database database (fun db ->
        let%bind () = Setup.add_institution ?profile db name in
        printf
          "Added %s.%s\n"
          (Terminal.text name)
          (if Option.is_some profile
           then ""
           else " Import an OFX file or a CSV file to configure its layout.");
        Ok ())))
;;

let institution_add_cmd =
  command
    "add"
    "Add an institution for generic CSV or OFX statements."
    Term.(
      const institution_add
      $ database
      $ Arg.(
          value
          & pos 0 (some string) None
          & info
              []
              ~docv:"NAME"
              ~doc:"Institution name; prompted on a terminal if omitted.")
      $ Arg.(
          value
          & pos 1 (some string) None
          & info
              []
              ~docv:"SAMPLE.csv"
              ~doc:"Sample CSV for layout setup; no transactions are imported."))
;;

let definition_rules (definition : Institution_definition.t) =
  let name = Institution_definition.institution definition in
  let%bind () =
    if
      List.contains_dup
        (List.map definition.rules ~f:(fun r -> r.name))
        ~compare:String.compare
    then Or_error.error_string "definition has duplicate rule names"
    else Ok ()
  in
  List.map definition.rules ~f:(fun (r : Institution_definition.rule) ->
    let kind =
      match r.action with
      | Type -> "type"
      | Card_reference -> "card-reference"
      | Withdrawal_reversal -> "withdrawal-reversal"
    in
    let%bind action = Classification_rule.action_of_fields kind r.target None in
    Classification_rule.create
      ~mode:Automatic
      ~account_type:r.account_type
      ~priority:r.priority
      ~name:r.name
      ~pattern:(Some r.pattern)
      ~institution:(Some name)
      ~match_type:(Option.map r.match_type ~f:Classification_rule.condition_of_string)
      ~case_sensitive:r.case_sensitive
      ~action
      ?field:r.field
      ())
  |> Or_error.combine_errors
;;

let read_institution_definition path =
  let%bind text = read_file path in
  let%bind definition = Json.decode Institution_definition.codec text in
  let%map _ = definition_rules definition in
  definition
;;

let check_institution_definition path sample account_id =
  let%bind definition = read_institution_definition path in
  let module A = (val Institution_definition.adapter definition : Adapter.S) in
  printf
    "Definition: %s\nFormats: %s\n"
    (Terminal.text A.institution)
    (String.concat ~sep:", " A.formats);
  match sample with
  | None ->
    print_endline "Definition is valid.";
    Ok ()
  | Some sample ->
    let%bind contents = read_file sample in
    let%bind format =
      A.probe ~filename:sample contents
      |> Or_error.of_option
           ~error:(Error.of_string "sample is not recognized by this definition")
    in
    let%bind document =
      A.parse
        ~context:{ account_label = Some (Option.value account_id ~default:"preview") }
        ~format
        contents
    in
    let events =
      document.events
      @ List.concat_map document.statements ~f:(fun s -> s.Document.events)
    in
    let%bind movements = List.map events ~f:A.decode |> Or_error.combine_errors in
    let%bind rules = definition_rules definition in
    let rules =
      List.sort rules ~compare:(fun a b ->
        match Int.compare a.Classification_rule.priority b.priority with
        | 0 -> String.compare a.name b.name
        | n -> n)
    in
    let%bind typing =
      rules
      |> List.filter_map ~f:(fun (rule : Classification_rule.t) ->
        match rule.action with
        | Type kind ->
          Some
            (let%map r =
               Source_typing.make_rule
                 ?field:rule.field
                 (Option.value_exn rule.pattern)
                 rule.account_type
                 rule.case_sensitive
                 (Movement_type.to_string kind)
             in
             Source_typing.with_match_type
               r
               (Option.map rule.match_type ~f:Classification_rule.condition_name))
        | _ -> None)
      |> Or_error.combine_errors
    in
    let typing = Source_typing.create ~institution:A.institution typing in
    let movements = List.map movements ~f:(Source_typing.apply typing) in
    printf
      "Sample: %s; %d statements, %d transactions, %d companion rows.\n"
      (Terminal.text format)
      (List.length document.statements)
      (List.length movements)
      (List.length document.companion_rows);
    List.take movements 5
    |> List.iter ~f:(fun m ->
      printf
        "  %s  %s %s  %s  %s\n"
        (Date.to_string m.Movement.booked_on)
        (Quantity.to_string m.amount)
        (Iso4217.to_alpha_code m.account.currency)
        (Option.value_map m.movement_type ~default:"unknown" ~f:Movement_type.to_string)
        (Terminal.text (String.concat ~sep:" | " m.descriptions)));
    (match
       List.find A.companions ~f:(fun (c : Companion.t) -> String.equal c.format format)
     with
     | None -> ()
     | Some companion ->
       List.take document.companion_rows 5
       |> List.iter ~f:(fun (number, fields) ->
         let row : Companion.row =
           { row_id = Int64.of_int number
           ; document_id = 0L
           ; institution = A.institution
           ; format
           ; row_number = number
           ; fields
           ; context = document.context
           }
         in
         match companion.describe row with
         | Error e -> eprintf "  Row %d: %s\n" number (Error.to_string_hum e)
         | Ok details ->
           printf
             "  Row %d: %s\n"
             number
             (String.concat
                ~sep:"; "
                (List.filter_map details ~f:(fun (key, value) ->
                   Option.map value ~f:(fun v ->
                     Terminal.text key ^ "=" ^ Terminal.text v))))));
    print_endline "Preview only; no data was imported.";
    Ok ()
;;

let institution_definition_check_cmd =
  command
    "check"
    "Validate an institution definition and preview a sample statement."
    Term.(
      const check_institution_definition
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"DEFINITION.json")
      $ Arg.(value & pos 1 (some string) None & info [] ~docv:"SAMPLE")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "account-id" ]
              ~docv:"LABEL"
              ~doc:"Account label for a primary CSV sample (default: preview)."))
;;

let show_institution_definition database name =
  let builtin = Builtin.definition name in
  let%bind () = require_database database in
  let%bind definitions = institution_definitions database in
  let%bind definition =
    match
      List.find definitions ~f:(fun d ->
        String.equal (Institution_definition.institution d) name)
    with
    | Some d -> Ok d
    | None ->
      Or_error.of_option builtin ~error:(Error.of_string "unknown institution definition")
  in
  print_endline (Json.encode ~format:Jsont.Indent Institution_definition.codec definition);
  Ok ()
;;

let institution_definition_show_cmd =
  command
    "show"
    "Show a bundled or installed institution definition as JSON."
    Term.(
      const show_institution_definition
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION"))
;;

let set_institution_definition database name path yes =
  let%bind () = require_database database in
  let%bind definition = read_institution_definition path in
  let module A = (val Institution_definition.adapter definition : Adapter.S) in
  if not (String.equal A.institution name)
  then
    Or_error.error_string "definition institution does not match the selected institution"
  else (
    let%bind registry = registry_for database in
    let%bind generic = with_database database Setup.institutions in
    if
      not
        (Registry.is_registered registry name || List.mem generic name ~equal:String.equal)
    then
      Or_error.errorf "unknown institution: %s; create it with fin institution add" name
    else (
      let registered =
        Registry.institutions registry
        |> List.filter ~f:(fun owner -> not (String.equal owner name))
        |> List.concat_map ~f:(Registry.aliases registry)
      in
      if
        List.contains_dup (name :: A.aliases) ~compare:String.compare
        || List.exists A.aliases ~f:(fun alias ->
          List.mem (registered @ generic) alias ~equal:String.equal)
      then Or_error.error_string "institution alias is already in use"
      else (
        let%bind _ = Registry.create [ (module A : Adapter.S) ] in
        let%bind rules = definition_rules definition in
        let json = Json.encode Institution_definition.codec definition in
        let%bind expected, expected_rules =
          with_database database (fun db ->
            let%bind definitions = Setup.institution_definitions db in
            let%map rules = Import.classification_rules db in
            List.Assoc.find definitions name ~equal:String.equal, rules)
        in
        let%bind source_unchanged =
          match expected with
          | None -> Ok false
          | Some text ->
            let%map old = Json.decode Institution_definition.codec text in
            Poly.equal old.source definition.source
        in
        let%bind _ =
          with_database database (fun db ->
            Ledger.set_institution_definition
              ~dry_run:true
              db
              ~name
              ~definition:json
              ~expected
              ~expected_rules
              ~rules
              ~source_unchanged)
        in
        let added, retained =
          List.partition_tf rules ~f:(fun r ->
            not (List.exists expected_rules ~f:(fun old -> String.equal old.name r.name)))
        in
        printf
          "Definition for %s: %s; %d new rules, %d existing rules retained.\n"
          (Terminal.text name)
          (String.concat ~sep:", " A.formats)
          (List.length added)
          (List.length retained);
        List.iter added ~f:(fun r ->
          printf
            "  add %s: %s -> %s\n"
            (Terminal.text r.name)
            (Classification_rule.action_name r.action)
            (Terminal.text (Classification_rule.action_target r.action)));
        List.iter retained ~f:(fun r ->
          printf "  retain existing %s\n" (Terminal.text r.name));
        let%bind confirmed =
          if yes
          then Ok true
          else if Unix.isatty Unix.stdin
          then
            Ok
              (Option.value_map
                 (prompt "Attach this definition? [Y/n]: ")
                 ~default:false
                 ~f:confirm_default_yes)
          else Or_error.error_string "attaching a definition requires a terminal or --yes"
        in
        if not confirmed
        then (
          print_endline "Nothing changed.";
          Ok ())
        else
          with_database database (fun db ->
            let%map count =
              Ledger.set_institution_definition
                db
                ~name
                ~definition:json
                ~expected
                ~expected_rules
                ~rules
                ~source_unchanged
            in
            printf
              "Attached definition for %s; added %d rules. Existing transactions and \
               rules are unchanged.\n"
              (Terminal.text name)
              count))))
;;

let institution_definition_set_cmd =
  command
    "set"
    "Attach an import definition to an existing institution."
    Term.(
      const set_institution_definition
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"DEFINITION.json")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Accept the preview without prompting."))
;;

let institution_companion_show database name =
  match Builtin.companion_layout name with
  | Some profile ->
    print_endline (Json.encode ~format:Jsont.Indent Csv_companion_layout.codec profile);
    Ok ()
  | None ->
    with_database database (fun db ->
      let%bind profile = Setup.institution_companion_profile db name in
      match profile with
      | None ->
        Or_error.error_string "no companion profile configured for this institution"
      | Some json ->
        let%map profile = Json.decode Csv_companion_layout.codec json in
        print_endline
          (Json.encode ~format:Jsont.Indent Csv_companion_layout.codec profile))
;;

let institution_companion_show_cmd =
  command
    "show"
    "Show the institution's CSV companion layout as JSON."
    Term.(
      const institution_companion_show
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION"))
;;

let institution_companion_set database name path yes =
  let%bind contents = read_file path in
  let%bind profile = Json.decode Csv_companion_layout.codec contents in
  let%bind () =
    if Option.is_none profile.currency
    then Or_error.error_string "a generic companion profile must specify its ISO currency"
    else Ok ()
  in
  let json = Json.encode Csv_companion_layout.codec profile in
  with_database database (fun db ->
    let%bind current = Setup.institution_companion_profile db name in
    let%bind () =
      Setup.set_institution_companion_profile ~dry_run:true db name ~expected:current json
    in
    printf
      "CSV companion profile for %s:\n%s\n"
      (Terminal.text name)
      (Json.encode ~format:Jsont.Indent Csv_companion_layout.codec profile);
    print_endline
      "CSV rows will enrich matching OFX movements. Previously imported rows keep their \
       original layout.";
    let%bind confirmed =
      if yes
      then Ok true
      else if Unix.isatty Unix.stdin
      then
        Ok
          (Option.value_map
             (prompt "Save this companion profile? [Y/n]: ")
             ~default:false
             ~f:confirm_default_yes)
      else
        Or_error.error_string "profile changes require confirmation; use --yes in scripts"
    in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () =
        Setup.set_institution_companion_profile db name ~expected:current json
      in
      printf "Saved companion profile for %s.\n" (Terminal.text name)))
;;

let institution_companion_set_cmd =
  command
    "set"
    "Configure an institution's OFX companion CSV layout."
    Term.(
      const institution_companion_set
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"FILE")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Save without prompting."))
;;

let institution_companion_clear database name yes =
  with_database database (fun db ->
    let%bind expected =
      Setup.institution_companion_profile db name
      |> Or_error.bind ~f:(function
        | Some profile -> Ok profile
        | None -> Or_error.error_string "no companion profile configured")
    in
    let%bind () =
      Setup.clear_institution_companion_profile ~dry_run:true db name ~expected
    in
    printf "Remove the CSV companion profile for %s?\n" (Terminal.text name);
    let%bind confirmed =
      if yes
      then Ok true
      else if Unix.isatty Unix.stdin
      then
        Ok
          (Option.value_map
             (prompt "Remove this companion profile? [Y/n]: ")
             ~default:false
             ~f:confirm_default_yes)
      else
        Or_error.error_string
          "profile removal requires confirmation; use --yes in scripts"
    in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () = Setup.clear_institution_companion_profile db name ~expected in
      printf "Removed companion profile for %s.\n" (Terminal.text name)))
;;

let institution_companion_clear_cmd =
  command
    "clear"
    "Remove a companion profile before importing companion rows."
    Term.(
      const institution_companion_clear
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Remove without prompting."))
;;

let institution_delete database name yes =
  with_database database (fun db ->
    let%bind () = Setup.delete_institution ~dry_run:true db name in
    printf "Delete institution %s and its saved CSV layout?\n" (Terminal.text name);
    let%bind confirmed =
      if yes
      then Ok true
      else if Unix.isatty Unix.stdin
      then
        Ok
          (Option.value_map
             (prompt "Delete this institution? [Y/n]: ")
             ~default:false
             ~f:confirm_default_yes)
      else Or_error.error_string "deletion requires confirmation; use --yes in scripts"
    in
    if not confirmed
    then (
      print_endline "Nothing deleted.";
      Ok ())
    else (
      let%bind () = Setup.delete_institution db name in
      print_endline "Institution deleted.";
      Ok ()))
;;

let institution_delete_cmd =
  command
    "delete"
    "Delete an unused user-added institution and its saved CSV layout."
    Term.(
      const institution_delete
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"User-added institution name.")
      $ Arg.(
          value & flag & info [ "yes" ] ~doc:"Delete without interactive confirmation."))
;;

type settlement_suggestion =
  { transaction : Review.bank_review
  ; card : Transfers.bank_account_info
  ; bank : Transfers.bank_account_info
  }

let account_reference (mapping : Bank_account_mapping.t) =
  { Bank_account_config.institution = mapping.institution
  ; bank_id = mapping.bank_id
  ; account_id = mapping.account_id
  ; account_type = mapping.account_type
  }
;;

let settlement_suggestions db =
  let%bind rows = Review.bank_review db in
  let%bind accounts = Transfers.bank_account_info db in
  let suggestions =
    List.filter_map rows ~f:(fun (row : Review.bank_review) ->
      let movement = row.transaction in
      let hint = row.source_evidence.card_reference in
      Option.bind hint ~f:(fun hint ->
        if
          (not (String.equal row.counter_account "equity:unclassified"))
          || Option.is_some row.reason
          || Quantity.(movement.amount >= zero)
          || not (Quantity.is_zero movement.fee)
        then None
        else (
          let banks =
            List.filter accounts ~f:(fun a ->
              String.equal a.Transfers.mapping.institution row.institution
              && String.equal a.mapping.bank_id movement.account.bank_id
              && String.equal a.mapping.account_id movement.account.account_id
              && String.equal a.mapping.account_type movement.account.account_type)
          in
          match banks with
          | [ bank ] when not (String.equal bank.iban_source "not_applicable") ->
            let parent = account_reference bank.mapping in
            let cards =
              List.filter accounts ~f:(fun card ->
                String.equal card.Transfers.mapping.institution row.institution
                && String.equal card.mapping.bank_id movement.account.bank_id
                && String.equal card.mapping.account_id hint
                && String.equal card.mapping.account_type "CREDITCRD"
                && Iso4217.equal card.currency movement.account.currency
                && Option.for_all card.settlement_account ~f:(fun linked ->
                  String.equal linked.institution parent.institution
                  && String.equal
                       (Bank_account_config.source_account linked)
                       (Bank_account_config.source_account parent)))
            in
            (match cards with
             | [ card ] ->
               let card_has_payment =
                 List.exists rows ~f:(fun other ->
                   let tx = other.Review.transaction in
                   String.equal other.institution card.mapping.institution
                   && String.equal tx.account.bank_id card.mapping.bank_id
                   && String.equal tx.account.account_id card.mapping.account_id
                   && String.equal tx.account.account_type card.mapping.account_type
                   && Iso4217.equal tx.account.currency movement.account.currency
                   && Quantity.equal tx.amount (Quantity.neg movement.amount)
                   && Int.abs (Date.diff tx.booked_on movement.booked_on) <= 7)
               in
               Option.some_if (not card_has_payment) { transaction = row; card; bank }
             | _ -> None)
          | _ -> None)))
  in
  Ok suggestions
;;

let print_settlement_suggestions suggestions =
  if List.is_empty suggestions
  then print_endline "No unambiguous card settlements found."
  else
    List.iter suggestions ~f:(fun s ->
      printf
        "#%Ld  %s  %s %s  %s / %s\n    %s\n"
        s.transaction.event_id
        (Date.to_string s.transaction.transaction.booked_on)
        (Quantity.to_string s.transaction.transaction.amount)
        (Iso4217.to_alpha_code s.transaction.transaction.account.currency)
        (Terminal.text s.card.mapping.institution)
        (Terminal.text s.card.mapping.account_id)
        (Terminal.text (String.concat ~sep:" | " s.transaction.transaction.descriptions)))
;;

let settlement_candidates_cmd =
  command
    "candidates"
    "Find unclassified card settlements with a unique card match."
    Term.(
      const (fun database ->
        with_database database (fun db ->
          let%map suggestions = settlement_suggestions db in
          print_settlement_suggestions suggestions))
      $ database)
;;

let card_setup_plan
      ~(card : Transfers.bank_account_info)
      ~(bank : Transfers.bank_account_info)
  =
  let proposed =
    if String.is_prefix card.mapping.ledger_account ~prefix:"liabilities:"
    then card.mapping.ledger_account
    else
      "liabilities:cards:"
      ^ card.mapping.institution
      ^ ":"
      ^ Bank_account_mapping.id card.mapping
  in
  let parent = account_reference bank.mapping in
  let needs_link =
    not
      (Option.value_map card.settlement_account ~default:false ~f:(fun linked ->
         String.equal linked.institution parent.institution
         && String.equal
              (Bank_account_config.source_account linked)
              (Bank_account_config.source_account parent)))
  in
  let needs_mapping = not (String.equal proposed card.mapping.ledger_account) in
  let update =
    Bank_account_config.
      { reference = account_reference card.mapping
      ; ledger_account = (if needs_mapping then Some proposed else None)
      ; iban = Keep
      ; settlement_account = (if needs_link then Set parent else Keep)
      }
  in
  proposed, needs_link, needs_mapping, update
;;

let setup_card
      db
      ~(card : Transfers.bank_account_info)
      ~(bank : Transfers.bank_account_info)
      ~yes
      ~allow_yes
  =
  let proposed, needs_link, needs_mapping, update = card_setup_plan ~card ~bank in
  if (not needs_link) && not needs_mapping
  then Ok true
  else if yes && not allow_yes
  then Or_error.error_string "card setup requires interactive confirmation"
  else if (not yes) && not (Unix.isatty Unix.stdin)
  then Or_error.error_string "card setup requires a terminal"
  else (
    printf
      "Card setup proposal: %s / %s\n"
      (Terminal.text card.mapping.institution)
      (Terminal.text card.mapping.account_id);
    if needs_mapping
    then
      printf
        "  Card account: %s -> %s\n"
        (Terminal.text card.mapping.ledger_account)
        (Terminal.text proposed);
    if needs_link
    then
      printf
        "  Pay from: %s / %s\n"
        (Terminal.text bank.mapping.institution)
        (Terminal.text bank.mapping.account_id);
    let confirmed =
      yes
      || Option.value_map
           (prompt "Apply this card setup? [Y/n]: ")
           ~default:false
           ~f:confirm_default_yes
    in
    if not confirmed
    then Ok false
    else (
      let%map _ = Accounts.apply_bank_accounts db [ update ] in
      true))
;;

let settlement_tui db suggestions =
  if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout)
  then Or_error.error_string "the full-screen review requires a terminal"
  else if List.is_empty suggestions
  then (
    print_endline "No unambiguous card settlements found.";
    Ok ())
  else (
    let choices =
      List.map suggestions ~f:(fun s ->
        let proposed, needs_link, needs_mapping, _ =
          card_setup_plan ~card:s.card ~bank:s.bank
        in
        let movement = s.transaction.transaction in
        let currency = Iso4217.to_alpha_code movement.account.currency in
        let title =
          Printf.sprintf
            "#%Ld  %s  %s %s"
            s.transaction.event_id
            (Date.to_string movement.booked_on)
            (Quantity.to_string movement.amount)
            currency
        in
        let details =
          [ "Description: " ^ String.concat ~sep:" | " movement.descriptions
          ; "Checking: " ^ s.bank.mapping.institution ^ " / " ^ s.bank.mapping.account_id
          ; "Card: " ^ s.card.mapping.institution ^ " / " ^ s.card.mapping.account_id
          ; "Card repayment: "
            ^ Quantity.to_string (Quantity.neg movement.amount)
            ^ " "
            ^ currency
          ; "Card account: " ^ proposed
          ]
          @ (if needs_mapping
             then [ "Change card account from: " ^ s.card.mapping.ledger_account ]
             else [])
          @ (if needs_link
             then [ "Link card to the checking account shown above." ]
             else [])
          @ [ "Only this settlement will be recorded." ]
        in
        Settlement_tui.{ title; details })
    in
    match Settlement_tui.run choices with
    | None ->
      print_endline "Nothing changed.";
      Ok ()
    | Some index ->
      let s = List.nth_exn suggestions index in
      let _, needs_link, needs_mapping, setup =
        card_setup_plan ~card:s.card ~bank:s.bank
      in
      let%bind () =
        if needs_link || needs_mapping
        then
          Accounts.confirm_card_settlement_with_setup
            db
            ~event_id:s.transaction.event_id
            ~card_id:(Bank_account_mapping.id s.card.mapping)
            ~expected_ledger:s.card.mapping.ledger_account
            ~expected_link:s.card.settlement_account
            ~setup
        else
          Transfers.confirm_card_settlement
            db
            s.transaction.event_id
            ~card_id:(Bank_account_mapping.id s.card.mapping)
            ~expected_ledger:s.card.mapping.ledger_account
      in
      print_endline
        "Card settlement recorded. Use fin journal export for an updated journal.";
      Ok ())
;;

let settlement_review database requested_event card_id yes tui =
  with_database database (fun db ->
    let%bind suggestions = settlement_suggestions db in
    let suggestions =
      List.filter suggestions ~f:(fun s ->
        Option.value_map
          card_id
          ~default:true
          ~f:(String.equal (Bank_account_mapping.id s.card.mapping)))
    in
    if tui
    then
      if yes
      then Or_error.error_string "--tui cannot be combined with --yes"
      else (
        let suggestions =
          List.filter suggestions ~f:(fun s ->
            Option.value_map
              requested_event
              ~default:true
              ~f:(Int64.equal s.transaction.event_id))
        in
        settlement_tui db suggestions)
    else (
      let%bind event_id =
        match requested_event with
        | Some id -> Ok (Some id)
        | None when yes -> Or_error.error_string "provide EVENT-ID when using --yes"
        | None when not (Unix.isatty Unix.stdin) ->
          Or_error.error_string "select a settlement in a terminal or provide EVENT-ID"
        | None ->
          print_settlement_suggestions suggestions;
          if List.is_empty suggestions
          then Ok None
          else (
            let rec choose () =
              match prompt "Choose an event ID (or q): " with
              | None -> Ok None
              | Some answer when String.Caseless.equal (String.strip answer) "q" ->
                Ok None
              | Some answer ->
                (match
                   Option.try_with (fun () -> Int64.of_string (String.strip answer))
                   |> Option.bind ~f:(fun id ->
                     List.find suggestions ~f:(fun s ->
                       Int64.equal s.transaction.event_id id))
                 with
                 | Some selected -> Ok (Some selected.transaction.event_id)
                 | None ->
                   eprintf "Choose a listed event ID or q.\n";
                   choose ())
            in
            choose ())
      in
      match event_id with
      | None ->
        print_endline "Nothing changed.";
        Ok ()
      | Some event_id ->
        let suggestion =
          List.find suggestions ~f:(fun s ->
            Int64.equal s.transaction.event_id event_id
            && Option.value_map
                 card_id
                 ~default:true
                 ~f:(String.equal (Bank_account_mapping.id s.card.mapping)))
        in
        let%bind row, card, planned =
          match suggestion with
          | None ->
            let%map row, card = Transfers.card_settlement_preview ?card_id db event_id in
            row, card, None
          | Some s ->
            let proposed, needs_link, needs_mapping, update =
              card_setup_plan ~card:s.card ~bank:s.bank
            in
            if (not needs_link) && not needs_mapping
            then (
              let%map row, card =
                Transfers.card_settlement_preview ?card_id db event_id
              in
              row, card, None)
            else if yes
            then Or_error.error_string "card setup requires interactive confirmation"
            else
              Ok
                ( s.transaction
                , s.card
                , Some (s, proposed, needs_link, needs_mapping, update) )
        in
        print_endline "Bank transaction:";
        Terminal.review [ row ];
        printf
          "Card: %s / %s (ID %s)\nCard account: %s\n"
          (Terminal.text card.mapping.institution)
          (Terminal.text card.mapping.account_id)
          (Bank_account_mapping.id card.mapping)
          (Terminal.text
             (Option.value_map
                planned
                ~default:card.mapping.ledger_account
                ~f:(fun (_, proposed, _, _, _) -> proposed)));
        Option.iter planned ~f:(fun (s, proposed, needs_link, needs_mapping, _) ->
          print_endline "Proposed card setup (saved only if you confirm):";
          if needs_mapping
          then
            printf
              "  Card account: %s -> %s\n"
              (Terminal.text card.mapping.ledger_account)
              (Terminal.text proposed);
          if needs_link
          then
            printf
              "  Pay from: %s / %s\n"
              (Terminal.text s.bank.mapping.institution)
              (Terminal.text s.bank.mapping.account_id));
        printf
          "Card repayment: %s %s\n"
          (Quantity.to_string (Quantity.neg row.transaction.amount))
          (Iso4217.to_alpha_code row.transaction.account.currency);
        print_endline
          "If the card statement already includes this payment, review it as a transfer \
           instead.";
        let%bind confirmed =
          if yes
          then Ok true
          else if Unix.isatty Unix.stdin
          then
            Ok
              (Option.value_map
                 (prompt
                    (if Option.is_some planned
                     then "Apply this setup and record this one settlement? [Y/n]: "
                     else "Record this card settlement? [Y/n]: "))
                 ~default:false
                 ~f:confirm_default_yes)
          else
            Or_error.error_string "settlement requires confirmation; use --yes in scripts"
        in
        if not confirmed
        then (
          print_endline "Nothing changed.";
          Ok ())
        else (
          let%bind () =
            match planned with
            | None ->
              Transfers.confirm_card_settlement
                ~card_id:(Bank_account_mapping.id card.mapping)
                ~expected_ledger:card.mapping.ledger_account
                db
                event_id
            | Some (s, _, _, _, setup) ->
              Accounts.confirm_card_settlement_with_setup
                db
                ~event_id
                ~card_id:(Bank_account_mapping.id s.card.mapping)
                ~expected_ledger:s.card.mapping.ledger_account
                ~expected_link:s.card.settlement_account
                ~setup
          in
          print_endline
            "Card settlement recorded. Use fin journal export for an updated journal.";
          Ok ())))
;;

let settlement_review_cmd =
  command
    "review"
    "Review and confirm a bank debit as a linked card settlement."
    Term.(
      const settlement_review
      $ database
      $ Arg.(
          value
          & pos 0 (some int64) None
          & info
              []
              ~docv:"EVENT-ID"
              ~doc:
                "Bank transaction ID; omit to choose a suggested settlement in a \
                 terminal.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "card" ]
              ~docv:"ID"
              ~doc:"Card account ID, needed if several cards are linked.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Confirm without prompting.")
      $ Arg.(
          value & flag & info [ "tui" ] ~doc:"Use the experimental full-screen review."))
;;

let settlement_undo database event_id yes =
  with_database database (fun db ->
    let%bind confirmed =
      if yes
      then Ok true
      else if Unix.isatty Unix.stdin
      then
        Ok
          (Option.value_map
             (prompt "Undo this card settlement? [Y/n]: ")
             ~default:false
             ~f:confirm_default_yes)
      else Or_error.error_string "undo requires confirmation; use --yes in scripts"
    in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%bind () = Transfers.undo_card_settlement db event_id in
      print_endline
        "Card settlement undone. Use fin journal export for an updated journal.";
      Ok ()))
;;

let settlement_undo_cmd =
  command
    "undo"
    "Undo a confirmed card settlement."
    Term.(
      const settlement_undo
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"EVENT-ID" ~doc:"Confirmed settlement event ID.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Undo without prompting."))
;;

let settlement_history_cmd =
  command
    "history"
    "List card settlement decisions as JSON."
    Term.(
      const (fun database ->
        with_database database (fun db ->
          let%map rows = Transfers.card_settlement_history db in
          Report_json.print (Jsont.list Report_json.card_settlement_change) rows))
      $ database)
;;

let find_rule db name =
  let%bind rules = Import.classification_rules db in
  List.find rules ~f:(fun r -> String.equal r.Classification_rule.name name)
  |> Or_error.of_option ~error:(Error.of_string ("unknown rule: " ^ name))
;;

let rule_add
      database
      name
      positional_pattern
      positional_target
      institution
      case_sensitive
      settlement
      match_type
      set_type
      category
      description
      automatic
      account_type
      priority
      field
      replace
  =
  let%bind pattern =
    match positional_pattern, description with
    | Some _, Some _ ->
      Or_error.error_string
        "provide the description either positionally or with --description"
    | Some p, None | None, Some p -> Ok (Some p)
    | None, None -> Ok None
  in
  let%bind target =
    match positional_target, category with
    | Some _, Some _ ->
      Or_error.error_string "provide the category either positionally or with --category"
    | Some c, None | None, Some c -> Ok (Some c)
    | None, None -> Ok None
  in
  let parse_type = function
    | None -> Ok None
    | Some value when String.is_empty (String.strip value) || String.equal value "unknown"
      -> Or_error.error_string "choose a movement type from fin operation type list"
    | Some value -> Ok (Some (Movement_type.of_string value))
  in
  let%bind match_type =
    match match_type with
    | Some "unknown" -> Ok (Some Classification_rule.Unknown)
    | other ->
      let%map kind = parse_type other in
      Option.map kind ~f:(fun kind -> Classification_rule.Is kind)
  in
  let%bind set_type = parse_type set_type in
  let%bind action =
    match settlement, target, set_type with
    | true, Some card, None -> Ok (Classification_rule.Settlement card)
    | true, _, _ ->
      Or_error.error_string
        "settlement rules require a card ID and do not set a movement type"
    | false, Some account, Some kind ->
      Ok (Classification_rule.Type_and_category (kind, account))
    | false, Some account, None -> Ok (Classification_rule.Category account)
    | false, None, Some kind -> Ok (Classification_rule.Type kind)
    | false, None, None -> Or_error.error_string "specify --set-type, --category, or both"
  in
  let%bind () =
    match automatic, action with
    | true, (Classification_rule.Type _ | Category _ | Type_and_category _) -> Ok ()
    | true, Settlement _ -> Or_error.error_string "settlement rules require review"
    | true, (Card_reference | Withdrawal_reversal_hint) -> Ok ()
    | false, _ -> Ok ()
  in
  let%bind rule =
    Classification_rule.create
      ~mode:(if automatic then Automatic else Review)
      ~account_type
      ~priority
      ~name
      ~pattern
      ~institution
      ~match_type
      ~case_sensitive
      ~action
      ?field
      ()
  in
  with_database database (fun db ->
    let%bind observed = Restore.observed_provider_movement_types db in
    let%bind () =
      List.filter_opt
        [ Option.bind match_type ~f:(function
            | Is kind -> Some kind
            | Unknown -> None)
        ; set_type
        ]
      |> List.map ~f:(function
        | Movement_type.Other name when not (List.mem observed name ~equal:String.equal)
          -> Or_error.error_string "unknown movement type; see fin operation type list"
        | _ -> Ok ())
      |> Or_error.combine_errors_unit
    in
    let%bind () =
      match action with
      | Settlement id ->
        let%bind accounts = Transfers.bank_account_info db in
        (match
           List.filter accounts ~f:(fun a ->
             String.equal (Bank_account_mapping.id a.Transfers.mapping) id)
         with
         | [ card ]
           when List.mem
                  [ "CREDITCRD"; "credit-card" ]
                  card.mapping.account_type
                  ~equal:String.equal -> Ok ()
         | [ _ ] -> Or_error.error_string "settlement target must be a credit card"
         | [] -> Or_error.error_string "unknown card ID; see fin account list"
         | _ -> Or_error.error_string "card ID collision; rule is refused")
      | _ -> Ok ()
    in
    let%bind () =
      if replace
      then Ledger.replace_classification_rule db rule
      else Ledger.add_classification_rule db rule
    in
    printf
      "Saved rule %s (%s). Run fin rule preview %s to review existing matches.\n"
      (Terminal.text name)
      (if automatic then "automatic on import" else "review")
      (Terminal.text name);
    Ok ())
;;

let field_doc =
  "Match this field instead of the main description: text (any description), or a field \
   the source carries such as name, memo, reference or type. Automatic type rules only."
;;

let field_arg ~settlement =
  if settlement
  then Term.const None
  else
    Arg.(value & opt (some string) None & info [ "field" ] ~docv:"FIELD" ~doc:field_doc)
;;

let rule_add_cmd ~settlement =
  let pattern_arg =
    if settlement
    then
      Term.(
        const Option.some
        $ Arg.(
            required
            & pos 1 (some string) None
            & info [] ~docv:"REGEX" ~doc:"Description expression."))
    else
      Arg.(
        value
        & pos 1 (some string) None
        & info
            []
            ~docv:"REGEX"
            ~doc:"Optional description expression; alternatively use --description.")
  in
  let target_arg =
    if settlement
    then
      Term.(
        const Option.some
        $ Arg.(
            required
            & pos 2 (some string) None
            & info [] ~docv:"CARD-ID" ~doc:"Credit card ID from fin account list."))
    else
      Arg.(
        value
        & pos 2 (some string) None
        & info [] ~docv:"CATEGORY" ~doc:"Optional category; alternatively use --category.")
  in
  let set_type_arg =
    if settlement
    then Term.const None
    else
      Arg.(
        value
        & opt (some string) None
        & info
            [ "set-type" ]
            ~docv:"TYPE"
            ~doc:"Propose this movement type; see fin operation type list.")
  in
  let category_arg =
    if settlement
    then Term.const None
    else
      Arg.(
        value
        & opt (some string) None
        & info
            [ "category" ]
            ~docv:"ACCOUNT"
            ~doc:"Propose this expense or income account.")
  in
  let description_arg =
    if settlement
    then Term.const None
    else
      Arg.(
        value
        & opt (some string) None
        & info
            [ "description" ]
            ~docv:"REGEX"
            ~doc:
              "Perl-style expression: automatic rules match the first source \
               description; reviewed rules also match display names.")
  in
  command
    (if settlement then "add-settlement" else "add")
    (if settlement
     then "Save a rule for a linked card's settlements."
     else "Save a rule to assign a type, category, or both.")
    Term.(
      const rule_add
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Unique rule name.")
      $ pattern_arg
      $ target_arg
      $ institution_filter
      $ Arg.(
          value
          & flag
          & info [ "case-sensitive" ] ~doc:"Match descriptions case sensitively.")
      $ const settlement
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "match-type" ]
              ~docv:"TYPE"
              ~doc:
                "Match a movement type or unknown; automatic type actions match the \
                 parser type, while category-only rules match the imported type.")
      $ set_type_arg
      $ category_arg
      $ description_arg
      $ Arg.(
          value
          & flag
          & info
              [ "automatic" ]
              ~doc:"Apply this rule on import (settlements require review).")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account-type" ] ~docv:"TYPE" ~doc:"Match this source account type.")
      $ Arg.(
          value
          & opt int 1000
          & info [ "priority" ] ~docv:"NUMBER" ~doc:"Rule order; lower numbers run first.")
      $ field_arg ~settlement
      $ Arg.(
          value
          & flag
          & info
              [ "replace" ]
              ~doc:"Replace the named rule; existing transactions are unchanged."))
;;

let rule_add_evidence
      database
      name
      action
      pattern
      institution
      case_sensitive
      account_type
      match_type
      priority
      replace
  =
  let%bind rule =
    Classification_rule.create
      ~mode:Automatic
      ~account_type
      ~priority
      ~name
      ~pattern:(Some pattern)
      ~institution
      ~match_type:(Option.map match_type ~f:Classification_rule.condition_of_string)
      ~case_sensitive
      ~action
      ()
  in
  with_database database (fun db ->
    let%bind () =
      if replace
      then Ledger.replace_classification_rule db rule
      else Ledger.add_classification_rule db rule
    in
    printf
      "Saved evidence rule %s. It applies to future imports; run fin operation \
       refresh-evidence to preview existing changes.\n"
      (Terminal.text name);
    Ok ())
;;

let rule_add_evidence_cmd =
  command
    "add-evidence"
    "Add an automatic settlement-reference or withdrawal-reversal evidence rule."
    Term.(
      const rule_add_evidence
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Unique rule name.")
      $ Arg.(
          required
          & pos
              1
              (some
                 (enum
                    [ "card-reference", Classification_rule.Card_reference
                    ; "withdrawal-reversal", Classification_rule.Withdrawal_reversal_hint
                    ]))
              None
          & info
              []
              ~docv:"KIND"
              ~doc:"Evidence kind: card-reference or withdrawal-reversal.")
      $ Arg.(
          required
          & pos 2 (some string) None
          & info
              []
              ~docv:"REGEX"
              ~doc:
                "Match the first source description; card-reference requires a single \
                 description and uses capture group 1.")
      $ institution_filter
      $ Arg.(value & flag & info [ "case-sensitive" ] ~doc:"Match case sensitively.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account-type" ] ~docv:"TYPE" ~doc:"Match this source account type.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "match-type" ]
              ~docv:"TYPE"
              ~doc:"Match the stored imported type, or unknown.")
      $ Arg.(
          value
          & opt int 1000
          & info [ "priority" ] ~docv:"NUMBER" ~doc:"Lower numbers run first.")
      $ Arg.(value & flag & info [ "replace" ] ~doc:"Replace an existing rule."))
;;

let rule_list database json_output =
  with_database database (fun db ->
    let%map rules = Import.classification_rules db in
    if json_output
    then Report_json.print (Jsont.list Report_json.classification_rule) rules
    else if List.is_empty rules
    then print_endline "No rules configured."
    else
      List.iter rules ~f:(fun r ->
        printf
          "%s  mode=%s  priority=%d  %s -> %s  /%s/  %s  account-type=%s  match-type=%s  \
           set-type=%s  field=%s\n"
          (Terminal.text r.name)
          (match r.mode with
           | Automatic -> "automatic"
           | Review -> "review"
           | Disabled -> "disabled")
          r.priority
          (Classification_rule.action_name r.action)
          (Terminal.text (Classification_rule.action_target r.action))
          (Terminal.text (Option.value r.pattern ~default:"any description"))
          (Terminal.text (Option.value r.institution ~default:"all institutions"))
          (Terminal.text (Option.value r.account_type ~default:"any"))
          (Option.value_map
             r.match_type
             ~default:"any"
             ~f:Classification_rule.condition_name)
          (Option.value_map
             (Classification_rule.action_type r.action)
             ~default:"none"
             ~f:Movement_type.to_string)
          (Terminal.text (Option.value r.field ~default:"code"))))
;;

let rule_list_cmd =
  command
    "list"
    "List saved transaction rules."
    Term.(const rule_list $ database $ json_output)
;;

let rule_mode database name value =
  let%bind mode =
    match value with
    | "automatic" -> Ok Classification_rule.Automatic
    | "review" -> Ok Classification_rule.Review
    | "disabled" -> Ok Classification_rule.Disabled
    | _ -> Or_error.error_string "mode must be automatic, review, or disabled"
  in
  with_database database (fun db ->
    let%map () = Ledger.set_classification_rule_mode db name mode in
    printf
      "Rule %s is now %s. Existing transactions are unchanged.\n"
      (Terminal.text name)
      value)
;;

let rule_mode_cmd =
  command
    "mode"
    "Set a rule to automatic, review, or disabled."
    Term.(
      const rule_mode
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"MODE"))
;;

let print_rule_preview (rule : Classification_rule.t) matches =
  let eligible, skipped = List.partition_tf matches ~f:(fun m -> m.Accounts.eligible) in
  printf
    "Rule %s: %s -> %s; %d eligible, %d already reviewed or ineligible.\n"
    (Terminal.text rule.name)
    (Classification_rule.action_name rule.action)
    (Terminal.text (Classification_rule.action_target rule.action))
    (List.length eligible)
    (List.length skipped);
  let show_current m =
    Terminal.review [ m.Accounts.transaction ];
    if Option.is_none m.transaction.movement_type_override
    then print_endline "  Type source: imported evidence (no manual override)."
  in
  if not (List.is_empty eligible)
  then (
    print_endline "Eligible matches:";
    List.iter eligible ~f:(fun m ->
      show_current m;
      if m.apply_type
      then
        Option.iter (Classification_rule.action_type rule.action) ~f:(fun kind ->
          printf "  Proposed type: %s\n" (Movement_type.to_string kind));
      if m.apply_account
      then
        printf
          "  Proposed %s: %s\n"
          (match rule.action with
           | Settlement _ -> "settlement"
           | _ -> "category")
          (Terminal.text (Classification_rule.action_target rule.action));
      Option.iter m.settlement_ledger ~f:(fun ledger ->
        if m.apply_account then printf "  Settlement account: %s\n" (Terminal.text ledger));
      Option.iter m.explanation ~f:(fun reason ->
        printf "  Skipped action: %s\n" (Terminal.text reason));
      print_endline ""));
  if not (List.is_empty skipped)
  then (
    print_endline "Skipped matches:";
    List.iter skipped ~f:(fun m ->
      show_current m;
      printf
        "  Skipped: %s\n\n"
        (Terminal.text (Option.value m.explanation ~default:"ineligible"))))
;;

let rule_from_category database operation_id name contains institution save yes =
  with_database database (fun db ->
    let%bind event_id =
      bank_event_ids_for_operations db [ operation_id ] |> Result.map ~f:List.hd_exn
    in
    let%bind rows = Review.bank_review db in
    let%bind seed =
      List.find rows ~f:(fun (r : Review.bank_review) -> Int64.equal r.event_id event_id)
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
    in
    let%bind () =
      if Poly.equal seed.classification Review.Category
      then Ok ()
      else Or_error.error_string "choose a categorized transaction"
    in
    let text = Option.value contains ~default:seed.description |> String.strip in
    let%bind () =
      if
        String.is_empty text
        || not (String.Caseless.is_substring seed.description ~substring:text)
      then
        Or_error.error_string
          "--contains must be nonempty text in the transaction description"
      else Ok ()
    in
    let pattern =
      String.to_list text
      |> List.map ~f:(fun c ->
        let s = String.of_char c in
        if
          List.mem
            [ '.'; '^'; '$'; '*'; '+'; '?'; '('; ')'; '['; ']'; '{'; '}'; '|'; '\\' ]
            c
            ~equal:Char.equal
        then "\\" ^ s
        else s)
      |> String.concat
    in
    let%bind rule =
      Classification_rule.create
        ~mode:Review
        ~account_type:None
        ~priority:1000
        ~name
        ~pattern:(Some pattern)
        ~institution
        ~match_type:None
        ~case_sensitive:false
        ~action:(Category seed.counter_account)
        ()
    in
    let%bind matches = Accounts.preview_classification_rule db rule in
    print_rule_preview rule matches;
    if not save
    then (
      print_endline "Draft only; pass --save to add this review-mode rule.";
      Ok ())
    else (
      let confirmed =
        yes
        || Option.value_map
             (prompt "Save this review-mode rule? [y/N]: ")
             ~default:false
             ~f:confirm_default_no
      in
      if not confirmed
      then (
        print_endline "Rule not saved.";
        Ok ())
      else (
        let%bind () = Ledger.add_classification_rule db rule in
        printf "Saved review rule %s.\n" (Terminal.text name);
        Ok ())))
;;

let rule_from_category_cmd =
  command
    "from-category"
    "Preview a literal description rule from a categorized transaction."
    Term.(
      const rule_from_category
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"OPERATION-ID")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"NAME")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "contains" ]
              ~docv:"TEXT"
              ~doc:
                "Literal text from the example description; defaults to its full \
                 description.")
      $ institution_filter
      $ Arg.(
          value
          & flag
          & info [ "save" ] ~doc:"Save the previewed review-mode rule after confirmation.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"With --save, skip confirmation."))
;;

let rule_preview database name force json_output =
  with_database database (fun db ->
    let%bind rule = find_rule db name in
    let%bind () =
      match rule.action with
      | Card_reference | Withdrawal_reversal_hint ->
        Or_error.error_string
          "use fin operation refresh-evidence to preview automatic evidence changes"
      | Type _ when Option.is_some rule.field ->
        Or_error.error_string
          "this rule names a field and applies automatically; use fin rule test to \
           preview it"
      | _ -> Ok ()
    in
    let%map matches = Accounts.preview_classification_rule ~force db rule in
    if json_output
    then Report_json.print (Jsont.list Report_json.classification_rule_match) matches
    else print_rule_preview rule matches)
;;

let rule_preview_cmd =
  command
    "preview"
    "Preview a saved rule without changing transactions."
    Term.(
      const rule_preview
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Rule name from fin rule list.")
      $ Arg.(
          value
          & flag
          & info [ "force" ] ~doc:"Include existing categories that would change.")
      $ json_output)
;;

let rule_apply database name force yes =
  with_database database (fun db ->
    let%bind rule = find_rule db name in
    let%bind () =
      match rule.action with
      | Card_reference | Withdrawal_reversal_hint ->
        Or_error.error_string
          "use fin operation refresh-evidence to preview and apply evidence changes"
      | Type _ when Option.is_some rule.field ->
        Or_error.error_string
          "this rule names a field and applies automatically; use fin rule test to \
           preview it"
      | _ -> Ok ()
    in
    let%bind matches = Accounts.preview_classification_rule ~force db rule in
    print_rule_preview rule matches;
    let selected = List.filter matches ~f:(fun m -> m.Accounts.eligible) in
    if List.is_empty selected
    then (
      print_endline "Nothing to apply.";
      Ok ())
    else (
      let%bind confirmed =
        if yes
        then Ok true
        else if Unix.isatty Unix.stdin
        then
          Ok
            (Option.value_map
               (prompt "Apply to these transactions? [Y/n]: ")
               ~default:false
               ~f:confirm_default_yes)
        else Or_error.error_string "rule application requires a terminal or --yes"
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%bind changed =
          Accounts.apply_classification_rule ~force db ~expected:rule ~selected
        in
        printf "rule_changes=%d\n" changed;
        Ok ())))
;;

let rule_apply_cmd =
  command
    "apply"
    "Preview and explicitly apply a saved rule."
    Term.(
      const rule_apply
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Rule name from fin rule list.")
      $ Arg.(
          value
          & flag
          & info [ "force" ] ~doc:"Replace existing categories on matching transactions.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Apply without prompting."))
;;

let rule_delete database name yes =
  with_database database (fun db ->
    let%bind _ = find_rule db name in
    let%bind confirmed =
      if yes
      then Ok true
      else if Unix.isatty Unix.stdin
      then
        Ok
          (Option.value_map
             (prompt "Delete this rule? [Y/n]: ")
             ~default:false
             ~f:confirm_default_yes)
      else Or_error.error_string "rule deletion requires a terminal or --yes"
    in
    if not confirmed
    then (
      print_endline "Nothing deleted.";
      Ok ())
    else (
      let%bind () = Ledger.delete_classification_rule db name in
      print_endline "Rule deleted.";
      Ok ()))
;;

let rule_delete_cmd =
  command
    "delete"
    "Delete a saved rule; existing decisions are retained."
    Term.(
      const rule_delete
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Rule name from fin rule list.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Delete without prompting."))
;;

let rule_rename database old_name new_name =
  with_database database (fun db ->
    let%map () = Ledger.rename_classification_rule db ~old_name ~new_name in
    printf "Renamed rule %s to %s.\n" (Terminal.text old_name) (Terminal.text new_name))
;;

let rule_rename_cmd =
  command
    "rename"
    "Rename a saved rule; existing decision history is retained."
    Term.(
      const rule_rename
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"OLD-NAME" ~doc:"Existing rule name.")
      $ Arg.(
          required
          & pos 1 (some string) None
          & info [] ~docv:"NEW-NAME" ~doc:"New, unused rule name."))
;;

let print_type_rule_test
      (rule : Classification_rule.t)
      (result : Explain.type_rule_test)
      limit
  =
  let count f = List.count result.matches ~f:(fun r -> f r.Explain.outcome) in
  printf
    "Draft rule %s (automatic on import): %s -> %s, %s.\n"
    (Terminal.text rule.name)
    (Terminal.text (Option.value rule.pattern ~default:"any description"))
    (Terminal.text (Classification_rule.action_target rule.action))
    (Option.value_map rule.institution ~default:"all institutions" ~f:Terminal.text);
  printf "Scanned %d movements; %d match.\n" result.scanned (List.length result.matches);
  printf
    "  newly typed: %d\n\
    \  type changed: %d\n\
    \  already this type: %d\n\
    \  decided by an earlier rule: %d\n\
    \  manual type override: %d\n"
    (count (function
       | Newly_typed -> true
       | _ -> false))
    (count (function
       | Changed -> true
       | _ -> false))
    (count (function
       | Unchanged -> true
       | _ -> false))
    (count (function
       | Shadowed _ -> true
       | _ -> false))
    (count (function
       | Overridden -> true
       | _ -> false));
  printf
    "Movements without a type: %d -> %d.\n"
    result.untyped_before
    result.untyped_after;
  let name = Option.value_map ~default:"none" ~f:Movement_type.to_string in
  let section title select =
    let rows = List.filter result.matches ~f:(fun r -> select r.Explain.outcome) in
    if not (List.is_empty rows)
    then (
      printf "\n%s:\n" title;
      List.iter (List.take rows limit) ~f:(fun r ->
        printf
          "  #%Ld  %s  %s -> %s  %s%s\n"
          r.Explain.event_id
          (Terminal.text r.institution)
          (name r.before)
          (name r.after)
          (Terminal.text r.description)
          (match r.outcome with
           | Shadowed rule_name -> "  [" ^ Terminal.text rule_name ^ "]"
           | _ -> ""));
      let hidden = List.length rows - limit in
      if hidden > 0 then printf "  ... %d more (use --limit)\n" hidden)
  in
  section "Type changes" (function
    | Explain.Changed -> true
    | _ -> false);
  section "Decided by an earlier rule; the draft changes nothing here" (function
    | Explain.Shadowed _ -> true
    | _ -> false);
  section "Newly typed" (function
    | Explain.Newly_typed -> true
    | _ -> false);
  section "Manual type override; unchanged" (function
    | Explain.Overridden -> true
    | _ -> false)
;;

let rule_test
      database
      name
      positional_pattern
      institution
      case_sensitive
      match_type
      set_type
      description
      account_type
      priority
      field
      limit
      json_output
  =
  let%bind pattern =
    match positional_pattern, description with
    | Some _, Some _ ->
      Or_error.error_string
        "provide the description either positionally or with --description"
    | Some p, None | None, Some p -> Ok (Some p)
    | None, None -> Ok None
  in
  let parse_type = function
    | None -> Ok None
    | Some value when String.is_empty (String.strip value) || String.equal value "unknown"
      -> Or_error.error_string "choose a movement type from fin operation type list"
    | Some value -> Ok (Some (Movement_type.of_string value))
  in
  let%bind match_type =
    match match_type with
    | Some "unknown" -> Ok (Some Classification_rule.Unknown)
    | other ->
      let%map kind = parse_type other in
      Option.map kind ~f:(fun kind -> Classification_rule.Is kind)
  in
  let%bind kind =
    let%bind kind = parse_type set_type in
    Or_error.of_option
      kind
      ~error:(Error.of_string "specify --set-type: only type rules can be tested")
  in
  let%bind rule =
    Classification_rule.create
      ~mode:Automatic
      ~account_type
      ~priority
      ~name
      ~pattern
      ~institution
      ~match_type
      ~case_sensitive
      ~action:(Classification_rule.Type kind)
      ?field
      ()
  in
  with_database database (fun db ->
    let%bind observed = Restore.observed_provider_movement_types db in
    let%bind () =
      List.filter_opt
        [ Option.bind match_type ~f:(function
            | Is kind -> Some kind
            | Unknown -> None)
        ; Some kind
        ]
      |> List.map ~f:(function
        | Movement_type.Other name when not (List.mem observed name ~equal:String.equal)
          -> Or_error.error_string "unknown movement type; see fin operation type list"
        | _ -> Ok ())
      |> Or_error.combine_errors_unit
    in
    let%map result = Explain.test_type_rule db rule in
    if json_output
    then Report_json.print Report_json.type_rule_test result
    else print_type_rule_test rule result limit)
;;

let rule_test_cmd =
  command
    "test"
    "Test a draft type rule against stored movements without saving it."
    Term.(
      const rule_test
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"NAME"
              ~doc:"Draft rule name; a saved rule with this name is replaced in the test.")
      $ Arg.(
          value
          & pos 1 (some string) None
          & info
              []
              ~docv:"REGEX"
              ~doc:"Optional description expression; alternatively use --description.")
      $ institution_filter
      $ Arg.(
          value
          & flag
          & info [ "case-sensitive" ] ~doc:"Match descriptions case sensitively.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "match-type" ]
              ~docv:"TYPE"
              ~doc:"Match the type the parser reported, or unknown.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "set-type" ]
              ~docv:"TYPE"
              ~doc:"The movement type the rule assigns; see fin operation type list.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "description" ]
              ~docv:"REGEX"
              ~doc:"Perl-style expression matched against the first source description.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account-type" ] ~docv:"TYPE" ~doc:"Match this source account type.")
      $ Arg.(
          value
          & opt int 1000
          & info [ "priority" ] ~docv:"NUMBER" ~doc:"Rule order; lower numbers run first.")
      $ field_arg ~settlement:false
      $ Arg.(
          value
          & opt int 10
          & info [ "limit" ] ~docv:"COUNT" ~doc:"Examples shown per outcome.")
      $ json_output)
;;

let type_name = Option.value_map ~default:"no type" ~f:Movement_type.to_string

let print_explain_checks ~all (checks : Explanation.check list) =
  let hidden = ref 0 in
  List.iter checks ~f:(fun c ->
    match c.verdict with
    | Decides kind ->
      printf
        "       decides  %s  /%s/ -> %s   [%s]\n"
        (Terminal.text c.name)
        (Terminal.text c.pattern)
        (match kind with
         | Some kind -> Terminal.text (Movement_type.to_string kind)
         | None -> "leaves it untyped")
        (Terminal.text c.input)
    | Also_matches ->
      printf
        "       matches too, but an earlier rule decides  %s  /%s/   [%s]\n"
        (Terminal.text c.name)
        (Terminal.text c.pattern)
        (Terminal.text c.input)
    | Does_not_match reason ->
      if all
      then
        printf
          "       no match  %s  /%s/: %s\n"
          (Terminal.text c.name)
          (Terminal.text c.pattern)
          reason
      else Int.incr hidden);
  if !hidden > 0 then printf "       %d rules did not match (use --all)\n" !hidden;
  if List.is_empty checks then print_endline "       no rules apply"
;;

let print_explanation ~all (e : Explanation.t) =
  printf
    "Movement #%Ld  %s  %s  %s %s  (account type %s)\n"
    e.event_id
    (Terminal.text e.institution)
    e.booked_on
    e.amount
    e.currency
    (Terminal.text e.account_type);
  List.iter e.descriptions ~f:(fun d -> printf "  Description: %s\n" (Terminal.text d));
  printf
    "\nType: %s  (%s)\n"
    (type_name e.effective)
    (match e.source with
     | Manual_override -> "manual override"
     | Rules -> "type rules"
     | Untyped -> "nothing decides it");
  printf
    "  1. Manual override: %s\n"
    (Option.value_map e.override ~default:"none" ~f:Movement_type.to_string);
  printf
    "  2. Type rules, each on the field it names (default: description). Companion \
     columns count as fields:\n";
  print_explain_checks ~all e.checks;
  printf "     the rules give: %s\n\n" (type_name e.rules_type);
  match e.category with
  | Some (account, reason) ->
    printf "Category: %s (%s)\n" (Terminal.text account) (Terminal.text reason)
  | None ->
    print_endline "Category: none recorded. Category rules: see fin rule preview NAME."
;;

let rule_explain database operation_id all =
  with_database database (fun db ->
    let%bind event_id =
      bank_event_ids_for_operations db [ operation_id ] |> Result.map ~f:List.hd_exn
    in
    let%map explanation = Explain.explain_movement db event_id in
    print_explanation ~all explanation)
;;

let rule_explain_cmd =
  command
    "explain"
    "Explain how a movement got its type, rule by rule."
    Term.(
      const rule_explain
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"OPERATION-ID" ~doc:"Operation ID from fin operation list.")
      $ Arg.(
          value
          & flag
          & info [ "all" ] ~doc:"Also list the rules that did not match, with the reason."))
;;

let print_fields indent fields =
  let width =
    List.fold fields ~init:0 ~f:(fun acc (name, _) -> Int.max acc (String.length name))
  in
  List.iter fields ~f:(fun (name, value) ->
    printf "%s%-*s  %s\n" indent width (Terminal.text name) (Terminal.text value))
;;

let print_document indent (d : Transaction_detail.document) =
  printf
    "%sdocument #%Ld, sha256 %s, imported %s\n"
    indent
    d.id
    (String.prefix d.sha256 12)
    d.imported_at;
  List.iter d.paths ~f:(fun path -> printf "%s  file: %s\n" indent (Terminal.text path))
;;

let print_transaction_detail (t : Transaction_detail.t) =
  let e = t.explanation in
  printf
    "Transaction #%Ld  %s  %s  %s %s\n"
    e.event_id
    (Terminal.text e.institution)
    e.booked_on
    e.amount
    e.currency;
  List.iter e.descriptions ~f:(fun d -> printf "  Description: %s\n" (Terminal.text d));
  printf
    "  Type: %s (%s)\n"
    (type_name e.effective)
    (match e.source with
     | Manual_override -> "manual override"
     | Rules -> "type rules"
     | Untyped -> "nothing decides it");
  printf
    "  Category: %s\n"
    (match e.category with
     | Some (account, reason) ->
       Printf.sprintf "%s (%s)" (Terminal.text account) (Terminal.text reason)
     | None -> "none recorded");
  Option.iter t.transfer ~f:(fun (first, second, reason) ->
    printf
      "  Transfer: confirmed with #%Ld (%s)\n"
      (if Int64.equal first e.event_id then second else first)
      (Terminal.text reason));
  print_endline "\nSource";
  print_fields
    "  "
    [ "institution", t.venue
    ; "account", t.account
    ; "account type", e.account_type
    ; "kind", t.kind
    ; "source id", t.source_id
    ; "occurred", t.occurred
    ; "retrieved", t.retrieved_at
    ];
  print_endline "\nImported from";
  if List.is_empty t.documents
  then print_endline "  no stored document"
  else List.iter t.documents ~f:(print_document "  ");
  List.iter t.statements ~f:(fun s ->
    printf
      "  statement %s: account %s %s, %s, balance %s to %s\n"
      (Option.value_map s.reference ~default:"(no reference)" ~f:Terminal.text)
      (Terminal.text s.source_account)
      s.currency
      s.period
      s.opening
      s.closing);
  print_endline "\nSource fields (payload)";
  String.split_lines t.payload
  |> List.iter ~f:(fun line -> printf "  %s\n" (Terminal.text line));
  if not (List.is_empty t.fields)
  then (
    print_endline "\nSource columns (stored, matchable by rules)";
    print_fields "  " t.fields);
  (match t.companions with
   | [] -> print_endline "\nCompanion files: no matched rows"
   | companions ->
     printf "\nCompanion files: %d matched\n" (List.length companions);
     List.iter companions ~f:(fun c ->
       printf
         "  %s row %d, %s (%s: %s)\n"
         (Terminal.text c.format)
         c.row_number
         c.state
         (Terminal.text c.policy)
         (Terminal.text c.reason);
       print_document "    " c.document;
       print_fields "    " c.fields;
       Option.iter c.display_name ~f:(fun name ->
         printf "    display name: %s\n" (Terminal.text name));
       List.iter c.tags ~f:(fun (tag, value) ->
         printf "    tag %s: %s\n" (Terminal.text tag) (Terminal.text value))));
  print_endline "\nUse fin rule explain OPERATION-ID to inspect its type rules."
;;

let show_operation_links db event_id =
  let%map operations = Review.operation_ids_for_events db [ event_id ] in
  List.iter operations ~f:(fun (_, operation_id) ->
    printf "Operation: #%Ld (fin db event operations %Ld)\n" operation_id event_id)
;;

(* A source event without a bank movement, such as a CDC row, has no
   review detail. Show the operation it supports instead of failing. *)
let show_operation_only db event_id =
  let%bind operations = Operations.operations_for_event db event_id in
  match operations with
  | [] -> Or_error.error_string "unknown source event ID"
  | operations ->
    print_endline "This source event has no bank movement; showing its operations.";
    List.iter operations ~f:(fun (id, operation) ->
      printf "Operation #%Ld  %s\n" id (Temporal.to_string operation.Operation.occurred);
      let sources =
        List.filter_map operation.evidence ~f:(function
          | Operation.Stored_event id -> Some (Printf.sprintf "#%Ld" id)
          | File_row _ -> None)
      in
      printf "  Sources: %s\n" (String.concat ~sep:", " sources);
      List.iter operation.movements ~f:(fun movement ->
        let role =
          Option.value_map movement.Operation.identity ~default:"unknown" ~f:(fun id ->
            id.role)
        in
        let account =
          Option.value movement.account ~default:"account not assigned in operation"
        in
        printf
          "  %s: %s %s (%s)\n"
          (Terminal.text role)
          (Quantity.to_string movement.quantity)
          (Terminal.text (Commodity.display movement.commodity))
          (Terminal.text account)));
    Ok ()
;;

let transaction_show database operation_id =
  with_database database (fun db ->
    let%bind operation = Operations.get_operation db operation_id in
    let sources =
      List.filter_map operation.Operation.evidence ~f:(function
        | Stored_event id -> Some (Int64.to_string id)
        | File_row _ -> None)
    in
    printf "Source event IDs: %s\n" (String.concat ~sep:", " sources);
    let event_id =
      List.find_map operation.Operation.evidence ~f:(function
        | Stored_event id -> Some id
        | File_row _ -> None)
    in
    match event_id with
    | None -> Or_error.error_string "operation has no stored source event"
    | Some event_id ->
      (match Explain.show_movement db event_id with
       | Ok detail ->
         print_transaction_detail detail;
         let%map () = show_operation_links db event_id in
         if List.length operation.movements > 1
         then (
           print_endline "\nOperation movements";
           List.iter operation.movements ~f:(fun movement ->
             printf
               "  %s %s (%s)\n"
               (Quantity.to_string movement.Operation.quantity)
               (Terminal.text (Commodity.display movement.commodity))
               (Terminal.text
                  (Option.value movement.account ~default:"account not assigned"))))
       | Error _ -> show_operation_only db event_id))
;;

let transaction_show_cmd =
  command
    "show"
    "Show everything stored about one operation: its source, files, fields and decisions."
    Term.(
      const transaction_show
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"OPERATION-ID" ~doc:"Operation ID from fin operation list."))
;;

let operation_resolve database operation_id role account commodity_json symbol =
  let%bind commodity = Json.decode Commodity.codec commodity_json in
  let%bind symbol =
    match symbol, commodity with
    | Some symbol, _ -> Ok symbol
    | None, Commodity.Fiat currency -> Ok (Iso4217.to_alpha_code currency)
    | None, _ -> Or_error.error_string "choose a journal symbol with --symbol"
  in
  with_database database (fun db ->
    let%map changed =
      Review.resolve_operation_movement db ~operation_id ~role ~account ~commodity ~symbol
    in
    printf "movement_resolution_changes=%d\n" (if changed then 1 else 0))
;;

let operation_resolve_cmd =
  command
    "resolve"
    "Assign an account and canonical commodity to a proposed movement."
    Term.(
      const operation_resolve
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"OPERATION-ID")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"ROLE")
      $ Arg.(required & pos 2 (some string) None & info [] ~docv:"ACCOUNT")
      $ Arg.(required & pos 3 (some string) None & info [] ~docv:"COMMODITY-JSON")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "symbol" ]
              ~docv:"SYMBOL"
              ~doc:
                "Explicit hledger symbol (required for crypto; fiat uses its ISO code)."))
;;

let operation_exchange_review database operation_id undo yes =
  with_database database (fun db ->
    let%bind preview =
      if undo
      then Ok None
      else (
        let%map source, entry = Operations.operation_exchange_preview db operation_id in
        printf
          "Proposed exchange: %s  %s\n"
          (Date.to_string entry.date)
          (Terminal.text entry.description);
        List.iter entry.postings ~f:(fun (p : Accounting.posting) ->
          printf
            "  %-30s %s %s\n"
            (Terminal.text p.account)
            (Quantity.to_string p.quantity)
            (Terminal.text p.commodity));
        Some (source, entry))
    in
    let confirmed =
      yes
      || Option.value_map
           (prompt
              (if undo then "Undo exchange? [y/N]: " else "Confirm exchange? [y/N]: "))
           ~default:false
           ~f:confirm_default_no
    in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map changed =
        Operations.set_operation_exchange
          db
          ~operation_id
          ~confirm:(not undo)
          ~expected:preview
      in
      printf "exchange_changes=%d\n" (if changed then 1 else 0)))
;;

let operation_exchange_cmd ~undo =
  command
    (if undo then "undo" else "confirm")
    (if undo
     then "Undo a reviewed exchange."
     else "Review and confirm a resolved exchange.")
    Term.(
      const operation_exchange_review
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"OPERATION-ID")
      $ const undo
      $ Arg.(
          value & flag & info [ "yes" ] ~doc:"Apply without prompting after the preview."))
;;

let operation_exchange_export database =
  with_database database (fun db ->
    let%map snapshot = Operations.exchange_snapshot db in
    Report_json.print Exchange_portable.codec snapshot)
;;

let operation_exchange_export_cmd =
  command
    "export"
    "Export confirmed exchange decisions as portable JSON."
    Term.(const operation_exchange_export $ database)
;;

let operation_exchange_restore database path apply yes =
  let%bind contents = read_file path in
  let%bind snapshot = Json.decode Exchange_portable.codec contents in
  with_database database (fun db ->
    let%bind preview = Operations.preview_exchange_restore db snapshot in
    printf
      "Exchanges to restore: %d; already matching: %d\n"
      (List.length preview.exchange_decisions)
      preview.exchange_unchanged;
    if not (List.is_empty preview.exchange_problems)
    then (
      print_endline "Problems (restore is blocked):";
      List.iter preview.exchange_problems ~f:(fun p -> printf "  %s\n" (Terminal.text p)));
    if not apply
    then (
      print_endline "Preview only; pass --apply to restore.";
      Ok ())
    else if not (List.is_empty preview.exchange_problems)
    then Or_error.error_string "resolve exchange restore problems before applying"
    else if
      not
        (yes
         || Option.value_map
              (prompt "Restore these exchanges? [y/N]: ")
              ~default:false
              ~f:confirm_default_no)
    then (
      print_endline "Nothing restored.";
      Ok ())
    else (
      let%map changed = Operations.restore_exchanges db ~expected:preview snapshot in
      printf "exchange_changes=%d\n" changed))
;;

let operation_exchange_restore_cmd =
  command
    "restore"
    "Preview or restore confirmed exchanges after restoring movement resolutions."
    Term.(
      const operation_exchange_restore
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"FILE" ~doc:"Portable exchange snapshot.")
      $ Arg.(value & flag & info [ "apply" ] ~doc:"Apply after reviewing the preview.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"With --apply, skip confirmation."))
;;

let transaction_operations database event_id json_output =
  with_database database (fun db ->
    let%bind sources = Ledger.events db in
    let%bind () =
      if List.exists sources ~f:(fun (id, _) -> Int64.equal id event_id)
      then Ok ()
      else Or_error.error_string "unknown source event ID"
    in
    let%map operations = Operations.operations_for_event db event_id in
    if json_output
    then Report_json.print (Jsont.list Report_json.operation) operations
    else if List.is_empty operations
    then print_endline "No operation is linked to this transaction."
    else
      List.iter operations ~f:(fun (id, operation) ->
        printf "Operation #%Ld  %s\n" id (Temporal.to_string operation.Operation.occurred);
        let sources =
          List.filter_map operation.evidence ~f:(function
            | Operation.Stored_event id -> Some (Printf.sprintf "#%Ld" id)
            | File_row _ -> None)
        in
        printf "  Sources: %s\n" (String.concat ~sep:", " sources);
        List.iter operation.movements ~f:(fun movement ->
          let role =
            Option.value_map movement.Operation.identity ~default:"unknown" ~f:(fun id ->
              id.role)
          in
          let account =
            Option.value movement.account ~default:"account not assigned in operation"
          in
          printf
            "  %s: %s %s (%s)\n"
            (Terminal.text role)
            (Quantity.to_string movement.quantity)
            (Terminal.text (Commodity.display movement.commodity))
            (Terminal.text account))))
;;

let list_sources database institution json_output =
  with_database database (fun db ->
    let%map sources = Ledger.events db in
    let rows =
      List.filter sources ~f:(fun (_, event) ->
        Option.value_map
          institution
          ~default:true
          ~f:(String.equal event.Source_event.venue))
    in
    if json_output
    then Report_json.print (Jsont.list Report_json.source_event) rows
    else
      List.iter rows ~f:(fun (id, event) ->
        let field name =
          Option.value
            (List.Assoc.find event.Source_event.fields name ~equal:String.equal)
            ~default:""
        in
        let kind =
          match String.strip (field "Transaction Kind") with
          | "" -> event.kind
          | value -> value
        in
        printf
          "#%Ld  %s  %s  %s  %s %s  %s\n"
          id
          (Temporal.to_string event.occurred)
          (Terminal.text event.venue)
          (Terminal.text kind)
          (Terminal.text (field "Amount"))
          (Terminal.text (field "Currency"))
          (Terminal.text (field "Transaction Description"))))
;;

let transaction_sources_cmd =
  command
    "sources"
    "List imported source records, optionally for one institution."
    Term.(const list_sources $ database $ institution_filter $ json_output)
;;

let transaction_operations_cmd =
  command
    "operations"
    "Show operations linked to a source event."
    Term.(
      const transaction_operations
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"EVENT-ID" ~doc:"Source event ID.")
      $ json_output)
;;

let print_description_gaps gaps =
  if List.is_empty gaps
  then print_endline "Every movement has a type."
  else
    List.iter gaps ~f:(fun (g : Explain.description_gap) ->
      printf
        "%4d  %s  %s  %s..%s  debits %d, credits %d\n"
        g.count
        (Terminal.text g.institution)
        (if String.is_empty g.key
         then "(blank, or starts with a digit)"
         else Terminal.text ("\"" ^ g.key ^ "\""))
        (Date.to_string g.first_on)
        (Date.to_string g.last_on)
        g.debits
        g.credits;
      printf "      example: %s\n" (Terminal.text g.example);
      if not (String.is_empty g.pattern)
      then printf "      pattern: %s\n" (Terminal.text g.pattern))
;;

let rule_gaps database institution limit json_output =
  with_database database (fun db ->
    let%map gaps = Explain.description_gaps ?institution db in
    let gaps = List.take gaps limit in
    if json_output
    then Report_json.print (Jsont.list Report_json.description_gap) gaps
    else print_description_gaps gaps)
;;

let rule_gaps_cmd =
  command
    "gaps"
    "List movements without a type, grouped by how their descriptions start."
    Term.(
      const rule_gaps
      $ database
      $ institution_filter
      $ Arg.(
          value
          & opt int 20
          & info [ "limit" ] ~docv:"COUNT" ~doc:"Maximum number of groups.")
      $ json_output)
;;

let transaction_status database json_output =
  with_database database (fun db ->
    let%bind report = Review.transaction_status db in
    if json_output
    then Report_json.print Report_json.transaction_status report
    else (
      let percent n =
        match Report_json.percentage n report.total with
        | None -> "n/a"
        | Some value -> Printf.sprintf "%.1f%%" value
      in
      printf "Transactions: %d\n" report.total;
      printf
        "Classified: %d (%s)\n"
        (report.total - report.unclassified)
        (percent (report.total - report.unclassified));
      printf "  Categorized: %d (%s)\n" report.categorized (percent report.categorized);
      printf
        "  Transfers: %d\n  Settlements: %d\n  Reversals: %d\n"
        report.transfers
        report.settlements
        report.reversals;
      printf "Unclassified: %d\n" report.unclassified;
      let show_groups title groups =
        printf "\n%s:\n" title;
        if List.is_empty groups
        then print_endline "  (none)"
        else
          List.iter groups ~f:(fun (name, count) ->
            printf "  %s: %d\n" (Terminal.text name) count)
      in
      show_groups "By category" report.by_category;
      show_groups "By movement type" report.by_type);
    Ok ())
;;

let transaction_status_cmd =
  command
    "status"
    "Summarize accounting decisions and classification progress."
    Term.(const transaction_status $ database $ json_output)
;;

let bank_review database all json_output =
  with_database database (fun db ->
    let%bind rows = Review.bank_review db in
    let rows =
      List.filter rows ~f:(fun r ->
        all || String.equal r.Review.counter_account "equity:unclassified")
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

let category_similar database seed json_output =
  with_database database (fun db ->
    let%bind rows = Review.bank_review db in
    let%bind preview = Category_similarity.find ~seed rows in
    if json_output
    then Report_json.print Report_json.category_similar preview
    else (
      printf
        "Example and proposed category: %s\n"
        (Terminal.text preview.seed.counter_account);
      Terminal.review [ preview.seed ];
      print_endline
        "Matching unclassified transactions (across institutions, matching normalized \
         effective descriptions):";
      Terminal.review preview.candidates;
      if not (List.is_empty preview.conflicts)
      then (
        print_endline "Matching transactions classified differently:";
        Terminal.review preview.conflicts));
    Ok ())
;;

let category_similar_cmd =
  command
    "similar"
    "Preview unclassified transactions similar to a reviewed example."
    Term.(
      const category_similar
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info
              []
              ~docv:"EVENT-ID"
              ~doc:"Reviewed source event ID from fin operation show.")
      $ json_output)
;;

type portable_scope =
  | Catalog
  | Category_decisions
  | Rules

let portable_codec = function
  | Catalog -> Category_portable.categories_codec
  | Category_decisions -> Category_portable.decisions_codec
  | Rules -> Category_portable.rules_codec
;;

let select_portable scope (snapshot : Category_portable.t) =
  match scope with
  | Catalog -> { snapshot with rules = []; decisions = []; resolutions = [] }
  | Category_decisions -> { snapshot with categories = []; rules = [] }
  | Rules -> { snapshot with categories = []; decisions = []; resolutions = [] }
;;

let portable_export scope database =
  with_database database (fun db ->
    let%bind snapshot =
      match scope with
      | Catalog ->
        let%map categories = Setup.categories db in
        Category_portable.{ categories; rules = []; decisions = []; resolutions = [] }
      | Rules ->
        let%map rules = Import.classification_rules db in
        Category_portable.{ categories = []; rules; decisions = []; resolutions = [] }
      | Category_decisions ->
        let%map snapshot = Restore.category_snapshot db in
        select_portable scope snapshot
    in
    Report_json.print (portable_codec scope) snapshot;
    Ok ())
;;

let portable_export_cmd scope doc =
  command "export" doc Term.(const (portable_export scope) $ database)
;;

let category_export_cmd =
  portable_export_cmd Catalog "Export the category catalog as portable JSON."
;;

let category_decisions_export_cmd =
  portable_export_cmd
    Category_decisions
    "Export transaction categories and manual type overrides as portable JSON."
;;

let rules_export_cmd = portable_export_cmd Rules "Export stored rules as portable JSON."

let portable_restore scope database path apply yes institution =
  let%bind contents = read_file path in
  let%bind snapshot =
    match Json.decode (portable_codec scope) contents with
    | Ok snapshot -> Ok snapshot
    | Error scoped_error ->
      (match scope, Json.decode Category_portable.version4_decisions_codec contents with
       | Category_decisions, Ok legacy -> Ok legacy
       | _ ->
         (match
            scope, Json.decode Category_portable.previous_decisions_codec contents
          with
          | Category_decisions, Ok legacy -> Ok legacy
          | _ ->
            (match
               scope, Json.decode Category_portable.legacy_decisions_codec contents
             with
             | Category_decisions, Ok legacy -> Ok legacy
             | _ ->
               (match Json.decode Category_portable.codec contents with
                | Ok legacy -> Ok (select_portable scope legacy)
                | Error _ -> Error scoped_error))))
  in
  let%bind snapshot, skipped =
    match institution with
    | None -> Ok (snapshot, 0)
    | Some name ->
      let decisions =
        List.filter snapshot.decisions ~f:(fun (d : Category_portable.decision) ->
          String.equal d.institution name)
      in
      let resolutions =
        List.filter snapshot.resolutions ~f:(fun (r : Category_portable.resolution) ->
          String.equal r.source.institution name)
      in
      if List.is_empty decisions && List.is_empty resolutions
      then Or_error.errorf "snapshot has no decisions for institution %s" name
      else
        Ok
          ( { snapshot with decisions; resolutions }
          , List.length snapshot.decisions
            - List.length decisions
            + List.length snapshot.resolutions
            - List.length resolutions )
  in
  with_database database (fun db ->
    let%bind preview = Restore.preview_category_restore db snapshot in
    Option.iter institution ~f:(fun name ->
      printf "Institution: %s; decisions skipped: %d\n" (Terminal.text name) skipped);
    printf "Categories to add: %d\n" (List.length preview.categories_to_add);
    List.iter preview.categories_to_add ~f:(fun c -> printf "  %s\n" (Terminal.text c));
    (match scope with
     | Rules ->
       printf "Rules to add: %d\n" (List.length preview.rules_to_add);
       List.iter preview.rules_to_add ~f:(fun r ->
         printf "  %s\n" (Terminal.text r.Classification_rule.name))
     | Category_decisions ->
       printf
         "Decisions to restore: %d; already matching: %d\n"
         (List.length preview.decisions_to_apply)
         preview.unchanged;
       List.iter preview.decisions_to_apply ~f:(fun (d : Bank_category.t) ->
         printf "  #%Ld -> %s\n" d.event_id (Terminal.text d.account));
       printf "Type overrides to restore: %d\n" (List.length preview.types_to_apply);
       List.iter preview.types_to_apply ~f:(fun (id, kind) ->
         printf "  #%Ld -> %s\n" id (Terminal.text (Movement_type.to_string kind)));
       printf
         "Movement resolutions to restore: %d\n"
         (List.length preview.resolutions_to_apply)
     | Catalog -> ());
    if not (List.is_empty preview.problems)
    then (
      print_endline "Problems (restore is blocked):";
      List.iter preview.problems ~f:(fun p -> printf "  %s\n" (Terminal.text p)));
    if not apply
    then (
      print_endline "Preview only; pass --apply to restore.";
      Ok ())
    else if not (List.is_empty preview.problems)
    then Or_error.error_string "resolve restore problems before applying"
    else (
      let confirmed =
        yes
        || Option.value_map
             (prompt "Restore this snapshot? [y/N]: ")
             ~default:false
             ~f:confirm_default_no
      in
      if not confirmed
      then (
        print_endline "Nothing restored.";
        Ok ())
      else (
        let%bind changed = Restore.restore_categories db ~expected:preview snapshot in
        printf
          "categories_added=%d rules_added=%d category_changes=%d type_changes=%d\n"
          (List.length preview.categories_to_add)
          (List.length preview.rules_to_add)
          changed
          (List.length preview.types_to_apply);
        Ok ())))
;;

let portable_restore_cmd scope doc =
  let institution =
    match scope with
    | Category_decisions ->
      Arg.(
        value
        & opt (some string) None
        & info
            [ "institution" ]
            ~docv:"NAME"
            ~doc:
              "Restore only decisions for this institution; other decisions are skipped.")
    | Catalog | Rules -> Term.const None
  in
  command
    "restore"
    doc
    Term.(
      const (portable_restore scope)
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"FILE" ~doc:"Portable JSON export.")
      $ Arg.(
          value
          & flag
          & info
              [ "apply" ]
              ~doc:"Apply the preview after confirmation; otherwise only preview.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"With --apply, skip confirmation.")
      $ institution)
;;

let category_restore_cmd =
  portable_restore_cmd Catalog "Preview or restore a category catalog export."
;;

let category_decisions_restore_cmd =
  portable_restore_cmd
    Category_decisions
    "Preview or restore transaction categories and manual type overrides."
;;

let rules_restore_cmd = portable_restore_cmd Rules "Preview or restore stored rules."

let category_list database json_output =
  with_database database (fun db ->
    let%bind categories = Setup.categories db in
    if json_output
    then Report_json.print (Jsont.list Jsont.string) categories
    else List.iter categories ~f:(fun category -> printf "%s\n" (Terminal.text category));
    Ok ())
;;

let category_list_cmd =
  command
    "list"
    "List the personal income and expense category catalog."
    Term.(const category_list $ database $ json_output)
;;

let category_add database name =
  with_database database (fun db ->
    let%bind () = Setup.add_categories db [ name ] in
    printf "Category available: %s\n" (Terminal.text name);
    Ok ())
;;

let category_add_cmd =
  command
    "add"
    "Add an income or expense category to the catalog."
    Term.(
      const category_add
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"ACCOUNT"))
;;

let category_delete database name =
  with_database database (fun db ->
    let%bind () = Setup.delete_category db name in
    printf "Category deleted: %s\n" (Terminal.text name);
    Ok ())
;;

let category_delete_cmd =
  command
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Remove one unused catalog entry. Change transaction assignments and rules \
           that use the category first. Historical decisions and subcategories are \
           retained."
      ]
    "delete"
    "Delete an unused category from the catalog."
    Term.(
      const category_delete
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"ACCOUNT"))
;;

let starter_categories =
  [ "expenses:home"
  ; "expenses:utilities"
  ; "expenses:food:groceries"
  ; "expenses:food:restaurants"
  ; "expenses:transport"
  ; "expenses:health"
  ; "expenses:shopping"
  ; "expenses:entertainment"
  ; "expenses:travel"
  ; "expenses:taxes"
  ; "expenses:other"
  ; "income:salary"
  ; "income:interest"
  ; "income:other"
  ]
;;

let category_init database =
  if not (Unix.isatty Unix.stdin)
  then Or_error.error_string "category initialization requires a terminal"
  else
    with_database database (fun db ->
      let%bind existing = Setup.categories db in
      let available =
        List.filter starter_categories ~f:(fun name ->
          not (List.mem existing name ~equal:String.equal))
      in
      if List.is_empty available
      then (
        print_endline "All starter categories already exist.";
        Ok ())
      else (
        print_endline "Suggested starter categories:";
        List.iteri available ~f:(fun i name -> printf "  %d. %s\n" (i + 1) name);
        let rec select () =
          match
            prompt "Select all, none, or numbers separated by spaces (q to cancel): "
          with
          | None -> Ok None
          | Some input ->
            let input = String.strip input in
            if String.Caseless.equal input "q" || String.Caseless.equal input "none"
            then Ok None
            else if String.Caseless.equal input "all"
            then Ok (Some available)
            else (
              let nums =
                String.split_on_chars input ~on:[ ' '; ','; '\t' ]
                |> List.filter ~f:(Fn.non String.is_empty)
              in
              let indexes =
                List.map nums ~f:(fun n -> Option.try_with (fun () -> Int.of_string n))
              in
              if
                List.is_empty indexes
                || List.exists indexes ~f:(function
                  | None -> true
                  | Some n -> n < 1 || n > List.length available)
              then (
                eprintf "Enter all, none, or valid numbers.\n";
                select ())
              else
                Ok
                  (Some
                     (List.filteri available ~f:(fun i _ ->
                        List.mem indexes (Some (i + 1)) ~equal:(Option.equal Int.equal)))))
        in
        let%bind selected = select () in
        match selected with
        | None ->
          print_endline "No categories added.";
          Ok ()
        | Some selected ->
          print_endline "Categories to add:";
          List.iter selected ~f:(fun name -> printf "  %s\n" name);
          (match prompt "Add these categories? [Y/n]: " with
           | Some answer when confirm_default_yes answer ->
             let%bind () = Setup.add_categories db selected in
             printf "categories_added=%d\n" (List.length selected);
             Ok ()
           | _ ->
             print_endline "No categories added.";
             Ok ())))
;;

let category_init_cmd =
  command
    "init"
    "Choose suggested starter categories for your personal catalog."
    Term.(const category_init $ database)
;;

let category_review database operation_id =
  if not (Unix.isatty Unix.stdin)
  then Or_error.error_string "interactive category review requires a terminal"
  else
    with_database database (fun db ->
      let%bind event_id =
        bank_event_ids_for_operations db [ operation_id ] |> Result.map ~f:List.hd_exn
      in
      let%bind rows = Review.bank_review db in
      let%bind seed =
        List.find rows ~f:(fun (r : Review.bank_review) ->
          Int64.equal r.event_id event_id)
        |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
      in
      if
        not
          (Poly.equal seed.classification Review.Unclassified
           || Poly.equal seed.classification Review.Category)
      then
        Or_error.error_string
          "transaction belongs to a confirmed transfer, settlement or reversal"
      else (
        print_endline "Transaction to classify:";
        Terminal.review [ seed ];
        let%bind categories = Setup.categories db in
        if not (List.is_empty categories)
        then (
          print_endline "Available categories:";
          List.iteri categories ~f:(fun i category ->
            printf "  %d. %s\n" (i + 1) (Terminal.text category)));
        let rec pick_category () =
          match prompt "Category name or number (q to cancel): " with
          | None -> Ok None
          | Some input ->
            let input = String.strip input in
            if String.Caseless.equal input "q"
            then Ok None
            else (
              let name =
                match Option.try_with (fun () -> Int.of_string input) with
                | Some n -> Option.value (List.nth categories (n - 1)) ~default:""
                | None -> input
              in
              match
                Bank_category.create
                  ~event_id
                  ~account:name
                  ~reason:"Reviewed interactively"
              with
              | Ok _ when not (String.equal name "equity:unclassified") -> Ok (Some name)
              | Ok _ ->
                eprintf "Choose an income or expense category.\n";
                pick_category ()
              | Error e ->
                eprintf "%s\n" (Error.to_string_hum e);
                pick_category ())
        in
        let%bind category = pick_category () in
        match category with
        | None ->
          print_endline "Nothing classified.";
          Ok ()
        | Some category ->
          let example =
            { seed with
              counter_account = category
            ; reason = Some "Reviewed interactively"
            }
          in
          let%bind matches =
            if String.is_empty (String.strip seed.description)
            then
              Ok Category_similarity.{ seed = example; candidates = []; conflicts = [] }
            else
              Category_similarity.find
                ~seed:event_id
                (example
                 :: List.filter rows ~f:(fun r ->
                   not (Int64.equal r.Review.event_id event_id)))
          in
          let needs_category (r : Review.bank_review) =
            not
              (Poly.equal r.classification Review.Category
               && String.equal r.counter_account category)
          in
          let fuzzy =
            Category_similarity.fuzzy ~seed rows
            |> List.filter ~f:(fun ((r : Review.bank_review), _) ->
              Poly.equal r.classification Review.Unclassified)
          in
          let initial = matches.candidates @ List.map fuzzy ~f:fst in
          let eligible (r : Review.bank_review) =
            Poly.equal r.classification Review.Unclassified
            || Poly.equal r.classification Review.Category
          in
          let show_rows
                (selected : Review.bank_review list)
                (candidates : Review.bank_review list)
                show_fuzzy
            =
            print_endline "Matches (selected rows marked x):";
            if List.is_empty candidates then print_endline "  No matches.";
            List.iteri candidates ~f:(fun i (r : Review.bank_review) ->
              let chosen =
                List.exists selected ~f:(fun x ->
                  Int64.equal x.Review.event_id r.event_id)
              in
              let label =
                if show_fuzzy
                then
                  Option.value_map
                    (List.find fuzzy ~f:(fun (row, _) ->
                       Int64.equal row.Review.event_id r.event_id))
                    ~default:"exact"
                    ~f:(fun (_, score) -> Printf.sprintf "fuzzy %.2f" score)
                else "search"
              in
              printf
                "  %d. [%s] %s  #%Ld  %s  %s  %s %s  %s  (%s)\n"
                (i + 1)
                (if chosen then "x" else " ")
                label
                r.event_id
                (Terminal.text r.institution)
                (Date.to_string r.transaction.booked_on)
                (Quantity.to_string r.transaction.amount)
                (Iso4217.to_alpha_code r.transaction.account.currency)
                (Terminal.text r.description)
                (Terminal.text r.counter_account))
          in
          let rec select
                    (candidates : Review.bank_review list)
                    (selected : Review.bank_review list)
                    show_fuzzy
            =
            show_rows selected candidates show_fuzzy;
            printf
              "Selected: %d. all selects all selectable displayed matches and proceeds.\n"
              (List.length selected);
            print_endline
              "Enter numbers or ranges to select or deselect, e.g. 1-5,7,8-19.";
            print_endline "Commands: all, none, search, fuzzy, done, q";
            match prompt "Select matches: " with
            | None -> Ok None
            | Some input ->
              let input = String.strip input in
              if String.Caseless.equal input "q"
              then Ok None
              else if String.Caseless.equal input "done"
              then Ok (Some selected)
              else if String.Caseless.equal input "none"
              then Ok (Some [])
              else if String.Caseless.equal input "all"
              then
                Ok
                  (Some
                     (List.dedup_and_sort
                        ~compare:(fun (a : Review.bank_review) (b : Review.bank_review) ->
                          Int64.compare a.event_id b.event_id)
                        (selected @ List.filter candidates ~f:eligible)))
              else if
                String.Caseless.equal input "fuzzy" || String.Caseless.equal input "f"
              then select initial selected true
              else if
                String.Caseless.equal input "search" || String.Caseless.equal input "s"
              then (
                match prompt "Search description (empty to return): " with
                | None -> Ok None
                | Some query ->
                  let query = String.strip query in
                  if String.is_empty query
                  then select candidates selected show_fuzzy
                  else (
                    let results =
                      List.filter rows ~f:(fun (r : Review.bank_review) ->
                        (not (Int64.equal r.event_id event_id))
                        && needs_category r
                        && String.Caseless.is_substring r.description ~substring:query)
                    in
                    select results selected false))
              else (
                let number text =
                  Option.try_with (fun () -> Int.of_string (String.strip text))
                in
                let parse piece =
                  let range = String.split (String.strip piece) ~on:'-' in
                  let bounds =
                    match range with
                    | [ one ] -> Option.map (number one) ~f:(fun n -> n, n)
                    | [ first; last ] -> Option.both (number first) (number last)
                    | _ -> None
                  in
                  Option.bind bounds ~f:(fun (first, last) ->
                    if first < 1 || last < first || last > List.length candidates
                    then None
                    else
                      Some
                        (List.init
                           (last - first + 1)
                           ~f:(fun i -> List.nth_exn candidates (first + i - 1))))
                in
                let parsed = List.map (String.split input ~on:',') ~f:parse in
                if List.exists parsed ~f:Option.is_none
                then (
                  eprintf "Enter valid comma-separated numbers or ascending ranges.\n";
                  select candidates selected show_fuzzy)
                else (
                  let choices = List.concat (List.filter_opt parsed) in
                  if List.exists choices ~f:(Fn.non eligible)
                  then (
                    eprintf
                      "Only unclassified or categorized transactions can be selected.\n";
                    select candidates selected show_fuzzy)
                  else if
                    List.contains_dup
                      (List.map choices ~f:(fun r -> r.Review.event_id))
                      ~compare:Int64.compare
                  then (
                    eprintf "Select each match only once.\n";
                    select candidates selected show_fuzzy)
                  else (
                    let selected =
                      List.fold choices ~init:selected ~f:(fun selected r ->
                        if
                          List.exists selected ~f:(fun x ->
                            Int64.equal x.Review.event_id r.Review.event_id)
                        then
                          List.filter selected ~f:(fun x ->
                            not (Int64.equal x.Review.event_id r.Review.event_id))
                        else selected @ [ r ])
                    in
                    select candidates selected show_fuzzy)))
          in
          let%bind selected = select initial [] true in
          (match selected with
           | None ->
             print_endline "Nothing classified.";
             Ok ()
           | Some selected ->
             let chosen = if needs_category seed then seed :: selected else selected in
             if List.is_empty chosen
             then (
               print_endline "No category changes.";
               Ok ())
             else (
               printf "Proposed category: %s\n" (Terminal.text category);
               Terminal.review
                 (List.map chosen ~f:(fun r ->
                    { r with Review.counter_account = category; reason = None }));
               printf "Apply to %d transaction(s)? [Y/n]: " (List.length chosen);
               Out_channel.flush stdout;
               let confirmed =
                 Option.value_map
                   (In_channel.input_line In_channel.stdin)
                   ~default:false
                   ~f:confirm_default_yes
               in
               if not confirmed
               then (
                 print_endline "Nothing classified.";
                 Ok ())
               else (
                 let%bind decisions =
                   List.map chosen ~f:(fun (r : Review.bank_review) ->
                     Bank_category.create
                       ~event_id:r.event_id
                       ~account:category
                       ~reason:
                         (if Int64.equal r.event_id event_id
                          then "Reviewed interactively"
                          else
                            Printf.sprintf
                              "Selected during category review of event %Ld"
                              event_id))
                   |> Or_error.combine_errors
                 in
                 let expected =
                   List.map chosen ~f:(fun (r : Review.bank_review) ->
                     ( r.event_id
                     , Option.map r.reason ~f:(fun reason -> r.counter_account, reason) ))
                 in
                 let%bind changed =
                   Review.review_bank_categories db ~expected decisions
                 in
                 printf "category_changes=%d\n" changed;
                 Ok ())))))
;;

let category_queue database shortcuts types =
  if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout)
  then Or_error.error_string "the category queue requires a terminal"
  else (
    let%bind types =
      List.map types ~f:parse_movement_type_filter |> Or_error.combine_errors
    in
    with_database database (fun db ->
      let%bind rows = Review.bank_review db in
      let rows =
        List.filter rows ~f:(fun (row : Review.bank_review) ->
          Poly.equal row.classification Review.Unclassified
          && (List.is_empty types
              || List.exists types ~f:(fun filter ->
                matches_movement_type_filter filter row.movement_type)))
      in
      let%bind categories = Setup.categories db in
      Category_tui.run db rows categories shortcuts))
;;

let category_queue_cmd =
  command
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Review unclassified transactions one at a time. Assign with a displayed \
           letter, skip with s, search with /, browse with b, add a category with a, \
           undo with u, and quit with q. Assignments include other unclassified exact \
           descriptions in the queue; x toggles this. Undo reverses the whole batch. q \
           saves all staged changes atomically; Ctrl-C discards them."
      ; `P
          "Shortcuts are stable for the session; use --key LETTER=ACCOUNT to choose a \
           letter for an existing category. The letters a, b, q, s, u and x are \
           reserved."
      ]
    "queue"
    "Classify unreviewed transactions in a full-screen keyboard queue."
    Term.(
      const category_queue
      $ database
      $ Arg.(
          value
          & opt_all string []
          & info
              [ "key" ]
              ~docv:"LETTER=ACCOUNT"
              ~doc:"Assign a category shortcut; repeat for additional shortcuts.")
      $ Arg.(
          value
          & opt_all string []
          & info
              [ "type" ]
              ~docv:"TYPE"
              ~doc:
                "Only movements of this effective type, or unknown for no type; repeat \
                 to include multiple types."))
;;

let category_review_cmd =
  command
    "review"
    "Review or change a category and selected similar movements."
    Term.(
      const category_review
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info
              []
              ~docv:"OPERATION-ID"
              ~doc:
                "Transaction ID from fin operation list; existing categories may be \
                 changed."))
;;

let category_history database =
  with_database database (fun db ->
    let%bind rows = Review.bank_category_history db in
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
    let%bind rows = Transfers.transfer_candidates db in
    let%bind ranked = Transfer_ranking.analyze (Accounts.registry db) rows in
    let%bind extra =
      if all then Transfers.transfer_candidates ~include_reviewed:true db else Ok []
    in
    if json_output
    then Report_json.print Report_json.preview (ranked, extra)
    else (
      print_endline
        "Transfer candidates: evidence scores are not probabilities; nothing is \
         confirmed automatically.\n";
      if List.is_empty ranked
      then print_endline "No compatible unreviewed transfer candidates.";
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
      if all
      then
        List.iter extra ~f:(fun r ->
          if
            not
              (List.exists ranked ~f:(fun ranked ->
                 Int64.equal
                   ranked.Transfer_ranking.candidate.first_event_id
                   r.first_event_id
                 && Int64.equal ranked.candidate.second_event_id r.second_event_id))
          then (
            Terminal.transfer r;
            printf
              "  State: %s%s\n"
              (Option.value_map
                 r.state
                 ~default:"unreviewed; unavailable"
                 ~f:Bank_transfer.state_name)
              (if r.iban_compatible then "" else " (excluded: conflicting IBAN evidence)");
            List.iter r.iban_evidence ~f:(fun evidence ->
              printf "  %s\n" (Terminal.text evidence));
            Option.iter r.reason ~f:(fun reason ->
              printf "  Reason: %s\n" (Terminal.text reason));
            print_endline "")));
    Ok ())
;;

let transfer_candidates_cmd =
  command
    "candidates"
    "Show possible transfers between unclassified movements, highest score first."
    Term.(
      const transfer_candidates
      $ database
      $ Arg.(
          value
          & flag
          & info
              [ "all" ]
              ~doc:
                "Include reviewed pairs and movements already assigned to transfers; \
                 unreviewed categorized movements remain excluded.")
      $ json_output)
;;

let review_transfers database path =
  with_database database (fun db ->
    let%bind decisions =
      match path with
      | Some path ->
        let%bind contents = read_file path in
        Bank_transfer.of_json contents
      | None ->
        if not (Unix.isatty Unix.stdin)
        then
          Or_error.error_string
            "interactive review requires a terminal or a decisions file"
        else (
          let%bind candidates = Transfers.transfer_candidates db in
          let%bind ranked = Transfer_ranking.analyze (Accounts.registry db) candidates in
          print_endline
            "Review one candidate pair at a time. Choices are staged until you confirm \
             the whole batch.";
          print_endline
            "Confirm only pairs representing transfers between your own accounts.";
          print_endline
            "For the current pair: c = confirm, r = reject, s = skip.\n\
             For the batch: a = select supported pairs and finish, d = finish, q = cancel.\n\
             Other candidates remain for later review. Nothing is saved until the final \
             confirmation.";
          let make state (item : Transfer_ranking.ranked) =
            let c = item.candidate in
            Bank_transfer.create
              ~first_event_id:c.first_event_id
              ~second_event_id:c.second_event_id
              ~state
              ~reason:
                (if Poly.equal state Bank_transfer.Confirmed
                 then "Confirmed after interactive review"
                 else "Rejected after interactive review")
          in
          let overlaps (a : Bank_transfer.decision) (b : Bank_transfer.decision) =
            Poly.equal a.state Bank_transfer.Confirmed
            && Poly.equal b.state Bank_transfer.Confirmed
            && (Int64.equal a.first_event_id b.first_event_id
                || Int64.equal a.first_event_id b.second_event_id
                || Int64.equal a.second_event_id b.first_event_id
                || Int64.equal a.second_event_id b.second_event_id)
          in
          let stage pending decision =
            if List.exists pending ~f:(overlaps decision)
            then (
              print_endline "This conflicts with a pending confirmation; skipped.";
              pending)
            else decision :: pending
          in
          let rec loop pending = function
            | [] -> Ok (Some (List.rev pending))
            | (item : Transfer_ranking.ranked) :: rest ->
              let c = item.candidate in
              if
                List.exists pending ~f:(fun (d : Bank_transfer.decision) ->
                  Int64.equal
                    d.first_event_id
                    (Int64.min c.first_event_id c.second_event_id)
                  && Int64.equal
                       d.second_event_id
                       (Int64.max c.first_event_id c.second_event_id))
              then loop pending rest
              else (
                printf
                  "\nCurrent pair: #%Ld and #%Ld\n"
                  c.first_event_id
                  c.second_event_id;
                Terminal.transfer c;
                printf
                  "  Score: %d; confidence: %s; assignment: %s\n"
                  item.score
                  item.confidence
                  item.assignment.status;
                List.iter item.evidence ~f:(fun e ->
                  printf "  %s (%+d)\n" (Terminal.text e.Transfer_ranking.detail) e.points);
                match
                  prompt "Current pair or batch choice [c/r/s/a/d/q]: "
                  |> Option.map ~f:(fun s -> String.lowercase (String.strip s))
                with
                | None | Some "q" -> Ok None
                | Some "d" -> Ok (Some (List.rev pending))
                | Some "s" -> loop pending rest
                | (Some "c" | Some "r") as choice ->
                  let state =
                    if Poly.equal choice (Some "c")
                    then Bank_transfer.Confirmed
                    else Rejected
                  in
                  let%bind decision = make state item in
                  loop (stage pending decision) rest
                | Some "a" ->
                  let%bind supported =
                    List.filter ranked ~f:(fun r ->
                      String.equal r.Transfer_ranking.assignment.status "supported")
                    |> List.filter ~f:(fun r ->
                      let c = r.Transfer_ranking.candidate in
                      not
                        (List.exists pending ~f:(fun (d : Bank_transfer.decision) ->
                           Int64.equal
                             d.first_event_id
                             (Int64.min c.first_event_id c.second_event_id)
                           && Int64.equal
                                d.second_event_id
                                (Int64.max c.first_event_id c.second_event_id))))
                    |> List.map ~f:(make Bank_transfer.Confirmed)
                    |> Or_error.combine_errors
                  in
                  let updated = List.fold supported ~init:pending ~f:stage in
                  printf
                    "Staged %d supported pairs.\n"
                    (List.length updated - List.length pending);
                  print_endline "Remaining candidates are left for later review.";
                  Ok (Some (List.rev updated))
                | Some _ ->
                  print_endline "Choose c, r, s, a, d or q.";
                  loop pending (item :: rest))
          in
          let%bind pending = loop [] ranked in
          match pending with
          | None -> Ok []
          | Some [] -> Ok []
          | Some decisions ->
            printf "Pending decisions: %d\n" (List.length decisions);
            List.iter decisions ~f:(fun (d : Bank_transfer.decision) ->
              Option.iter
                (List.find ranked ~f:(fun r ->
                   let c = r.Transfer_ranking.candidate in
                   Int64.equal
                     d.first_event_id
                     (Int64.min c.first_event_id c.second_event_id)
                   && Int64.equal
                        d.second_event_id
                        (Int64.max c.first_event_id c.second_event_id)))
                ~f:(fun r -> Terminal.transfer r.candidate);
              printf
                "  #%Ld <-> #%Ld: %s\n"
                d.first_event_id
                d.second_event_id
                (Bank_transfer.state_name d.state));
            if
              Option.value_map
                (prompt "Apply these decisions? [Y/n]: ")
                ~default:false
                ~f:confirm_default_yes
            then Ok decisions
            else Ok [])
    in
    if List.is_empty decisions
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map changed =
        Transfers.set_bank_transfers
          ~require_unreviewed:(Option.is_none path)
          db
          decisions
      in
      printf "transfer_changes=%d\n" changed))
;;

let review_transfers_cmd =
  command
    "review"
    "Review candidates by descending score, or apply JSON confirm/reject decisions."
    Term.(
      const review_transfers
      $ database
      $ Arg.(
          value
          & pos 0 (some string) None
          & info
              []
              ~docv:"DECISIONS"
              ~doc:"Optional JSON transfer review decisions; omit for interactive review."))
;;

let transfer_export database =
  with_database database (fun db ->
    let%map snapshot = Operations.transfer_snapshot db in
    Report_json.print Transfer_portable.codec snapshot)
;;

let transfer_link database first_operation second_operation first_role second_role yes =
  with_database database (fun db ->
    let%bind preview =
      Operations.preview_general_transfer
        db
        ~first_operation
        ~first_role
        ~second_operation
        ~second_role
    in
    printf "Proposed transfer:\n";
    List.iter
      [ preview.first; preview.second ]
      ~f:(fun (leg : Operations.general_transfer_leg) ->
        printf
          "  operation #%Ld (%s): %s %s  %s\n"
          leg.operation_id
          (Terminal.text leg.role)
          (Quantity.to_string leg.quantity)
          (Terminal.text leg.symbol)
          (Terminal.text leg.account));
    if
      not
        (yes
         || Option.value_map
              (prompt "Confirm transfer? [y/N]: ")
              ~default:false
              ~f:confirm_default_no)
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () = Operations.confirm_general_transfer db ~expected:preview in
      print_endline "transfer_changes=1"))
;;

let transfer_link_cmd =
  command
    "link"
    "Review and link two resolved single-movement operations."
    Term.(
      const transfer_link
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"FIRST-OPERATION-ID")
      $ Arg.(required & pos 1 (some int64) None & info [] ~docv:"SECOND-OPERATION-ID")
      $ Arg.(
          value
          & opt string "primary"
          & info
              [ "first-role" ]
              ~docv:"ROLE"
              ~doc:"First movement role (default: primary).")
      $ Arg.(
          value
          & opt string "primary"
          & info
              [ "second-role" ]
              ~docv:"ROLE"
              ~doc:"Second movement role (default: primary).")
      $ Arg.(
          value
          & flag
          & info [ "yes" ] ~doc:"Confirm without prompting after the preview."))
;;

let transfer_unlink database operation_id yes =
  with_database database (fun db ->
    let%bind links = Review.operation_transfer_links db in
    let%bind link =
      List.find links ~f:(fun l ->
        Int64.equal l.first_operation_id operation_id
        || Int64.equal l.second_operation_id operation_id)
      |> Or_error.of_option
           ~error:(Error.of_string "operation is not a confirmed transfer")
    in
    printf
      "Transfer: operation #%Ld <-> #%Ld\n"
      link.first_operation_id
      link.second_operation_id;
    if
      not
        (yes
         || Option.value_map
              (prompt "Undo transfer? [y/N]: ")
              ~default:false
              ~f:confirm_default_no)
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () = Operations.undo_general_transfer db operation_id in
      print_endline "transfer_changes=1"))
;;

let transfer_unlink_cmd =
  command
    "unlink"
    "Undo a reviewed non-bank transfer link."
    Term.(
      const transfer_unlink
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"OPERATION-ID")
      $ Arg.(
          value & flag & info [ "yes" ] ~doc:"Undo without prompting after the preview."))
;;

let transfer_export_cmd =
  command
    "export"
    "Export confirmed transfer links with source evidence."
    Term.(const transfer_export $ database)
;;

let transfer_restore database path apply yes =
  let%bind contents = read_file path in
  let%bind snapshot =
    match Json.decode Transfer_portable.codec contents with
    | Ok snapshot -> Ok snapshot
    | Error error ->
      (match Json.decode Transfer_portable.legacy_codec contents with
       | Ok snapshot -> Ok snapshot
       | Error _ -> Error error)
  in
  with_database database (fun db ->
    let%bind preview = Operations.preview_transfer_restore db snapshot in
    printf
      "Transfers to restore: %d; already matching: %d\n"
      (List.length preview.decisions + List.length preview.general_decisions)
      preview.unchanged;
    List.iter preview.decisions ~f:(fun d ->
      printf "  #%Ld <-> #%Ld\n" d.Bank_transfer.first_event_id d.second_event_id);
    if not (List.is_empty preview.problems)
    then (
      print_endline "Problems (restore is blocked):";
      List.iter preview.problems ~f:(fun p -> printf "  %s\n" (Terminal.text p)));
    if not apply
    then (
      print_endline "Preview only; pass --apply to restore.";
      Ok ())
    else if not (List.is_empty preview.problems)
    then Or_error.error_string "resolve transfer restore problems before applying"
    else if
      not
        (yes
         || Option.value_map
              (prompt "Restore these transfers? [y/N]: ")
              ~default:false
              ~f:confirm_default_no)
    then (
      print_endline "Nothing restored.";
      Ok ())
    else (
      let%map changed = Operations.restore_transfers db ~expected:preview snapshot in
      printf "transfer_changes=%d\n" changed))
;;

let transfer_restore_cmd =
  command
    "restore"
    "Preview or restore confirmed transfer links."
    Term.(
      const transfer_restore
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"FILE" ~doc:"Portable transfer snapshot.")
      $ Arg.(value & flag & info [ "apply" ] ~doc:"Apply after reviewing the preview.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"With --apply, skip confirmation."))
;;

let transfer_undo database first second yes =
  with_database database (fun db ->
    let%bind decisions = Restore.bank_transfer_decisions db in
    let first, second = Int64.min first second, Int64.max first second in
    let%bind previous =
      List.find decisions ~f:(fun (d : Bank_transfer.decision) ->
        Int64.equal d.first_event_id first
        && Int64.equal d.second_event_id second
        && Poly.equal d.state Bank_transfer.Confirmed)
      |> Or_error.of_option
           ~error:(Error.of_string "no confirmed transfer for these event IDs")
    in
    let%bind rows = Review.bank_review db in
    let pair =
      List.filter rows ~f:(fun r ->
        Int64.equal r.Review.event_id first || Int64.equal r.event_id second)
    in
    if List.length pair <> 2
    then Or_error.error_string "transfer movements are missing"
    else (
      print_endline
        "Undo this confirmed transfer? Both movements will return to unclassified.";
      Terminal.review pair;
      let%bind confirmed =
        if yes
        then Ok true
        else if Unix.isatty Unix.stdin
        then
          Ok
            (Option.value_map
               (prompt "Undo this transfer? [Y/n]: ")
               ~default:false
               ~f:confirm_default_yes)
        else Or_error.error_string "undo requires a terminal or --yes"
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%bind decision =
          Bank_transfer.create
            ~first_event_id:first
            ~second_event_id:second
            ~state:Rejected
            ~reason:"Undone by user"
        in
        let%map changed =
          Transfers.set_bank_transfers ~expected_previous:previous db [ decision ]
        in
        printf "transfer_changes=%d\n" changed)))
;;

let transfer_undo_cmd =
  command
    "undo"
    "Undo a confirmed transfer and return both movements to review."
    Term.(
      const transfer_undo
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"FIRST-EVENT-ID" ~doc:"First source event ID.")
      $ Arg.(
          required
          & pos 1 (some int64) None
          & info [] ~docv:"SECOND-EVENT-ID" ~doc:"Second source event ID.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Undo without prompting."))
;;

let reversal_candidates database =
  with_database database (fun db ->
    let%map candidates = Transfers.reversal_candidates db in
    if List.is_empty candidates
    then print_endline "No unreviewed reversal candidates."
    else
      List.iter candidates ~f:(fun (c : Transfers.reversal_candidate) ->
        printf "Original #%Ld; reversal #%Ld\n" c.original.event_id c.reversal.event_id;
        Terminal.review [ c.original; c.reversal ]))
;;

let reversal_candidates_cmd =
  command
    "candidates"
    "Show possible full withdrawal reversals."
    Term.(const reversal_candidates $ database)
;;

let reversal_review database original reversal undo yes =
  with_database database (fun db ->
    let%bind rows = Review.bank_review db in
    let find id =
      List.find rows ~f:(fun r -> Int64.equal r.Review.event_id id)
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
    in
    let%bind first = find original in
    let%bind second = find reversal in
    if not undo
    then (
      print_endline "Confirm this original withdrawal and its reversal?";
      Terminal.review [ first; second ])
    else (
      print_endline "Undo this confirmed reversal?";
      Terminal.review [ first; second ]);
    let%bind confirmed =
      if yes
      then Ok true
      else if Unix.isatty Unix.stdin
      then
        Ok
          (Option.value_map
             (prompt "Apply this decision? [Y/n]: ")
             ~default:false
             ~f:confirm_default_yes)
      else Or_error.error_string "reversal review requires a terminal or --yes"
    in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () = Transfers.reversal_change db ~original ~reversal ~undo in
      print_endline (if undo then "Reversal undone." else "Reversal confirmed.")))
;;

let reversal_review_cmd ~undo =
  command
    (if undo then "undo" else "review")
    (if undo
     then "Undo a confirmed reversal pair."
     else "Confirm a full withdrawal reversal pair.")
    Term.(
      const reversal_review
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"ORIGINAL-ID" ~doc:"Original withdrawal event ID.")
      $ Arg.(
          required
          & pos 1 (some int64) None
          & info [] ~docv:"REVERSAL-ID" ~doc:"Reversal event ID.")
      $ const undo
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Confirm without prompting."))
;;

let reversal_history_cmd =
  command
    "history"
    "Show the append-only reversal decision history."
    Term.(
      const (fun database ->
        with_database database (fun db ->
          let%map changes = Transfers.reversal_history db in
          List.iter changes ~f:(fun (r : Transfers.reversal_history_entry) ->
            printf
              "#%Ld  #%Ld -> #%Ld  %s  %s\n"
              r.id
              r.original
              r.reversal
              (match r.action with
               | Reversal_confirmed -> "confirmed"
               | Reversal_undone -> "undone")
              (Timestamp.to_rfc3339 r.changed_at))))
      $ database)
;;

let transfer_history database =
  with_database database (fun db ->
    let%bind rows = Transfers.bank_transfer_history db in
    Report_json.print (Jsont.list Report_json.transfer_change) rows;
    Ok ())
;;

let transfer_history_cmd =
  command
    "history"
    "Inspect the append-only transfer review history."
    Term.(const transfer_history $ database)
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
    let%bind config = Accounts.export_bank_accounts db in
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
    let%bind result = Accounts.apply_bank_accounts ~dry_run db config in
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
    let%bind current = Accounts.export_bank_accounts db in
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
              Accounts.apply_bank_accounts db edited)
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
    let%bind rows = Accounts.bank_account_history db in
    Report_json.print (Jsont.list Report_json.account_change) rows;
    Ok ())
;;

let accounts_delete database account_id dry_run yes =
  with_database database (fun db ->
    let%bind accounts = Transfers.bank_account_info db in
    let matches =
      List.filter accounts ~f:(fun (a : Transfers.bank_account_info) ->
        String.equal (Bank_account_mapping.id a.mapping) account_id)
    in
    let%bind account =
      match matches with
      | [ account ] -> Ok account
      | [] -> Or_error.error_string "no matching account ID; see fin account list"
      | _ -> Or_error.error_string "account ID collision; deletion is refused"
    in
    let%bind preview = Accounts.delete_account ~dry_run:true db account.mapping in
    printf
      "Account: %s / %s (%s), ID %s\nTransactions: %d\nSource documents: %d\n"
      account.mapping.institution
      account.mapping.account_id
      account.mapping.account_type
      account_id
      preview.transactions
      preview.documents;
    if dry_run
    then (
      print_endline "Dry run: nothing deleted.";
      Ok ())
    else (
      let%bind confirmed =
        if yes
        then Ok true
        else if Unix.isatty Unix.stdin
        then (
          Out_channel.output_string stdout "Delete this account and its records? [Y/n]: ";
          Out_channel.flush stdout;
          Ok
            (Option.value_map
               (In_channel.input_line In_channel.stdin)
               ~default:false
               ~f:confirm_default_yes))
        else
          Or_error.error_string
            "refusing to delete without confirmation; use --yes in scripts"
      in
      if not confirmed
      then (
        print_endline "Nothing deleted.";
        Ok ())
      else (
        let%bind _ = Accounts.delete_account db account.mapping in
        let journal =
          Stdlib.Filename.concat (Stdlib.Filename.dirname database) "bank.journal"
        in
        let%bind () =
          if Stdlib.Sys.file_exists journal
          then
            (let%bind contents = Finances_hledger.export db in
             write_file journal contents)
            |> Or_error.tag
                 ~tag:"account deleted; failed to refresh the generated journal"
          else Ok ()
        in
        print_endline "Account and its records deleted.";
        Ok ())))
;;

let accounts_link_card database card_id bank_id yes =
  with_database database (fun db ->
    let%bind accounts = Transfers.bank_account_info db in
    let find id =
      match
        List.filter accounts ~f:(fun a ->
          String.equal (Bank_account_mapping.id a.Transfers.mapping) id)
      with
      | [ account ] -> Ok account
      | [] -> Or_error.errorf "unknown account ID: %s" id
      | _ -> Or_error.errorf "account ID collision: %s" id
    in
    let%bind card = find card_id in
    let%bind bank = find bank_id in
    if
      not
        (List.mem
           [ "CREDITCRD"; "credit-card" ]
           card.mapping.account_type
           ~equal:String.equal)
    then Or_error.error_string "the first account must be a credit card"
    else if String.equal bank.iban_source "not_applicable" || String.equal card_id bank_id
    then
      Or_error.error_string "the settlement account must be a separate non-card account"
    else if not (Iso4217.equal card.currency bank.currency)
    then
      Or_error.error_string "the card and settlement account must use the same currency"
    else (
      let%bind applied = setup_card db ~card ~bank ~yes ~allow_yes:true in
      if applied
      then print_endline "Card setup complete."
      else print_endline "Nothing changed.";
      Ok ()))
;;

let account_cmd =
  Cmd.group
    (Cmd.info
       "account"
       ~doc:"Manage bank account settings stored in SQLite; export and import snapshots.")
    [ command
        "list"
        "List account details, IBAN provenance, coverage and card settlement links."
        Term.(const list_bank_accounts $ database $ json_output)
    ; command
        "link-card"
        "Link a credit card to the account that pays it."
        Term.(
          const accounts_link_card
          $ database
          $ Arg.(
              required
              & pos 0 (some string) None
              & info [] ~docv:"CARD-ID" ~doc:"Credit card ID from fin account list.")
          $ Arg.(
              required
              & pos 1 (some string) None
              & info
                  []
                  ~docv:"ACCOUNT-ID"
                  ~doc:"Settlement account ID from fin account list.")
          $ Arg.(
              value
              & flag
              & info [ "yes" ] ~doc:"Apply the proposed setup without prompting."))
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
        "delete"
        "Delete one account and its transactions after showing what will be removed."
        Term.(
          const accounts_delete
          $ database
          $ Arg.(
              required
              & pos 0 (some string) None
              & info [] ~docv:"ID" ~doc:"Account ID shown by fin account list.")
          $ Arg.(
              value
              & flag
              & info
                  [ "dry-run" ]
                  ~doc:"Show what would be deleted without changing data.")
          $ Arg.(
              value
              & flag
              & info [ "yes" ] ~doc:"Delete without interactive confirmation."))
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

let command_argv () =
  let argv = Stdlib.Array.copy Stdlib.Sys.argv in
  if Stdlib.Array.length argv > 1
  then
    argv.(1)
    <- (match argv.(1) with
        | "op" -> "operation"
        | "acc" -> "account"
        | "inst" -> "institution"
        | "cat" -> "category"
        | command -> command);
  if
    Stdlib.Array.length argv > 2
    && String.equal argv.(1) "operation"
    && String.equal argv.(2) "cat"
  then argv.(2) <- "category";
  if Stdlib.Array.length argv > 1 && String.equal argv.(1) "ledger"
  then (
    let args = Array.to_list argv in
    let rec split prefix = function
      | (("--database" | "-d") as option) :: value :: rest ->
        split (value :: option :: prefix) rest
      | option :: rest when String.is_prefix option ~prefix:"--database=" ->
        split (option :: prefix) rest
      | [ option ]
        when String.equal option "-h" || String.is_prefix option ~prefix:"--help" ->
        List.rev (option :: prefix)
      | "--" :: rest -> List.rev prefix @ ("--" :: rest)
      | rest -> List.rev prefix @ ("--" :: rest)
    in
    match args with
    | program :: name :: rest -> Array.of_list (program :: name :: split [] rest)
    | _ -> argv)
  else argv
;;

let () =
  Stdlib.exit
    (Cmd.eval
       ~argv:(command_argv ())
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
                    fin account list\n\
                    fin operation list\n\
                    fin operation review\n\
                    fin journal export > my-finances.journal"
               ; `P
                   "Use $(b,fin paths) to inspect storage locations, or append \
                    $(b,--help) to any subcommand for its options."
               ; `P
                   "Command shortcuts: $(b,op) means $(b,operation); $(b,acc) means \
                    $(b,account); $(b,inst) means $(b,institution); $(b,cat) means \
                    $(b,category)."
               ])
          [ setup_cmd
          ; status_cmd
          ; paths_cmd
          ; ledger_cmd
          ; backup_cmd
          ; restore_cmd
          ; delete_cmd
          ; account_cmd
          ; import_bank_cmd
          ; group
              "category"
              "Manage the personal income and expense category catalog."
              [ category_list_cmd
              ; category_add_cmd
              ; category_delete_cmd
              ; category_init_cmd
              ; category_export_cmd
              ; category_restore_cmd
              ]
          ; group
              "institution"
              "Manage institutions and their importers."
              [ institution_list_cmd
              ; institution_show_cmd
              ; institution_add_cmd
              ; institution_delete_cmd
              ; group
                  "definition"
                  "Validate and install institution definitions."
                  [ institution_definition_show_cmd
                  ; institution_definition_check_cmd
                  ; institution_definition_set_cmd
                  ]
              ; import_history_cmd
              ; group
                  "companion"
                  "Configure CSV enrichment layouts."
                  [ institution_companion_show_cmd
                  ; institution_companion_set_cmd
                  ; institution_companion_clear_cmd
                  ]
              ]
          ; group
              "settlement"
              "Review credit-card settlements."
              [ settlement_candidates_cmd
              ; settlement_review_cmd
              ; settlement_undo_cmd
              ; settlement_history_cmd
              ]
          ; group
              "reversal"
              "Review and undo transaction reversals."
              [ reversal_candidates_cmd
              ; reversal_review_cmd ~undo:false
              ; reversal_review_cmd ~undo:true
              ; reversal_history_cmd
              ]
          ; group
              "rule"
              "Manage automatic and reviewed transaction rules."
              [ rule_list_cmd
              ; rules_export_cmd
              ; rules_restore_cmd
              ; rule_from_category_cmd
              ; rule_mode_cmd
              ; rule_add_cmd ~settlement:false
              ; rule_add_cmd ~settlement:true
              ; rule_add_evidence_cmd
              ; rule_preview_cmd
              ; rule_apply_cmd
              ; rule_delete_cmd
              ; rule_rename_cmd
              ; rule_test_cmd
              ; rule_explain_cmd
              ; rule_gaps_cmd
              ]
          ; group
              "db"
              "Initialize and inspect local source evidence."
              [ init
              ; events
              ; ingest_cmd
              ; transaction_sources_cmd
              ; group "event" "Inspect one source event." [ transaction_operations_cmd ]
              ]
          ; group
              "statement"
              "Inspect and reconcile imported statements and companion files."
              [ statements_cmd; companions_cmd; match_companions_cmd ]
          ; group
              "transfer"
              "Review transfer candidates and decisions."
              [ transfer_candidates_cmd
              ; review_transfers_cmd
              ; transfer_link_cmd
              ; transfer_unlink_cmd
              ; transfer_export_cmd
              ; transfer_restore_cmd
              ; transfer_undo_cmd
              ; transfer_history_cmd
              ]
          ; group
              "operation"
              "Inspect and classify imported operations."
              [ transaction_list_cmd
              ; transaction_status_cmd
              ; transaction_show_cmd
              ; operation_resolve_cmd
              ; group
                  "exchange"
                  "Review commodity exchanges."
                  [ operation_exchange_cmd ~undo:false
                  ; operation_exchange_cmd ~undo:true
                  ; operation_exchange_export_cmd
                  ; operation_exchange_restore_cmd
                  ]
              ; category_decisions_export_cmd
              ; category_decisions_restore_cmd
              ; refresh_transaction_evidence_cmd
              ; group
                  "type"
                  "Inspect and override movement types."
                  [ transaction_type_list_cmd
                  ; transaction_type_show_cmd
                  ; transaction_type_set_cmd
                  ; transaction_type_reset_cmd
                  ; transaction_type_history_cmd
                  ]
              ; bank_review_cmd
              ; group
                  "category"
                  "Manage reviewed categories."
                  [ categorize_bank_cmd
                  ; category_set_cmd
                  ; category_history_cmd
                  ; category_similar_cmd
                  ; category_review_cmd
                  ; category_queue_cmd
                  ]
              ]
          ; group
              "journal"
              "Project and render accounting entries."
              [ project_bank_cmd; export_hledger_cmd ]
          ]))
;;
