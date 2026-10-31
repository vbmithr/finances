open Base
open Stdio
open Finances
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
  Store.with_open ~registry:Builtin.registry path f
;;

let companion_profiles path =
  let%bind profiles =
    Store.with_open ~registry:Builtin.registry path Store.institution_companion_profiles
  in
  profiles
  |> List.map ~f:(fun (institution, json) ->
    let%map profile = Json.decode Csv_companion_layout.codec json in
    institution, profile)
  |> Or_error.combine_errors
;;

let institution_definitions path =
  let%bind rows =
    Store.with_open ~registry:Builtin.registry path Store.institution_definitions
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
  Store.with_open ~registry path f
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

let command ?(man = []) name doc term =
  Cmd.v
    (Cmd.info name ~doc ~man)
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
     \032 fin transaction list    List imported transactions\n\
     \032 fin transaction review  Review unclassified movements\n\
     \032 fin statement list       Inspect statement reconciliation\n\
     \032 fin settlement review    Review card settlements\n\
     \032 fin reversal candidates  Find possible reversals\n\
     \032 fin rule --help          Manage classification rules\n\
     \032 fin transfer candidates  Find possible transfers\n\
     \032 fin journal export       Produce an hledger journal\n\n\
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
     \032 fin transaction list%s\n\
     \032 fin transaction review%s\n\
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
  let module Status = Finances_import.Bank_status in
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
    print_endline
      "Next: fin transaction review; fin transfer candidates; fin institution history.");
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
    with_database database (fun db -> Store.document_contexts db ~institution)
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
    with_database database (fun db -> Store.institution_profile db institution)
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
        let date_format =
          if
            (not (List.is_empty date_values))
            && List.for_all date_values ~f:(Re.execp iso_date)
          then Csv_profile.Iso
          else (
            eprintf
              "Dates must be interpreted consistently; examples: %s\n"
              (String.concat ~sep:", " (List.take date_values 3));
            choose
              "Date format:"
              [ "YYYY-MM-DD", [], Csv_profile.Iso
              ; "DD/MM/YYYY or DD-MM-YYYY", [ "day-first" ], Csv_profile.Day_first
              ; "MM/DD/YYYY or MM-DD-YYYY", [ "month-first" ], Csv_profile.Month_first
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
    let%bind companion = Store.institution_companion_profile db institution in
    let%bind () =
      if String.equal expected_kind "csv-movement" && Option.is_some companion
      then
        Or_error.error_string
          "this institution uses a CSV companion; import it with fin import INSTITUTION \
           PATH"
      else Ok ()
    in
    let%bind events = Store.events db in
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
          let%bind existing = Store.institutions db in
          if List.mem existing institution ~equal:String.equal
          then (
            match !selected_profile with
            | None -> Ok ()
            | Some profile ->
              Store.set_institution_profile_if_missing db institution profile)
          else Store.add_institution ?profile:!selected_profile db institution)
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

let generic_statement_files paths =
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
                   [ ".csv"; ".ofx"; ".zip" ]
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
  then Or_error.error_string "no CSV, OFX or ZIP files found"
  else Ok files
;;

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
      ]
    "import"
    "Import a bank's statement files, apply mappings, and publish a bank journal."
    Term.(
      const (fun database first paths journal max_mib account_id provider ->
        let%bind () = require_database database in
        let%bind runtime_registry = registry_for database in
        let institutions =
          List.filter (Registry.institutions runtime_registry) ~f:(fun institution ->
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
          then with_database database Store.institutions
          else Ok []
        in
        match institutions with
        | [ institution ] ->
          if List.is_empty paths
          then Or_error.error_string "provide at least one statement path"
          else import_bank database institution paths journal max_mib account_id provider
        | _ :: _ :: _ -> Or_error.errorf "ambiguous institution alias: %s" first
        | [] when List.mem user_institutions first ~equal:String.equal ->
          if List.is_empty paths
          then Or_error.error_string "provide at least one statement file or directory"
          else if Option.is_some provider
          then
            Or_error.error_string
              "--provider is unnecessary when the institution is named"
          else (
            let%bind files = generic_statement_files paths in
            let%bind companion =
              with_database database (fun db ->
                Store.institution_companion_profile db first)
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
                  "generic CSV and OFX are separate movement sources; import one format \
                   to avoid duplicate transactions"
              else if not (List.is_empty ofx)
              then (
                let%bind () = ensure_generic_source database first "bank-transaction" in
                import_bank database first ofx journal max_mib account_id None)
              else (
                let%bind () = ensure_generic_source database first "csv-movement" in
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
            "name the bank before a directory, for example: fin import wise DIRECTORY"
        | [] when Stdlib.Sys.file_exists first ->
          import_bank database "csv" (first :: paths) journal max_mib account_id provider
        | [] -> Or_error.errorf "unknown institution or file: %s" first)
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
              ~doc:"Institution name for a generic CSV; prompted on a terminal."))
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
    "list"
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

type movement_type_filter =
  | Unknown_type
  | Known_type of Movement_type.t

let list_transactions
      database
      account_id
      institution
      from_date
      to_date
      search
      movement_type
      classification
      limit
      order
      json_output
  =
  let parse_date label = function
    | None -> Ok None
    | Some text ->
      Or_error.try_with (fun () ->
        let date = Date.of_string text in
        if not (String.equal text (Date.to_string date))
        then failwith "expected YYYY-MM-DD";
        Some date)
      |> Or_error.tag ~tag:label
  in
  let%bind from_date = parse_date "--from" from_date in
  let%bind to_date = parse_date "--to" to_date in
  let%bind movement_type =
    match movement_type with
    | None -> Ok None
    | Some "unknown" -> Ok (Some Unknown_type)
    | Some value when String.is_empty (String.strip value) ->
      Or_error.error_string "--type must not be empty"
    | Some value -> Ok (Some (Known_type (Movement_type.of_string value)))
  in
  let%bind () =
    match from_date, to_date, limit with
    | Some first, Some last, _ when Date.compare first last > 0 ->
      Or_error.error_string "--from must not be after --to"
    | _, _, Some n when n <= 0 -> Or_error.error_string "--limit must be positive"
    | _ -> Ok ()
  in
  with_database database (fun db ->
    let%bind accounts = Store.bank_account_info db in
    let%bind () =
      match account_id with
      | None -> Ok ()
      | Some id ->
        (match
           List.count accounts ~f:(fun a ->
             String.equal (Bank_account_mapping.id a.Store.mapping) id)
         with
         | 1 -> Ok ()
         | 0 -> Or_error.error_string "unknown account ID; see fin account list"
         | _ -> Or_error.error_string "account ID collision; filtering is refused")
    in
    let ids = Hashtbl.Poly.create () in
    List.iter accounts ~f:(fun a ->
      Hashtbl.set
        ids
        ~key:(a.Store.mapping.institution, Bank_account_mapping.source_account a.mapping)
        ~data:(Bank_account_mapping.id a.mapping));
    let%bind reviews = Store.bank_review db in
    let%bind rows =
      List.map reviews ~f:(fun (review : Store.bank_review) ->
        let account = review.transaction.account in
        let source =
          Json.encode
            (Jsont.list Jsont.string)
            [ account.bank_id; account.account_id; account.account_type ]
        in
        match Hashtbl.find ids (review.institution, source) with
        | None -> Or_error.error_string "transaction references an unknown account"
        | Some account_id -> Ok { Report_json.account_id; review })
      |> Or_error.combine_errors
    in
    let rows =
      List.filter rows ~f:(fun (row : Report_json.transaction) ->
        let r = row.review in
        Option.value_map account_id ~default:true ~f:(String.equal row.account_id)
        && Option.value_map institution ~default:true ~f:(String.equal r.institution)
        && Option.value_map movement_type ~default:true ~f:(function
          | Unknown_type -> Option.is_none r.movement_type
          | Known_type kind ->
            Option.value_map r.movement_type ~default:false ~f:(Poly.equal kind))
        && Option.value_map classification ~default:true ~f:(Poly.equal r.classification)
        && Option.value_map from_date ~default:true ~f:(fun date ->
          Date.compare r.transaction.booked_on date >= 0)
        && Option.value_map to_date ~default:true ~f:(fun date ->
          Date.compare r.transaction.booked_on date <= 0)
        && Option.value_map search ~default:true ~f:(fun text ->
          Option.value_map r.display_name ~default:false ~f:(fun name ->
            String.Caseless.is_substring name ~substring:text)
          || List.exists r.transaction.descriptions ~f:(fun d ->
            String.Caseless.is_substring d ~substring:text)))
      |> List.sort
           ~compare:(fun (a : Report_json.transaction) (b : Report_json.transaction) ->
             let compare (a : Report_json.transaction) (b : Report_json.transaction) =
               match
                 String.compare
                   (Temporal.to_string a.review.occurred)
                   (Temporal.to_string b.review.occurred)
               with
               | 0 -> Int64.compare a.review.event_id b.review.event_id
               | n -> n
             in
             if order then compare a b else compare b a)
    in
    let rows = Option.value_map limit ~default:rows ~f:(List.take rows) in
    if json_output
    then Report_json.print (Jsont.list Report_json.transaction) rows
    else if Unix.isatty Unix.stdout && not (List.is_empty rows)
    then (
      let pager = Option.value (Sys.getenv "PAGER") ~default:"less -FRX" in
      if String.is_empty (String.strip pager)
      then Terminal.transactions rows
      else (
        let out = Unix.open_process_out pager in
        Exn.protect
          ~f:(fun () ->
            try Terminal.transactions ~out rows with
            | Sys_error _ -> ())
          ~finally:(fun () ->
            try ignore (Unix.close_process_out out) with
            | Sys_error _ -> ())))
    else Terminal.transactions rows;
    Ok ())
;;

let transaction_list_cmd =
  command
    "list"
    "List imported transactions, newest first. Terminal tables use PAGER (default: less \
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
          & info
              [ "institution" ]
              ~docv:"NAME"
              ~doc:"Only transactions from this provider.")
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
              ~doc:"ASCII case-insensitive display name or description substring.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "type" ]
              ~docv:"TYPE"
              ~doc:
                "Movement type as displayed, or unknown for transactions without a type. \
                 Provider-specific types are accepted.")
      $ Arg.(
          value
          & opt
              (some
                 (enum
                    [ "unclassified", Store.Unclassified
                    ; "category", Category
                    ; "transfer", Transfer
                    ; "settlement", Settlement
                    ; "reversal", Reversal
                    ]))
              None
          & info
              [ "classification" ]
              ~docv:"CLASSIFICATION"
              ~doc:
                "Accounting classification: unclassified, category, transfer, settlement \
                 or reversal.")
      $ Arg.(
          value
          & opt (some int) None
          & info [ "limit" ] ~docv:"COUNT" ~doc:"Maximum number of transactions.")
      $ Arg.(
          value
          & opt (enum [ "asc", true; "desc", false ]) false
          & info
              [ "order" ]
              ~docv:"ORDER"
              ~doc:"Order by source event date or timestamp: asc or desc (default: desc).")
      $ json_output)
;;

let refresh_transaction_types database institution yes =
  with_database database (fun db ->
    let%bind changes = Store.preview_movement_types ?institution db in
    let name = Option.value_map ~default:"unknown" ~f:Movement_type.to_string in
    if List.is_empty changes
    then (
      print_endline "No movement type changes.";
      Ok ())
    else (
      printf "Proposed movement type changes: %d\n" (List.length changes);
      List.iter changes ~f:(fun (change : Store.movement_type_change) ->
        printf
          "  #%Ld  %s  %s -> %s  %s\n"
          change.event_id
          (Terminal.text change.institution)
          (name change.before)
          (name change.after)
          (Terminal.text change.description));
      let%bind confirmed =
        if yes
        then Ok true
        else if Unix.isatty Unix.stdin
        then
          Ok
            (Option.value_map
               (prompt "Apply these movement type changes? [Y/n]: ")
               ~default:false
               ~f:confirm_default_yes)
        else Or_error.error_string "type refresh requires a terminal or --yes"
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%map count = Store.refresh_movement_types ?institution db ~expected:changes in
        printf "movement_types_changed=%d\n" count)))
;;

let refresh_reported_types database institution yes =
  with_database database (fun db ->
    let%bind changes = Store.preview_reported_types ?institution db in
    let name = Option.value_map ~default:"unknown" ~f:Movement_type.to_string in
    if List.is_empty changes
    then (
      print_endline "No reported type changes.";
      Ok ())
    else (
      printf "Proposed reported type changes: %d\n" (List.length changes);
      List.iter changes ~f:(fun (c : Store.reported_type_change) ->
        printf
          "  CSV row #%Ld%s  %s  %s  %s -> %s\n"
          c.row_id
          (Option.value_map
             c.event_id
             ~default:""
             ~f:(Printf.sprintf " (transaction #%Ld)"))
          (Terminal.text c.institution)
          (Terminal.text (Option.value c.code ~default:"(no longer decided)"))
          (name c.before)
          (name c.after));
      let%bind confirmed =
        if yes
        then Ok true
        else if Unix.isatty Unix.stdin
        then
          Ok
            (Option.value_map
               (prompt "Apply these reported type changes? [Y/n]: ")
               ~default:false
               ~f:confirm_default_yes)
        else Or_error.error_string "reported type refresh requires a terminal or --yes"
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%map count = Store.refresh_reported_types ?institution db ~expected:changes in
        printf "reported_types_changed=%d\n" count)))
;;

let refresh_reported_types_cmd =
  command
    "refresh-reported-types"
    "Preview and refresh types from imported provider codes."
    Term.(
      const refresh_reported_types
      $ database
      $ institution_filter
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Apply the preview without prompting."))
;;

let refresh_transaction_evidence database institution yes =
  with_database database (fun db ->
    let%bind changes = Store.preview_source_evidence ?institution db in
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
      List.iter changes ~f:(fun (change : Store.source_evidence_change) ->
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
        let%map count = Store.refresh_source_evidence ?institution db ~expected:changes in
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

let refresh_transaction_types_cmd =
  command
    "refresh-types"
    "Preview and refresh imported movement types from stored source evidence."
    Term.(
      const refresh_transaction_types
      $ database
      $ institution_filter
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Apply the preview without prompting."))
;;

let transaction_type_list database =
  with_database database (fun db ->
    let%map provider_types = Store.observed_provider_movement_types db in
    print_endline "Common movement types:";
    List.iter Movement_type.common ~f:(fun (kind, description) ->
      printf "  %-18s %s\n" (Movement_type.to_string kind) description);
    print_endline "\nProvider-specific types in imported data:";
    if List.is_empty provider_types
    then print_endline "  None."
    else List.iter provider_types ~f:(fun name -> printf "  %s\n" (Terminal.text name));
    print_endline
      "\nUse --type unknown with fin tx list to find movements without a type.")
;;

let transaction_type_list_cmd =
  command
    "list"
    "List common and observed provider-specific movement types."
    Term.(const transaction_type_list $ database)
;;

let transaction_type_set database event_id value =
  let%bind kind =
    if String.is_empty (String.strip value) || String.equal value "unknown"
    then
      Or_error.error_string
        "choose a type from fin tx type list; use fin tx type reset to remove an override"
    else Ok (Movement_type.of_string value)
  in
  with_database database (fun db ->
    let%map changed =
      Store.set_movement_type_override db ~event_id ~movement_type:(Some kind)
    in
    print_endline
      (if changed
       then "Movement type override saved."
       else "Movement type override already set."))
;;

let transaction_type_set_cmd =
  command
    "set"
    "Set a manual movement type; see fin tx type list."
    Term.(
      const transaction_type_set
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"EVENT-ID" ~doc:"Transaction event ID.")
      $ Arg.(
          required
          & pos 1 (some string) None
          & info [] ~docv:"TYPE" ~doc:"Type from fin tx type list."))
;;

let transaction_type_reset database event_id =
  with_database database (fun db ->
    let%map changed = Store.set_movement_type_override db ~event_id ~movement_type:None in
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
          & info [] ~docv:"EVENT-ID" ~doc:"Transaction event ID."))
;;

let transaction_type_show database event_id =
  with_database database (fun db ->
    let%bind rows = Store.bank_review db in
    let%map row =
      List.find rows ~f:(fun r -> Int64.equal r.Store.event_id event_id)
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
          & info [] ~docv:"EVENT-ID" ~doc:"Transaction event ID."))
;;

let transaction_type_history database json_output =
  with_database database (fun db ->
    let%map rows = Store.movement_type_history db in
    if json_output
    then Report_json.print (Jsont.list Report_json.movement_type_change) rows
    else if List.is_empty rows
    then print_endline "No movement type changes."
    else
      List.iter rows ~f:(fun (r : Store.movement_type_history_entry) ->
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

let institution_list database =
  let%bind () = require_database database in
  let%bind definitions = institution_definitions database in
  List.iter (Registry.institutions Builtin.registry) ~f:(fun institution ->
    if not (String.equal institution "csv")
    then (
      let aliases = Registry.aliases Builtin.registry institution |> List.tl_exn in
      if List.is_empty aliases
      then printf "%s (built-in)\n" institution
      else
        printf
          "%s (built-in; aliases: %s)\n"
          institution
          (String.concat ~sep:", " aliases)));
  List.iter definitions ~f:(fun definition ->
    let name = Institution_definition.institution definition in
    if not (Registry.is_registered Builtin.registry name)
    then printf "%s (definition)\n" (Terminal.text name));
  with_database database (fun db ->
    let%map names = Store.institutions db in
    List.iter names ~f:(fun name ->
      if
        not
          (List.exists definitions ~f:(fun definition ->
             String.equal name (Institution_definition.institution definition)))
      then printf "%s (generic)\n" (Terminal.text name)))
;;

let institution_list_cmd =
  command
    "list"
    "List built-in and user-added institutions."
    Term.(const institution_list $ database)
;;

let print_institution_description (d : Institution_definition.description) =
  printf "%s\n" (Terminal.text d.institution);
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
      | Reported_type -> "reported-type"
      | Reported_other -> "reported-other"
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
  let%map summary = Store.institution_summary db institution in
  match summary.first_on, summary.last_on with
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
  | _ -> print_endline "\n  Imported data: none."
;;

let institution_show database name =
  let%bind installed =
    if Stdlib.Sys.file_exists database then institution_definitions database else Ok []
  in
  let%bind definition =
    match
      List.find installed ~f:(fun d ->
        List.mem (Institution_definition.names d) name ~equal:String.equal)
    with
    | Some d -> Ok d
    | None ->
      Builtin.definition name
      |> Or_error.of_option
           ~error:
             (Error.of_string
                "unknown institution, or one without a definition; see fin institution \
                 list")
  in
  print_institution_description (Institution_definition.describe definition);
  print_institution_rules definition;
  if Stdlib.Sys.file_exists database
  then
    with_database database (fun db ->
      print_institution_data db (Institution_definition.institution definition))
  else (
    print_endline "\n  Imported data: no database yet.";
    Ok ())
;;

let institution_show_cmd =
  command
    "show"
    "Show what fin reads for an institution, its default rules and what is imported."
    Term.(
      const institution_show
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION"))
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
    let%bind existing = with_database database Store.institutions in
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
        let%bind () = Store.add_institution ?profile db name in
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
      | Reported_type -> "reported-type"
      | Reported_other -> "reported-other"
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
    let%bind generic = with_database database Store.institutions in
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
            let%bind definitions = Store.institution_definitions db in
            let%map rules = Store.classification_rules db in
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
            Store.set_institution_definition
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
              Store.set_institution_definition
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
      let%bind profile = Store.institution_companion_profile db name in
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
    let%bind current = Store.institution_companion_profile db name in
    let%bind () =
      Store.set_institution_companion_profile ~dry_run:true db name ~expected:current json
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
        Store.set_institution_companion_profile db name ~expected:current json
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
      Store.institution_companion_profile db name
      |> Or_error.bind ~f:(function
        | Some profile -> Ok profile
        | None -> Or_error.error_string "no companion profile configured")
    in
    let%bind () =
      Store.clear_institution_companion_profile ~dry_run:true db name ~expected
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
      let%map () = Store.clear_institution_companion_profile db name ~expected in
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
    let%bind () = Store.delete_institution ~dry_run:true db name in
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
      let%bind () = Store.delete_institution db name in
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
  { transaction : Store.bank_review
  ; card : Store.bank_account_info
  ; bank : Store.bank_account_info
  }

let account_reference (mapping : Bank_account_mapping.t) =
  { Bank_account_config.institution = mapping.institution
  ; bank_id = mapping.bank_id
  ; account_id = mapping.account_id
  ; account_type = mapping.account_type
  }
;;

let settlement_suggestions db =
  let%bind rows = Store.bank_review db in
  let%bind accounts = Store.bank_account_info db in
  let suggestions =
    List.filter_map rows ~f:(fun (row : Store.bank_review) ->
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
              String.equal a.Store.mapping.institution row.institution
              && String.equal a.mapping.bank_id movement.account.bank_id
              && String.equal a.mapping.account_id movement.account.account_id
              && String.equal a.mapping.account_type movement.account.account_type)
          in
          match banks with
          | [ bank ] when not (String.equal bank.iban_source "not_applicable") ->
            let parent = account_reference bank.mapping in
            let cards =
              List.filter accounts ~f:(fun card ->
                String.equal card.Store.mapping.institution row.institution
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
                   let tx = other.Store.transaction in
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

let card_setup_plan ~(card : Store.bank_account_info) ~(bank : Store.bank_account_info) =
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
      ~(card : Store.bank_account_info)
      ~(bank : Store.bank_account_info)
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
      let%map _ = Store.apply_bank_accounts db [ update ] in
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
          Store.confirm_card_settlement_with_setup
            db
            ~event_id:s.transaction.event_id
            ~card_id:(Bank_account_mapping.id s.card.mapping)
            ~expected_ledger:s.card.mapping.ledger_account
            ~expected_link:s.card.settlement_account
            ~setup
        else
          Store.confirm_card_settlement
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
            let%map row, card = Store.card_settlement_preview ?card_id db event_id in
            row, card, None
          | Some s ->
            let proposed, needs_link, needs_mapping, update =
              card_setup_plan ~card:s.card ~bank:s.bank
            in
            if (not needs_link) && not needs_mapping
            then (
              let%map row, card = Store.card_settlement_preview ?card_id db event_id in
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
              Store.confirm_card_settlement
                ~card_id:(Bank_account_mapping.id card.mapping)
                ~expected_ledger:card.mapping.ledger_account
                db
                event_id
            | Some (s, _, _, _, setup) ->
              Store.confirm_card_settlement_with_setup
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
      let%bind () = Store.undo_card_settlement db event_id in
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
          & info [] ~docv:"EVENT-ID" ~doc:"Confirmed settlement transaction ID.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Undo without prompting."))
;;

let settlement_history_cmd =
  command
    "history"
    "List card settlement decisions as JSON."
    Term.(
      const (fun database ->
        with_database database (fun db ->
          let%map rows = Store.card_settlement_history db in
          Report_json.print (Jsont.list Report_json.card_settlement_change) rows))
      $ database)
;;

let find_rule db name =
  let%bind rules = Store.classification_rules db in
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
      -> Or_error.error_string "choose a movement type from fin tx type list"
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
    | ( true
      , (Card_reference | Withdrawal_reversal_hint | Reported_type _ | Reported_other _) )
      -> Ok ()
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
      ()
  in
  with_database database (fun db ->
    let%bind observed = Store.observed_provider_movement_types db in
    let%bind () =
      List.filter_opt
        [ Option.bind match_type ~f:(function
            | Is kind -> Some kind
            | Unknown -> None)
        ; set_type
        ]
      |> List.map ~f:(function
        | Movement_type.Other name when not (List.mem observed name ~equal:String.equal)
          -> Or_error.error_string "unknown movement type; see fin tx type list"
        | _ -> Ok ())
      |> Or_error.combine_errors_unit
    in
    let%bind () =
      match action with
      | Settlement id ->
        let%bind accounts = Store.bank_account_info db in
        (match
           List.filter accounts ~f:(fun a ->
             String.equal (Bank_account_mapping.id a.Store.mapping) id)
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
      then Store.replace_classification_rule db rule
      else Store.add_classification_rule db rule
    in
    printf
      "Saved rule %s (%s). Run fin rule preview %s to review existing matches.\n"
      (Terminal.text name)
      (if automatic then "automatic on import" else "review")
      (Terminal.text name);
    Ok ())
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
            ~doc:"Propose this movement type; see fin tx type list.")
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
      then Store.replace_classification_rule db rule
      else Store.add_classification_rule db rule
    in
    printf
      "Saved evidence rule %s. It applies to future imports; run fin tx refresh-evidence \
       to preview existing changes.\n"
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

let rule_add_reported_type
      database
      name
      institution
      pattern
      target
      case_sensitive
      priority
      field
      replace
  =
  let action =
    if String.is_prefix target ~prefix:"other:"
    then Classification_rule.Reported_other (String.drop_prefix target 6)
    else
      Reported_type
        (if String.equal target "unknown"
         then None
         else Some (Movement_type.of_string target))
  in
  let%bind rule =
    Classification_rule.create
      ~mode:Automatic
      ~account_type:None
      ~priority
      ~name
      ~pattern:(Some pattern)
      ~institution:(Some institution)
      ~match_type:None
      ~case_sensitive
      ~action
      ?field
      ()
  in
  with_database database (fun db ->
    let%bind () =
      if replace
      then Store.replace_classification_rule db rule
      else Store.add_classification_rule db rule
    in
    printf
      "Saved reported-type rule %s. It applies to future imports; run fin tx \
       refresh-reported-types to preview existing changes.\n"
      (Terminal.text name);
    Ok ())
;;

let rule_add_reported_type_cmd =
  command
    "add-reported-type"
    "Map an institution's reported type code, or a named companion-file column, to a \
     movement type."
    Term.(
      const rule_add_reported_type
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Unique rule name.")
      $ Arg.(
          required
          & pos 1 (some string) None
          & info [] ~docv:"INSTITUTION" ~doc:"Institution whose source codes are matched.")
      $ Arg.(
          required
          & pos 2 (some string) None
          & info
              []
              ~docv:"REGEX"
              ~doc:"Expression matched against the reported type code.")
      $ Arg.(
          required
          & pos 3 (some string) None
          & info
              []
              ~docv:"TYPE"
              ~doc:"Movement type, unknown, or other:PREFIX to retain unrecognized codes.")
      $ Arg.(value & flag & info [ "case-sensitive" ] ~doc:"Match case sensitively.")
      $ Arg.(
          value
          & opt int (-1000)
          & info
              [ "priority" ]
              ~docv:"NUMBER"
              ~doc:"Lower numbers run first (default: -1000).")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "field" ]
              ~docv:"COLUMN"
              ~doc:
                "Match this column of the institution's companion file, named as in its \
                 header, instead of the bank's type code.")
      $ Arg.(value & flag & info [ "replace" ] ~doc:"Replace an existing rule."))
;;

let rule_list database json_output =
  with_database database (fun db ->
    let%map rules = Store.classification_rules db in
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
    let%map () = Store.set_classification_rule_mode db name mode in
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
  let eligible, skipped = List.partition_tf matches ~f:(fun m -> m.Store.eligible) in
  printf
    "Rule %s: %s -> %s; %d eligible, %d already reviewed or ineligible.\n"
    (Terminal.text rule.name)
    (Classification_rule.action_name rule.action)
    (Terminal.text (Classification_rule.action_target rule.action))
    (List.length eligible)
    (List.length skipped);
  let show_current m =
    Terminal.review [ m.Store.transaction ];
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

let rule_preview database name json_output =
  with_database database (fun db ->
    let%bind rule = find_rule db name in
    let%bind () =
      match rule.action with
      | Card_reference | Withdrawal_reversal_hint ->
        Or_error.error_string
          "use fin tx refresh-evidence to preview automatic evidence changes"
      | Reported_type _ | Reported_other _ ->
        Or_error.error_string
          "use fin tx refresh-reported-types to preview automatic reported type changes"
      | _ -> Ok ()
    in
    let%map matches = Store.preview_classification_rule db rule in
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
      $ json_output)
;;

let rule_apply database name yes =
  with_database database (fun db ->
    let%bind rule = find_rule db name in
    let%bind () =
      match rule.action with
      | Card_reference | Withdrawal_reversal_hint ->
        Or_error.error_string
          "use fin tx refresh-evidence to preview and apply evidence changes"
      | Reported_type _ | Reported_other _ ->
        Or_error.error_string
          "use fin tx refresh-reported-types to preview and apply reported type changes"
      | _ -> Ok ()
    in
    let%bind matches = Store.preview_classification_rule db rule in
    print_rule_preview rule matches;
    let selected = List.filter matches ~f:(fun m -> m.Store.eligible) in
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
        let%bind changed = Store.apply_classification_rule db ~expected:rule ~selected in
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
      let%bind () = Store.delete_classification_rule db name in
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
    let%map () = Store.rename_classification_rule db ~old_name ~new_name in
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
      (result : Store.type_rule_test)
      limit
  =
  let count f = List.count result.matches ~f:(fun r -> f r.Store.outcome) in
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
    let rows = List.filter result.matches ~f:(fun r -> select r.Store.outcome) in
    if not (List.is_empty rows)
    then (
      printf "\n%s:\n" title;
      List.iter (List.take rows limit) ~f:(fun r ->
        printf
          "  #%Ld  %s  %s -> %s  %s%s\n"
          r.Store.event_id
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
    | Store.Changed -> true
    | _ -> false);
  section "Decided by an earlier rule; the draft changes nothing here" (function
    | Store.Shadowed _ -> true
    | _ -> false);
  section "Newly typed" (function
    | Store.Newly_typed -> true
    | _ -> false);
  section "Manual type override; unchanged" (function
    | Store.Overridden -> true
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
      -> Or_error.error_string "choose a movement type from fin tx type list"
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
      ()
  in
  with_database database (fun db ->
    let%bind observed = Store.observed_provider_movement_types db in
    let%bind () =
      List.filter_opt
        [ Option.bind match_type ~f:(function
            | Is kind -> Some kind
            | Unknown -> None)
        ; Some kind
        ]
      |> List.map ~f:(function
        | Movement_type.Other name when not (List.mem observed name ~equal:String.equal)
          -> Or_error.error_string "unknown movement type; see fin tx type list"
        | _ -> Ok ())
      |> Or_error.combine_errors_unit
    in
    let%map result = Store.test_type_rule db rule in
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
              ~doc:"The movement type the rule assigns; see fin tx type list.")
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
        "       decides  %s  /%s/ -> %s\n"
        (Terminal.text c.name)
        (Terminal.text c.pattern)
        (match kind with
         | Some kind -> Terminal.text (Movement_type.to_string kind)
         | None -> "leaves it untyped")
    | Also_matches ->
      printf
        "       matches too, but an earlier rule decides  %s  /%s/\n"
        (Terminal.text c.name)
        (Terminal.text c.pattern)
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
     | Import_rules -> "import rules"
     | Companion_code -> "the companion file's code"
     | Untyped -> "nothing decides it");
  printf
    "  1. Manual override: %s\n"
    (Option.value_map e.override ~default:"none" ~f:Movement_type.to_string);
  (match e.reported_code with
   | None -> print_endline "  2. Bank code: this source has none"
   | Some code ->
     printf "  2. Bank code %S, reported-type rules:\n" (Terminal.text code);
     print_explain_checks ~all e.code_checks);
  printf
    "  3. First description %S, type rules:\n"
    (Terminal.text (Option.value (List.hd e.descriptions) ~default:""));
  print_explain_checks ~all e.description_checks;
  printf "     import rules give: %s\n" (type_name e.rules_type);
  (match e.companion with
   | [] -> print_endline "  4. Companion file: no matched rows with a code"
   | codes ->
     printf
       "  4. Companion file codes: %s\n"
       (String.concat
          ~sep:", "
          (List.map codes ~f:(fun (code, kind) ->
             Printf.sprintf "%s -> %s" (Terminal.text code) (type_name kind))));
     printf
       "     they agree on: %s (used only when import rules give no type)\n"
       (type_name e.companion_type));
  printf
    "\n\
     Type saved with the movement at import (before overrides and the companion): %s%s\n"
    (type_name e.stored)
    (if Option.equal Poly.equal e.stored e.rules_type
     then ", the same as the import rules now give"
     else
       Printf.sprintf
         "; the import rules now give %s, so fin transaction refresh-types would update \
          it"
         (type_name e.rules_type));
  match e.category with
  | Some (account, reason) ->
    printf "Category: %s (%s)\n" (Terminal.text account) (Terminal.text reason)
  | None ->
    print_endline "Category: none recorded. Category rules: see fin rule preview NAME."
;;

let rule_explain database event_id all =
  with_database database (fun db ->
    let%map explanation = Store.explain_movement db event_id in
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
          & info [] ~docv:"EVENT-ID" ~doc:"Transaction event ID from fin tx list.")
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
     | Import_rules -> "import rules"
     | Companion_code -> "the companion file's code"
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
  print_endline "\nSource fields";
  String.split_lines t.payload
  |> List.iter ~f:(fun line -> printf "  %s\n" (Terminal.text line));
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
         printf "    tag %s: %s\n" (Terminal.text tag) (Terminal.text value));
       Option.iter c.reported ~f:(fun (value, kind) ->
         printf "    type rules matched %s: %s\n" (Terminal.text value) (type_name kind))));
  printf "\nHow its type was decided: fin rule explain %Ld\n" e.event_id
;;

let transaction_show database event_id =
  with_database database (fun db ->
    let%map detail = Store.show_movement db event_id in
    print_transaction_detail detail)
;;

let transaction_show_cmd =
  command
    "show"
    "Show everything stored about one transaction: its source, files, fields and \
     decisions."
    Term.(
      const transaction_show
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"EVENT-ID" ~doc:"Transaction event ID from fin tx list."))
;;

let print_description_gaps gaps =
  if List.is_empty gaps
  then print_endline "Every movement has a type."
  else
    List.iter gaps ~f:(fun (g : Store.description_gap) ->
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
    let%map gaps = Store.description_gaps ?institution db in
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

let category_similar database seed json_output =
  with_database database (fun db ->
    let%bind rows = Store.bank_review db in
    let%bind preview = Category_similarity.find ~seed rows in
    if json_output
    then Report_json.print Report_json.category_similar preview
    else (
      printf
        "Example and proposed category: %s\n"
        (Terminal.text preview.seed.counter_account);
      Terminal.review [ preview.seed ];
      print_endline
        "Matching unclassified transactions (same institution and card merchant or \
         description, ignoring ASCII case and whitespace):";
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
              ~doc:"Reviewed transaction event ID from fin transaction list.")
      $ json_output)
;;

let category_review database event_id =
  if not (Unix.isatty Unix.stdin)
  then Or_error.error_string "interactive category review requires a terminal"
  else
    with_database database (fun db ->
      let%bind rows = Store.bank_review db in
      let%bind seed =
        List.find rows ~f:(fun (r : Store.bank_review) -> Int64.equal r.event_id event_id)
        |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
      in
      if not (String.equal seed.counter_account "equity:unclassified")
      then
        Or_error.error_string
          "transaction is already classified or belongs to a confirmed transfer"
      else (
        print_endline "Transaction to classify:";
        Terminal.review [ seed ];
        let categories =
          List.filter_map rows ~f:(fun (r : Store.bank_review) ->
            Option.some_if
              (String.is_prefix r.counter_account ~prefix:"expenses:"
               || String.is_prefix r.counter_account ~prefix:"income:")
              r.counter_account)
          |> List.dedup_and_sort ~compare:String.compare
        in
        if not (List.is_empty categories)
        then (
          print_endline "Previously used categories:";
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
            if
              ((not (Poly.equal seed.movement_type (Some Movement_type.Card)))
               || Option.value_map seed.display_name ~default:true ~f:(fun name ->
                 String.is_empty (String.strip name)))
              && List.for_all seed.transaction.descriptions ~f:(fun s ->
                String.is_empty (String.strip s))
            then
              Ok Category_similarity.{ seed = example; candidates = []; conflicts = [] }
            else
              Category_similarity.find
                ~seed:event_id
                (example
                 :: List.filter rows ~f:(fun r ->
                   not (Int64.equal r.Store.event_id event_id)))
          in
          if not (List.is_empty matches.conflicts)
          then (
            print_endline "Similar transactions with different reviewed categories:";
            Terminal.review matches.conflicts);
          let candidates = matches.candidates in
          if not (List.is_empty candidates)
          then (
            print_endline
              "Similar unclassified transactions (same institution and normalized card \
               merchant or description):";
            List.iteri candidates ~f:(fun i (r : Store.bank_review) ->
              printf
                "  %d. #%Ld  %s  %s %s  %s\n"
                (i + 1)
                r.event_id
                (Date.to_string r.transaction.booked_on)
                (Quantity.to_string r.transaction.amount)
                (Iso4217.to_alpha_code r.transaction.account.currency)
                (Terminal.text
                   (Option.value
                      r.display_name
                      ~default:(String.concat ~sep:" | " r.transaction.descriptions)))));
          let rec select () =
            if List.is_empty candidates
            then Ok (Some [])
            else (
              match
                prompt "Select matches (all, none, comma-separated numbers, or q): "
              with
              | None -> Ok None
              | Some input ->
                let input = String.strip input in
                if String.Caseless.equal input "q"
                then Ok None
                else if String.Caseless.equal input "all"
                then Ok (Some candidates)
                else if String.Caseless.equal input "none"
                then Ok (Some [])
                else (
                  let selected =
                    String.split input ~on:','
                    |> List.map ~f:(fun s ->
                      Option.try_with (fun () -> Int.of_string (String.strip s))
                      |> Option.bind ~f:(fun n ->
                        if n <= 0 then None else List.nth candidates (n - 1)))
                  in
                  if List.is_empty selected || List.exists selected ~f:Option.is_none
                  then (
                    eprintf "Enter valid, comma-separated match numbers.\n";
                    select ())
                  else (
                    let selected = List.filter_opt selected in
                    if
                      List.contains_dup
                        (List.map selected ~f:(fun r -> r.Store.event_id))
                        ~compare:Int64.compare
                    then (
                      eprintf "Select each match only once.\n";
                      select ())
                    else Ok (Some selected))))
          in
          let%bind selected = select () in
          (match selected with
           | None ->
             print_endline "Nothing classified.";
             Ok ()
           | Some selected ->
             let chosen = seed :: selected in
             printf "Proposed category: %s\n" (Terminal.text category);
             Terminal.review
               (List.map chosen ~f:(fun r ->
                  { r with Store.counter_account = category; reason = None }));
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
                 List.map chosen ~f:(fun (r : Store.bank_review) ->
                   Bank_category.create
                     ~event_id:r.event_id
                     ~account:category
                     ~reason:
                       (if Int64.equal r.event_id event_id
                        then "Reviewed interactively"
                        else Printf.sprintf "Reviewed as similar to event %Ld" event_id))
                 |> Or_error.combine_errors
               in
               let%bind changed =
                 Store.set_bank_categories ~only_unclassified:true db decisions
               in
               printf "category_changes=%d\n" changed;
               Ok ()))))
;;

let category_review_cmd =
  command
    "review"
    "Interactively classify a transaction and selected similar movements."
    Term.(
      const category_review
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info
              []
              ~docv:"EVENT-ID"
              ~doc:"Unclassified transaction ID from fin transaction list."))
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
    let%bind rows = Store.transfer_candidates db in
    let%bind ranked = Transfer_ranking.analyze (Store.registry db) rows in
    let%bind extra =
      if all then Store.transfer_candidates ~include_reviewed:true db else Ok []
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
    "Show possible transfers with evidence and ranking; nothing is confirmed."
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
          let%bind candidates = Store.transfer_candidates db in
          let%bind ranked = Transfer_ranking.analyze (Store.registry db) candidates in
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
        Store.set_bank_transfers ~require_unreviewed:(Option.is_none path) db decisions
      in
      printf "transfer_changes=%d\n" changed))
;;

let review_transfers_cmd =
  command
    "review"
    "Review a candidate interactively, or apply JSON confirm/reject decisions."
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

let transfer_undo database first second yes =
  with_database database (fun db ->
    let%bind decisions = Store.bank_transfer_decisions db in
    let first, second = Int64.min first second, Int64.max first second in
    let%bind previous =
      List.find decisions ~f:(fun (d : Bank_transfer.decision) ->
        Int64.equal d.first_event_id first
        && Int64.equal d.second_event_id second
        && Poly.equal d.state Bank_transfer.Confirmed)
      |> Or_error.of_option
           ~error:(Error.of_string "no confirmed transfer for these event IDs")
    in
    let%bind rows = Store.bank_review db in
    let pair =
      List.filter rows ~f:(fun r ->
        Int64.equal r.Store.event_id first || Int64.equal r.event_id second)
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
          Store.set_bank_transfers ~expected_previous:previous db [ decision ]
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
          & info [] ~docv:"FIRST-EVENT-ID" ~doc:"First transaction event ID.")
      $ Arg.(
          required
          & pos 1 (some int64) None
          & info [] ~docv:"SECOND-EVENT-ID" ~doc:"Second transaction event ID.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Undo without prompting."))
;;

let reversal_candidates database =
  with_database database (fun db ->
    let%map candidates = Store.reversal_candidates db in
    if List.is_empty candidates
    then print_endline "No unreviewed reversal candidates."
    else
      List.iter candidates ~f:(fun (c : Store.reversal_candidate) ->
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
    let%bind rows = Store.bank_review db in
    let find id =
      List.find rows ~f:(fun r -> Int64.equal r.Store.event_id id)
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
      let%map () = Store.reversal_change db ~original ~reversal ~undo in
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
          let%map changes = Store.reversal_history db in
          List.iter changes ~f:(fun (r : Store.reversal_history_entry) ->
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

let accounts_delete database account_id dry_run yes =
  with_database database (fun db ->
    let%bind accounts = Store.bank_account_info db in
    let matches =
      List.filter accounts ~f:(fun (a : Store.bank_account_info) ->
        String.equal (Bank_account_mapping.id a.mapping) account_id)
    in
    let%bind account =
      match matches with
      | [ account ] -> Ok account
      | [] -> Or_error.error_string "no matching account ID; see fin account list"
      | _ -> Or_error.error_string "account ID collision; deletion is refused"
    in
    let%bind preview = Store.delete_account ~dry_run:true db account.mapping in
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
        let%bind _ = Store.delete_account db account.mapping in
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
    let%bind accounts = Store.bank_account_info db in
    let find id =
      match
        List.filter accounts ~f:(fun a ->
          String.equal (Bank_account_mapping.id a.Store.mapping) id)
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
        | "tx" | "txn" -> "transaction"
        | "acc" -> "account"
        | "inst" -> "institution"
        | command -> command);
  argv
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
                    fin transaction list\n\
                    fin transaction review\n\
                    fin journal export > my-finances.journal"
               ; `P
                   "Use $(b,fin paths) to inspect storage locations, or append \
                    $(b,--help) to any subcommand for its options."
               ; `P
                   "Command shortcuts: $(b,tx) and $(b,txn) mean $(b,transaction); \
                    $(b,acc) means $(b,account); $(b,inst) means $(b,institution)."
               ])
          [ setup_cmd
          ; status_cmd
          ; paths_cmd
          ; backup_cmd
          ; restore_cmd
          ; delete_cmd
          ; account_cmd
          ; import_bank_cmd
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
              ; rule_mode_cmd
              ; rule_add_cmd ~settlement:false
              ; rule_add_cmd ~settlement:true
              ; rule_add_evidence_cmd
              ; rule_add_reported_type_cmd
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
              [ init; events; ingest_cmd ]
          ; group
              "statement"
              "Inspect and reconcile imported statements and companion files."
              [ statements_cmd; companions_cmd; match_companions_cmd ]
          ; group
              "transfer"
              "Review transfer candidates and decisions."
              [ transfer_candidates_cmd
              ; review_transfers_cmd
              ; transfer_undo_cmd
              ; transfer_history_cmd
              ]
          ; group
              "transaction"
              "Inspect and classify imported transactions."
              [ transaction_list_cmd
              ; transaction_show_cmd
              ; refresh_transaction_types_cmd
              ; refresh_reported_types_cmd
              ; refresh_transaction_evidence_cmd
              ; group
                  "type"
                  "Inspect and override transaction movement types."
                  [ transaction_type_list_cmd
                  ; transaction_type_show_cmd
                  ; transaction_type_set_cmd
                  ; transaction_type_reset_cmd
                  ; transaction_type_history_cmd
                  ]
              ; bank_review_cmd
              ; group
                  "category"
                  "Manage reviewed transaction categories."
                  [ categorize_bank_cmd
                  ; category_history_cmd
                  ; category_similar_cmd
                  ; category_review_cmd
                  ]
              ]
          ; group
              "journal"
              "Project and render accounting entries."
              [ project_bank_cmd; export_hledger_cmd ]
          ]))
;;
