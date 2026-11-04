open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

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
      confirm ~yes ~destructive:true "Restore?")
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
    let%bind confirmed = confirm ~yes ~destructive:true "Delete these files?" in
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
     \032 fin convert wise PATH…  Print statements as records\n\
     \032 fin records check FILE  Validate a statement converter's output\n\
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
