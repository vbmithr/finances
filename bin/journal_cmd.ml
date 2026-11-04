open Base
open Stdio
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

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

let ledger_cmd : unit Cmd.t =
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

let cmd =
  group
    "journal"
    "Project and render accounting entries."
    [ project_bank_cmd; export_hledger_cmd ]
;;
