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

let prompt text =
  Out_channel.flush stdout;
  Out_channel.output_string stderr text;
  Out_channel.flush stderr;
  In_channel.input_line In_channel.stdin
;;

(* The one way fin asks before changing anything, after showing what would change.
   [--yes] skips the question; without a terminal it is required. The default
   answer is yes ([Y/n]), except for [~destructive] actions that delete data or
   replace state, which default to no ([y/N]). *)
let confirm ~yes ~destructive question =
  if yes
  then Ok true
  else if Unix.isatty Unix.stdin
  then (
    let suffix = if destructive then "[y/N]" else "[Y/n]" in
    Ok
      (Option.value_map
         (prompt (Printf.sprintf "%s %s: " question suffix))
         ~default:false
         ~f:(if destructive then confirm_default_no else confirm_default_yes)))
  else
    Or_error.errorf
      "cannot ask \"%s\" without a terminal; pass --yes to confirm it"
      question
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
    let%bind definition =
      Json.decode Institution_definition.codec json
      |> Result.map_error ~f:(fun error ->
        Error.of_string
          (Printf.sprintf
             "institution %s: its saved definition cannot be read; older kinds are no \
              longer accepted, see CHANGES.md: %s"
             name
             (Error.to_string_hum error)))
    in
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

let group name doc commands = Cmd.group (Cmd.info name ~doc) commands
