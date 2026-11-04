open Base
open Cmdliner
open Or_error.Let_syntax
open Cli

let print_records_summary (records : Records.t) =
  Stdio.printf
    "institution=%s accounts=%d statements=%d balances=%d records=%d\n"
    records.institution
    (List.length records.accounts)
    (List.length records.statements)
    (List.length records.balances)
    (List.length records.records)
;;

let check_records path =
  let%bind contents = read_file path in
  let%map documents = Records.of_string ~file:path contents in
  List.iter documents ~f:print_records_summary
;;

let records_check_cmd =
  command
    "check"
    "Validate a records file, as a converter prints it, without importing it."
    Term.(
      const check_records
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE"))
;;

let converters = [ "wise", (Wise_converter.recognizes, Wise_converter.convert) ]

(* The same bound as fin import's default, on a file and on what an archive expands to. *)
let convert_read_limit = 67108864L

(* What one input states. A directory or an archive holds whatever was put in it, so
   a file the converter does not recognize is reported and skipped; a file named on the
   command line was meant to be converted. *)
let rec convert_input ((recognizes, convert) as converter) ~explicit path =
  let tagged path result =
    Result.map_error result ~f:(fun error ->
      Error.of_string (path ^ ": " ^ Error.to_string_hum error))
  in
  let name = Stdlib.Filename.basename path in
  let skip reason =
    Stdio.eprintf "skipped %s: %s\n" path reason;
    Ok []
  in
  let%bind stat =
    tagged
      path
      (Or_error.try_with (fun () ->
         if explicit then Unix.LargeFile.stat path else Unix.LargeFile.lstat path))
  in
  match stat.st_kind with
  | S_DIR ->
    let%bind names =
      tagged path (Or_error.try_with (fun () -> Stdlib.Sys.readdir path |> Array.to_list))
    in
    List.sort names ~compare:String.compare
    |> List.filter ~f:(fun name -> not (Finances_import.Zip_archive.ignored_path name))
    |> List.map ~f:(fun name ->
      convert_input converter ~explicit:false (Stdlib.Filename.concat path name))
    |> Or_error.all
    |> Result.map ~f:List.concat
  | S_REG when (not explicit) && Int64.(stat.st_size > convert_read_limit) ->
    skip "larger than the read limit"
  | S_REG when Finances_import.Zip_archive.is_zip path ->
    let%bind contents = read_file path in
    let%bind members =
      tagged
        path
        (Finances_import.Zip_archive.files ~max_bytes:convert_read_limit contents)
    in
    let known, skipped =
      List.partition_tf members ~f:(fun (_, contents) -> recognizes contents)
    in
    List.iter skipped ~f:(fun (member, _) ->
      Stdio.eprintf "skipped %s/%s: not a recognized statement\n" path member);
    if List.is_empty members then Stdio.eprintf "skipped %s: empty archive\n" path;
    List.map known ~f:(fun (member, contents) ->
      convert ~name:(name ^ "/" ^ member) contents |> tagged (path ^ "/" ^ member))
    |> Or_error.all
  | S_REG ->
    let%bind contents = read_file path in
    if explicit || recognizes contents
    then Result.map (convert ~name contents |> tagged path) ~f:List.return
    else skip "not a recognized statement"
  | _ when explicit -> tagged path (Or_error.error_string "not a file or directory")
  | _ -> skip "not a regular file or directory"
;;

let convert_statements institution paths output =
  let%bind convert =
    match
      List.Assoc.find converters ~equal:String.equal (String.lowercase institution)
    with
    | Some convert -> Ok convert
    | None ->
      Or_error.errorf
        "no converter for %s; fin has one for: %s"
        institution
        (String.concat ~sep:", " (List.map converters ~f:fst))
  in
  let%bind documents =
    List.map paths ~f:(convert_input convert ~explicit:true)
    |> Or_error.all
    |> Result.map ~f:List.concat
  in
  let%bind () =
    if List.is_empty documents
    then Or_error.error_string "no recognized statement in what was given"
    else Ok ()
  in
  let%bind records = Records.merge documents in
  (* A built-in converter passes the same checks as anyone else's output. *)
  let text = Records.to_string records in
  let%bind (_ : Records.t list) = Records.of_string text in
  match output with
  | None ->
    Stdio.Out_channel.output_string Stdio.stdout text;
    Ok ()
  | Some path ->
    let%bind contents =
      if String.is_suffix path ~suffix:".zst" then Records.compress text else Ok text
    in
    let%map () = write_file path contents in
    print_records_summary records
;;

let convert_cmd =
  command
    "convert"
    "Print statement files as records, without importing them."
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Reads statement files, directories and ZIP archives, and prints what they \
           state as one section of a records file, in JSON Lines. Nothing is stored and \
           no database is needed. A Wise CAMT file gives counted records, and a Wise CSV \
           gives observations that describe them. A row stated by several files appears \
           once; if they state it differently, nothing is printed."
      ; `P
          "A directory is walked recursively in name order, leaving out hidden and \
           partial files and symbolic links. A file ending in $(b,.zip) is opened in \
           memory. In either, every statement is converted, and anything the converter \
           does not recognize, such as Wise's MT940 files, is reported on standard error \
           and skipped. A corrupt or encrypted member, a nested archive, a statement \
           that fails to parse, or finding no statement at all stops the command."
      ; `P
          "With $(b,--output), writes the file instead and prints a summary. A name \
           ending in $(b,.zst) compresses it with zstd. Everything that reads records \
           accepts both forms."
      ]
    Term.(
      const convert_statements
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION")
      $ Arg.(non_empty & pos_right 0 string [] & info [] ~docv:"PATH")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "o"; "output" ]
              ~docv:"PATH"
              ~doc:
                "Write to $(docv) instead of standard output; compress if it ends in \
                 .zst."))
;;

let cmd =
  group
    "records"
    "Work with records files printed by statement converters."
    [ records_check_cmd ]
;;
