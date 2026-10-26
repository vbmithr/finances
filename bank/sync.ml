open Core
open Finances

type report =
  { inbox : Inbox.report
  ; mapping_changes : int
  ; projection : Store.projection_report
  ; csv : Store.csv_report
  ; bunq_csv : Finances.Store.csv_report
  ; wise_csv : Store.csv_report
  ; journal : string
  }

let canonical path =
  if Stdlib.Sys.file_exists path
  then Filename_unix.realpath path
  else
    Filename.concat
      (Filename_unix.realpath (Filename.dirname path))
      (Filename.basename path)
;;

let within ~directory path =
  String.equal directory path
  || String.is_prefix
       path
       ~prefix:(if String.equal directory "/" then "/" else directory ^ "/")
;;

let same_file a b =
  String.equal a b
  ||
  match Or_error.try_with (fun () -> Core_unix.stat a, Core_unix.stat b) with
  | Error _ -> false
  | Ok (a, b) -> a.st_dev = b.st_dev && a.st_ino = b.st_ino
;;

let write_journal ~path contents =
  Or_error.try_with (fun () ->
    let staging = Option.value (Sys.getenv "TMPDIR") ~default:(Filename.dirname path) in
    let temporary, output =
      Filename_unix.open_temp_file
        ~in_dir:staging
        ~perm:0o600
        "finances-journal"
        ".journal"
    in
    Exn.protect
      ~finally:(fun () ->
        (try Out_channel.close output with
         | _ -> ());
        if Stdlib.Sys.file_exists temporary then Core_unix.unlink temporary)
      ~f:(fun () ->
        Out_channel.output_string output contents;
        Out_channel.close output;
        Core_unix.rename ~src:temporary ~dst:path))
  |> fun result ->
  Or_error.tag
    result
    ~tag:"atomic journal publication (TMPDIR and journal must share a filesystem)"
;;

let run ?accounts ?max_file_bytes ~database ~institution ~directory ~journal () =
  let open Or_error.Let_syntax in
  let%bind database, directory, journal, accounts =
    Or_error.try_with (fun () ->
      if Poly.((Core_unix.lstat directory).st_kind <> Core_unix.S_DIR)
      then failwith "inbox must be a directory, not a symbolic link";
      let directory = Filename_unix.realpath directory in
      let database = canonical database
      and journal = canonical journal in
      let accounts = Option.map accounts ~f:canonical in
      if
        List.exists
          (database :: journal :: Option.to_list accounts)
          ~f:(within ~directory)
      then
        failwith "database, journal and account configuration must be outside the inbox";
      if
        same_file database journal
        || Option.exists accounts ~f:(fun config ->
          same_file journal config || same_file database config)
      then failwith "database, journal and account configuration must be distinct files";
      (match Or_error.try_with (fun () -> Core_unix.stat journal) with
       | Ok stat when Poly.(stat.st_kind <> Core_unix.S_REG) ->
         failwith "journal destination must be a regular file"
       | _ -> ());
      database, directory, journal, accounts)
  in
  let%bind mappings =
    match accounts with
    | None -> Ok []
    | Some path ->
      let%bind contents = Or_error.try_with (fun () -> In_channel.read_all path) in
      Bank_account_mapping.of_json contents
  in
  Store.with_open database (fun db ->
    let%bind inbox =
      Inbox.scan
        ?max_file_bytes
        ~exclude:(database :: journal :: Option.to_list accounts)
        db
        ~institution
        ~directory
    in
    let%bind mapping_changes = Store.set_bank_account_mappings db mappings in
    let%bind csv = Store.match_bank_csv db in
    let%bind wise_csv = Store.match_wise_csv db in
    let%bind bunq_csv = Store.match_bunq_csv db in
    let%bind projection = Store.project_bank db in
    let%bind contents = Finances_hledger.export db in
    let%map () = write_journal ~path:journal contents in
    { inbox; mapping_changes; projection; csv; wise_csv; bunq_csv; journal })
;;
