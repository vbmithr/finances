open Base
open Stdio
open Finances

type report =
  { inbox : Inbox.report
  ; mapping_changes : int
  ; identity_changes : int
  ; settlement_changes : int
  ; projection : Store.projection_report
  ; csv : Store.csv_report
  ; bunq_csv : Finances.Store.csv_report
  ; wise_csv : Store.csv_report
  ; journal : string
  }

let canonical path =
  let realpath path =
    match Bos.OS.Path.realpath (Fpath.v path) with
    | Ok path -> Fpath.to_string path
    | Error (`Msg message) -> failwith message
  in
  if Stdlib.Sys.file_exists path
  then realpath path
  else
    Stdlib.Filename.concat
      (realpath (Stdlib.Filename.dirname path))
      (Stdlib.Filename.basename path)
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
  match Or_error.try_with (fun () -> Unix.LargeFile.stat a, Unix.LargeFile.stat b) with
  | Error _ -> false
  | Ok (a, b) -> a.st_dev = b.st_dev && a.st_ino = b.st_ino
;;

let write_journal ~path contents =
  let bos_result result =
    Result.map_error result ~f:(fun (`Msg message) -> Error.of_string message)
  in
  let open Or_error.Let_syntax in
  (let%bind staging, destination =
     Or_error.try_with (fun () ->
       ( Fpath.v
           (Option.value (Sys.getenv "TMPDIR") ~default:(Stdlib.Filename.dirname path))
       , Fpath.v path ))
   in
   let%bind published =
     bos_result
       (Bos.OS.File.with_tmp_oc
          ~mode:0o600
          ~dir:staging
          "finances-journal-%s.journal"
          (fun temporary output () ->
             let%bind () =
               Or_error.try_with (fun () ->
                 Out_channel.output_string output contents;
                 Out_channel.flush output)
             in
             bos_result (Bos.OS.Path.move ~force:true temporary destination))
          ())
   in
   published)
  |> fun result ->
  Or_error.tag
    result
    ~tag:"atomic journal publication (TMPDIR and journal must share a filesystem)"
;;

let run
      ?account_id
      ?accounts
      ?max_file_bytes
      ~database
      ~institution
      ~directory
      ~journal
      ()
  =
  let open Or_error.Let_syntax in
  let%bind database, directory, journal, accounts =
    Or_error.try_with (fun () ->
      if Poly.((Unix.LargeFile.lstat directory).st_kind <> Unix.S_DIR)
      then failwith "inbox must be a directory, not a symbolic link";
      let directory = canonical directory in
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
      (match Or_error.try_with (fun () -> Unix.LargeFile.stat journal) with
       | Ok stat when Poly.(stat.st_kind <> Unix.S_REG) ->
         failwith "journal destination must be a regular file"
       | _ -> ());
      database, directory, journal, accounts)
  in
  let%bind account_config =
    match accounts with
    | None -> Ok []
    | Some path ->
      let%bind contents = Or_error.try_with (fun () -> In_channel.read_all path) in
      Bank_account_config.of_json contents
  in
  Store.with_open database (fun db ->
    let%bind () =
      match account_id with
      | None -> Ok ()
      | Some _ when String.equal institution "revolut" -> Ok ()
      | Some _ -> Or_error.error_string "--account-id is only supported for Revolut"
    in
    let%bind inbox =
      Inbox.scan
        ?account_id
        ?max_file_bytes
        ~exclude:(database :: journal :: Option.to_list accounts)
        db
        ~institution
        ~directory
    in
    let%bind account_changes = Store.apply_bank_accounts db account_config in
    let mapping_changes = account_changes.mapping_changes
    and identity_changes = account_changes.identity_changes
    and settlement_changes = account_changes.settlement_changes in
    let%bind csv = Store.match_bank_csv db in
    let%bind wise_csv = Store.match_wise_csv db in
    let%bind bunq_csv = Store.match_bunq_csv db in
    let%bind projection = Store.project_bank db in
    let%bind contents = Finances_hledger.export db in
    let%map () = write_journal ~path:journal contents in
    { inbox
    ; mapping_changes
    ; identity_changes
    ; settlement_changes
    ; projection
    ; csv
    ; wise_csv
    ; bunq_csv
    ; journal
    })
;;
