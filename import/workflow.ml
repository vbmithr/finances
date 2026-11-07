open Base
open Stdio
open Finances_store

type report =
  { inputs : Files.report
  ; projection : Ledger.projection_report
  ; companions : Ledger.companion_report
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

let run ~registry ?account_id ?max_file_bytes ~database ~institution ~paths ~journal () =
  let open Or_error.Let_syntax in
  let%bind database, paths, journal =
    Or_error.try_with (fun () ->
      if List.is_empty paths then failwith "no file or directory to import";
      let roots =
        List.map paths ~f:(fun path ->
          match (Unix.LargeFile.lstat path).st_kind with
          | S_DIR -> `Directory (canonical path)
          | S_REG -> `File (canonical path)
          | _ ->
            Printf.failwithf "%s must be a file or directory, not a symbolic link" path ())
      in
      let database = canonical database
      and journal = canonical journal in
      if
        List.exists [ database; journal ] ~f:(fun control ->
          List.exists roots ~f:(function
            | `Directory directory -> within ~directory control
            | `File file -> same_file file control))
      then
        failwith
          "database and journal must not be import inputs or lie inside an imported \
           directory";
      if same_file database journal
      then failwith "database and journal must be distinct files";
      (match Or_error.try_with (fun () -> Unix.LargeFile.stat journal) with
       | Ok stat when Poly.(stat.st_kind <> Unix.S_REG) ->
         failwith "journal destination must be a regular file"
       | _ -> ());
      database, paths, journal)
  in
  Setup.with_open ~registry database (fun db ->
    let%bind () =
      match account_id with
      | None -> Ok ()
      | Some _ ->
        (match Registry.find registry institution with
         | Some (module A : Adapter.S) when A.takes_account_label -> Ok ()
         | _ ->
           Or_error.errorf
             "--account-id is only accepted for banks whose exports do not identify \
              their account (%s)"
             (List.filter (Registry.institutions registry) ~f:(fun i ->
                match Registry.find registry i with
                | Some (module A : Adapter.S) -> A.takes_account_label
                | None -> false)
              |> String.concat ~sep:", "))
    in
    let%bind inputs =
      Files.scan
        ?account_id
        ?max_file_bytes
        ~exclude:[ database; journal ]
        db
        ~institution
        ~paths
    in
    let%bind companions = Ledger.match_companions db in
    let%bind projection = Ledger.project_bank db in
    let%bind contents = Finances_hledger.export db in
    let%map () = write_journal ~path:journal contents in
    { inputs; projection; companions; journal })
;;
