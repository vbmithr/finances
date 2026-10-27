open Base
open Stdio
open Finances

type ignored =
  { path : string
  ; reason : string
  }

type report =
  { files : Bank_import.file_report list
  ; ignored : ignored list
  }

let snapshot ~max_file_bytes path =
  Or_error.try_with (fun () ->
    let before = Unix.LargeFile.lstat path in
    if Poly.(before.st_kind <> Unix.S_REG) then failwith "not a regular file";
    if Int64.(before.st_size > max_file_bytes)
    then failwith "file exceeds configured read limit";
    let fd = Unix.openfile path [ O_RDONLY; O_NONBLOCK; O_CLOEXEC ] 0 in
    let input = Unix.in_channel_of_descr fd in
    Exn.protect
      ~finally:(fun () -> In_channel.close input)
      ~f:(fun () ->
        let opened = Unix.LargeFile.fstat fd in
        if
          Poly.(opened.st_kind <> Unix.S_REG)
          || before.st_dev <> opened.st_dev
          || before.st_ino <> opened.st_ino
        then failwith "file changed before reading; retry after copying completes";
        let buffer = Buffer.create 4096 in
        ignore
          (In_channel.input_buffer
             input
             buffer
             ~len:(Int64.to_int_exn before.st_size + 1));
        let after = Unix.LargeFile.fstat fd in
        let final = Unix.LargeFile.lstat path in
        let unchanged (a : Unix.LargeFile.stats) (b : Unix.LargeFile.stats) =
          a.st_dev = b.st_dev
          && a.st_ino = b.st_ino
          && Int64.equal a.st_size b.st_size
          && Float.equal a.st_mtime b.st_mtime
          && Float.equal a.st_ctime b.st_ctime
        in
        if
          Poly.(final.st_kind <> Unix.S_REG)
          || (not (unchanged before after && unchanged after final))
          || Int64.(of_int (Buffer.length buffer) <> before.st_size)
        then failwith "file changed during reading; retry after copying completes";
        Buffer.contents buffer))
;;

let scan ?account_id ?(max_file_bytes = 67108864L) ?(exclude = []) db ~institution ~paths =
  let open Or_error.Let_syntax in
  let%bind () =
    Or_error.try_with (fun () ->
      if Int64.(max_file_bytes <= 0L || max_file_bytes >= of_int Int.max_value)
      then failwith "file read limit must be positive and fit in memory";
      if List.is_empty paths then failwith "no file or directory to sync";
      List.iter paths ~f:(fun path ->
        match (Unix.LargeFile.lstat path).st_kind with
        | S_DIR | S_REG -> ()
        | _ ->
          Printf.failwithf "%s must be a file or directory, not a symbolic link" path ()))
  in
  let excluded =
    List.filter_map exclude ~f:(fun path ->
      match Or_error.try_with (fun () -> Unix.LargeFile.stat path) with
      | Error _ -> None
      | Ok stat -> Some (stat.st_dev, stat.st_ino))
  in
  let seen = Hash_set.Poly.create () in
  let files = ref []
  and ignored = ref [] in
  let skip path reason =
    ignored := { path; reason } :: !ignored;
    Ok ()
  in
  let record_error path error =
    let%map report =
      Store.record_bank_read_error
        db
        ~institution
        ~path
        ~message:(Error.to_string_hum error)
    in
    files := report :: !files
  in
  let import path =
    match snapshot ~max_file_bytes path with
    | Error error -> record_error path error
    | Ok contents ->
      let%map report =
        Store.import_bank_file ?account_label:account_id db ~institution ~path ~contents
      in
      files := report :: !files
  in
  let rec visit = function
    | [] -> Ok ()
    | path :: rest ->
      let%bind () = walk ~explicit:false path in
      visit rest
  (* Paths given explicitly are always processed: the hidden and partial-file name
     filters only apply to what a directory walk finds. *)
  and walk ~explicit path =
    match Or_error.try_with (fun () -> Unix.LargeFile.lstat path) with
    | Error error -> record_error path error
    | Ok stat ->
      let name = Stdlib.Filename.basename path in
      let identity = stat.st_dev, stat.st_ino in
      if List.exists excluded ~f:(Poly.equal identity)
      then skip path "control file alias"
      else if
        (not explicit)
        && (String.is_prefix name ~prefix:"."
            || List.exists [ ".part"; ".tmp"; "~" ] ~f:(fun suffix ->
              String.is_suffix name ~suffix))
      then skip path "hidden or partial-file path"
      else if Hash_set.mem seen identity
      then skip path "already listed"
      else (
        Hash_set.add seen identity;
        match stat.st_kind with
        | S_DIR ->
          (match
             Or_error.try_with (fun () ->
               Stdlib.Sys.readdir path
               |> Array.to_list
               |> List.sort ~compare:String.compare)
           with
           | Error error -> record_error path error
           | Ok names -> visit (List.map names ~f:(Stdlib.Filename.concat path)))
        | S_REG -> import path
        | S_LNK -> skip path "symbolic link"
        | _ -> skip path "not a regular file or directory")
  in
  let%map () =
    List.fold_result paths ~init:() ~f:(fun () path -> walk ~explicit:true path)
  in
  { files = List.rev !files; ignored = List.rev !ignored }
;;
