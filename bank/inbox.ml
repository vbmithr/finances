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

let scan
      ?account_id
      ?(max_file_bytes = 67108864L)
      ?(exclude = [])
      db
      ~institution
      ~directory
  =
  let open Or_error.Let_syntax in
  let%bind () =
    Or_error.try_with (fun () ->
      if Int64.(max_file_bytes <= 0L || max_file_bytes >= of_int Int.max_value)
      then failwith "file read limit must be positive and fit in memory";
      if Poly.((Unix.LargeFile.lstat directory).st_kind <> Unix.S_DIR)
      then failwith "inbox must be a directory, not a symbolic link")
  in
  let excluded =
    List.filter_map exclude ~f:(fun path ->
      match Or_error.try_with (fun () -> Unix.LargeFile.stat path) with
      | Error _ -> None
      | Ok stat -> Some (stat.st_dev, stat.st_ino))
  in
  let files = ref []
  and ignored = ref [] in
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
  let rec walk path =
    match Or_error.try_with (fun () -> Unix.LargeFile.lstat path) with
    | Error error -> record_error path error
    | Ok stat ->
      let name = Stdlib.Filename.basename path in
      if
        List.exists excluded ~f:(fun (device, inode) ->
          device = stat.st_dev && inode = stat.st_ino)
      then (
        ignored := { path; reason = "control file alias" } :: !ignored;
        Ok ())
      else if
        String.is_prefix name ~prefix:"."
        || List.exists [ ".part"; ".tmp"; "~" ] ~f:(fun suffix ->
          String.is_suffix name ~suffix)
      then (
        ignored := { path; reason = "hidden or partial-file path" } :: !ignored;
        Ok ())
      else (
        match stat.st_kind with
        | S_DIR ->
          (match
             Or_error.try_with (fun () ->
               Stdlib.Sys.readdir path
               |> Array.to_list
               |> List.sort ~compare:String.compare)
           with
           | Error error -> record_error path error
           | Ok names ->
             let rec visit = function
               | [] -> Ok ()
               | name :: rest ->
                 let%bind () = walk (Stdlib.Filename.concat path name) in
                 visit rest
             in
             visit names)
        | S_REG ->
          (match snapshot ~max_file_bytes path with
           | Error error -> record_error path error
           | Ok contents ->
             let%map report =
               Store.import_bank_file ?account_id db ~institution ~path ~contents
             in
             files := report :: !files)
        | S_LNK ->
          ignored := { path; reason = "symbolic link" } :: !ignored;
          Ok ()
        | _ ->
          ignored := { path; reason = "not a regular file or directory" } :: !ignored;
          Ok ())
  in
  (* Do not apply the hidden-name filter to the explicitly selected root. *)
  let%bind names =
    Or_error.try_with (fun () ->
      Stdlib.Sys.readdir directory |> Array.to_list |> List.sort ~compare:String.compare)
  in
  let rec visit = function
    | [] -> Ok ()
    | name :: rest ->
      let%bind () = walk (Stdlib.Filename.concat directory name) in
      visit rest
  in
  let%map () = visit names in
  { files = List.rev !files; ignored = List.rev !ignored }
;;
