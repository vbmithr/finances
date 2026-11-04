open Base

let is_zip path = String.is_suffix (String.lowercase path) ~suffix:".zip"

let decode ~max_bytes contents =
  let open Or_error.Let_syntax in
  let%bind archive =
    Or_error.try_with_join (fun () ->
      Zipc.of_binary_string contents |> Result.map_error ~f:Error.of_string)
  in
  if Zipc.member_count archive > 1024
  then Or_error.error_string "ZIP archive exceeds the 1024-member limit"
  else (
    let members = Zipc.fold (fun member acc -> member :: acc) archive [] |> List.rev in
    let%map _ =
      List.fold_result members ~init:0L ~f:(fun total member ->
        match Zipc.Member.kind member with
        | Dir -> Ok total
        | File file ->
          let size = Int64.of_int (Zipc.File.decompressed_size file) in
          if
            Poly.equal (Zipc.File.compression file) Stored
            && Zipc.File.compressed_size file <> Zipc.File.decompressed_size file
          then Or_error.error_string "ZIP stored member sizes disagree"
          else if Int64.(size < 0L || size > max_bytes - total)
          then Or_error.error_string "ZIP expanded contents exceed configured read limit"
          else Ok Int64.(total + size))
    in
    members)
;;

(* Do not sanitize a bad name into another member's name. Although nothing is
   written to disk, keep unambiguous provenance and ordinary relative paths. *)
let valid_path path =
  let parts = String.split path ~on:'/' in
  (not (String.is_empty path))
  && (not (String.is_prefix path ~prefix:"/"))
  && (not
        (String.exists path ~f:(fun c ->
           Char.to_int c < 32
           || Char.equal c '\127'
           || Char.equal c '\\'
           || Char.equal c ':')))
  && not
       (List.exists parts ~f:(fun part -> String.equal part ".." || String.equal part "."))
;;

let ignored_path path =
  String.split path ~on:'/'
  |> List.exists ~f:(fun name ->
    String.is_prefix name ~prefix:"."
    || List.exists [ ".part"; ".tmp"; "~" ] ~f:(fun suffix ->
      String.is_suffix name ~suffix))
;;

let contents file =
  Or_error.try_with_join (fun () ->
    let open Or_error.Let_syntax in
    let%bind bytes =
      Zipc.File.to_binary_string file |> Result.map_error ~f:Error.of_string
    in
    if String.length bytes = Zipc.File.decompressed_size file
    then Ok bytes
    else Or_error.error_string "ZIP member expanded size differs from metadata")
;;

let files ~max_bytes archive =
  let open Or_error.Let_syntax in
  let%bind members = decode ~max_bytes archive in
  List.filter_map members ~f:(fun member ->
    let name = Zipc.Member.path member in
    let failure message = Some (Or_error.errorf "%S: %s" name message) in
    if not (valid_path name)
    then failure "unsafe ZIP member path"
    else if ignored_path name
    then None
    else (
      match Zipc.Member.kind member with
      | Dir -> None
      | File file ->
        if not (Zipc.File.can_extract file)
        then failure "encrypted ZIP member or unsupported compression"
        else if is_zip name
        then failure "nested ZIP archives are not supported"
        else
          Some
            (contents file
             |> Result.map ~f:(fun contents -> name, contents)
             |> Result.map_error ~f:(fun error ->
               Error.tag error ~tag:(Printf.sprintf "%S" name)))))
  |> Or_error.all
;;
