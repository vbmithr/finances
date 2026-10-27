open Base

let bos_result r = Result.map_error r ~f:(fun (`Msg m) -> Error.of_string m)

(* NAME.YYYYMMDDTHHMMSSZ.old, as left by fin db reset in revisions 12 and 13. *)
let is_archive_of ~name candidate =
  match String.chop_prefix candidate ~prefix:(name ^ ".") with
  | None -> false
  | Some rest ->
    (match String.chop_suffix rest ~suffix:".old" with
     | None -> false
     | Some stamp ->
       String.length stamp = 16
       && Char.equal stamp.[8] 'T'
       && Char.equal stamp.[15] 'Z'
       && String.for_alli stamp ~f:(fun i c -> i = 8 || i = 15 || Char.is_digit c))
;;

let files ~database ~config =
  let open Or_error.Let_syntax in
  let directory = Stdlib.Filename.dirname database in
  let names =
    let database = Stdlib.Filename.basename database in
    [ database
    ; database ^ "-journal"
    ; database ^ "-wal"
    ; database ^ "-shm"
    ; "bank.journal"
    ]
  in
  let%bind present =
    bos_result (Bos.OS.Dir.exists (Fpath.v directory))
    |> Or_error.bind ~f:(function
      | false -> Ok []
      | true ->
        let%map entries =
          bos_result (Bos.OS.Dir.contents ~rel:true (Fpath.v directory))
        in
        List.map entries ~f:Fpath.to_string
        |> List.filter ~f:(fun entry ->
          List.mem names entry ~equal:String.equal
          || List.exists
               [ Stdlib.Filename.basename database; "bank.journal" ]
               ~f:(fun name -> is_archive_of ~name entry))
        |> List.sort ~compare:String.compare
        |> List.map ~f:(Stdlib.Filename.concat directory))
  in
  let%map config =
    match config with
    | None -> Ok []
    | Some path ->
      let%map exists = bos_result (Bos.OS.File.exists (Fpath.v path)) in
      if exists then [ path ] else []
  in
  present @ config
;;

let delete files =
  let open Or_error.Let_syntax in
  let%map () =
    List.fold_result files ~init:() ~f:(fun () path ->
      bos_result (Bos.OS.File.delete ~must_exist:false (Fpath.v path)))
  in
  (* rmdir only succeeds on an empty directory, which is what we want. *)
  List.map files ~f:Stdlib.Filename.dirname
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:(fun directory ->
    try Unix.rmdir directory with
    | Unix.Unix_error _ -> ())
;;
