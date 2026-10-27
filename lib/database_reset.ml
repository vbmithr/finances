open Base

type moved =
  { from : string
  ; to_ : string
  }

let bos_result r = Result.map_error r ~f:(fun (`Msg m) -> Error.of_string m)

let suffix now =
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time now in
  Printf.sprintf ".%04d%02d%02dT%02d%02d%02dZ.old" y m d hh mm ss
;;

(* The database last, so that a failure leaves it in place and the command can be
   run again. *)
let candidates database =
  let journal = Stdlib.Filename.(concat (dirname database) "bank.journal") in
  [ journal; database ^ "-journal"; database ^ "-wal"; database ^ "-shm"; database ]
;;

let archive ~now ~database =
  let open Or_error.Let_syntax in
  let suffix = suffix now in
  let%bind present =
    List.filter_map (candidates database) ~f:(fun path ->
      match bos_result (Bos.OS.File.exists (Fpath.v path)) with
      | Ok false -> None
      | Ok true -> Some (Ok path)
      | Error _ as e -> Some e)
    |> Or_error.combine_errors
  in
  let%bind () =
    List.map present ~f:(fun path ->
      let%bind exists = bos_result (Bos.OS.Path.exists (Fpath.v (path ^ suffix))) in
      if exists then Or_error.errorf "%s already exists" (path ^ suffix) else return ())
    |> Or_error.combine_errors_unit
  in
  let%map moved =
    List.fold_result present ~init:[] ~f:(fun moved from ->
      let to_ = from ^ suffix in
      let%map () = bos_result (Bos.OS.Path.move (Fpath.v from) (Fpath.v to_)) in
      { from; to_ } :: moved)
  in
  List.rev moved
;;
