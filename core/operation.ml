open Base

type evidence =
  | Stored_event of int64
  | File_row of
      { document : string
      ; row : int
      }
[@@deriving compare, equal]

type movement =
  { identity : Movement_identity.t option
  ; account : string option
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  ; evidence : evidence list
  }

type t =
  { occurred : Temporal.t
  ; evidence : evidence list
  ; movements : movement list
  }

let validate_evidence evidence =
  if List.is_empty evidence
  then Or_error.error_string "at least one source reference is required"
  else if List.contains_dup evidence ~compare:compare_evidence
  then Or_error.error_string "duplicate source reference"
  else
    List.map evidence ~f:(function
      | Stored_event id when Int64.(id > 0L) -> Ok ()
      | File_row { document; row } when row > 0 ->
        Result.map (Validation.text "source document" document) ~f:ignore
      | _ -> Or_error.error_string "invalid source reference")
    |> Or_error.combine_errors_unit
;;

let movement ?identity ?account ~commodity ~quantity ~evidence () =
  let open Or_error.Let_syntax in
  let%bind () = validate_evidence evidence in
  let%bind account =
    Option.map account ~f:Validation.account
    |> Option.value_map ~default:(Ok None) ~f:(Result.map ~f:Option.some)
  in
  let%bind quantity = Or_error.try_with (fun () -> Quantity.finite_exn quantity) in
  Ok { identity; account; commodity; quantity; evidence }
;;

let create ~occurred ~evidence ~movements =
  let open Or_error.Let_syntax in
  let%bind () = validate_evidence evidence in
  let%bind () =
    match occurred with
    | Temporal.Date date -> Or_error.try_with (fun () -> Date.validate_exn date)
    | Temporal.Instant _ -> Ok ()
  in
  let identities =
    List.filter_map movements ~f:(fun (movement : movement) -> movement.identity)
  in
  if List.contains_dup identities ~compare:Movement_identity.compare
  then Or_error.error_string "duplicate stable movement identity"
  else if
    List.exists movements ~f:(fun (movement : movement) ->
      List.exists movement.evidence ~f:(fun source ->
        not (List.mem evidence source ~equal:equal_evidence)))
  then Or_error.error_string "movement refers to evidence outside its operation"
  else Ok { occurred; evidence; movements }
;;
