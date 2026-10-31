open Base

type rule =
  { description : string
  ; account_type : string option
  ; case_sensitive : bool
  ; kind : Movement_type.t
  ; match_type : string option
  ; compiled : Re.re
  ; field : string option
  }

type t =
  { institution : string
  ; rules : rule list
  }

let rules t = t.rules
let with_match_type r match_type = { r with match_type }
let create ~institution rules = { institution; rules }
let institution t = t.institution

let empty institution =
  Or_error.map (Validation.token "institution" institution) ~f:(fun institution ->
    { institution; rules = [] })
;;

let make_rule ?field description account_type case_sensitive kind =
  let open Or_error.Let_syntax in
  let%bind _ = Validation.text "description expression" description in
  let%bind () =
    match field with
    | None -> Ok ()
    | Some name ->
      let%map _ = Validation.text "field" name in
      ()
  in
  let%bind () =
    match account_type with
    | None -> Ok ()
    | Some value ->
      let%map _ = Validation.text "account type" value in
      ()
  in
  let%bind _ = Validation.text "movement type" kind in
  let%bind () =
    if String.equal kind "unknown"
    then Or_error.error_string "unknown is not a movement type"
    else Ok ()
  in
  let%bind compiled =
    Or_error.try_with (fun () ->
      Re.Perl.compile_pat ~opts:(if case_sensitive then [] else [ `Caseless ]) description)
    |> Or_error.tag ~tag:"invalid source type expression"
  in
  Ok
    { description
    ; account_type
    ; case_sensitive
    ; kind = Movement_type.of_string kind
    ; match_type = None
    ; compiled
    ; field
    }
;;

(* A rule tests the main description unless it names a field; it matches when any
   value under that name does, and the value it matched is returned. *)
let matched_value (r : rule) (movement : Movement.t) =
  if
    Option.value_map
      r.account_type
      ~default:true
      ~f:(String.equal movement.account.account_type)
    && Option.value_map r.match_type ~default:true ~f:(fun expected ->
      match expected, movement.movement_type with
      | "unknown", None -> true
      | "unknown", Some _ -> false
      | _, Some kind -> String.equal expected (Movement_type.to_string kind)
      | _, None -> false)
  then
    List.find
      (Movement.field_values movement (Option.value r.field ~default:"description"))
      ~f:(Re.execp r.compiled)
  else None
;;

let matches r movement = Option.is_some (matched_value r movement)

(* The first rule that matches decides, with the value it matched. *)
let decide t movement =
  List.find_map t.rules ~f:(fun r ->
    Option.map (matched_value r movement) ~f:(fun value -> r, value))
;;

let apply t movement =
  match decide t movement with
  | None -> movement
  | Some (r, value) ->
    Movement.with_movement_type movement (Some (Movement_type.expand r.kind ~value))
;;
