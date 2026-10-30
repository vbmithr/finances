open Base

type action =
  | Category of string
  | Settlement of string
  | Type of Movement_type.t
  | Type_and_category of Movement_type.t * string

type type_condition =
  | Unknown
  | Is of Movement_type.t

let condition_name = function
  | Unknown -> "unknown"
  | Is kind -> Movement_type.to_string kind
;;

let condition_of_string = function
  | "unknown" -> Unknown
  | value -> Is (Movement_type.of_string value)
;;

type t =
  { name : string
  ; pattern : string option
  ; institution : string option
  ; match_type : type_condition option
  ; case_sensitive : bool
  ; action : action
  }

let action_name = function
  | Category _ -> "category"
  | Settlement _ -> "settlement"
  | Type _ -> "type"
  | Type_and_category _ -> "type-category"
;;

let action_target = function
  | Category value | Settlement value | Type_and_category (_, value) -> value
  | Type value -> Movement_type.to_string value
;;

let action_type = function
  | Type value | Type_and_category (value, _) -> Some value
  | Category _ | Settlement _ -> None
;;

let action_of_fields kind value type_value =
  match kind, type_value with
  | "category", None -> Ok (Category value)
  | "settlement", None -> Ok (Settlement value)
  | "type", None -> Ok (Type (Movement_type.of_string value))
  | "type-category", Some name ->
    Ok (Type_and_category (Movement_type.of_string name, value))
  | _ -> Or_error.error_string "invalid rule action"
;;

let create ~name ~pattern ~institution ~match_type ~case_sensitive ~action =
  let open Or_error.Let_syntax in
  let%bind name = Validation.token "rule name" name in
  let%bind () =
    match pattern with
    | None when Option.is_none match_type && Option.is_none institution ->
      Or_error.error_string "rule requires an institution, description or type condition"
    | None -> Ok ()
    | Some pattern ->
      let%map _ = Transaction_pattern.compile ~case_sensitive pattern in
      ()
  in
  let%bind institution =
    match institution with
    | None -> Ok None
    | Some value ->
      let%map value = Validation.token "institution" value in
      Some value
  in
  let validate_type = function
    | Movement_type.Other text
      when String.is_empty (String.strip text) || String.equal text "unknown" ->
      Or_error.error_string "invalid movement type"
    | _ -> Ok ()
  in
  let%bind () =
    Option.value_map match_type ~default:(Ok ()) ~f:(function
      | Unknown -> Ok ()
      | Is kind -> validate_type kind)
  in
  let%bind () = Option.value_map (action_type action) ~default:(Ok ()) ~f:validate_type in
  let%bind () =
    match action with
    | Category account | Type_and_category (_, account) ->
      let%bind _ = Bank_category.create ~event_id:1L ~account ~reason:"rule" in
      if String.equal account "equity:unclassified"
      then Or_error.error_string "rule category must be an expense or income account"
      else Ok ()
    | Settlement card_id ->
      let%map _ = Validation.token "card ID" card_id in
      ()
    | Type _ -> Ok ()
  in
  Ok { name; pattern; institution; match_type; case_sensitive; action }
;;
