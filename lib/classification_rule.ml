open Base

type action =
  | Category of string
  | Settlement of string
  | Type of Movement_type.t
  | Type_and_category of Movement_type.t * string
  | Card_reference
  | Withdrawal_reversal_hint
  | Reported_type of Movement_type.t option
  | Reported_other of string

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

type mode =
  | Automatic
  | Review
  | Disabled

type t =
  { mode : mode
  ; account_type : string option
  ; priority : int
  ; name : string
  ; pattern : string option
  ; institution : string option
  ; match_type : type_condition option
  ; case_sensitive : bool
  ; action : action
  ; field : string option
  }

let action_name = function
  | Category _ -> "category"
  | Settlement _ -> "settlement"
  | Type _ -> "type"
  | Type_and_category _ -> "type-category"
  | Card_reference -> "card-reference"
  | Withdrawal_reversal_hint -> "withdrawal-reversal"
  | Reported_type _ -> "reported-type"
  | Reported_other _ -> "reported-other"
;;

let action_target = function
  | Category value | Settlement value | Type_and_category (_, value) -> value
  | Type value -> Movement_type.to_string value
  | Card_reference -> "capture-group-1"
  | Withdrawal_reversal_hint -> "withdrawal-reversal"
  | Reported_type value ->
    Option.value_map value ~default:"unknown" ~f:Movement_type.to_string
  | Reported_other prefix -> prefix
;;

let action_type = function
  | Type value | Type_and_category (value, _) -> Some value
  | Category _
  | Settlement _
  | Card_reference
  | Withdrawal_reversal_hint
  | Reported_type _
  | Reported_other _ -> None
;;

let action_of_fields kind value type_value =
  match kind, type_value with
  | "category", None -> Ok (Category value)
  | "settlement", None -> Ok (Settlement value)
  | "type", None -> Ok (Type (Movement_type.of_string value))
  | "card-reference", None -> Ok Card_reference
  | "withdrawal-reversal", None -> Ok Withdrawal_reversal_hint
  | "reported-type", None ->
    Ok
      (Reported_type
         (if String.equal value "unknown"
          then None
          else Some (Movement_type.of_string value)))
  | "reported-other", None -> Ok (Reported_other value)
  | "type-category", Some name ->
    Ok (Type_and_category (Movement_type.of_string name, value))
  | _ -> Or_error.error_string "invalid rule action"
;;

let create
      ~mode
      ~account_type
      ~priority
      ~name
      ~pattern
      ~institution
      ~match_type
      ~case_sensitive
      ~action
      ?field
      ()
  =
  let open Or_error.Let_syntax in
  let%bind field =
    match field, action with
    | None, _ -> Ok None
    | Some name, (Reported_type _ | Reported_other _) ->
      let%map name = Validation.text "field" name in
      Some name
    | Some _, _ ->
      Or_error.error_string "only reported-type rules can name a field to match"
  in
  let%bind name = Validation.token "rule name" name in
  let%bind () =
    match mode, action with
    | Automatic, Settlement _ -> Or_error.error_string "settlement rules require review"
    | ( Review
      , (Card_reference | Withdrawal_reversal_hint | Reported_type _ | Reported_other _) )
      -> Or_error.error_string "evidence rules must be automatic or disabled"
    | _ -> Ok ()
  in
  let%bind () =
    match account_type with
    | None -> Ok ()
    | Some value ->
      let%map _ = Validation.text "account type" value in
      ()
  in
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
    | (Reported_type _ | Reported_other _)
      when Option.is_some account_type || Option.is_some match_type ->
      Or_error.error_string
        "reported-type rules do not accept account or movement-type conditions"
    | Category account | Type_and_category (_, account) ->
      let%bind _ = Bank_category.create ~event_id:1L ~account ~reason:"rule" in
      if String.equal account "equity:unclassified"
      then Or_error.error_string "rule category must be an expense or income account"
      else Ok ()
    | Settlement card_id ->
      let%map _ = Validation.token "card ID" card_id in
      ()
    | Card_reference | Withdrawal_reversal_hint ->
      (match pattern with
       | None -> Or_error.error_string "evidence rules require a description expression"
       | Some pattern ->
         let action =
           match action with
           | Card_reference -> Source_evidence.Card_reference
           | _ -> Withdrawal_reversal
         in
         let%map _ =
           Source_evidence.make_rule
             ~action
             ~pattern
             ~case_sensitive
             ~account_type
             ~match_type:None
         in
         ())
    | Reported_type kind ->
      (match pattern, institution, kind with
       | None, _, _ | _, None, _ ->
         Or_error.error_string
           "reported-type rules require an institution and code expression"
       | Some _, Some _, Some kind -> validate_type kind
       | Some _, Some _, None -> Ok ())
    | Reported_other prefix ->
      (match pattern, institution with
       | None, _ | _, None ->
         Or_error.error_string
           "reported-type rules require an institution and code expression"
       | Some _, Some _ ->
         let%map _ = Validation.text "provider type prefix" prefix in
         ())
    | Type _ -> Ok ()
  in
  Ok
    { mode
    ; account_type
    ; priority
    ; name
    ; pattern
    ; institution
    ; match_type
    ; case_sensitive
    ; action
    ; field
    }
;;
