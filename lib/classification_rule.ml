open Base

type action =
  | Category of string
  | Settlement of string

type t =
  { name : string
  ; pattern : string
  ; institution : string option
  ; case_sensitive : bool
  ; action : action
  }

let action_name = function
  | Category _ -> "category"
  | Settlement _ -> "settlement"
;;

let action_target = function
  | Category value | Settlement value -> value
;;

let action_of_fields kind value =
  match kind with
  | "category" -> Ok (Category value)
  | "settlement" -> Ok (Settlement value)
  | _ -> Or_error.error_string "unknown rule action"
;;

let create ~name ~pattern ~institution ~case_sensitive ~action =
  let open Or_error.Let_syntax in
  let%bind name = Validation.token "rule name" name in
  let%bind _ = Transaction_pattern.compile ~case_sensitive pattern in
  let%bind institution =
    match institution with
    | None -> Ok None
    | Some value ->
      let%map value = Validation.token "institution" value in
      Some value
  in
  let%bind action =
    match action with
    | Category account ->
      let%bind _ = Bank_category.create ~event_id:1L ~account ~reason:"rule" in
      if String.equal account "equity:unclassified"
      then Or_error.error_string "rule category must be an expense or income account"
      else Ok (Category account)
    | Settlement card_id ->
      let%map card_id = Validation.token "card ID" card_id in
      Settlement card_id
  in
  Ok { name; pattern; institution; case_sensitive; action }
;;
