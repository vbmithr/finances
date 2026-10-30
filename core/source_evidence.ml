open Base

type action =
  | Card_reference
  | Withdrawal_reversal

type default =
  { institution : string
  ; name : string
  ; pattern : string
  ; case_sensitive : bool
  ; action : action
  }

let action_name = function
  | Card_reference -> "card-reference"
  | Withdrawal_reversal -> "withdrawal-reversal"
;;

let action_codec =
  Jsont.enum
    [ "card-reference", Card_reference; "withdrawal-reversal", Withdrawal_reversal ]
;;

let default_codec =
  Jsont.Object.map (fun institution name pattern case_sensitive action ->
    { institution; name; pattern; case_sensitive; action })
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun r -> r.institution)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "pattern" Jsont.string ~enc:(fun r -> r.pattern)
  |> Jsont.Object.mem "case_sensitive" Jsont.bool ~enc:(fun r -> r.case_sensitive)
  |> Jsont.Object.mem "action" action_codec ~enc:(fun r -> r.action)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

type rule =
  { action : action
  ; account_type : string option
  ; match_type : string option
  ; compiled : Re.re
  }

let make_rule ~action ~pattern ~case_sensitive ~account_type ~match_type =
  let open Or_error.Let_syntax in
  let%bind _ = Validation.text "evidence expression" pattern in
  let%bind compiled =
    Or_error.try_with (fun () ->
      Re.Perl.compile_pat ~opts:(if case_sensitive then [] else [ `Caseless ]) pattern)
  in
  if Poly.equal action Card_reference && Re.group_count compiled < 2
  then Or_error.error_string "card reference rules require a capturing group"
  else Ok { action; account_type; match_type; compiled }
;;

type t =
  { card_reference : string option
  ; withdrawal_reversal : bool
  }

let empty = { card_reference = None; withdrawal_reversal = false }

let apply rules (movement : Movement.t) =
  let description =
    Option.value_map (List.hd movement.descriptions) ~default:"" ~f:String.strip
  in
  List.fold rules ~init:empty ~f:(fun result rule ->
    let type_matches =
      Option.value_map rule.match_type ~default:true ~f:(fun expected ->
        match expected, movement.movement_type with
        | "unknown", None -> true
        | "unknown", Some _ -> false
        | _, Some kind -> String.equal expected (Movement_type.to_string kind)
        | _, None -> false)
    in
    if
      not
        (type_matches
         && Option.value_map
              rule.account_type
              ~default:true
              ~f:(String.equal movement.account.account_type))
    then result
    else (
      match Re.exec_opt rule.compiled description with
      | None -> result
      | Some groups ->
        (match rule.action with
         | Withdrawal_reversal -> { result with withdrawal_reversal = true }
         | Card_reference when List.length movement.descriptions <> 1 -> result
         | Card_reference when Option.is_some result.card_reference -> result
         | Card_reference ->
           let card_reference =
             Re.Group.get_opt groups 1 |> Option.filter ~f:(Fn.non String.is_empty)
           in
           { result with card_reference })))
;;
