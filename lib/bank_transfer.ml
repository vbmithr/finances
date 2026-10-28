open Base

type state =
  | Confirmed
  | Rejected

let state_name = function
  | Confirmed -> "confirmed"
  | Rejected -> "rejected"
;;

let state_of_string = function
  | "confirmed" -> Ok Confirmed
  | "rejected" -> Ok Rejected
  | _ -> Or_error.error_string "invalid transfer decision"
;;

type decision =
  { first_event_id : int64
  ; second_event_id : int64
  ; state : state
  ; reason : string
  }

let create ~first_event_id ~second_event_id ~state ~reason =
  let open Or_error.Let_syntax in
  let%bind reason = Validation.text "transfer reason" (String.strip reason) in
  if
    Int64.(
      first_event_id <= 0L || second_event_id <= 0L || first_event_id = second_event_id)
  then Or_error.error_string "transfer requires two distinct positive event IDs"
  else
    Ok
      { first_event_id = Int64.min first_event_id second_event_id
      ; second_event_id = Int64.max first_event_id second_event_id
      ; state
      ; reason
      }
;;

let decision_jsont =
  Jsont.Object.map (fun first_event_id second_event_id state reason ->
    create ~first_event_id ~second_event_id ~state ~reason |> Json.or_error)
  |> Jsont.Object.mem "first_event_id" Json.int64_codec ~enc:(fun t -> t.first_event_id)
  |> Jsont.Object.mem "second_event_id" Json.int64_codec ~enc:(fun t -> t.second_event_id)
  |> Jsont.Object.mem
       "decision"
       (Jsont.enum [ "confirm", Confirmed; "reject", Rejected ])
       ~enc:(fun t -> t.state)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun t -> t.reason)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let of_json = Json.decode (Jsont.list decision_jsont)

let account first second =
  Printf.sprintf
    "assets:transfers:pair-%Ld-%Ld"
    (Int64.min first second)
    (Int64.max first second)
;;

let eligible (a : Source_event.t) (ta : Movement.t) (b : Source_event.t) (tb : Movement.t)
  =
  (not (String.equal a.venue b.venue && String.equal a.account b.account))
  && Iso4217.equal ta.account.currency tb.account.currency
  && (not (Quantity.is_zero (Movement.principal ta)))
  && Quantity.(Movement.principal ta + Movement.principal tb = zero)
  && Int.abs (Date.diff ta.booked_on tb.booked_on) <= 7
;;
