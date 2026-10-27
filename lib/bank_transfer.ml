open Core

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

let of_json contents =
  Or_error.try_with (fun () ->
    let open Yojson.Safe.Util in
    Yojson.Safe.from_string contents
    |> to_list
    |> List.map ~f:(fun json ->
      let fields = to_assoc json in
      if
        List.length fields <> 4
        || List.contains_dup (List.map fields ~f:fst) ~compare:String.compare
      then failwith "expected first_event_id, second_event_id, decision and reason";
      let id k =
        match member k json with
        | `Int n -> Int64.of_int n
        | `Intlit s -> Int64.of_string s
        | _ -> failwith "transfer event ID must be an integer"
      in
      let state =
        match member "decision" json |> to_string with
        | "confirm" -> Confirmed
        | "reject" -> Rejected
        | _ -> failwith "decision must be confirm or reject"
      in
      create
        ~first_event_id:(id "first_event_id")
        ~second_event_id:(id "second_event_id")
        ~state
        ~reason:(member "reason" json |> to_string)
      |> Or_error.ok_exn))
;;

let account first second =
  sprintf
    "assets:transfers:pair-%Ld-%Ld"
    (Int64.min first second)
    (Int64.max first second)
;;

let principal tx = Bigdecimal.(tx.Bank_transaction.amount + tx.fee)

let eligible
      (a : Source_event.t)
      (ta : Bank_transaction.t)
      (b : Source_event.t)
      (tb : Bank_transaction.t)
  =
  (not (String.equal a.venue b.venue && String.equal a.account b.account))
  && String.equal ta.account.currency tb.account.currency
  && (not (Bigdecimal.is_zero (principal ta)))
  && Bigdecimal.(principal ta + principal tb = zero)
  && Int.abs (Date.diff ta.booked_on tb.booked_on) <= 7
;;
