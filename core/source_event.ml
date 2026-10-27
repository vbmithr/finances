open Base

type t =
  { venue : string
  ; account : string
  ; kind : string
  ; source_id : string
  ; occurred : Temporal.t
  ; payload : string
  }

let make ~venue ~account ~kind ~source_id ~occurred ~payload =
  let open Or_error.Let_syntax in
  let%bind venue = Validation.text "venue" venue in
  let%bind account = Validation.text "source account" account in
  let%bind kind = Validation.text "event kind" kind in
  let%map source_id = Validation.text "source ID" source_id in
  { venue; account; kind; source_id; occurred; payload }
;;

let create ~venue ~account ~kind ~source_id ~occurred_at ~payload =
  make ~venue ~account ~kind ~source_id ~occurred:(Temporal.Instant occurred_at) ~payload
;;

let create_dated ~venue ~account ~kind ~source_id ~occurred_on ~payload =
  let open Or_error.Let_syntax in
  let%bind () = Or_error.try_with (fun () -> Date.validate_exn occurred_on) in
  make ~venue ~account ~kind ~source_id ~occurred:(Temporal.Date occurred_on) ~payload
;;
