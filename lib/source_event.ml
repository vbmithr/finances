open Core

type occurrence =
  | At_ms of int64
  | On_date of Date.t

type t =
  { venue : string
  ; account : string
  ; kind : string
  ; source_id : string
  ; occurred : occurrence
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

let create ~venue ~account ~kind ~source_id ~occurred_at_ms ~payload =
  if Int64.(occurred_at_ms < 0L)
  then Or_error.error_string "event timestamp must be nonnegative Unix milliseconds"
  else make ~venue ~account ~kind ~source_id ~occurred:(At_ms occurred_at_ms) ~payload
;;

let create_dated ~venue ~account ~kind ~source_id ~occurred_on ~payload =
  make ~venue ~account ~kind ~source_id ~occurred:(On_date occurred_on) ~payload
;;
