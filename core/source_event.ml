open Base

type t =
  { venue : string
  ; account : string
  ; kind : string
  ; source_id : string
  ; occurred : Temporal.t
  ; payload : string
  ; fields : (string * string) list
  }

let make ?(fields = []) ~venue ~account ~kind ~source_id ~occurred ~payload () =
  let open Or_error.Let_syntax in
  let%bind venue = Validation.text "venue" venue in
  let%bind account = Validation.text "source account" account in
  let%bind kind = Validation.text "event kind" kind in
  let%map source_id = Validation.text "source ID" source_id in
  { venue; account; kind; source_id; occurred; payload; fields }
;;

let create ?fields ~venue ~account ~kind ~source_id ~occurred_at ~payload () =
  make
    ?fields
    ~venue
    ~account
    ~kind
    ~source_id
    ~occurred:(Temporal.Instant occurred_at)
    ~payload
    ()
;;

let create_dated ?fields ~venue ~account ~kind ~source_id ~occurred_on ~payload () =
  let open Or_error.Let_syntax in
  let%bind () = Or_error.try_with (fun () -> Date.validate_exn occurred_on) in
  make
    ?fields
    ~venue
    ~account
    ~kind
    ~source_id
    ~occurred:(Temporal.Date occurred_on)
    ~payload
    ()
;;

let with_fields t fields = { t with fields }
