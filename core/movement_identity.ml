open Base

type t =
  { venue : string
  ; source_account : string
  ; source_kind : string
  ; source_id : string
  ; role : string
  }
[@@deriving compare, equal]

let create ~venue ~source_account ~source_kind ~source_id ~role =
  let open Or_error.Let_syntax in
  let%bind venue = Validation.text "source venue" venue in
  let%bind source_account = Validation.text "source account" source_account in
  let%bind source_kind = Validation.text "source kind" source_kind in
  let%bind source_id = Validation.text "source ID" source_id in
  let%map role = Validation.token "movement role" role in
  { venue; source_account; source_kind; source_id; role }
;;

let of_event (event : Source_event.t) ~role =
  create
    ~venue:event.venue
    ~source_account:event.account
    ~source_kind:event.kind
    ~source_id:event.source_id
    ~role
;;

let codec =
  Jsont.Object.map (fun venue source_account source_kind source_id role ->
    Json.or_error (create ~venue ~source_account ~source_kind ~source_id ~role))
  |> Jsont.Object.mem "venue" Jsont.string ~enc:(fun t -> t.venue)
  |> Jsont.Object.mem "source_account" Jsont.string ~enc:(fun t -> t.source_account)
  |> Jsont.Object.mem "source_kind" Jsont.string ~enc:(fun t -> t.source_kind)
  |> Jsont.Object.mem "source_id" Jsont.string ~enc:(fun t -> t.source_id)
  |> Jsont.Object.mem "role" Jsont.string ~enc:(fun t -> t.role)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
