open Base

type t =
  { event_id : int64
  ; account : string
  ; reason : string
  }

let create ~event_id ~account ~reason =
  let open Or_error.Let_syntax in
  let%bind account = Validation.account account in
  let%bind reason = Validation.text "category reason" (String.strip reason) in
  if Int64.(event_id <= 0L)
  then Or_error.error_string "event_id must be positive"
  else if
    not
      (String.equal account "equity:unclassified"
       || String.is_prefix account ~prefix:"expenses:"
       || String.is_prefix account ~prefix:"income:")
  then
    Or_error.error_string
      "category account must be expenses:..., income:..., or equity:unclassified"
  else Ok { event_id; account; reason }
;;

let jsont =
  Jsont.Object.map (fun event_id account reason ->
    create ~event_id ~account ~reason |> Json.or_error)
  |> Jsont.Object.mem "event_id" Json.int64_codec ~enc:(fun t -> t.event_id)
  |> Jsont.Object.mem "account" Jsont.string ~enc:(fun t -> t.account)
  |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun t -> t.reason)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let of_json = Json.decode (Jsont.list jsont)
