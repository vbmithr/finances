open Base

(* Stable, self-contained evidence: a movement can be decoded without the
   profile that originally selected its columns. *)
type t =
  { account_id : string
  ; account_type : string
  ; date : Date.t
  ; description : string
  ; amount : Quantity.t
  ; currency : Iso4217.t
  ; fields : string list
  ; occurrence : int
  }

let identity ~institution t =
  Json.encode (Jsont.list Jsont.string) [ institution; t.account_id; t.account_type ]
;;

let members map =
  map
  |> Jsont.Object.mem "format" (Json.tag "csv-movement-v1") ~enc:(fun _ -> ())
  |> Jsont.Object.mem "account_id" Jsont.string ~enc:(fun (t : t) -> t.account_id)
  |> Jsont.Object.mem "account_type" Jsont.string ~enc:(fun (t : t) -> t.account_type)
  |> Jsont.Object.mem "date" Json.date ~enc:(fun t -> t.date)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun t -> t.description)
  |> Jsont.Object.mem "amount" Json.quantity ~enc:(fun t -> t.amount)
  |> Jsont.Object.mem "currency" Json.currency ~enc:(fun t -> t.currency)
  |> Jsont.Object.mem "fields" (Jsont.list Jsont.string) ~enc:(fun t -> t.fields)
;;

let base_codec = Jsont.Object.enc_only () |> members |> Jsont.Object.finish

let codec =
  Jsont.Object.map
    (fun () account_id account_type date description amount currency fields occurrence ->
       { account_id
       ; account_type
       ; date
       ; description
       ; amount
       ; currency
       ; fields
       ; occurrence
       })
  |> members
  |> Jsont.Object.mem "occurrence" Json.integer ~enc:(fun t -> t.occurrence)
  |> Jsont.Object.finish
;;

let source_id t =
  Digestif.SHA256.(to_hex (digest_string (Json.encode base_codec t)))
  ^ ":"
  ^ Int.to_string t.occurrence
;;

let event ~institution t =
  let open Or_error.Let_syntax in
  let%bind _ = Validation.token "CSV account label" t.account_id in
  let%bind _ = Validation.token "CSV account type" t.account_type in
  if t.occurrence < 1
  then Or_error.error_string "invalid CSV occurrence"
  else
    Source_event.create_dated
      ~venue:institution
      ~account:(identity ~institution t)
      ~kind:"csv-movement"
      ~source_id:(source_id t)
      ~occurred_on:t.date
      ~payload:(Json.encode codec t)
;;

let decode (source : Source_event.t) =
  let open Or_error.Let_syntax in
  let%bind t = Json.decode codec source.payload in
  let%bind expected = event ~institution:source.venue t in
  if
    not
      (String.equal source.venue expected.venue
       && String.equal source.kind expected.kind
       && String.equal source.account expected.account
       && String.equal source.source_id expected.source_id
       && Poly.equal source.occurred expected.occurred)
  then Or_error.error_string "CSV source identity disagrees with payload"
  else (
    let own_iban =
      match t.account_type with
      | "prepaid-card" | "credit-card" -> Movement.Not_applicable
      | _ -> Movement.Unknown
    in
    Movement.create
      ~account:
        { bank_id = source.venue
        ; account_id = t.account_id
        ; account_type = t.account_type
        ; currency = t.currency
        }
      ~own_iban
      ~booked_on:t.date
      ~amount:t.amount
      [ t.description ])
;;

let events ?(institution = "csv") ~account_id rows =
  let counts = Hashtbl.create (module String) in
  List.map rows ~f:(fun (row : Csv_profile.row) ->
    let t =
      { account_id
      ; account_type = row.account_type
      ; date = row.date
      ; description = row.description
      ; amount = row.amount
      ; currency = row.currency
      ; fields = row.fields
      ; occurrence = 1
      }
    in
    let key = Json.encode base_codec t in
    Hashtbl.incr counts key;
    event ~institution { t with occurrence = Hashtbl.find_exn counts key })
  |> Or_error.combine_errors
;;
