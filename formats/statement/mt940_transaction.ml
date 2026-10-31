open Base

module Make (P : sig
    val profile : Mt940_profile.t
  end) =
struct
  let profile = P.profile

  type t =
    { account_id : string
    ; currency : Iso4217.t
    ; booked_on : Date.t
    ; value_on : Date.t
    ; amount : Quantity.t
    ; counterparty : string
    ; name : string
    ; description : string
    ; occurrence : int
    ; source_id : string
    ; payload : string
    }

  let normalize s =
    String.split_on_chars s ~on:[ ' '; '\t'; '\r'; '\n' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> String.concat ~sep:" "
  ;;

  let account_identity t =
    Json.encode
      (Jsont.list Jsont.string)
      [ profile.institution
      ; t.account_id
      ; profile.account_prefix ^ Iso4217.to_alpha_code t.currency
      ]
  ;;

  let key ~account_id ~booked_on ~value_on ~amount ~counterparty ~name ~description =
    Json.encode
      (Jsont.list Jsont.string)
      (List.map
         [ account_id
         ; Date.to_string booked_on
         ; Date.to_string value_on
         ; Quantity.to_string amount
         ; normalize counterparty
         ; normalize name
         ; normalize description
         ]
         ~f:Fn.id)
  ;;

  let csv_key t =
    key
      ~account_id:t.account_id
      ~booked_on:t.booked_on
      ~value_on:t.value_on
      ~amount:t.amount
      ~counterparty:t.counterparty
      ~name:t.name
      ~description:t.description
  ;;

  let rec create
            ~account_id
            ~currency
            ~booked_on
            ~value_on
            ~amount
            ~counterparty
            ~name
            ~description
            ~occurrence
    =
    let open Or_error.Let_syntax in
    let%bind account_id = Validation.token (profile.name ^ " account") account_id in
    let code = Iso4217.to_alpha_code currency in
    if occurrence < 1
    then Or_error.error_string ("invalid " ^ profile.name ^ " occurrence")
    else (
      let counterparty = normalize counterparty
      and name = normalize name
      and description = normalize description in
      let base =
        key ~account_id ~booked_on ~value_on ~amount ~counterparty ~name ~description
      in
      let source_id =
        Digestif.SHA256.(to_hex (digest_string (code ^ base)))
        ^ ":"
        ^ Int.to_string occurrence
      in
      let tx =
        { account_id
        ; currency
        ; booked_on
        ; value_on
        ; amount
        ; counterparty
        ; name
        ; description
        ; occurrence
        ; source_id
        ; payload = ""
        }
      in
      Ok { tx with payload = Json.encode (Lazy.force jsont) tx })

  and jsont =
    lazy
      (Jsont.Object.map
         (fun
             ()
              account_id
              currency
              booked_on
              value_on
              amount
              counterparty
              name
              description
              occurrence
            ->
            create
              ~account_id
              ~currency
              ~booked_on
              ~value_on
              ~amount
              ~counterparty
              ~name
              ~description
              ~occurrence
            |> Json.or_error)
       |> Jsont.Object.mem "format" (Json.tag profile.payload_tag) ~enc:(fun _ -> ())
       |> Jsont.Object.mem "account_id" Jsont.string ~enc:(fun t -> t.account_id)
       |> Jsont.Object.mem "currency" Json.currency ~enc:(fun t -> t.currency)
       |> Jsont.Object.mem "booked_on" Json.date ~enc:(fun t -> t.booked_on)
       |> Jsont.Object.mem "value_on" Json.date ~enc:(fun t -> t.value_on)
       |> Jsont.Object.mem "amount" Json.quantity ~enc:(fun t -> t.amount)
       |> Jsont.Object.mem "counterparty" Jsont.string ~enc:(fun t -> t.counterparty)
       |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
       |> Jsont.Object.mem "description" Jsont.string ~enc:(fun t -> t.description)
       |> Jsont.Object.mem "occurrence" Json.integer ~enc:(fun t -> t.occurrence)
       |> Jsont.Object.finish)
  ;;

  let jsont = Lazy.force jsont

  let source_transaction (event : Source_event.t) =
    Or_error.try_with (fun () ->
      if
        not
          (String.equal event.kind profile.kind
           && String.equal event.venue profile.institution)
      then failwith ("not a " ^ profile.name ^ " event");
      let tx = Json.decode jsont event.payload |> Or_error.ok_exn in
      if
        not
          (String.equal event.account (account_identity tx)
           && String.equal event.source_id tx.source_id
           &&
           match event.occurred with
           | Temporal.Date d -> Date.equal d tx.booked_on
           | _ -> false)
      then failwith (profile.name ^ " identity disagrees with payload");
      tx)
  ;;
end
