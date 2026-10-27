open Base

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
  Yojson.Safe.to_string
    (`List
        [ `String "bunq"
        ; `String t.account_id
        ; `String ("current-" ^ Iso4217.to_alpha_code t.currency)
        ])
;;

let key ~account_id ~booked_on ~value_on ~amount ~counterparty ~name ~description =
  Yojson.Safe.to_string
    (`List
        (List.map
           [ account_id
           ; Date.to_string booked_on
           ; Date.to_string value_on
           ; Quantity.to_string amount
           ; normalize counterparty
           ; normalize name
           ; normalize description
           ]
           ~f:(fun s -> `String s)))
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

let create
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
  let%bind account_id = Validation.token "Bunq account" account_id in
  let code = Iso4217.to_alpha_code currency in
  if occurrence < 1
  then Or_error.error_string "invalid Bunq occurrence"
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
    let payload =
      Yojson.Safe.to_string
        (`Assoc
            [ "format", `String "bunq-v1"
            ; "account_id", `String account_id
            ; "currency", `String code
            ; "booked_on", `String (Date.to_string booked_on)
            ; "value_on", `String (Date.to_string value_on)
            ; "amount", `String (Quantity.to_string amount)
            ; "counterparty", `String counterparty
            ; "name", `String name
            ; "description", `String description
            ; "occurrence", `Int occurrence
            ])
    in
    Ok
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
      ; payload
      })
;;

let source_transaction (event : Source_event.t) =
  Or_error.try_with (fun () ->
    if not (String.equal event.kind "bunq-transaction" && String.equal event.venue "bunq")
    then failwith "not a Bunq event";
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string event.payload in
    let field k = member k json |> to_string in
    if not (String.equal (field "format") "bunq-v1")
    then failwith "unsupported Bunq payload";
    let tx =
      create
        ~account_id:(field "account_id")
        ~currency:
          (Validation.currency "Bunq currency" (field "currency") |> Or_error.ok_exn)
        ~booked_on:(Date.of_string (field "booked_on"))
        ~value_on:(Date.of_string (field "value_on"))
        ~amount:(Quantity.of_string (field "amount"))
        ~counterparty:(field "counterparty")
        ~name:(field "name")
        ~description:(field "description")
        ~occurrence:(member "occurrence" json |> to_int)
      |> Or_error.ok_exn
    in
    if
      not
        (String.equal event.account (account_identity tx)
         && String.equal event.source_id tx.source_id
         &&
         match event.occurred with
         | On_date d -> Date.equal d tx.booked_on
         | _ -> false)
    then failwith "Bunq identity disagrees with payload";
    tx)
;;
