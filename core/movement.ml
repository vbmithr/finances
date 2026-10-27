open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : Iso4217.t
  }

type own_iban =
  | Known of Iban.t
  | Unknown
  | Not_applicable

type t =
  { account : account
  ; own_iban : own_iban
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Quantity.t
  ; fee : Quantity.t
  ; descriptions : string list
  ; counterparty_iban : Iban.t option
  }

let create
      ~account
      ~own_iban
      ~booked_on
      ?value_on
      ~amount
      ?(fee = Quantity.zero)
      ?counterparty_iban
      descriptions
  =
  let open Or_error.Let_syntax in
  let%bind _ = Validation.text "bank ID" account.bank_id in
  let%bind _ = Validation.text "account ID" account.account_id in
  let%bind _ = Validation.text "account type" account.account_type in
  if Quantity.(fee < zero)
  then Or_error.error_string "movement fee must be non-negative"
  else
    Ok
      { account
      ; own_iban
      ; booked_on
      ; value_on
      ; amount
      ; fee
      ; descriptions
      ; counterparty_iban
      }
;;

let principal t = Quantity.(t.amount + t.fee)
