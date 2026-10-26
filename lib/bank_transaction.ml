open Core

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : string
  }

type t =
  { account : account
  ; booked_on : Date.t
  ; amount : Bigdecimal.t
  ; name : string option
  ; memo : string option
  }

let is_event (event : Source_event.t) =
  List.mem [ "bank-transaction"; "wise-transaction" ] event.kind ~equal:String.equal
;;

let of_event (event : Source_event.t) =
  let open Or_error.Let_syntax in
  if String.equal event.kind "wise-transaction"
  then (
    let%map tx = Wise_camt.source_transaction event in
    let account =
      { bank_id = tx.account.bank_id
      ; account_id = tx.account.account_id
      ; account_type = tx.account.account_type
      ; currency = tx.account.currency
      }
    in
    { account
    ; booked_on = tx.booked_on
    ; amount = tx.amount
    ; name = Some tx.description
    ; memo = None
    })
  else (
    let%map a, tx = Bank_ofx.source_transaction event in
    let account =
      { bank_id = a.bank_id
      ; account_id = a.account_id
      ; account_type = a.account_type
      ; currency = a.currency
      }
    in
    { account
    ; booked_on = tx.booked_on
    ; amount = tx.amount
    ; name = tx.name
    ; memo = tx.memo
    })
;;
