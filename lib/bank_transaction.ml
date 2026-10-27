open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : string
  }

type t =
  { account : account
  ; booked_on : Date.t
  ; amount : Quantity.t
  ; fee : Quantity.t
  ; name : string option
  ; memo : string option
  }

let is_event (event : Source_event.t) =
  List.mem
    [ "bank-transaction"; "wise-transaction"; "bunq-transaction"; "revolut-transaction" ]
    event.kind
    ~equal:String.equal
;;

let of_event (event : Source_event.t) =
  let open Or_error.Let_syntax in
  if String.equal event.kind "revolut-transaction"
  then (
    let%map tx = Revolut_csv.source_transaction event in
    let account =
      { bank_id = "revolut"
      ; account_id = tx.account_id
      ; account_type = "current-" ^ tx.currency
      ; currency = tx.currency
      }
    in
    { account
    ; booked_on = tx.booked_on
    ; amount = tx.amount
    ; fee = tx.fee
    ; name = Some tx.description
    ; memo = None
    })
  else if String.equal event.kind "bunq-transaction"
  then (
    let%map tx = Bunq_transaction.source_transaction event in
    let account =
      { bank_id = "bunq"
      ; account_id = tx.account_id
      ; account_type = "current-" ^ tx.currency
      ; currency = tx.currency
      }
    in
    { account
    ; booked_on = tx.booked_on
    ; amount = tx.amount
    ; fee = Quantity.zero
    ; name = Some tx.name
    ; memo = Some tx.description
    })
  else if String.equal event.kind "wise-transaction"
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
    ; fee = Quantity.zero
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
    ; fee = Quantity.zero
    ; name = tx.name
    ; memo = tx.memo
    })
;;
