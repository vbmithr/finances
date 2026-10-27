open Base

let institution = "wise"
let aliases = []
let kinds = [ "wise-transaction" ]

let decode event =
  let open Or_error.Let_syntax in
  let%bind tx = Wise_camt.source_transaction event in
  Movement.create
    ~account:
      { bank_id = tx.account.bank_id
      ; account_id = tx.account.account_id
      ; account_type = tx.account.account_type
      ; currency = tx.account.currency
      }
    ~own_iban:(Adapter_support.own_iban tx.account.account_id)
    ~booked_on:tx.booked_on
    ?value_on:tx.value_on
    ~amount:tx.amount
    [ tx.description ]
;;
