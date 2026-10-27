open Base

let institution = "revolut"
let aliases = []
let kinds = [ "revolut-transaction" ]

let decode event =
  let open Or_error.Let_syntax in
  let%bind tx = Revolut_csv.source_transaction event in
  Movement.create
    ~account:
      { bank_id = "revolut"
      ; account_id = tx.account_id
      ; account_type = "current-" ^ tx.currency
      ; currency = tx.currency
      }
    ~own_iban:(Adapter_support.own_iban tx.account_id)
    ~booked_on:tx.booked_on
    ~amount:tx.amount
    ~fee:tx.fee
    [ tx.description ]
;;
