open Base

let institution = "bunq"
let aliases = []
let kinds = [ "bunq-transaction" ]

let decode event =
  let open Or_error.Let_syntax in
  let%bind tx = Bunq_transaction.source_transaction event in
  Movement.create
    ~account:
      { bank_id = "bunq"
      ; account_id = tx.account_id
      ; account_type = "current-" ^ tx.currency
      ; currency = tx.currency
      }
    ~own_iban:(Adapter_support.own_iban tx.account_id)
    ~booked_on:tx.booked_on
    ~value_on:tx.value_on
    ~amount:tx.amount
    ?counterparty_iban:(Iban.of_string tx.counterparty |> Result.ok)
    [ tx.name; tx.description ]
;;
