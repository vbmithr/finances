open Base

let institution = "boursobank"
let aliases = [ "boursorama" ]
let kinds = [ "bank-transaction" ]

let decode event =
  let open Or_error.Let_syntax in
  let%bind (a : Bank_ofx.account), tx = Bank_ofx.source_transaction event in
  (* OFX credit-card accounts are card numbers, never IBANs. *)
  let own_iban : Movement.own_iban =
    if String.equal a.account_type "CREDITCRD"
    then Not_applicable
    else Adapter_support.own_iban a.account_id
  in
  Movement.create
    ~account:
      { bank_id = a.bank_id
      ; account_id = a.account_id
      ; account_type = a.account_type
      ; currency = a.currency
      }
    ~own_iban
    ~booked_on:tx.booked_on
    ~amount:tx.amount
    (List.filter_opt [ tx.name; tx.memo ])
;;
