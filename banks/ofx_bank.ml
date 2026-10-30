open Base

(** Shared OFX 2.x operations: transactions are identified by FITID
    and share the [bank-transaction] kind across institutions. *)
let kinds = [ "bank-transaction" ]

let ofx = "ofx"
let formats = [ ofx ]
let takes_account_label = false
let overlap = Adapter.Source_ids
let companions = []

let decode event =
  let open Or_error.Let_syntax in
  let%bind (a : Ofx_source.account), tx = Ofx_source.source_transaction event in
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

let probe ~filename contents =
  let prefix = Adapter_support.prefix contents in
  Option.some_if
    (String.is_substring prefix ~substring:"<OFX"
     || String.is_prefix prefix ~prefix:"OFXHEADER:"
     || String.equal (Adapter_support.extension filename) ".ofx")
    ofx
;;

let parse ~institution contents =
  let open Or_error.Let_syntax in
  let%bind document = Ofx_source.parse contents in
  let%map events =
    List.concat_map document.statements ~f:(fun statement ->
      List.map statement.transactions ~f:(fun trn ->
        Source_event.create_dated
          ~venue:institution
          ~account:(Ofx_source.account_identity statement.account)
          ~kind:"bank-transaction"
          ~source_id:trn.fitid
          ~occurred_on:trn.booked_on
          ~payload:trn.source_payload))
    |> Or_error.combine_errors
  in
  { Document.empty with events }
;;

let make ~institution () : (module Adapter.S) =
  (module struct
    let institution = institution
    let aliases = []
    let kinds = kinds
    let decode = decode
    let formats = formats
    let probe = probe
    let takes_account_label = takes_account_label

    let parse ~context:_ ~format contents =
      if String.equal format ofx
      then parse ~institution contents
      else Or_error.errorf "unsupported OFX format: %s" format
    ;;

    let overlap = overlap
    let companions = companions
  end)
;;
