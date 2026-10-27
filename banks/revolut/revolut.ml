open Base

let institution = "revolut"
let aliases = []
let kinds = [ "revolut-transaction" ]
let csv = "revolut_csv"
let formats = [ csv ]
let takes_account_label = true
let overlap = Adapter.Day_inventory
let companions = []

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

let probe ~filename:_ contents = Option.some_if (Revolut_csv.recognizes contents) csv

(* The export does not name its account; the user's label stands for it and is
   kept with the document. *)
let parse ~(context : Adapter.context) ~format:_ contents =
  let open Or_error.Let_syntax in
  let%bind account_id =
    Or_error.of_option
      context.account_label
      ~error:
        (Error.of_string "Revolut CSV has no account identifier; supply --account-id")
  in
  let%bind (document : Revolut_csv.t) = Revolut_csv.parse ~account_id contents in
  let%map events =
    List.map document.transactions ~f:(fun (tx : Revolut_csv.transaction) ->
      Source_event.create_dated
        ~venue:institution
        ~account:(Revolut_csv.account_identity tx)
        ~kind:"revolut-transaction"
        ~source_id:tx.source_id
        ~occurred_on:tx.booked_on
        ~payload:tx.payload)
    |> Or_error.combine_errors
  in
  { Document.empty with events; context = [ "account_id", account_id ] }
;;
