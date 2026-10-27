open Base

let institution = "wise"
let aliases = []
let kinds = [ "wise-transaction" ]
let camt = "camt053"
let csv = "wise_csv"
let formats = [ camt; csv ]
let takes_account_label = false
let overlap = Adapter.Source_ids

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

let probe ~filename:_ contents =
  let prefix = Adapter_support.prefix contents in
  if String.is_substring prefix ~substring:"camt.053"
  then Some camt
  else if
    String.is_substring prefix ~substring:"TransferWise ID"
    && String.is_substring prefix ~substring:"Date Time"
  then Some csv
  else None
;;

let parse ~context:_ ~format contents =
  let open Or_error.Let_syntax in
  if String.equal format camt
  then (
    let%bind (document : Wise_camt.t) = Wise_camt.parse contents in
    let%map statements =
      List.map document.statements ~f:(fun (statement : Wise_camt.statement) ->
        let%map events =
          List.map statement.transactions ~f:(fun (trn : Wise_camt.transaction) ->
            Source_event.create
              ~venue:institution
              ~account:(Wise_camt.account_identity trn.account)
              ~kind:"wise-transaction"
              ~source_id:trn.source_id
              ~occurred_at_ms:trn.occurred_at_ms
              ~payload:trn.payload)
          |> Or_error.combine_errors
        in
        { Document.source_account = Wise_camt.account_identity statement.account
        ; reference = Some statement.statement_id
        ; currency = statement.account.currency
        ; period_start = statement.period_start
        ; period_end = statement.period_end
        ; opening_as_of = statement.opening_at
        ; closing_as_of = statement.closing_at
        ; opening = statement.opening
        ; closing = statement.closing
        ; events
        })
      |> Or_error.combine_errors
    in
    { Document.empty with statements })
  else (
    let%map rows = Wise_csv.parse contents in
    { Document.empty with
      companion_rows =
        List.map rows ~f:(fun (row : Wise_csv.row) -> row.row_number, row.fields)
    })
;;

let row (r : Companion.row) = Wise_csv.of_fields ~row_number:r.row_number r.fields

let companion : Companion.t =
  { format = csv
  ; policy = "wise-csv-v1"
  ; match_rows =
      (fun rows events ->
        let open Or_error.Let_syntax in
        let%bind inputs =
          List.map rows ~f:(fun (r : Companion.row) ->
            let%map row = row r in
            { Wise_csv.row_id = r.row_id; document_id = r.document_id; row })
          |> Or_error.combine_errors
        in
        let%map transactions =
          List.map events ~f:(fun (id, event) ->
            let%map tx = Wise_camt.source_transaction event in
            id, tx)
          |> Or_error.combine_errors
        in
        Wise_csv.match_rows inputs transactions)
  ; annotate =
      (fun r ->
        let open Or_error.Let_syntax in
        let%map row = row r in
        { Companion.value_on = None
        ; tags =
            List.filter_opt
              [ Some ("wise-reported-fees", Quantity.to_string row.fees)
              ; Option.map row.merchant ~f:(fun s -> "wise-merchant", s)
              ; Option.map row.payment_reference ~f:(fun s -> "wise-payment-reference", s)
              ]
        })
  ; describe =
      (fun r ->
        let open Or_error.Let_syntax in
        let%map row = row r in
        Adapter_support.
          [ "reference", Some row.reference
          ; "booked_at", Some row.booked_local
          ; "amount", Some (quantity row.amount)
          ; "currency", Some (Iso4217.to_alpha_code row.currency)
          ; "description", Some row.description
          ; "fees", Some (quantity row.fees)
          ; "running_balance", Some (quantity row.running_balance)
          ; "payment_reference", row.payment_reference
          ; "merchant", row.merchant
          ; "payee", row.payee
          ])
  }
;;

let companions = [ companion ]
