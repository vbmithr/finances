open Base

let institution = "bunq"
let aliases = []
let kinds = [ "bunq-transaction" ]
let mt940 = "bunq_mt940"
let csv = "bunq_csv"
let formats = [ mt940; csv ]
let takes_account_label = false
let overlap = Adapter.Day_inventory

let decode event =
  let open Or_error.Let_syntax in
  let%bind tx = Bunq_transaction.source_transaction event in
  Movement.create
    ~account:
      { bank_id = "bunq"
      ; account_id = tx.account_id
      ; account_type = "current-" ^ Iso4217.to_alpha_code tx.currency
      ; currency = tx.currency
      }
    ~own_iban:(Adapter_support.own_iban tx.account_id)
    ~booked_on:tx.booked_on
    ~value_on:tx.value_on
    ~amount:tx.amount
    ?counterparty_iban:(Iban.of_string tx.counterparty |> Result.ok)
    [ tx.name; tx.description ]
;;

let probe ~filename contents =
  if
    Bunq_mt940.recognizes contents
    || String.equal (Adapter_support.extension filename) ".mt940"
  then Some mt940
  else if Bunq_csv.recognizes contents
  then Some csv
  else None
;;

let parse ~context:_ ~format contents =
  let open Or_error.Let_syntax in
  if String.equal format mt940
  then (
    let%bind (document : Bunq_mt940.t) = Bunq_mt940.parse contents in
    let%map events =
      List.map document.transactions ~f:(fun (tx : Bunq_transaction.t) ->
        Source_event.create_dated
          ~venue:institution
          ~account:(Bunq_transaction.account_identity tx)
          ~kind:"bunq-transaction"
          ~source_id:tx.source_id
          ~occurred_on:tx.booked_on
          ~payload:tx.payload)
      |> Or_error.combine_errors
    in
    { Document.empty with
      statements =
        [ { source_account =
              Yojson.Safe.to_string
                (`List
                    [ `String "bunq"
                    ; `String document.account_id
                    ; `String ("current-" ^ Iso4217.to_alpha_code document.currency)
                    ])
          ; reference = None
          ; currency = document.currency
          ; period_start = Date.to_string document.opening_on
          ; period_end = Date.to_string document.closing_on
          ; opening_as_of = Date.to_string document.opening_on
          ; closing_as_of = Date.to_string document.closing_on
          ; opening = document.opening
          ; closing = document.closing
          ; events
          }
        ]
    })
  else (
    let%map rows = Bunq_csv.parse contents in
    { Document.empty with
      companion_rows =
        List.map rows ~f:(fun (row : Bunq_csv.row) -> row.row_number, row.fields)
    })
;;

let row (r : Companion.row) = Bunq_csv.of_fields ~row_number:r.row_number r.fields

let companion : Companion.t =
  { format = csv
  ; policy = "bunq-csv-v1"
  ; match_rows =
      (fun rows events ->
        let open Or_error.Let_syntax in
        let%bind inputs =
          List.map rows ~f:(fun (r : Companion.row) ->
            let%map row = row r in
            { Bunq_csv.row_id = r.row_id; document_id = r.document_id; row })
          |> Or_error.combine_errors
        in
        let%map transactions =
          List.map events ~f:(fun (id, event) ->
            let%map tx = Bunq_transaction.source_transaction event in
            id, tx)
          |> Or_error.combine_errors
        in
        Bunq_csv.match_rows inputs transactions)
  ; annotate =
      (fun r ->
        let open Or_error.Let_syntax in
        let%map row = row r in
        { Companion.value_on = None
        ; tags =
            (if String.is_empty row.counterparty
             then []
             else [ "bunq-counterparty", row.counterparty ])
        })
  ; describe =
      (fun r ->
        let open Or_error.Let_syntax in
        let%map row = row r in
        Adapter_support.
          [ "account_id", Some row.account_id
          ; "booked_on", Some (date row.booked_on)
          ; "value_on", Some (date row.value_on)
          ; "amount", Some (quantity row.amount)
          ; "counterparty", Some row.counterparty
          ; "name", Some row.name
          ; "description", Some row.description
          ])
  }
;;

let companions = [ companion ]
