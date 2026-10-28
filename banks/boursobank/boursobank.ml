open Base

(* BoursoBank is an OFX bank whose CSV export enriches the OFX movements. *)
include Ofx_bank

let institution = "boursobank"
let aliases = [ "boursorama" ]
let csv = "csv"
let formats = [ ofx; csv ]

let probe ~filename contents =
  match Ofx_bank.probe ~filename contents with
  | Some format -> Some format
  | None -> Option.some_if (Bank_csv.recognizes contents) csv
;;

let parse ~context:_ ~format contents =
  if String.equal format csv
  then
    Adapter_support.companion_document
      Bank_csv.parse
      ~number:(fun (row : Bank_csv.row) -> row.row_number)
      ~fields:(fun row -> row.fields)
      contents
  else Ofx_bank.parse ~institution contents
;;

let companion : Companion.t =
  Adapter_support.companion
    ~format:csv
    ~policy:Bank_csv_match.policy
    ~of_fields:Bank_csv.of_fields
    ~input:(fun r row ->
      { Bank_csv_match.row_id = r.row_id
      ; document_id = r.document_id
      ; institution = r.institution
      ; row
      })
    ~candidate:(fun event_id (event : Source_event.t) ->
      let open Or_error.Let_syntax in
      let%map account, trn = Bank_ofx.source_transaction event in
      { Bank_csv_match.event_id
      ; institution = event.venue
      ; account_id = account.account_id
      ; booked_on = trn.booked_on
      ; amount = trn.amount
      ; descriptions = List.filter_opt [ trn.name; trn.memo ]
      })
    ~match_rows:Bank_csv_match.match_rows
    ~annotate:(fun row ->
      { Companion.value_on = row.value_on
      ; tags =
          List.filter_opt
            [ Option.map row.category ~f:(fun s -> "bank-category", s)
            ; Option.map row.parent_category ~f:(fun s -> "bank-parent-category", s)
            ; Option.map row.suggested_description ~f:(fun s ->
                "bank-suggested-description", s)
            ]
      })
    ~describe:(fun row ->
      Adapter_support.
        [ "account_id", Some row.account_id
        ; "booked_on", Some (date row.booked_on)
        ; "value_on", optional date row.value_on
        ; "amount", Some (quantity row.amount)
        ; "description", Some row.description
        ; "suggested_description", row.suggested_description
        ; "category", row.category
        ; "parent_category", row.parent_category
        ; "comment", row.comment
        ; "account_label", row.account_label
        ; "reported_balance", optional quantity row.reported_balance
        ; "pointage", row.pointage
        ])
;;

let companions = [ companion ]
