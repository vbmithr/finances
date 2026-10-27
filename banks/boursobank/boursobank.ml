open Base

(* BoursoBank is an OFX bank whose CSV export enriches the OFX movements. *)
include Ofx_bank.Make (struct
    let institution = "boursobank"
    let aliases = [ "boursorama" ]
  end)

let csv = "csv"
let formats = [ ofx; csv ]

let probe ~filename contents =
  match probe ~filename contents with
  | Some format -> Some format
  | None -> Option.some_if (Bank_csv.recognizes contents) csv
;;

let parse ~context ~format contents =
  if String.equal format csv
  then
    Or_error.map (Bank_csv.parse contents) ~f:(fun rows ->
      { Document.empty with
        companion_rows =
          List.map rows ~f:(fun (row : Bank_csv.row) -> row.row_number, row.fields)
      })
  else parse ~context ~format contents
;;

let row (r : Companion.row) = Bank_csv.of_fields ~row_number:r.row_number r.fields

let companion : Companion.t =
  { format = csv
  ; policy = Bank_csv_match.policy
  ; match_rows =
      (fun rows events ->
        let open Or_error.Let_syntax in
        let%bind inputs =
          List.map rows ~f:(fun (r : Companion.row) ->
            let%map row = row r in
            { Bank_csv_match.row_id = r.row_id
            ; document_id = r.document_id
            ; institution = r.institution
            ; row
            })
          |> Or_error.combine_errors
        in
        let%map candidates =
          List.map events ~f:(fun (event_id, (event : Source_event.t)) ->
            let%map account, trn = Bank_ofx.source_transaction event in
            { Bank_csv_match.event_id
            ; institution = event.venue
            ; account_id = account.account_id
            ; booked_on = trn.booked_on
            ; amount = trn.amount
            ; descriptions = List.filter_opt [ trn.name; trn.memo ]
            })
          |> Or_error.combine_errors
        in
        Bank_csv_match.match_rows inputs candidates)
  ; annotate =
      (fun r ->
        let open Or_error.Let_syntax in
        let%map row = row r in
        { Companion.value_on = row.value_on
        ; tags =
            List.filter_opt
              [ Option.map row.category ~f:(fun s -> "bank-category", s)
              ; Option.map row.parent_category ~f:(fun s -> "bank-parent-category", s)
              ; Option.map row.suggested_description ~f:(fun s ->
                  "bank-suggested-description", s)
              ]
        })
  ; describe =
      (fun r ->
        let open Or_error.Let_syntax in
        let%map row = row r in
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
  }
;;

let companions = [ companion ]
