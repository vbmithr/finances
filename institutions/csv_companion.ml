open Base

let make ~snapshot ~format ~policy ~layout : Companion.t =
  let schema = Csv_companion_layout.schema layout in
  let of_fields ~row_number fields = Csv_schema.of_fields schema ~row_number fields in
  let schemas = Hashtbl.create (module String) in
  let decode_row (row : Companion.row) =
    if not snapshot
    then of_fields ~row_number:row.row_number row.fields
    else (
      match List.Assoc.find row.context "companion_profile" ~equal:String.equal with
      | None -> Or_error.error_string "companion document has no saved layout"
      | Some json ->
        let open Or_error.Let_syntax in
        let%bind schema =
          match Hashtbl.find schemas json with
          | Some schema -> Ok schema
          | None ->
            let%map profile = Json.decode Csv_companion_layout.codec json in
            let schema = Csv_companion_layout.schema profile in
            Hashtbl.set schemas ~key:json ~data:schema;
            schema
        in
        Csv_schema.of_fields schema ~row_number:row.row_number row.fields)
  in
  let header (row : Companion.row) =
    if not snapshot
    then Ok layout.header
    else (
      match List.Assoc.find row.context "companion_profile" ~equal:String.equal with
      | None -> Or_error.error_string "companion document has no saved layout"
      | Some json ->
        Or_error.map (Json.decode Csv_companion_layout.codec json) ~f:(fun p -> p.header))
  in
  Adapter_support.companion_with_row
    ~decode_row
    ~header
    ~format
    ~policy
    ~input:(fun source (row : Csv_companion_layout.row) ->
      let metadata =
        [ "value date", Option.map row.value_on ~f:Date.to_string
        ; "suggested description", row.suggested_description
        ; "category", row.category
        ; "parent category", row.parent_category
        ; "comment", row.comment
        ]
      in
      let row : Csv_exact_match.row =
        { account_id = row.account_id
        ; booked_on = row.booked_on
        ; amount = row.amount
        ; currency = row.currency
        ; description = row.description
        ; metadata
        }
      in
      { Csv_exact_match.row_id = source.row_id
      ; document_id = source.document_id
      ; institution = source.institution
      ; row
      })
    ~candidate:(fun event_id (event : Source_event.t) ->
      let open Or_error.Let_syntax in
      let%map movement = Ofx_bank.decode event in
      { Csv_exact_match.event_id
      ; institution = event.venue
      ; account_id = movement.account.account_id
      ; booked_on = movement.booked_on
      ; amount = movement.amount
      ; currency = Some movement.account.currency
      ; descriptions = movement.descriptions
      })
    ~match_rows:Csv_exact_match.match_rows
    ~annotate:(fun row ->
      { Companion.value_on = row.value_on
      ; display_name = None
      ; movement_type = None
      ; reported_type = None
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
