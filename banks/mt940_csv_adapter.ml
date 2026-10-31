open Base

let make (profile : Mt940_profile.t) : (module Adapter.S) =
  let module P = struct
    let profile = profile
  end
  in
  let module Transaction = Mt940_transaction.Make (P) in
  let module Source = Mt940_source.Make (P) in
  let module Csv = Mt940_csv_companion.Make (P) in
  let module A = struct
    let institution = profile.institution
    let aliases = profile.aliases
    let kinds = [ profile.kind ]
    let formats = [ profile.format; profile.companion_format ]
    let takes_account_label = false
    let overlap = Adapter.Day_inventory

    let decode event =
      let open Or_error.Let_syntax in
      let%bind tx = Transaction.source_transaction event in
      Movement.create
        ~account:
          { bank_id = institution
          ; account_id = tx.account_id
          ; account_type = profile.account_prefix ^ Iso4217.to_alpha_code tx.currency
          ; currency = tx.currency
          }
        ~own_iban:(Adapter_support.own_iban tx.account_id)
        ~booked_on:tx.booked_on
        ~value_on:tx.value_on
        ~amount:tx.amount
        ?counterparty_iban:(Iban.of_string tx.counterparty |> Result.ok)
        [ tx.name; tx.description ]
    ;;

    let probe ~filename:_ contents =
      if Source.recognizes contents
      then Some profile.format
      else if Csv.recognizes contents
      then Some profile.companion_format
      else None
    ;;

    let parse ~context:_ ~format contents =
      let open Or_error.Let_syntax in
      if String.equal format profile.format
      then (
        let%bind (document : Source.t) = Source.parse contents in
        let%map events =
          List.map document.transactions ~f:(fun (tx : Transaction.t) ->
            Source_event.create_dated
              ~venue:institution
              ~account:(Transaction.account_identity tx)
              ~kind:profile.kind
              ~source_id:tx.source_id
              ~occurred_on:tx.booked_on
              ~payload:tx.payload)
          |> Or_error.combine_errors
        in
        { Document.empty with
          statements =
            [ { source_account =
                  Json.encode
                    (Jsont.list Jsont.string)
                    [ institution
                    ; document.account_id
                    ; profile.account_prefix ^ Iso4217.to_alpha_code document.currency
                    ]
              ; reference = None
              ; currency = document.currency
              ; period_start = Temporal.Date document.opening_on
              ; period_end = Temporal.Date document.closing_on
              ; opening_as_of = Temporal.Date document.opening_on
              ; closing_as_of = Temporal.Date document.closing_on
              ; opening = document.opening
              ; closing = document.closing
              ; events
              }
            ]
        })
      else if String.equal format profile.companion_format
      then
        Adapter_support.companion_document
          Csv.parse
          ~number:(fun (row : Csv.row) -> row.row_number)
          ~fields:(fun row -> row.fields)
          contents
      else Or_error.errorf "unsupported statement format: %s" format
    ;;

    let companion : Companion.t =
      Adapter_support.companion
        ~format:profile.companion_format
        ~policy:profile.matching_policy
        ~of_fields:Csv.of_fields
        ~input:(fun r row -> { Csv.row_id = r.row_id; document_id = r.document_id; row })
        ~candidate:(fun id event ->
          let open Or_error.Let_syntax in
          let%map tx = Transaction.source_transaction event in
          id, tx)
        ~match_rows:Csv.match_rows
        ~annotate:(fun row ->
          { Companion.value_on = None
          ; display_name = None
          ; movement_type = None
          ; reported_type = None
          ; tags =
              (if String.is_empty row.counterparty
               then []
               else [ profile.metadata_prefix ^ "-counterparty", row.counterparty ])
          })
        ~describe:(fun row ->
          Adapter_support.
            [ "account_id", Some row.account_id
            ; "booked_on", Some (date row.booked_on)
            ; "value_on", Some (date row.value_on)
            ; "amount", Some (quantity row.amount)
            ; "counterparty", Some row.counterparty
            ; "name", Some row.name
            ; "description", Some row.description
            ])
    ;;

    let companions = [ companion ]
  end
  in
  (module A : Adapter.S)
;;
