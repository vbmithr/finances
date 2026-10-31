open Base

let make ~profile ~csv_profile ?(aliases = []) ~csv ~policy ~metadata_prefix ()
  : (module Adapter.S)
  =
  let source = Camt_source.create profile in
  let institution = profile.Camt_profile.institution in
  let camt = "camt053" in
  let annotate (row : Csv_clock_companion.row) : Companion.annotation =
    { value_on = None
    ; display_name = row.merchant
    ; movement_type = None
    ; reported_type = row.details_type
    ; tags =
        List.filter_opt
          [ Some (metadata_prefix ^ "-reported-fees", Quantity.to_string row.fees)
          ; Option.map row.merchant ~f:(fun s -> metadata_prefix ^ "-merchant", s)
          ; Option.map row.payment_reference ~f:(fun s ->
              metadata_prefix ^ "-payment-reference", s)
          ]
    }
  in
  let module Adapter_impl = struct
    let institution = institution
    let aliases = aliases
    let kinds = [ profile.event_kind ]
    let formats = [ camt; csv ]
    let takes_account_label = false
    let overlap = Adapter.Source_ids

    let decode event =
      let open Or_error.Let_syntax in
      let%bind tx = Camt_source.source_transaction source event in
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
        String.is_substring
          prefix
          ~substring:csv_profile.Csv_clock_profile.columns.reference
        && String.is_substring prefix ~substring:csv_profile.columns.datetime
      then Some csv
      else None
    ;;

    let parse ~context:_ ~format contents =
      let open Or_error.Let_syntax in
      if String.equal format camt
      then (
        let%bind (document : Camt_source.t) = Camt_source.parse source contents in
        let%map statements =
          List.map document.statements ~f:(fun (statement : Camt_source.statement) ->
            let%map events =
              List.map statement.transactions ~f:(fun (trn : Camt_source.transaction) ->
                Source_event.create
                  ~venue:institution
                  ~account:(Camt_source.account_identity trn.account)
                  ~kind:profile.event_kind
                  ~source_id:trn.source_id
                  ~occurred_at:trn.booked_at
                  ~payload:trn.payload)
              |> Or_error.combine_errors
            in
            { Document.source_account = Camt_source.account_identity statement.account
            ; reference = Some statement.statement_id
            ; currency = statement.account.currency
            ; period_start = Temporal.Instant statement.period_start
            ; period_end = Temporal.Instant statement.period_end
            ; opening_as_of = Temporal.Instant statement.opening_at
            ; closing_as_of = Temporal.Instant statement.closing_at
            ; opening = statement.opening
            ; closing = statement.closing
            ; events
            })
          |> Or_error.combine_errors
        in
        { Document.empty with statements })
      else if String.equal format csv
      then
        Adapter_support.companion_document
          (Csv_clock_companion.parse csv_profile profile)
          ~number:(fun (row : Csv_clock_companion.row) -> row.row_number)
          ~fields:(fun row -> row.fields)
          contents
      else Or_error.errorf "unsupported statement format: %s" format
    ;;

    let companion : Companion.t =
      Adapter_support.companion
        ~format:csv
        ~policy
        ~of_fields:(Csv_clock_companion.of_fields csv_profile profile)
        ~input:(fun r row ->
          { Csv_clock_companion.row_id = r.row_id; document_id = r.document_id; row })
        ~candidate:(fun id event ->
          let open Or_error.Let_syntax in
          let%map tx = Camt_source.source_transaction source event in
          id, tx)
        ~match_rows:(Csv_clock_companion.match_rows csv_profile)
        ~header:(fun _ -> Ok csv_profile.header)
        ~annotate
        ~describe:(fun row ->
          Adapter_support.
            [ "reference", Some row.reference
            ; "booked_at", Some (Timestamp.to_rfc3339 row.booked_at)
            ; "amount", Some (quantity row.amount)
            ; "currency", Some (Iso4217.to_alpha_code row.currency)
            ; "description", Some row.description
            ; "fees", Some (quantity row.fees)
            ; "running_balance", Some (quantity row.running_balance)
            ; "payment_reference", row.payment_reference
            ; "merchant", row.merchant
            ; "payee", row.payee
            ; "details_type", row.details_type
            ])
    ;;

    let companions = [ companion ]
  end
  in
  (module Adapter_impl : Adapter.S)
;;
