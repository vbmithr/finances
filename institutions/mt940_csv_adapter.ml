open Base

let decode (profile : Mt940_profile.t) engine event =
  let open Or_error.Let_syntax in
  let%bind tx = Mt940_transaction.source_transaction engine event in
  Movement.create
    ~account:
      { bank_id = profile.institution
      ; account_id = tx.account_id
      ; account_type = profile.account_prefix ^ Iso4217.to_alpha_code tx.currency
      ; currency = tx.currency
      }
    ~own_iban:(Adapter_support.own_iban tx.account_id)
    ~booked_on:tx.booked_on
    ~value_on:tx.value_on
    ~amount:tx.amount
    ?counterparty_iban:(Iban.of_string tx.counterparty |> Result.ok)
    ~fields:[ "name", tx.name; "counterparty", tx.counterparty ]
    ~main_description:1
    [ tx.name; tx.description ]
;;

let probe (profile : Mt940_profile.t) ~filename:_ contents =
  if Mt940_source.recognizes profile contents
  then Some profile.format
  else if Mt940_csv_companion.recognizes profile contents
  then Some profile.companion_format
  else None
;;

let parse (profile : Mt940_profile.t) engine ~context:_ ~format contents =
  let open Or_error.Let_syntax in
  if String.equal format profile.format
  then (
    let%bind (document : Mt940_source.t) = Mt940_source.parse engine contents in
    let%map events =
      List.map document.transactions ~f:(fun (tx : Mt940_transaction.t) ->
        Source_event.create_dated
          ~venue:profile.institution
          ~account:(Mt940_transaction.account_identity engine tx)
          ~kind:profile.kind
          ~source_id:tx.source_id
          ~occurred_on:tx.booked_on
          ~payload:tx.payload
          ())
      |> Or_error.combine_errors
    in
    { Document.empty with
      statements =
        [ { source_account =
              Json.encode
                (Jsont.list Jsont.string)
                [ profile.institution
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
      (Mt940_csv_companion.parse profile)
      ~number:(fun (row : Mt940_csv_companion.row) -> row.row_number)
      ~fields:(fun row -> row.fields)
      contents
  else Or_error.errorf "unsupported statement format: %s" format
;;

let companion (profile : Mt940_profile.t) engine : Companion.t =
  Adapter_support.companion
    ~format:profile.companion_format
    ~policy:profile.matching_policy
    ~of_fields:(Mt940_csv_companion.of_fields profile)
    ~input:(fun r row ->
      { Mt940_csv_companion.row_id = r.row_id; document_id = r.document_id; row })
    ~candidate:(fun id event ->
      let open Or_error.Let_syntax in
      let%map tx = Mt940_transaction.source_transaction engine event in
      id, tx)
    ~match_rows:(Mt940_csv_companion.match_rows profile)
    ~header:(fun _ -> Ok profile.header)
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

let make (profile : Mt940_profile.t) : (module Adapter.S) =
  let engine = Mt940_transaction.make profile in
  Adapter_support.adapter
    ~institution:profile.institution
    ~aliases:profile.aliases
    ~kinds:[ profile.kind ]
    ~formats:[ profile.format; profile.companion_format ]
    ~takes_account_label:false
    ~overlap:Adapter.Day_inventory
    ~companions:[ companion profile engine ]
    ~decode:(decode profile engine)
    ~probe:(probe profile)
    ~parse:(parse profile engine)
    ()
;;
