open Base

let make (profile : Csv_running_profile.t) =
  let module Source = Csv_running_source.Make (struct
      let profile = profile
    end)
  in
  Adapter_support.csv_adapter
    ~institution:profile.institution
    ~aliases:profile.aliases
    ~format:profile.format
    ~kind:profile.kind
    ~overlap:Adapter.Day_inventory
    ~takes_account_label:true
    ~account_label_error:
      (profile.name ^ " CSV import requires an account label with --account-id")
    ~recognizes:Source.recognizes
    ~parse:(fun ~context contents ->
      let open Or_error.Let_syntax in
      let account_id = Option.value_exn context.Adapter.account_label in
      let%bind (document : Source.t) = Source.parse ~account_id contents in
      let%map events =
        List.map document.transactions ~f:(fun (tx : Source.transaction) ->
          Source_event.create_dated
            ~venue:profile.institution
            ~account:(Source.account_identity tx)
            ~kind:profile.kind
            ~source_id:tx.source_id
            ~occurred_on:tx.booked_on
            ~fields:tx.fields
            ~payload:tx.payload
            ())
        |> Or_error.combine_errors
      in
      { Document.empty with events; context = [ "account_id", account_id ] })
    ~decode:(fun event ->
      let open Or_error.Let_syntax in
      let%bind tx = Source.source_transaction event in
      Movement.create
        ~account:
          { bank_id = profile.institution
          ; account_id = tx.account_id
          ; account_type = profile.account_prefix ^ Iso4217.to_alpha_code tx.currency
          ; currency = tx.currency
          }
        ~own_iban:(Adapter_support.own_iban tx.account_id)
        ~booked_on:tx.booked_on
        ~amount:tx.amount
        ~fee:tx.fee
        [ tx.description ])
    ()
;;
