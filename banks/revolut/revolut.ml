open Base

(* The export does not name its account; the user's label stands for it and is
   kept with the document. *)
include
  (val Adapter_support.csv_adapter
         ~institution:"revolut"
         ~format:"revolut_csv"
         ~kind:"revolut-transaction"
         ~overlap:Adapter.Day_inventory
         ~takes_account_label:true
         ~account_label_error:
           "Revolut CSV files do not say which Revolut account they come from: name it \
            with --account-id LABEL, for example main, and use the same label every \
            time."
         ~recognizes:Revolut_csv.recognizes
         ~parse:(fun ~context contents ->
           let open Or_error.Let_syntax in
           let account_id = Option.value_exn context.Adapter.account_label in
           let%bind (document : Revolut_csv.t) = Revolut_csv.parse ~account_id contents in
           let%map events =
             List.map document.transactions ~f:(fun (tx : Revolut_csv.transaction) ->
               Source_event.create_dated
                 ~venue:"revolut"
                 ~account:(Revolut_csv.account_identity tx)
                 ~kind:"revolut-transaction"
                 ~source_id:tx.source_id
                 ~occurred_on:tx.booked_on
                 ~payload:tx.payload)
             |> Or_error.combine_errors
           in
           { Document.empty with events; context = [ "account_id", account_id ] })
         ~decode:(fun event ->
           let open Or_error.Let_syntax in
           let%bind tx = Revolut_csv.source_transaction event in
           Movement.create
             ~account:
               { bank_id = "revolut"
               ; account_id = tx.account_id
               ; account_type = "current-" ^ Iso4217.to_alpha_code tx.currency
               ; currency = tx.currency
               }
             ~own_iban:(Adapter_support.own_iban tx.account_id)
             ~booked_on:tx.booked_on
             ~amount:tx.amount
             ~fee:tx.fee
             [ tx.description ])
         ())
