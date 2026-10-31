open Base

let make ?(institution = "csv") ?profile () =
  Adapter_support.csv_adapter
    ~institution
    ~format:"generic_csv"
    ~kind:"csv-movement"
    ~overlap:Adapter.Day_inventory
    ~takes_account_label:true
    ~account_label_error:"CSV imports require an account label with --account-id"
    ~recognizes:(fun contents ->
      Option.value_map profile ~default:false ~f:(fun (p : Csv_profile.t) ->
        Csv_reader.recognizes ~separator:p.separator ~trim:false ~header:p.header contents))
    ~parse:(fun ~context contents ->
      let open Or_error.Let_syntax in
      let%bind profile =
        Or_error.of_option profile ~error:(Error.of_string "CSV layout is not configured")
      in
      let%bind account_id =
        Or_error.of_option
          context.Adapter.account_label
          ~error:(Error.of_string "CSV account label is required")
      in
      let%bind rows = Csv_profile.parse profile contents in
      let%map events =
        Csv_movement.events ~institution ~header:profile.header ~account_id rows
      in
      { Document.empty with
        events
      ; context =
          [ "account_id", account_id
          ; "csv_profile", Json.encode Csv_profile.codec profile
          ]
      })
    ~decode:Csv_movement.decode
    ()
;;

include (val make ())
