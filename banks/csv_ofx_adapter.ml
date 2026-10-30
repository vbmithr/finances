open Base

let make ~institution ~layout () : (module Adapter.S) =
  let schema = Csv_companion_layout.schema layout in
  let csv = "generic-companion-csv" in
  let companion =
    Csv_companion.make ~snapshot:true ~format:csv ~policy:"csv-exact-v1" ~layout
  in
  (module struct
    let institution = institution
    let aliases = []
    let kinds = Ofx_bank.kinds
    let decode = Ofx_bank.decode
    let settlement_card_hint = Ofx_bank.settlement_card_hint
    let withdrawal_reversal_hint = Ofx_bank.withdrawal_reversal_hint
    let formats = [ Ofx_bank.ofx; csv ]
    let takes_account_label = false
    let overlap = Adapter.Source_ids
    let companions = [ companion ]

    let probe ~filename contents =
      match Ofx_bank.probe ~filename contents with
      | Some format -> Some format
      | None -> Option.some_if (Csv_schema.recognizes schema contents) csv
    ;;

    let parse ~context:_ ~format contents =
      if String.equal format Ofx_bank.ofx
      then Ofx_bank.parse ~institution contents
      else if String.equal format csv
      then
        let open Or_error.Let_syntax in
        let%map document =
          Adapter_support.companion_document
            (Csv_schema.parse schema)
            ~number:(fun (row : Csv_companion_layout.row) -> row.row_number)
            ~fields:(fun row -> row.fields)
            contents
        in
        { document with
          context = [ "companion_profile", Json.encode Csv_companion_layout.codec layout ]
        }
      else Or_error.errorf "unsupported statement format: %s" format
    ;;
  end)
;;
