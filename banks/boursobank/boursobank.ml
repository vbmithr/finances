open Base

(* BoursoBank is an OFX bank whose CSV export enriches the OFX movements. *)
include Ofx_bank

let institution = "boursobank"
let aliases = [ "boursorama"; "bourso" ]
let csv = "csv"
let formats = [ ofx; csv ]

let settlement_card_hint (movement : Movement.t) =
  match movement.descriptions with
  | [ description ] ->
    Option.bind
      (List.find_map
         [ "Relevé différé Carte "; "Relev{ diff{r{ Carte " ]
         ~f:(fun prefix -> String.chop_prefix (String.strip description) ~prefix))
      ~f:(fun card -> Option.some_if (not (String.is_empty card)) card)
  | _ -> None
;;

let withdrawal_reversal_hint (movement : Movement.t) =
  Option.value_map (List.hd movement.descriptions) ~default:false ~f:(fun text ->
    String.is_prefix (String.uppercase (String.strip text)) ~prefix:"ANNUL RETRAIT DAB ")
;;

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

let companion =
  Csv_companion.make
    ~snapshot:false
    ~format:csv
    ~policy:"bourso-csv-exact-v1"
    ~layout:Bank_csv.profile
;;

let companions = [ companion ]
