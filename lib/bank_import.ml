open Core

type format =
  | Ofx
  | Camt053
  | Wise_csv
  | Csv
  | Qif
  | Spreadsheet
  | Unknown

type outcome =
  | Imported
  | Unchanged
  | Rejected
  | Unsupported
  | Unreadable

type file_report =
  { attempt_id : int64
  ; institution : string
  ; path : string
  ; format : format
  ; outcome : outcome
  ; document_id : int64 option
  ; sha256 : string option
  ; inserted : int
  ; existing : int
  ; message : string option
  ; attempted_at : string
  }

let format_name = function
  | Ofx -> "ofx"
  | Camt053 -> "camt053"
  | Wise_csv -> "wise_csv"
  | Csv -> "csv"
  | Qif -> "qif"
  | Spreadsheet -> "spreadsheet"
  | Unknown -> "unknown"
;;

let outcome_name = function
  | Imported -> "imported"
  | Unchanged -> "unchanged"
  | Rejected -> "rejected"
  | Unsupported -> "unsupported"
  | Unreadable -> "unreadable"
;;

let format_of_string = function
  | "ofx" -> Ok Ofx
  | "camt053" -> Ok Camt053
  | "wise_csv" -> Ok Wise_csv
  | "csv" -> Ok Csv
  | "qif" -> Ok Qif
  | "spreadsheet" -> Ok Spreadsheet
  | "unknown" -> Ok Unknown
  | _ -> Or_error.error_string "invalid stored bank import format"
;;

let outcome_of_string = function
  | "imported" -> Ok Imported
  | "unchanged" -> Ok Unchanged
  | "rejected" -> Ok Rejected
  | "unsupported" -> Ok Unsupported
  | "unreadable" -> Ok Unreadable
  | _ -> Or_error.error_string "invalid stored bank import outcome"
;;

let detect_format ~path contents =
  let prefix = String.prefix contents 4096 |> String.strip in
  if
    String.is_substring prefix ~substring:"<OFX"
    || String.is_prefix prefix ~prefix:"OFXHEADER:"
  then Ofx
  else if String.is_substring prefix ~substring:"camt.053"
  then Camt053
  else if
    String.is_substring prefix ~substring:"TransferWise ID"
    && String.is_substring prefix ~substring:"Date Time"
  then Wise_csv
  else if String.is_prefix prefix ~prefix:"!Type:"
  then Qif
  else if
    String.is_prefix contents ~prefix:"PK\003\004"
    || String.is_prefix contents ~prefix:"\208\207\017\224"
  then Spreadsheet
  else if
    String.is_substring prefix ~substring:"Date Opération"
    && String.is_substring prefix ~substring:"Numéro Compte"
  then Csv
  else (
    match String.lowercase (Stdlib.Filename.extension path) with
    | ".ofx" -> Ofx
    | ".csv" -> Csv
    | ".qif" -> Qif
    | ".xls" | ".xlsx" -> Spreadsheet
    | _ -> Unknown)
;;
