open Core

type format =
  | Ofx
  | Camt053
  | Wise_csv
  | Bunq_mt940
  | Bunq_csv
  | Revolut_csv
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

val detect_format : path:string -> string -> format
val format_name : format -> string
val format_of_string : string -> format Or_error.t
val outcome_name : outcome -> string
val outcome_of_string : string -> outcome Or_error.t
