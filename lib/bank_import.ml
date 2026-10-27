open Base

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
  ; format : string
  ; outcome : outcome
  ; document_id : int64 option
  ; sha256 : string option
  ; inserted : int
  ; existing : int
  ; message : string option
  ; attempted_at : string
  }

let outcome_name = function
  | Imported -> "imported"
  | Unchanged -> "unchanged"
  | Rejected -> "rejected"
  | Unsupported -> "unsupported"
  | Unreadable -> "unreadable"
;;

let outcome_of_string = function
  | "imported" -> Ok Imported
  | "unchanged" -> Ok Unchanged
  | "rejected" -> Ok Rejected
  | "unsupported" -> Ok Unsupported
  | "unreadable" -> Ok Unreadable
  | _ -> Or_error.error_string "invalid stored bank import outcome"
;;

(** Recorded when no registered bank recognizes a file. *)
let unknown_format = "unknown"
