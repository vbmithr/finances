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
  ; format : string (** The adapter's format identifier. *)
  ; outcome : outcome
  ; document_id : int64 option
  ; sha256 : string option
  ; inserted : int
  ; existing : int
  ; message : string option
  ; attempted_at : string
  }

val outcome_name : outcome -> string
val outcome_of_string : string -> outcome Or_error.t

(** Recorded when no registered bank recognizes a file. *)
val unknown_format : string
