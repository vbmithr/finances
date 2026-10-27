open Base

type state =
  | Matched
  | Unmatched
  | Ambiguous
  | Conflict

type result =
  { row_id : int64
  ; event_id : int64 option
  ; state : state
  ; reason : string
  ; candidates : int64 list
  }

val state_name : state -> string
val state_of_string : string -> state Or_error.t
