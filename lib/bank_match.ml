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

let state_name = function
  | Matched -> "matched"
  | Unmatched -> "unmatched"
  | Ambiguous -> "ambiguous"
  | Conflict -> "conflict"
;;

let state_of_string = function
  | "matched" -> Ok Matched
  | "unmatched" -> Ok Unmatched
  | "ambiguous" -> Ok Ambiguous
  | "conflict" -> Ok Conflict
  | _ -> Or_error.error_string "invalid CSV match state"
;;
