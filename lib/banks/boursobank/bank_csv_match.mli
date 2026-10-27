open Base

type state = Bank_match.state =
  | Matched
  | Unmatched
  | Ambiguous
  | Conflict

type input =
  { row_id : int64
  ; document_id : int64
  ; institution : string
  ; row : Bank_csv.row
  }

type candidate =
  { event_id : int64
  ; institution : string
  ; account_id : string
  ; booked_on : Date.t
  ; amount : Quantity.t
  ; descriptions : string list
  }

type result = Bank_match.result =
  { row_id : int64
  ; event_id : int64 option
  ; state : state
  ; reason : string
  ; candidates : int64 list
  }

val policy : string
val state_name : state -> string
val state_of_string : string -> state Or_error.t

(** Requires an exact institution/account/date/amount/normalized-description
    match. Repeated rows in one document cannot claim one source event.
    Conflicting nonempty metadata across observations requires review. *)
val match_rows : input list -> candidate list -> result list
