open Core

type state =
  | Confirmed
  | Rejected

val state_name : state -> string
val state_of_string : string -> state Or_error.t

type decision = private
  { first_event_id : int64
  ; second_event_id : int64
  ; state : state
  ; reason : string
  }

val create
  :  first_event_id:int64
  -> second_event_id:int64
  -> state:state
  -> reason:string
  -> decision Or_error.t

val of_json : string -> decision list Or_error.t
val account : int64 -> int64 -> string

val eligible
  :  Source_event.t
  -> Bank_transaction.t
  -> Source_event.t
  -> Bank_transaction.t
  -> bool
