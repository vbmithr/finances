open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : string
  }

type transaction = private
  { account : account
  ; reference : string
  ; booked_at : string
  ; instant_ns : string
  ; occurred_at_ms : int64
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Quantity.t
  ; credit : bool
  ; description : string
  ; source_id : string
  ; payload : string
  }

type statement = private
  { account : account
  ; statement_id : string
  ; period_start : string
  ; period_end : string
  ; opening_at : string
  ; closing_at : string
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; transactions : transaction list
  }

type t = private
  { contents : string
  ; statements : statement list
  }

val parse : string -> t Or_error.t
val account_identity : account -> string
val normalize_reference : description:string -> string -> string
val local_millisecond : string -> string
val source_transaction : Source_event.t -> transaction Or_error.t
