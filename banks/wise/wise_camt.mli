open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : Iso4217.t
  }

type transaction = private
  { account : account
  ; reference : string
  ; booked_at : Ptime.t
  ; booked_at_source : string
    (** Original offset-bearing lexeme for evidence and local CSV matching. *)
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
  ; period_start : Ptime.t
  ; period_end : Ptime.t
  ; opening_at : Ptime.t
  ; closing_at : Ptime.t
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
