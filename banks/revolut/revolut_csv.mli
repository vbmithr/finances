open Base

type transaction = private
  { account_id : string
  ; currency : string
  ; started_at : string
  ; completed_at : string
  ; booked_on : Date.t
  ; transaction_type : string
  ; description : string
  ; principal : Quantity.t
  ; fee : Quantity.t
  ; amount : Quantity.t
  ; balance : Quantity.t option
  ; occurrence : int
  ; source_id : string
  ; payload : string
  }

type t = private
  { contents : string
  ; transactions : transaction list
  }

val recognizes : string -> bool
val parse : account_id:string -> string -> t Or_error.t
val account_identity : transaction -> string

(** Running balance is a document observation, not part of source identity;
    reconstructed transactions expose None for that unavailable observation. *)
val source_transaction : Source_event.t -> transaction Or_error.t
