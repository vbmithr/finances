open Base

(** A source row and its proposed movements before it has a database event ID. *)
type leg = private
  { role : string
  ; account : string option (** Ledger account supplied by the source interpreter. *)
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  }

type row = private
  { source : Source_event.t
  ; legs : leg list
  }

val leg
  :  account:string option
  -> role:string
  -> commodity:Commodity.t
  -> quantity:Quantity.t
  -> leg Or_error.t

val row : source:Source_event.t -> legs:leg list -> row Or_error.t
