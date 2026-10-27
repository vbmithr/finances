open Base

type t = private
  { contents : string
  ; account_id : string
  ; currency : Iso4217.t
  ; opening_on : Date.t
  ; closing_on : Date.t
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; transactions : Bunq_transaction.t list
  }

val recognizes : string -> bool
val parse : string -> t Or_error.t
