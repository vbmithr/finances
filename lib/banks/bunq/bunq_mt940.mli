open Core

type t = private
  { contents : string
  ; account_id : string
  ; currency : string
  ; opening_on : Date.t
  ; closing_on : Date.t
  ; opening : Bigdecimal.t
  ; closing : Bigdecimal.t
  ; transactions : Bunq_transaction.t list
  }

val recognizes : string -> bool
val parse : string -> t Or_error.t
