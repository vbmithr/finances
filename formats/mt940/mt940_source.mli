open Base

(** The currently supported statement shape; additional MT940 dialects may
    require extending the parser. Dates remain calendar dates. *)
type t = private
  { contents : string
  ; account_id : string
  ; currency : Iso4217.t
  ; opening_on : Date.t
  ; closing_on : Date.t
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; transactions : Mt940_transaction.t list
  }

val recognizes : Mt940_profile.t -> string -> bool
val parse : Mt940_transaction.engine -> string -> t Or_error.t
