open Base

(** A linear perpetual. Prices and P&L are in the quote commodity; settlement
    conversion is a separate operation and must use an explicit FX/oracle rate. *)
type t = private
  { venue : string
  ; market : string
  ; base : string
  ; quote : string
  ; settlement : string
  ; multiplier : Quantity.t
  }

val linear_perpetual
  :  venue:string
  -> market:string
  -> base:string
  -> quote:string
  -> settlement:string
  -> multiplier:Quantity.t
  -> t Or_error.t
