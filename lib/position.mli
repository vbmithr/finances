open Base

(** A position observation, separate from collateral and accounting. Signed
    quantity is positive for a long and negative for a short. *)
type t = private
  { instrument : Instrument.t
  ; quantity : Quantity.t
  ; entry_price : Quantity.t
  }

val create
  :  instrument:Instrument.t
  -> quantity:Quantity.t
  -> entry_price:Quantity.t
  -> t Or_error.t

val unrealized_pnl_in_quote : mark:Quantity.t -> t -> Quantity.t Or_error.t
