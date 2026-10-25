open Core

(** A position observation, separate from collateral and accounting. Signed
    quantity is positive for a long and negative for a short. *)
type t = private
  { instrument : Instrument.t
  ; quantity : Bigdecimal.t
  ; entry_price : Bigdecimal.t
  }

val create
  :  instrument:Instrument.t
  -> quantity:Bigdecimal.t
  -> entry_price:Bigdecimal.t
  -> t Or_error.t

val unrealized_pnl_in_quote : mark:Bigdecimal.t -> t -> Bigdecimal.t Or_error.t
