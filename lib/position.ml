open Core

type t =
  { instrument : Instrument.t
  ; quantity : Bigdecimal.t
  ; entry_price : Bigdecimal.t
  }

let create ~instrument ~quantity ~entry_price =
  if Bigdecimal.(entry_price <= zero)
  then Or_error.error_string "entry price must be positive"
  else Ok { instrument; quantity; entry_price }
;;

let unrealized_pnl_in_quote ~mark t =
  if Bigdecimal.(mark <= zero)
  then Or_error.error_string "mark price must be positive"
  else Ok Bigdecimal.(t.quantity * t.instrument.multiplier * (mark - t.entry_price))
;;
