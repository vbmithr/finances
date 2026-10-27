open Base

type t =
  { instrument : Instrument.t
  ; quantity : Quantity.t
  ; entry_price : Quantity.t
  }

let create ~instrument ~quantity ~entry_price =
  let open Or_error.Let_syntax in
  let%bind quantity = Or_error.try_with (fun () -> Quantity.finite_exn quantity) in
  let%bind entry_price = Or_error.try_with (fun () -> Quantity.finite_exn entry_price) in
  if Quantity.(entry_price <= zero)
  then Or_error.error_string "entry price must be positive"
  else Ok { instrument; quantity; entry_price }
;;

let unrealized_pnl_in_quote ~mark t =
  let open Or_error.Let_syntax in
  let%bind mark = Or_error.try_with (fun () -> Quantity.finite_exn mark) in
  if Quantity.(mark <= zero)
  then Or_error.error_string "mark price must be positive"
  else Ok Quantity.(t.quantity * t.instrument.multiplier * (mark - t.entry_price))
;;
