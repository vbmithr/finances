open Base

(** Balanced exchange entry with one trading posting per commodity. *)
val exchange
  :  date:Date.t
  -> description:string
  -> (string * Commodity.t * string * Quantity.t) list
  -> Accounting.entry Or_error.t

(** Direct own-account transfer in one canonical commodity. *)
val transfer
  :  date:Date.t
  -> description:string
  -> (string * Commodity.t * string * Quantity.t) list
  -> Accounting.entry Or_error.t
