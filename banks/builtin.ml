open Base

(** The banks built into [fin]. External adapters are added by building a
    custom executable with a longer list. *)
let adapters : (module Adapter.S) list =
  [ (module Boursobank); (module Bunq); (module Revolut); (module Wise) ]
;;

let registry = Registry.create adapters |> Or_error.ok_exn
