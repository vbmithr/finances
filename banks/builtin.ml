open Base

(** The banks built into [fin]. External adapters are added by building a
    custom executable with a longer list. *)
let adapters : (module Adapter.S) list =
  [ (module Boursobank)
  ; (module Bunq)
  ; (module Revolut)
  ; (module Wise)
  ; (module Csv_adapter)
  ]
;;

let fallback = "csv-movement", Csv_movement.decode

let source_typing =
  Json.decode (Jsont.list Source_typing.codec) Source_type_defaults.json
  |> Or_error.ok_exn
;;

let registry = Registry.create ~fallback ~source_typing adapters |> Or_error.ok_exn

let registry_with_csv_profile ?(institution = "csv") profile =
  let adapters =
    List.filter adapters ~f:(fun (module A : Adapter.S) ->
      not (String.equal A.institution institution))
  in
  Registry.create
    ~fallback
    ~source_typing
    (Csv_adapter.make ~institution ~profile () :: adapters)
  |> Or_error.ok_exn
;;
