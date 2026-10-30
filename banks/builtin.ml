open Base

(** The banks built into [fin]. External adapters are added by building a
    custom executable with a longer list. *)
let adapters : (module Adapter.S) list =
  let presets =
    List.map Institution_preset.all ~f:(fun (p : Institution_preset.t) ->
      Csv_ofx_adapter.make
        ~institution:p.institution
        ~aliases:p.aliases
        ~csv_format:p.companion_format
        ~policy:p.matching_policy
        ~snapshot:p.snapshot
        ~layout:p.layout
        ())
  in
  presets @ [ (module Bunq); (module Revolut); (module Wise); (module Csv_adapter) ]
;;

let companion_layout name =
  Option.map (Institution_preset.find name) ~f:(fun p -> p.layout)
;;

let fallback = "csv-movement", Csv_movement.decode
let fallbacks = [ "bank-transaction", Ofx_bank.decode ]

let source_typing =
  Json.decode (Jsont.list Source_typing.codec) Source_type_defaults.json
  |> Or_error.ok_exn
;;

let source_evidence =
  Json.decode (Jsont.list Source_evidence.default_codec) Source_evidence_defaults.json
  |> Or_error.ok_exn
;;

let registry =
  Registry.create ~fallback ~fallbacks ~source_typing ~source_evidence adapters
  |> Or_error.ok_exn
;;

let registry_with_companions profiles =
  let dynamic =
    List.map profiles ~f:(fun (institution, layout) ->
      Csv_ofx_adapter.make ~institution ~layout ())
  in
  Registry.create
    ~fallback
    ~fallbacks
    ~source_typing
    ~source_evidence
    ~fallback_institutions:(List.map profiles ~f:fst)
    (dynamic @ adapters)
  |> Or_error.ok_exn
;;

let registry_with_csv_profile ?(companions = []) ?(institution = "csv") profile =
  let dynamic =
    List.filter_map companions ~f:(fun (name, layout) ->
      Option.some_if
        (not (String.equal name institution))
        (Csv_ofx_adapter.make ~institution:name ~layout ()))
  in
  let adapters =
    List.filter adapters ~f:(fun (module A : Adapter.S) ->
      not (String.equal A.institution institution))
  in
  Registry.create
    ~fallback
    ~fallbacks
    ~source_typing
    ~source_evidence
    ~fallback_institutions:(institution :: List.map companions ~f:fst)
    ((Csv_adapter.make ~institution ~profile () :: dynamic) @ adapters)
  |> Or_error.ok_exn
;;

let registry_with_ofx ?(companions = []) ~institution () =
  let dynamic =
    List.map companions ~f:(fun (name, layout) ->
      Csv_ofx_adapter.make ~institution:name ~layout ())
  in
  let extra =
    if List.Assoc.mem companions institution ~equal:String.equal
    then []
    else [ Ofx_bank.make ~institution () ]
  in
  let adapters =
    List.filter adapters ~f:(fun (module A : Adapter.S) ->
      not (String.equal A.institution institution))
  in
  Registry.create
    ~fallback
    ~fallbacks
    ~source_typing
    ~source_evidence
    ~fallback_institutions:(institution :: List.map companions ~f:fst)
    (extra @ dynamic @ adapters)
  |> Or_error.ok_exn
;;
