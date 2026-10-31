open Base

(** The banks built into [fin]. External adapters are added by building a
    custom executable with a longer list. *)
let adapters : (module Adapter.S) list =
  let presets =
    List.map Institution_preset.all ~f:(fun p ->
      Institution_definition.adapter { source = Ofx_csv p; rules = [] })
  in
  let camt_presets =
    List.map Camt_institution_preset.all ~f:(fun p ->
      Institution_definition.adapter { source = Camt_csv p; rules = [] })
  in
  let running =
    List.map Csv_running_profile.all ~f:(fun p ->
      Institution_definition.adapter { source = Running_csv p; rules = [] })
  in
  let mt940 =
    List.map Mt940_profile.all ~f:(fun p ->
      Institution_definition.adapter { source = Mt940_csv p; rules = [] })
  in
  presets @ camt_presets @ running @ mt940 @ [ (module Csv_adapter) ]
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

let reported_types =
  Json.decode (Jsont.list Reported_type_defaults.codec) Reported_type_defaults_data.json
  |> Or_error.ok_exn
;;

let registry =
  Registry.create
    ~fallback
    ~fallbacks
    ~source_typing
    ~source_evidence
    ~reported_types
    adapters
  |> Or_error.ok_exn
;;

let without_definitions definitions adapters =
  List.filter adapters ~f:(fun (module A : Adapter.S) ->
    not
      (List.exists definitions ~f:(fun definition ->
         String.equal A.institution (Institution_definition.institution definition))))
;;

let registry_with_companions ?(definitions = []) profiles =
  let adapters = without_definitions definitions adapters in
  let profiles =
    List.filter profiles ~f:(fun (name, _) ->
      not
        (List.exists definitions ~f:(fun d ->
           String.equal name (Institution_definition.institution d))))
  in
  let dynamic =
    List.map profiles ~f:(fun (institution, layout) ->
      Csv_ofx_adapter.make ~institution ~layout ())
  in
  Registry.create
    ~fallback
    ~fallbacks
    ~source_typing
    ~source_evidence
    ~reported_types
    ~fallback_institutions:(List.map profiles ~f:fst)
    (dynamic @ List.map definitions ~f:Institution_definition.adapter @ adapters)
  |> Or_error.ok_exn
;;

let registry_with_csv_profile
      ?(definitions = [])
      ?(companions = [])
      ?(institution = "csv")
      profile
  =
  let adapters = without_definitions definitions adapters in
  let companions =
    List.filter companions ~f:(fun (name, _) ->
      not
        (List.exists definitions ~f:(fun d ->
           String.equal name (Institution_definition.institution d))))
  in
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
    ~reported_types
    ~fallback_institutions:(institution :: List.map companions ~f:fst)
    ((Csv_adapter.make ~institution ~profile () :: dynamic)
     @ List.map definitions ~f:Institution_definition.adapter
     @ adapters)
  |> Or_error.ok_exn
;;

let registry_with_ofx ?(definitions = []) ?(companions = []) ~institution () =
  let adapters = without_definitions definitions adapters in
  let companions =
    List.filter companions ~f:(fun (name, _) ->
      not
        (List.exists definitions ~f:(fun d ->
           String.equal name (Institution_definition.institution d))))
  in
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
    ~reported_types
    ~fallback_institutions:(institution :: List.map companions ~f:fst)
    (extra @ dynamic @ List.map definitions ~f:Institution_definition.adapter @ adapters)
  |> Or_error.ok_exn
;;
