open Base

(** The banks built into [fin], one definition each: source profile and default
    rules. [fin institution definition show] prints the same files. External
    adapters are added by building a custom executable with a longer list. *)
let definitions : Institution_definition.t list =
  List.map
    [ Bundled_definitions.boursobank
    ; Bundled_definitions.wise
    ; Bundled_definitions.revolut
    ; Bundled_definitions.bunq
    ]
    ~f:(fun json -> Json.decode Institution_definition.codec json |> Or_error.ok_exn)
;;

(** Movement CSV sources (one source row, one or two commodity movements). They
    are data like the banks and register as adapters like the banks. *)
let movement_profiles : Movement_csv.profile list =
  List.map [ Bundled_definitions.cdc ] ~f:(fun json ->
    Json.decode Movement_csv.profile_codec json |> Or_error.ok_exn)
;;

let movement_profile name =
  List.find movement_profiles ~f:(fun p ->
    List.mem (Movement_csv.names p) name ~equal:String.equal)
;;

let definition name =
  List.find definitions ~f:(fun d ->
    List.mem (Institution_definition.names d) name ~equal:String.equal)
;;

(** The type a bundled institution declares in its definition or movement profile. *)
let institution_type name =
  match definition name, movement_profile name with
  | Some d, _ -> Some (Institution_definition.institution_type d)
  | None, Some profile -> Some profile.Movement_csv.institution_type
  | None, None ->
    List.find_map (Lazy.force Csv_converter.bundled) ~f:(fun definition ->
      Option.some_if
        (String.equal (Csv_converter.institution definition) name)
        (Csv_converter.institution_type definition))
;;

(* Typed accessors for tests and tools that need one bundled profile. *)
let ofx_preset name =
  Option.bind (definition name) ~f:(fun d ->
    match d.source with
    | Ofx_csv p -> Some p
    | Camt_csv _ | Csv _ | Running_csv _ | Mt940_csv _ -> None)
;;

let camt_preset name =
  Option.bind (definition name) ~f:(fun d ->
    match d.source with
    | Camt_csv p -> Some p
    | Ofx_csv _ | Csv _ | Running_csv _ | Mt940_csv _ -> None)
;;

let running_profile name =
  Option.bind (definition name) ~f:(fun d ->
    match d.source with
    | Running_csv p -> Some p
    | Ofx_csv _ | Camt_csv _ | Csv _ | Mt940_csv _ -> None)
;;

let mt940_profile name =
  Option.bind (definition name) ~f:(fun d ->
    match d.source with
    | Mt940_csv p -> Some p
    | Ofx_csv _ | Camt_csv _ | Csv _ | Running_csv _ -> None)
;;

(* Institutions imported through records. The others move here one at a time, once the records
   of their real files give what their adapter gives. *)
let records_institutions =
  List.filter (Lazy.force Csv_converter.bundled) ~f:(fun definition ->
    List.mem
      [ "fdm"; "cdc"; "revolut" ]
      (Csv_converter.institution definition)
      ~equal:String.equal)
;;

let adapters : (module Adapter.S) list =
  List.filter definitions ~f:(fun d ->
    not
      (List.exists records_institutions ~f:(fun definition ->
         String.equal
           (Csv_converter.institution definition)
           (Institution_definition.institution d))))
  |> List.map ~f:Institution_definition.adapter
  |> fun old_adapters ->
  old_adapters
  @ List.map records_institutions ~f:(fun definition ->
    Records_adapter.make
      ~institution:(Csv_converter.institution definition)
      ~overlap:(Csv_converter.overlap definition)
      [ Records_adapter.of_definition "csv" definition ])
  @ [ (module Csv_adapter) ]
;;

let companion_layout name = Option.map (ofx_preset name) ~f:(fun p -> p.layout)
let fallback = "csv-movement", Csv_movement.decode
let fallbacks = [ "bank-transaction", Ofx_bank.decode ]
let type_rules = List.concat_map definitions ~f:Institution_definition.default_type_rules

let source_evidence =
  List.concat_map definitions ~f:Institution_definition.default_source_evidence
;;

let registry =
  Registry.create ~fallback ~fallbacks ~type_rules ~source_evidence adapters
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
    ~type_rules
    ~source_evidence
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
    ~type_rules
    ~source_evidence
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
    ~type_rules
    ~source_evidence
    ~fallback_institutions:(institution :: List.map companions ~f:fst)
    (extra @ dynamic @ List.map definitions ~f:Institution_definition.adapter @ adapters)
  |> Or_error.ok_exn
;;
