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

let csv_definition name =
  List.find (Lazy.force Csv_converter.bundled) ~f:(fun definition ->
    String.equal (Csv_converter.institution definition) name)
;;

let csv_source name =
  Option.map (csv_definition name) ~f:(Records_adapter.of_definition "csv")
;;

(* Institutions imported through records, with how overlapping files reconcile and what can read
   their files. The others move here one at a time, once the records of their real files give
   what their adapter gives. *)
let records_institutions
  : (string * string list * Adapter.overlap * Records_adapter.source list) list
  =
  let csv_only name =
    Option.map (csv_definition name) ~f:(fun definition ->
      name, [], Csv_converter.overlap definition, Option.to_list (csv_source name))
  in
  let bunq =
    Option.map (mt940_profile "bunq") ~f:(fun profile ->
      let mt940 : Records_adapter.source =
        { format = "mt940"
        ; recognizes =
            (fun ~filename:_ contents -> Mt940_converter.recognizes profile contents)
        ; convert = Mt940_converter.convert profile
        ; tags = []
        ; journal_description = None
        }
      in
      "bunq", [], Adapter.Day_inventory, mt940 :: Option.to_list (csv_source "bunq"))
  in
  let ofx : Records_adapter.source =
    { format = "ofx"
    ; recognizes =
        (* A broken file named .ofx is meant to be one, and should say so. *)
        (fun ~filename contents ->
          Ofx_converter.recognizes contents
          || String.equal (Adapter_support.extension filename) ".ofx")
    ; convert = Ofx_converter.convert ~institution:"boursobank"
    ; tags = []
    ; journal_description = None
    }
  in
  let boursobank =
    Option.map (ofx_preset "boursobank") ~f:(fun preset ->
      ( "boursobank"
      , preset.aliases
      , Adapter.Source_ids
      , ofx :: Option.to_list (csv_source "boursobank") ))
  in
  List.filter_opt [ csv_only "fdm"; csv_only "cdc"; csv_only "revolut"; bunq; boursobank ]
;;

let adapters : (module Adapter.S) list =
  let moved name =
    List.exists records_institutions ~f:(fun (institution, _, _, _) ->
      String.equal institution name)
  in
  (List.filter definitions ~f:(fun d ->
     not (moved (Institution_definition.institution d)))
   |> List.map ~f:Institution_definition.adapter)
  @ List.map records_institutions ~f:(fun (institution, aliases, overlap, sources) ->
    Records_adapter.make ~aliases ~institution ~overlap sources)
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
