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
    ; Bundled_definitions.cdc
    ; Bundled_definitions.kraken
    ; Bundled_definitions.fdm
    ; Bundled_definitions.ethereum
    ; Bundled_definitions.polygon
    ]
    ~f:(fun json -> Json.decode Institution_definition.codec json |> Or_error.ok_exn)
;;

let definition name =
  List.find definitions ~f:(fun d ->
    List.mem (Institution_definition.names d) name ~equal:String.equal)
;;

(** The type a bundled institution declares in its definition. *)
let institution_type name = Option.map (definition name) ~f:Institution_definition.typ

let records_of = definition

(* Typed accessors for tests and tools that need one bundled profile. *)
let camt_profile name =
  Option.bind (records_of name) ~f:(fun r ->
    match r.statement with
    | Some (Camt p) -> Some p
    | _ -> None)
;;

let mt940_profile name =
  Option.bind (records_of name) ~f:(fun r ->
    match r.statement with
    | Some (Mt940 p) -> Some p
    | _ -> None)
;;

let csv_definition name = Option.bind (records_of name) ~f:(fun r -> List.hd r.csv)

let adapters : (module Adapter.S) list =
  List.map definitions ~f:Institution_definition.adapter
;;

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
