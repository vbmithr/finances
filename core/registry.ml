open Base

type t =
  { adapters : (module Adapter.S) Map.M(String).t
  ; kinds : Set.M(String).t
  ; fallbacks : (string * (Source_event.t -> Movement.t Or_error.t)) list
  ; fallback_institutions : Set.M(String).t
  ; type_rules : Type_default.t list
  ; source_typing : Source_typing.t Map.M(String).t
  ; source_evidence : Source_evidence.default list
  }

let create
      ?fallback
      ?(fallbacks = [])
      ?(fallback_institutions = [])
      ?(type_rules = [])
      ?(source_evidence = [])
      adapters
  =
  let fallback_institutions = Set.of_list (module String) fallback_institutions in
  let fallbacks = Option.to_list fallback @ fallbacks in
  let open Or_error.Let_syntax in
  let%bind entries =
    List.map adapters ~f:(fun (module A : Adapter.S) ->
      let%bind institution = Validation.token "institution" A.institution in
      let companion_formats = List.map A.companions ~f:(fun c -> c.Companion.format) in
      if List.is_empty A.kinds
      then Or_error.errorf "adapter %s declares no event kinds" institution
      else if List.contains_dup A.formats ~compare:String.compare
      then Or_error.errorf "adapter %s declares a format twice" institution
      else if
        List.exists companion_formats ~f:(fun f ->
          not (List.mem A.formats f ~equal:String.equal))
        || List.contains_dup companion_formats ~compare:String.compare
      then
        Or_error.errorf
          "adapter %s has companions for undeclared or repeated formats"
          institution
      else Ok (institution, (module A : Adapter.S)))
    |> Or_error.combine_errors
  in
  match Map.of_alist (module String) entries with
  | `Duplicate_key institution ->
    Or_error.errorf "institution %s is registered twice" institution
  | `Ok adapters ->
    let kinds =
      Map.data adapters
      |> List.concat_map ~f:(fun (module A : Adapter.S) -> A.kinds)
      |> Set.of_list (module String)
    in
    let%bind profiles =
      let by_institution = Hashtbl.create (module String) in
      List.iter type_rules ~f:(fun (r : Type_default.t) ->
        Hashtbl.add_multi by_institution ~key:r.institution ~data:r);
      Hashtbl.to_alist by_institution
      |> List.map ~f:(fun (institution, rules) ->
        let ordered =
          List.sort rules ~compare:(fun (a : Type_default.t) (b : Type_default.t) ->
            match Int.compare a.priority b.priority with
            | 0 -> String.compare a.name b.name
            | c -> c)
        in
        let%map typing =
          List.map ordered ~f:(fun (r : Type_default.t) ->
            let%map rule =
              Source_typing.make_rule
                ?field:r.field
                r.pattern
                r.account_type
                r.case_sensitive
                r.target
            in
            Source_typing.with_match_type rule r.match_type)
          |> Or_error.combine_errors
        in
        institution, Source_typing.create ~institution typing)
      |> Or_error.combine_errors
    in
    Ok
      { adapters
      ; kinds
      ; fallbacks
      ; fallback_institutions
      ; type_rules
      ; source_typing = Map.of_alist_exn (module String) profiles
      ; source_evidence
      }
;;

let institutions t = Map.keys t.adapters
let is_registered t institution = Map.mem t.adapters institution

let aliases t institution =
  match Map.find t.adapters institution with
  | None -> [ institution ]
  | Some (module A) -> institution :: A.aliases
;;

let is_movement t (event : Source_event.t) = Set.mem t.kinds event.kind
let default_source_typing t institution = Map.find t.source_typing institution
let default_source_evidence t = t.source_evidence
let default_type_rules t = t.type_rules

let movement ?source_typing t (event : Source_event.t) =
  let result =
    match Map.find t.adapters event.venue with
    | Some (module A) when List.mem A.kinds event.kind ~equal:String.equal ->
      A.decode event
    | Some _ when not (Set.mem t.fallback_institutions event.venue) ->
      Or_error.errorf "institution %s does not produce %s events" event.venue event.kind
    | Some _ | None ->
      (match List.Assoc.find t.fallbacks event.kind ~equal:String.equal with
       | Some decode -> decode event
       | None ->
         Or_error.errorf
           "no adapter can decode %s for institution %s"
           event.kind
           event.venue)
  in
  let profile = Option.first_some source_typing (default_source_typing t event.venue) in
  Or_error.map result ~f:(fun movement ->
    (* The columns stored beside the event are fields too; the adapter's own
       named fields come first. *)
    let movement =
      if List.is_empty event.fields
      then movement
      else Movement.with_fields movement (movement.fields @ event.fields)
    in
    Option.value_map profile ~default:movement ~f:(fun p ->
      Source_typing.apply p movement))
;;

let find t institution = Map.find t.adapters institution

let probe t ~filename contents =
  Map.to_alist t.adapters
  |> List.filter_map ~f:(fun (institution, (module A : Adapter.S)) ->
    Option.map (A.probe ~filename contents) ~f:(fun format -> institution, format))
;;

let companions t =
  Map.to_alist t.adapters
  |> List.concat_map ~f:(fun (institution, (module A : Adapter.S)) ->
    List.map A.companions ~f:(fun companion -> institution, companion))
;;
