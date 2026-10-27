open Base

type t =
  { adapters : (module Adapter.S) Map.M(String).t
  ; kinds : Set.M(String).t
  }

let create adapters =
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
    Ok { adapters; kinds }
;;

let institutions t = Map.keys t.adapters
let is_registered t institution = Map.mem t.adapters institution

let aliases t institution =
  match Map.find t.adapters institution with
  | None -> [ institution ]
  | Some (module A) -> institution :: A.aliases
;;

let is_movement t (event : Source_event.t) = Set.mem t.kinds event.kind

let movement t (event : Source_event.t) =
  match Map.find t.adapters event.venue with
  | Some (module A) when List.mem A.kinds event.kind ~equal:String.equal -> A.decode event
  | Some _ ->
    Or_error.errorf "institution %s does not produce %s events" event.venue event.kind
  | None -> Or_error.errorf "no adapter is registered for institution %s" event.venue
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
