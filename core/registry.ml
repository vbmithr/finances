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
      if List.is_empty A.kinds
      then Or_error.errorf "adapter %s declares no event kinds" institution
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
