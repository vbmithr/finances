open Base

type action =
  | Type of Movement_type.t option
  | Other of string

type t =
  { institution : string
  ; name : string
  ; priority : int
  ; pattern : string
  ; case_sensitive : bool
  ; action : action
  }

let codec =
  Jsont.Object.map (fun institution name priority pattern case_sensitive kind target ->
    let institution = Validation.token "institution" institution |> Json.or_error in
    let name = Validation.token "rule name" name |> Json.or_error in
    let _ = Transaction_pattern.compile ~case_sensitive pattern |> Json.or_error in
    let action =
      match kind with
      | "type" ->
        Type
          (if String.equal target "unknown"
           then None
           else Some (Movement_type.of_string target))
      | "other" -> Other (Validation.text "provider type prefix" target |> Json.or_error)
      | _ -> failwith "invalid reported type action"
    in
    { institution; name; priority; pattern; case_sensitive; action })
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun p -> p.name)
  |> Jsont.Object.mem "priority" Json.integer ~enc:(fun p -> p.priority)
  |> Jsont.Object.mem "pattern" Jsont.string ~enc:(fun p -> p.pattern)
  |> Jsont.Object.mem "case_sensitive" Jsont.bool ~enc:(fun p -> p.case_sensitive)
  |> Jsont.Object.mem "action" Jsont.string ~enc:(fun p ->
    match p.action with
    | Type _ -> "type"
    | Other _ -> "other")
  |> Jsont.Object.mem "target" Jsont.string ~enc:(fun p ->
    match p.action with
    | Type kind -> Option.value_map kind ~default:"unknown" ~f:Movement_type.to_string
    | Other prefix -> prefix)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
