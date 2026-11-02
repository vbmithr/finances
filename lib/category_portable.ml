open Base

type decision =
  { institution : string
  ; source_account : string
  ; kind : string
  ; source_id : string
  ; occurred : string
  ; payload_sha256 : string
  ; fields_sha256 : string
  ; movement_role : string
  ; account : string option
  ; reason : string option
  ; type_override : string option
  }

type t =
  { categories : string list
  ; rules : Classification_rule.t list
  ; decisions : decision list
  }

let decision ~legacy =
  let map =
    Jsont.Object.map
      (fun
          institution
           source_account
           kind
           source_id
           occurred
           payload_sha256
           fields_sha256
           movement_role
           account
           reason
           type_override
         ->
         { institution
         ; source_account
         ; kind
         ; source_id
         ; occurred
         ; payload_sha256
         ; fields_sha256
         ; movement_role
         ; account
         ; reason
         ; type_override
         })
    |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun r -> r.institution)
    |> Jsont.Object.mem "source_account" Jsont.string ~enc:(fun r -> r.source_account)
    |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun r -> r.kind)
    |> Jsont.Object.mem "source_id" Jsont.string ~enc:(fun r -> r.source_id)
    |> Jsont.Object.mem "occurred" Jsont.string ~enc:(fun r -> r.occurred)
    |> Jsont.Object.mem "payload_sha256" Jsont.string ~enc:(fun r -> r.payload_sha256)
    |> Jsont.Object.mem "fields_sha256" Jsont.string ~enc:(fun r -> r.fields_sha256)
  in
  let map =
    if legacy
    then
      Jsont.Object.mem
        "movement_role"
        Jsont.string
        ~dec_absent:"primary"
        ~enc:(fun r -> r.movement_role)
        map
    else Jsont.Object.mem "movement_role" Jsont.string ~enc:(fun r -> r.movement_role) map
  in
  map
  |> Jsont.Object.opt_mem "account" Jsont.string ~enc:(fun r -> r.account)
  |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun r -> r.reason)
  |> Jsont.Object.opt_mem "type_override" Jsont.string ~enc:(fun r -> r.type_override)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let rule =
  Jsont.Object.map
    (fun
        name
         mode
         account_type
         priority
         pattern
         institution
         match_type
         case_sensitive
         action
         target
         set_type
         field
       ->
       let open Or_error.Let_syntax in
       let result =
         let%bind mode =
           match mode with
           | "automatic" -> Ok Classification_rule.Automatic
           | "review" -> Ok Review
           | "disabled" -> Ok Disabled
           | _ -> Or_error.error_string "invalid rule mode"
         in
         let%bind action = Classification_rule.action_of_fields action target set_type in
         Classification_rule.create
           ~name
           ~mode
           ~account_type
           ~priority
           ~pattern
           ~institution
           ~match_type:(Option.map match_type ~f:Classification_rule.condition_of_string)
           ~case_sensitive
           ~action
           ?field
           ()
       in
       Json.or_error result)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (r : Classification_rule.t) ->
    r.Classification_rule.name)
  |> Jsont.Object.mem "mode" Jsont.string ~enc:(fun (r : Classification_rule.t) ->
    match r.mode with
    | Automatic -> "automatic"
    | Review -> "review"
    | Disabled -> "disabled")
  |> Jsont.Object.mem
       "account_type"
       (Jsont.option Jsont.string)
       ~enc:(fun (r : Classification_rule.t) -> r.account_type)
  |> Jsont.Object.mem "priority" Jsont.int ~enc:(fun (r : Classification_rule.t) ->
    r.priority)
  |> Jsont.Object.mem
       "pattern"
       (Jsont.option Jsont.string)
       ~enc:(fun (r : Classification_rule.t) -> r.pattern)
  |> Jsont.Object.mem
       "institution"
       (Jsont.option Jsont.string)
       ~enc:(fun (r : Classification_rule.t) -> r.institution)
  |> Jsont.Object.mem
       "match_type"
       (Jsont.option Jsont.string)
       ~enc:(fun (r : Classification_rule.t) ->
         Option.map r.match_type ~f:Classification_rule.condition_name)
  |> Jsont.Object.mem "case_sensitive" Jsont.bool ~enc:(fun (r : Classification_rule.t) ->
    r.case_sensitive)
  |> Jsont.Object.mem "action" Jsont.string ~enc:(fun (r : Classification_rule.t) ->
    Classification_rule.action_name r.action)
  |> Jsont.Object.mem "target" Jsont.string ~enc:(fun (r : Classification_rule.t) ->
    Classification_rule.action_target r.action)
  |> Jsont.Object.mem
       "set_type"
       (Jsont.option Jsont.string)
       ~enc:(fun (r : Classification_rule.t) ->
         match r.action with
         | Type_and_category (t, _) -> Some (Movement_type.to_string t)
         | _ -> None)
  |> Jsont.Object.mem
       "field"
       (Jsont.option Jsont.string)
       ~enc:(fun (r : Classification_rule.t) -> r.field)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map (fun () categories rules decisions -> { categories; rules; decisions })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "categories" (Jsont.list Jsont.string) ~enc:(fun r -> r.categories)
  |> Jsont.Object.mem "rules" (Jsont.list rule) ~enc:(fun r -> r.rules)
  |> Jsont.Object.mem
       "decisions"
       (Jsont.list (decision ~legacy:true))
       ~enc:(fun r -> r.decisions)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

(* Scoped version 2 exports deliberately have distinct fields.  The combined
   version 1 codec remains available to read existing backups. *)
let categories_codec =
  Jsont.Object.map (fun () categories -> { categories; rules = []; decisions = [] })
  |> Jsont.Object.mem "version" (Json.version 2) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "categories" (Jsont.list Jsont.string) ~enc:(fun r -> r.categories)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let rules_codec =
  Jsont.Object.map (fun () rules -> { categories = []; rules; decisions = [] })
  |> Jsont.Object.mem "version" (Json.version 2) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "rules" (Jsont.list rule) ~enc:(fun r -> r.rules)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let decisions_codec_for ~legacy version =
  Jsont.Object.map (fun () decisions -> { categories = []; rules = []; decisions })
  |> Jsont.Object.mem "version" (Json.version version) ~enc:(fun _ -> ())
  |> Jsont.Object.mem
       "decisions"
       (Jsont.list (decision ~legacy))
       ~enc:(fun r -> r.decisions)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let decisions_codec = decisions_codec_for ~legacy:false 4
let previous_decisions_codec = decisions_codec_for ~legacy:true 3
let legacy_decisions_codec = decisions_codec_for ~legacy:true 2
