open Base

type rule =
  { description : string
  ; account_type : string option
  ; case_sensitive : bool
  ; kind : Movement_type.t
  ; match_type : string option
  ; compiled : Re.re
  ; field : string option
  }

(** A rule on the bank's own transaction code, such as a CSV type column. *)
type reported_action =
  | Kind of Movement_type.t option
  | Other_prefix of string

type reported_rule =
  { code_pattern : string
  ; code_case_sensitive : bool
  ; code_action : reported_action
  ; code_compiled : Transaction_pattern.t
  }

type t =
  { institution : string
  ; rules : rule list
  ; reported : reported_rule list
  }

let make_reported_rule pattern case_sensitive action =
  let open Or_error.Let_syntax in
  let%bind compiled = Transaction_pattern.compile ~case_sensitive pattern in
  let%map () =
    match action with
    | Kind _ -> Ok ()
    | Other_prefix prefix ->
      let%map _ = Validation.text "provider type prefix" prefix in
      ()
  in
  { code_pattern = pattern
  ; code_case_sensitive = case_sensitive
  ; code_action = action
  ; code_compiled = compiled
  }
;;

let make_rule ?field description account_type case_sensitive kind =
  let open Or_error.Let_syntax in
  let%bind _ = Validation.text "description expression" description in
  let%bind () =
    match field with
    | None -> Ok ()
    | Some name ->
      let%map _ = Validation.text "field" name in
      ()
  in
  let%bind () =
    match account_type with
    | None -> Ok ()
    | Some value ->
      let%map _ = Validation.text "account type" value in
      ()
  in
  let%bind _ = Validation.text "movement type" kind in
  let%bind () =
    if String.equal kind "unknown"
    then Or_error.error_string "unknown is not a movement type"
    else Ok ()
  in
  let%bind compiled =
    Or_error.try_with (fun () ->
      Re.Perl.compile_pat ~opts:(if case_sensitive then [] else [ `Caseless ]) description)
    |> Or_error.tag ~tag:"invalid source type expression"
  in
  Ok
    { description
    ; account_type
    ; case_sensitive
    ; kind = Movement_type.of_string kind
    ; match_type = None
    ; compiled
    ; field
    }
;;

let rule_codec =
  Jsont.Object.map (fun description account_type case_sensitive kind ->
    make_rule description account_type case_sensitive kind |> Json.or_error)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "account_type" (Jsont.option Jsont.string) ~enc:(fun r ->
    r.account_type)
  |> Jsont.Object.mem "case_sensitive" Jsont.bool ~enc:(fun r -> r.case_sensitive)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun r -> Movement_type.to_string r.kind)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map (fun () institution rules ->
    let institution = Validation.token "institution" institution |> Json.or_error in
    { institution; rules; reported = [] })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "rules" (Jsont.list rule_codec) ~enc:(fun p -> p.rules)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let of_json text = Json.decode codec text
let to_json t = Json.encode ~format:Jsont.Indent codec t
let institution t = t.institution
let rules t = t.rules
let with_match_type r match_type = { r with match_type }
let create ?(reported = []) ~institution rules = { institution; rules; reported }
let reported_rules t = t.reported

let empty institution =
  Or_error.map (Validation.token "institution" institution) ~f:(fun institution ->
    { institution; rules = []; reported = [] })
;;

(* A rule tests the main description unless it names a field; it matches when any
   value under that name does. *)
let matches (r : rule) (movement : Movement.t) =
  Option.value_map
    r.account_type
    ~default:true
    ~f:(String.equal movement.account.account_type)
  && Option.value_map r.match_type ~default:true ~f:(fun expected ->
    match expected, movement.movement_type with
    | "unknown", None -> true
    | "unknown", Some _ -> false
    | _, Some kind -> String.equal expected (Movement_type.to_string kind)
    | _, None -> false)
  && List.exists
       (Movement.field_values movement (Option.value r.field ~default:"description"))
       ~f:(Re.execp r.compiled)
;;

let reported_type t code =
  List.find_map t.reported ~f:(fun r ->
    Option.some_if
      (Transaction_pattern.matches
         r.code_compiled
         ~display_name:None
         ~descriptions:[ code ])
      (match r.code_action with
       | Kind kind -> kind
       | Other_prefix prefix -> Some (Movement_type.Other (prefix ^ code))))
  |> Option.join
;;

(* The bank's own code gives the parser-reported type; description rules, whose
   [match_type] conditions see that type, may then refine it. *)
let apply t movement =
  let movement =
    Option.value_map movement.Movement.reported_code ~default:movement ~f:(fun code ->
      Option.value_map (reported_type t code) ~default:movement ~f:(fun kind ->
        Movement.with_movement_type movement (Some kind)))
  in
  match List.find t.rules ~f:(fun r -> matches r movement) with
  | None -> movement
  | Some r -> Movement.with_movement_type movement (Some r.kind)
;;
