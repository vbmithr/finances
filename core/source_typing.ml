open Base

type rule =
  { description : string
  ; account_type : string option
  ; case_sensitive : bool
  ; kind : Movement_type.t
  ; compiled : Re.re
  }

type t =
  { institution : string
  ; rules : rule list
  }

let make_rule description account_type case_sensitive kind =
  let open Or_error.Let_syntax in
  let%bind _ = Validation.text "description expression" description in
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
    ; compiled
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
    { institution; rules })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "rules" (Jsont.list rule_codec) ~enc:(fun p -> p.rules)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let of_json text = Json.decode codec text
let to_json t = Json.encode ~format:Jsont.Indent codec t
let institution t = t.institution

let empty institution =
  Or_error.map (Validation.token "institution" institution) ~f:(fun institution ->
    { institution; rules = [] })
;;

let apply t movement =
  let description =
    Option.value_map (List.hd movement.Movement.descriptions) ~default:"" ~f:String.strip
  in
  match
    List.find t.rules ~f:(fun r ->
      Option.value_map
        r.account_type
        ~default:true
        ~f:(String.equal movement.account.account_type)
      && Re.execp r.compiled description)
  with
  | None -> movement
  | Some r -> Movement.with_movement_type movement (Some r.kind)
;;
