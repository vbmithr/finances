open Base

type t =
  { institution : string
  ; aliases : string list
  ; companion_format : string
  ; matching_policy : string
  ; snapshot : bool
  ; layout : Csv_companion_layout.t
  }

let codec =
  Jsont.Object.map
    (fun institution aliases companion_format matching_policy snapshot layout ->
       let institution = Validation.token "institution" institution |> Json.or_error in
       let aliases =
         List.map aliases ~f:(fun alias ->
           Validation.token "institution alias" alias |> Json.or_error)
       in
       let companion_format =
         Validation.token "companion format" companion_format |> Json.or_error
       in
       let matching_policy =
         Validation.token "matching policy" matching_policy |> Json.or_error
       in
       { institution; aliases; companion_format; matching_policy; snapshot; layout })
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "aliases" (Jsont.list Jsont.string) ~enc:(fun p -> p.aliases)
  |> Jsont.Object.mem "companion_format" Jsont.string ~enc:(fun p -> p.companion_format)
  |> Jsont.Object.mem "matching_policy" Jsont.string ~enc:(fun p -> p.matching_policy)
  |> Jsont.Object.mem "snapshot" Jsont.bool ~enc:(fun p -> p.snapshot)
  |> Jsont.Object.mem "layout" Csv_companion_layout.codec ~enc:(fun p -> p.layout)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let all = Json.decode (Jsont.list codec) Institution_presets_data.json |> Or_error.ok_exn

let find name =
  List.find all ~f:(fun p ->
    List.mem (p.institution :: p.aliases) name ~equal:String.equal)
;;
