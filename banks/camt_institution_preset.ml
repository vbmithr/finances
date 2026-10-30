open Base

type t =
  { institution : string
  ; aliases : string list
  ; companion_format : string
  ; matching_policy : string
  ; metadata_prefix : string
  ; camt_profile : Camt_profile.t
  ; csv_profile : Csv_clock_profile.t
  }

let codec =
  Jsont.Object.map
    (fun institution aliases companion_format matching_policy metadata_prefix ->
       let institution = Validation.token "institution" institution |> Json.or_error in
       let aliases =
         List.map aliases ~f:(fun a ->
           Validation.token "institution alias" a |> Json.or_error)
       in
       let companion_format =
         Validation.token "companion format" companion_format |> Json.or_error
       in
       let matching_policy =
         Validation.token "matching policy" matching_policy |> Json.or_error
       in
       let metadata_prefix =
         Validation.token "metadata prefix" metadata_prefix |> Json.or_error
       in
       let camt_profile = Camt_profile.find institution |> Option.value_exn in
       let csv_profile = Csv_clock_profile.find institution |> Option.value_exn in
       { institution
       ; aliases
       ; companion_format
       ; matching_policy
       ; metadata_prefix
       ; camt_profile
       ; csv_profile
       })
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "aliases" (Jsont.list Jsont.string) ~enc:(fun p -> p.aliases)
  |> Jsont.Object.mem "companion_format" Jsont.string ~enc:(fun p -> p.companion_format)
  |> Jsont.Object.mem "matching_policy" Jsont.string ~enc:(fun p -> p.matching_policy)
  |> Jsont.Object.mem "metadata_prefix" Jsont.string ~enc:(fun p -> p.metadata_prefix)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let all =
  Json.decode (Jsont.list codec) Camt_institution_presets_data.json |> Or_error.ok_exn
;;
