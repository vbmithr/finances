open Base

type reference_field =
  | Proprietary_code
  | Account_servicer_reference

type hex_normalization =
  { prefix : string
  ; description : string
  ; length : int
  }

type reference_normalization =
  | Preserve
  | Canonical_hex of hex_normalization

type t =
  { institution : string
  ; namespace : string
  ; servicer_contains : string option
  ; account_type_prefix : string
  ; event_kind : string
  ; payload_tag : string
  ; reference_field : reference_field
  ; reference_normalization : reference_normalization
  }

let reference_field_codec =
  Jsont.enum
    [ "proprietary-code", Proprietary_code
    ; "account-servicer-reference", Account_servicer_reference
    ]
;;

let hex_codec =
  Jsont.Object.map (fun prefix description length ->
    let prefix = Validation.text "reference prefix" prefix |> Json.or_error in
    let description =
      Validation.text "reference description" description |> Json.or_error
    in
    if length <= 0 then failwith "reference hex length must be positive";
    { prefix; description; length })
  |> Jsont.Object.mem "prefix" Jsont.string ~enc:(fun p -> p.prefix)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun p -> p.description)
  |> Jsont.Object.mem "length" Json.integer ~enc:(fun p -> p.length)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let normalization_codec =
  Jsont.map
    (Jsont.option hex_codec)
    ~dec:(function
      | None -> Preserve
      | Some config -> Canonical_hex config)
    ~enc:(function
      | Preserve -> None
      | Canonical_hex config -> Some config)
;;

let codec =
  Jsont.Object.map
    (fun
        ()
         institution
         namespace
         servicer_contains
         account_type_prefix
         event_kind
         payload_tag
         reference_field
         reference_normalization
       ->
       let institution = Validation.token "institution" institution |> Json.or_error in
       let namespace = Validation.text "CAMT namespace" namespace |> Json.or_error in
       let servicer_contains =
         Option.map servicer_contains ~f:(fun value ->
           Validation.text "servicer text" value |> Json.or_error)
       in
       let account_type_prefix =
         Validation.text "account type prefix" account_type_prefix |> Json.or_error
       in
       let event_kind = Validation.token "event kind" event_kind |> Json.or_error in
       let payload_tag = Validation.token "payload tag" payload_tag |> Json.or_error in
       { institution
       ; namespace
       ; servicer_contains
       ; account_type_prefix
       ; event_kind
       ; payload_tag
       ; reference_field
       ; reference_normalization
       })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "namespace" Jsont.string ~enc:(fun p -> p.namespace)
  |> Jsont.Object.mem "servicer_contains" (Jsont.option Jsont.string) ~enc:(fun p ->
    p.servicer_contains)
  |> Jsont.Object.mem "account_type_prefix" Jsont.string ~enc:(fun p ->
    p.account_type_prefix)
  |> Jsont.Object.mem "event_kind" Jsont.string ~enc:(fun p -> p.event_kind)
  |> Jsont.Object.mem "payload_tag" Jsont.string ~enc:(fun p -> p.payload_tag)
  |> Jsont.Object.mem "reference_field" reference_field_codec ~enc:(fun p ->
    p.reference_field)
  |> Jsont.Object.mem "reference_normalization" normalization_codec ~enc:(fun p ->
    p.reference_normalization)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
