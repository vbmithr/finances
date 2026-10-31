open Base

type columns =
  { booked_on : string
  ; value_on : string
  ; amount : string
  ; account_id : string
  ; counterparty : string
  ; name : string
  ; description : string
  }

type t =
  { institution : string
  ; name : string
  ; format : string
  ; companion_format : string
  ; kind : string
  ; payload_tag : string
  ; account_prefix : string
  ; recognizer_prefix : string
  ; matching_policy : string
  ; remittance_marker : string
  ; name_marker : string
  ; counterparty_marker : string
  ; entry_pattern : string
  ; metadata_prefix : string
  ; aliases : string list
  ; header : string list
  ; columns : columns
  }

let columns_codec =
  Jsont.Object.map
    (fun booked_on value_on amount account_id counterparty name description ->
       { booked_on; value_on; amount; account_id; counterparty; name; description })
  |> Jsont.Object.mem "booked_on" Jsont.string ~enc:(fun (p : columns) -> p.booked_on)
  |> Jsont.Object.mem "value_on" Jsont.string ~enc:(fun (p : columns) -> p.value_on)
  |> Jsont.Object.mem "amount" Jsont.string ~enc:(fun (p : columns) -> p.amount)
  |> Jsont.Object.mem "account_id" Jsont.string ~enc:(fun (p : columns) -> p.account_id)
  |> Jsont.Object.mem "counterparty" Jsont.string ~enc:(fun (p : columns) ->
    p.counterparty)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (p : columns) -> p.name)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun (p : columns) -> p.description)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map
    (fun
        ()
         institution
         name
         aliases
         format
         companion_format
         kind
         payload_tag
         account_prefix
         recognizer_prefix
         matching_policy
         remittance_marker
         name_marker
         counterparty_marker
         entry_pattern
         header
         columns
         metadata_prefix
       ->
       let institution = Validation.token "institution" institution |> Json.or_error in
       let aliases =
         List.map aliases ~f:(fun a -> Validation.token "alias" a |> Json.or_error)
       in
       List.iter
         [ format; companion_format; kind; payload_tag; matching_policy; metadata_prefix ]
         ~f:(fun v ->
           ignore (Validation.token "MT940 profile token" v |> Json.or_error : string));
       List.iter
         [ name
         ; account_prefix
         ; recognizer_prefix
         ; remittance_marker
         ; name_marker
         ; counterparty_marker
         ]
         ~f:(fun v ->
           ignore (Validation.text "MT940 profile field" v |> Json.or_error : string));
       let compiled = Re.Perl.compile_pat entry_pattern in
       if Re.group_count compiled < 6
       then failwith "MT940 entry pattern requires five capture groups";
       if List.contains_dup header ~compare:String.compare
       then failwith "duplicate CSV header";
       List.iter
         [ columns.booked_on
         ; columns.value_on
         ; columns.amount
         ; columns.account_id
         ; columns.counterparty
         ; columns.name
         ; columns.description
         ]
         ~f:(fun c ->
           if not (List.mem header c ~equal:String.equal)
           then failwith ("missing CSV column: " ^ c));
       { institution
       ; name
       ; aliases
       ; format
       ; companion_format
       ; kind
       ; payload_tag
       ; account_prefix
       ; recognizer_prefix
       ; matching_policy
       ; remittance_marker
       ; name_marker
       ; counterparty_marker
       ; entry_pattern
       ; header
       ; columns
       ; metadata_prefix
       })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun p -> p.name)
  |> Jsont.Object.mem "aliases" (Jsont.list Jsont.string) ~enc:(fun p -> p.aliases)
  |> Jsont.Object.mem "format" Jsont.string ~enc:(fun p -> p.format)
  |> Jsont.Object.mem "companion_format" Jsont.string ~enc:(fun p -> p.companion_format)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun p -> p.kind)
  |> Jsont.Object.mem "payload_tag" Jsont.string ~enc:(fun p -> p.payload_tag)
  |> Jsont.Object.mem "account_prefix" Jsont.string ~enc:(fun p -> p.account_prefix)
  |> Jsont.Object.mem "recognizer_prefix" Jsont.string ~enc:(fun p -> p.recognizer_prefix)
  |> Jsont.Object.mem "matching_policy" Jsont.string ~enc:(fun p -> p.matching_policy)
  |> Jsont.Object.mem "remittance_marker" Jsont.string ~enc:(fun p -> p.remittance_marker)
  |> Jsont.Object.mem "name_marker" Jsont.string ~enc:(fun p -> p.name_marker)
  |> Jsont.Object.mem "counterparty_marker" Jsont.string ~enc:(fun p ->
    p.counterparty_marker)
  |> Jsont.Object.mem "entry_pattern" Jsont.string ~enc:(fun p -> p.entry_pattern)
  |> Jsont.Object.mem "header" (Jsont.list Jsont.string) ~enc:(fun p -> p.header)
  |> Jsont.Object.mem "columns" columns_codec ~enc:(fun p -> p.columns)
  |> Jsont.Object.mem "metadata_prefix" Jsont.string ~enc:(fun p -> p.metadata_prefix)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let all = Json.decode (Jsont.list codec) Mt940_profiles_data.json |> Or_error.ok_exn
let find institution = List.find all ~f:(fun p -> String.equal p.institution institution)
