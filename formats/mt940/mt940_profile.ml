open Base

type t =
  { institution : string
  ; name : string
  ; kind : string
  ; payload_tag : string
  ; account_prefix : string
  ; recognizer_prefix : string
  ; remittance_marker : string
  ; name_marker : string
  ; counterparty_marker : string
  }

(* Older installed definitions contain these CSV companion fields. Accept them
   when loading saved JSON, but no longer retain or emit them: records-based
   CSV matching gets its settings from the separate CSV definition. *)
let codec =
  Jsont.Object.map
    (fun
        ()
         institution
         name
         _aliases
         _format
         _companion_format
         kind
         payload_tag
         account_prefix
         recognizer_prefix
         _matching_policy
         remittance_marker
         name_marker
         counterparty_marker
         legacy_entry_pattern
         _header
         _columns
         _metadata_prefix
       ->
       let institution = Validation.token "institution" institution |> Json.or_error in
       List.iter [ kind; payload_tag ] ~f:(fun v ->
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
       Option.iter legacy_entry_pattern ~f:(fun pattern ->
         if
           not
             (String.equal
                pattern
                "^([0-9]{6})([0-9]{4})([CD])([0-9]+,[0-9]*)(N[A-Z]{3})NONREF$")
         then failwith "custom MT940 entry patterns are no longer supported");
       { institution
       ; name
       ; kind
       ; payload_tag
       ; account_prefix
       ; recognizer_prefix
       ; remittance_marker
       ; name_marker
       ; counterparty_marker
       })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun p -> p.name)
  |> Jsont.Object.opt_mem "aliases" Jsont.json ~enc:(fun _ -> None)
  |> Jsont.Object.opt_mem "format" Jsont.json ~enc:(fun _ -> None)
  |> Jsont.Object.opt_mem "companion_format" Jsont.json ~enc:(fun _ -> None)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun p -> p.kind)
  |> Jsont.Object.mem "payload_tag" Jsont.string ~enc:(fun p -> p.payload_tag)
  |> Jsont.Object.mem "account_prefix" Jsont.string ~enc:(fun p -> p.account_prefix)
  |> Jsont.Object.mem "recognizer_prefix" Jsont.string ~enc:(fun p -> p.recognizer_prefix)
  |> Jsont.Object.opt_mem "matching_policy" Jsont.json ~enc:(fun _ -> None)
  |> Jsont.Object.mem "remittance_marker" Jsont.string ~enc:(fun p -> p.remittance_marker)
  |> Jsont.Object.mem "name_marker" Jsont.string ~enc:(fun p -> p.name_marker)
  |> Jsont.Object.mem "counterparty_marker" Jsont.string ~enc:(fun p ->
    p.counterparty_marker)
  |> Jsont.Object.opt_mem "entry_pattern" Jsont.string ~enc:(fun _ -> None)
  |> Jsont.Object.opt_mem "header" Jsont.json ~enc:(fun _ -> None)
  |> Jsont.Object.opt_mem "columns" Jsont.json ~enc:(fun _ -> None)
  |> Jsont.Object.opt_mem "metadata_prefix" Jsont.json ~enc:(fun _ -> None)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
