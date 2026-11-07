open Base

type statement =
  | Camt of Camt_profile.t
  | Mt940 of Mt940_profile.t
  | Ofx

(* An institution whose files reach fin as records: a statement format, a CSV layout, or both. *)
type records =
  { institution : string
  ; aliases : string list
  ; statement : statement option
  ; csv : Csv_converter.t list (* Each export layout the institution has. *)
  ; stored_formats : string list
  }

type csv =
  { institution : string
  ; aliases : string list
  ; profile : Csv_profile.t
  }

type source =
  | Records of records
  | Csv of csv

type action =
  | Type
  | Card_reference
  | Withdrawal_reversal

type rule =
  { name : string
  ; pattern : string
  ; case_sensitive : bool
  ; priority : int
  ; account_type : string option
  ; match_type : string option
  ; action : action
  ; target : string
  ; field : string option
  }

type t =
  { source : source
  ; institution_type : Institution_type.t
  ; rules : rule list
  }

let action_codec =
  Jsont.enum
    [ "type", Type
    ; "card-reference", Card_reference
    ; "withdrawal-reversal", Withdrawal_reversal
    ]
;;

let rule_codec =
  Jsont.Object.map
    (fun
        name
         pattern
         case_sensitive
         priority
         account_type
         match_type
         action
         target
         field
       ->
       let name = Validation.token "rule name" name |> Json.or_error in
       let _ = Transaction_pattern.compile ~case_sensitive pattern |> Json.or_error in
       { name
       ; pattern
       ; case_sensitive
       ; priority
       ; account_type
       ; match_type
       ; action
       ; target
       ; field
       })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "pattern" Jsont.string ~enc:(fun r -> r.pattern)
  |> Jsont.Object.mem "case_sensitive" Jsont.bool ~dec_absent:false ~enc:(fun r ->
    r.case_sensitive)
  |> Jsont.Object.mem "priority" Json.integer ~dec_absent:1000 ~enc:(fun r -> r.priority)
  |> Jsont.Object.opt_mem "account_type" Jsont.string ~enc:(fun r -> r.account_type)
  |> Jsont.Object.opt_mem "match_type" Jsont.string ~enc:(fun r -> r.match_type)
  |> Jsont.Object.mem "action" action_codec ~enc:(fun r -> r.action)
  |> Jsont.Object.mem "target" Jsont.string ~enc:(fun r -> r.target)
  |> Jsont.Object.opt_mem "field" Jsont.string ~enc:(fun r -> r.field)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let csv_codec =
  Jsont.Object.map (fun institution aliases profile ->
    let institution = Validation.token "institution" institution |> Json.or_error in
    let aliases =
      List.map aliases ~f:(fun x ->
        Validation.token "institution alias" x |> Json.or_error)
    in
    if List.is_empty profile.Csv_profile.header
    then failwith "CSV header must not be empty";
    List.iter
      ([ profile.date_column; profile.description_column; profile.amount_column ]
       @ Option.to_list (Option.map profile.skip_when ~f:fst))
      ~f:(fun name -> ignore (Csv_profile.column profile.header name : int));
    { institution; aliases; profile })
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "aliases" (Jsont.list Jsont.string) ~enc:(fun p -> p.aliases)
  |> Jsont.Object.mem "profile" Csv_profile.codec ~enc:(fun p -> p.profile)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

(* One layout, or a list of them, such as an explorer's transactions and token transfers. *)
let layouts_codec =
  let ok = function
    | Ok value -> value
    | Error error -> raise (Jsont.Error error)
  in
  Jsont.map
    Jsont.json
    ~dec:(fun json ->
      match json with
      | Jsont.Array _ -> ok (Jsont.Json.decode' (Jsont.list Csv_converter.codec) json)
      | _ -> [ ok (Jsont.Json.decode' Csv_converter.codec json) ])
    ~enc:(function
      | [ one ] -> ok (Jsont.Json.encode' Csv_converter.codec one)
      | many -> ok (Jsont.Json.encode' (Jsont.list Csv_converter.codec) many))
;;

let records_codec =
  Jsont.Object.map (fun institution aliases camt mt940 ofx csv stored_formats ->
    let statement =
      match camt, mt940, ofx with
      | Some profile, None, None -> Some (Camt profile)
      | None, Some profile, None -> Some (Mt940 profile)
      | None, None, Some true -> Some Ofx
      | None, None, (None | Some false) -> None
      | _ -> failwith "a definition has one statement format"
    in
    let csv = Option.value csv ~default:[] in
    if Option.is_none statement && List.is_empty csv
    then failwith "a definition needs a statement format or a CSV layout";
    ({ institution; aliases; statement; csv; stored_formats } : records))
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun (r : records) -> r.institution)
  |> Jsont.Object.mem
       "aliases"
       (Jsont.list Jsont.string)
       ~dec_absent:[]
       ~enc:(fun (r : records) -> r.aliases)
  |> Jsont.Object.opt_mem "camt" Camt_profile.codec ~enc:(fun (r : records) ->
    match r.statement with
    | Some (Camt p) -> Some p
    | _ -> None)
  |> Jsont.Object.opt_mem "mt940" Mt940_profile.codec ~enc:(fun (r : records) ->
    match r.statement with
    | Some (Mt940 p) -> Some p
    | _ -> None)
  |> Jsont.Object.opt_mem "ofx" Jsont.bool ~enc:(fun (r : records) ->
    match r.statement with
    | Some Ofx -> Some true
    | _ -> None)
  |> Jsont.Object.opt_mem "csv" layouts_codec ~enc:(fun (r : records) ->
    Option.some_if (not (List.is_empty r.csv)) r.csv)
  |> Jsont.Object.mem
       "stored_formats"
       (Jsont.list Jsont.string)
       ~dec_absent:[]
       ~enc_omit:List.is_empty
       ~enc:(fun (r : records) -> r.stored_formats)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map (fun () kind institution_type records csv rules ->
    match kind, records, csv with
    | "records", Some r, None -> { source = Records r; institution_type; rules }
    | "csv", None, Some p -> { source = Csv p; institution_type; rules }
    | _ -> failwith "definition kind and source do not agree")
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun p ->
    match p.source with
    | Records _ -> "records"
    | Csv _ -> "csv")
  |> Jsont.Object.mem
       "type"
       Institution_type.jsont
       ~dec_absent:Institution_type.Bank
       ~enc:(fun p -> p.institution_type)
  |> Jsont.Object.opt_mem "records" records_codec ~enc:(fun p ->
    match p.source with
    | Records r -> Some r
    | Csv _ -> None)
  |> Jsont.Object.opt_mem "csv" csv_codec ~enc:(fun p ->
    match p.source with
    | Csv p -> Some p
    | Records _ -> None)
  |> Jsont.Object.mem "rules" (Jsont.list rule_codec) ~dec_absent:[] ~enc:(fun p ->
    p.rules)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let institution t =
  match t.source with
  | Records r -> r.institution
  | Csv p -> p.institution
;;

(* What can read this institution's files, statement format first. *)
let sources (r : records) =
  let statement =
    Option.map r.statement ~f:(function
      | Camt profile ->
        Records_adapter.statement_source
          "camt"
          ~recognizes:(fun ~filename:_ contents -> Camt_converter.recognizes contents)
          ~convert:(Camt_converter.convert profile)
      | Mt940 profile ->
        Records_adapter.statement_source
          "mt940"
          ~recognizes:(fun ~filename:_ contents ->
            Mt940_converter.recognizes profile contents)
          ~convert:(Mt940_converter.convert profile)
      | Ofx ->
        Records_adapter.statement_source
          "ofx"
          ~recognizes:
            (* A broken file named .ofx is meant to be one, and should say so. *)
            (fun
              ~filename contents ->
            Ofx_converter.recognizes contents
            || String.equal (Adapter_support.extension filename) ".ofx")
          ~convert:(Ofx_converter.convert ~institution:r.institution))
  in
  (* The first layout keeps the name [csv], which stored rows may already carry. *)
  let layouts =
    List.mapi r.csv ~f:(fun i definition ->
      let format = if i = 0 then "csv" else Printf.sprintf "csv-%d" (i + 1) in
      Records_adapter.of_definition format definition)
  in
  Option.to_list statement @ layouts
;;

let overlap (r : records) : Adapter.overlap =
  match r.statement, r.csv with
  | Some (Camt _ | Ofx), _ -> Source_ids
  | Some (Mt940 _), _ -> Day_inventory
  | None, csv :: _ -> Csv_converter.overlap csv
  | None, [] -> Source_ids
;;

let adapter t =
  match t.source with
  | Records r ->
    Records_adapter.make
      ~aliases:r.aliases
      ~stored_formats:r.stored_formats
      ~institution:r.institution
      ~overlap:(overlap r)
      (sources r)
  | Csv p ->
    let module A =
      (val Csv_adapter.make ~institution:p.institution ~profile:p.profile () : Adapter.S)
    in
    (module struct
      include A

      let aliases = p.aliases
    end : Adapter.S)
;;

let aliases t =
  match t.source with
  | Records r -> r.aliases
  | Csv p -> p.aliases
;;

let names t = institution t :: aliases t
let institution_type t = t.institution_type

(* Default rules, in the shapes the registry and the store consume. A bundled
   definition's [Type] rules are named [INSTITUTION-default-NN] with priority
   NN-1, which is how the store names them when it installs them. *)

(* The type rules that ship with the institution. They keep their own names and
   priorities when installed, and a rule that names a field tests that field. *)
let default_type_rules t =
  List.filter_map t.rules ~f:(fun r ->
    match r.action with
    | Type ->
      Some
        { Type_default.institution = institution t
        ; name = r.name
        ; priority = r.priority
        ; pattern = r.pattern
        ; case_sensitive = r.case_sensitive
        ; account_type = r.account_type
        ; match_type = r.match_type
        ; field = r.field
        ; target = r.target
        }
    | Card_reference | Withdrawal_reversal -> None)
;;

let default_source_evidence t =
  List.filter_map t.rules ~f:(fun r ->
    let default action =
      Some
        { Source_evidence.institution = institution t
        ; name = r.name
        ; pattern = r.pattern
        ; case_sensitive = r.case_sensitive
        ; action
        }
    in
    match r.action with
    | Card_reference -> default Source_evidence.Card_reference
    | Withdrawal_reversal -> default Source_evidence.Withdrawal_reversal
    | Type -> None)
;;

type description =
  { institution : string
  ; aliases : string list
  ; kind : string
  ; fields : (string * string) list
  ; unread : string list
  ; rule_inputs : string list
  }

(* What fin reads from a source, by role, and what it leaves unread. Rules can
   match less than fin reads: see [rule_inputs]. *)
let describe t =
  let unread header used =
    List.filter header ~f:(fun name -> not (List.mem used name ~equal:String.equal))
    |> List.dedup_and_sort ~compare:String.compare
  in
  let base kind fields header used rule_inputs =
    { institution = institution t
    ; aliases = aliases t
    ; kind
    ; fields
    ; unread = unread header used
    ; rule_inputs
    }
  in
  match t.source with
  | Records r ->
    let statement =
      match r.statement with
      | Some (Camt _) -> [ "statement", "CAMT.053" ]
      | Some (Mt940 _) -> [ "statement", "MT940" ]
      | Some Ofx -> [ "statement", "OFX" ]
      | None -> []
    in
    let roles = List.concat_map r.csv ~f:Csv_converter.roles in
    base
      "Records"
      (statement
       @ List.map roles ~f:(fun (role, columns) -> role, String.concat ~sep:", " columns)
      )
      (List.concat_map r.csv ~f:Csv_converter.header)
      (List.concat_map roles ~f:snd)
      [ "the description (type rules)"
      ; "any field of the row, named as the source names it, with --field (type rules)"
      ]
  | Csv p ->
    let profile = p.profile in
    let fields =
      [ "date", profile.date_column
      ; "description", profile.description_column
      ; "amount", profile.amount_column
      ]
      @ Option.to_list (Option.map profile.skip_when ~f:(fun (n, _) -> "skip rows by", n))
    in
    base
      "Generic CSV (primary)"
      fields
      profile.header
      (List.map fields ~f:snd)
      [ Printf.sprintf "the description column %S (type rules)" profile.description_column
      ; "any column of the file, named by its header, with --field (type rules)"
      ]
;;
