open Base

type csv =
  { institution : string
  ; aliases : string list
  ; profile : Csv_profile.t
  }

type source =
  | Ofx_csv of Institution_preset.t
  | Camt_csv of Camt_institution_preset.t
  | Mt940_csv of Mt940_profile.t
  | Running_csv of Csv_running_profile.t
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

let camt_codec =
  Jsont.Object.map
    (fun
        institution
         aliases
         companion_format
         matching_policy
         metadata_prefix
         camt_profile
         csv_profile
       ->
       let institution = Validation.token "institution" institution |> Json.or_error in
       let aliases =
         List.map aliases ~f:(fun x ->
           Validation.token "institution alias" x |> Json.or_error)
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
       if
         not
           (String.equal institution camt_profile.Camt_profile.institution
            && String.equal institution csv_profile.Csv_clock_profile.institution)
       then failwith "institution must agree with both CAMT and CSV profiles";
       { Camt_institution_preset.institution
       ; aliases
       ; companion_format
       ; matching_policy
       ; metadata_prefix
       ; camt_profile
       ; csv_profile
       })
  |> Jsont.Object.mem
       "institution"
       Jsont.string
       ~enc:(fun (p : Camt_institution_preset.t) -> p.institution)
  |> Jsont.Object.mem
       "aliases"
       (Jsont.list Jsont.string)
       ~enc:(fun (p : Camt_institution_preset.t) -> p.aliases)
  |> Jsont.Object.mem
       "companion_format"
       Jsont.string
       ~enc:(fun (p : Camt_institution_preset.t) -> p.companion_format)
  |> Jsont.Object.mem
       "matching_policy"
       Jsont.string
       ~enc:(fun (p : Camt_institution_preset.t) -> p.matching_policy)
  |> Jsont.Object.mem
       "metadata_prefix"
       Jsont.string
       ~enc:(fun (p : Camt_institution_preset.t) -> p.metadata_prefix)
  |> Jsont.Object.mem
       "camt_profile"
       Camt_profile.codec
       ~enc:(fun (p : Camt_institution_preset.t) -> p.camt_profile)
  |> Jsont.Object.mem
       "csv_profile"
       Csv_clock_profile.codec
       ~enc:(fun (p : Camt_institution_preset.t) -> p.csv_profile)
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

let codec =
  Jsont.Object.map (fun () kind ofx camt csv running mt940 rules ->
    match kind, ofx, camt, csv, running, mt940 with
    | "ofx-csv", Some p, None, None, None, None -> { source = Ofx_csv p; rules }
    | "camt-csv", None, Some p, None, None, None -> { source = Camt_csv p; rules }
    | "csv", None, None, Some p, None, None -> { source = Csv p; rules }
    | "running-csv", None, None, None, Some p, None -> { source = Running_csv p; rules }
    | "mt940-csv", None, None, None, None, Some p -> { source = Mt940_csv p; rules }
    | _ -> failwith "definition kind and source profile do not agree")
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun p ->
    match p.source with
    | Ofx_csv _ -> "ofx-csv"
    | Camt_csv _ -> "camt-csv"
    | Csv _ -> "csv"
    | Running_csv _ -> "running-csv"
    | Mt940_csv _ -> "mt940-csv")
  |> Jsont.Object.opt_mem "ofx" Institution_preset.codec ~enc:(fun p ->
    match p.source with
    | Ofx_csv p -> Some p
    | _ -> None)
  |> Jsont.Object.opt_mem "camt" camt_codec ~enc:(fun p ->
    match p.source with
    | Camt_csv p -> Some p
    | _ -> None)
  |> Jsont.Object.opt_mem "csv" csv_codec ~enc:(fun p ->
    match p.source with
    | Csv p -> Some p
    | _ -> None)
  |> Jsont.Object.opt_mem "running" Csv_running_profile.codec ~enc:(fun p ->
    match p.source with
    | Running_csv p -> Some p
    | _ -> None)
  |> Jsont.Object.opt_mem "mt940" Mt940_profile.codec ~enc:(fun p ->
    match p.source with
    | Mt940_csv p -> Some p
    | _ -> None)
  |> Jsont.Object.mem "rules" (Jsont.list rule_codec) ~dec_absent:[] ~enc:(fun p ->
    p.rules)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let institution t =
  match t.source with
  | Ofx_csv p -> p.institution
  | Camt_csv p -> p.institution
  | Csv p -> p.institution
  | Running_csv p -> p.institution
  | Mt940_csv p -> p.institution
;;

let adapter t =
  match t.source with
  | Running_csv p -> Csv_running_adapter.make p
  | Mt940_csv p -> Mt940_csv_adapter.make p
  | Ofx_csv p ->
    Csv_ofx_adapter.make
      ~institution:p.institution
      ~aliases:p.aliases
      ~csv_format:p.companion_format
      ~policy:p.matching_policy
      ~snapshot:p.snapshot
      ~layout:p.layout
      ()
  | Csv p ->
    let module A =
      (val Csv_adapter.make ~institution:p.institution ~profile:p.profile () : Adapter.S)
    in
    (module struct
      include A

      let aliases = p.aliases
    end : Adapter.S)
  | Camt_csv p ->
    Camt_csv_adapter.make
      ~profile:p.camt_profile
      ~csv_profile:p.csv_profile
      ~aliases:p.aliases
      ~csv:p.companion_format
      ~policy:p.matching_policy
      ~metadata_prefix:p.metadata_prefix
      ()
;;

let aliases t =
  match t.source with
  | Ofx_csv p -> p.aliases
  | Camt_csv p -> p.aliases
  | Csv p -> p.aliases
  | Running_csv p -> p.aliases
  | Mt940_csv p -> p.aliases
;;

let names t = institution t :: aliases t

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
  | Ofx_csv p ->
    let layout = p.layout in
    let name i = List.nth layout.header i |> Option.value ~default:"?" in
    let required =
      [ "account", layout.account_id
      ; "booked on", layout.booked_on
      ; "amount", layout.amount
      ; "description", layout.description
      ]
    in
    let optional =
      [ "value date", layout.value_on
      ; "suggested description", layout.suggested_description
      ; "category", layout.category
      ; "parent category", layout.parent_category
      ; "comment", layout.comment
      ; "account label", layout.account_label
      ; "reported balance", layout.reported_balance
      ; "pointage", layout.pointage
      ]
    in
    let fields =
      List.map required ~f:(fun (role, i) -> role, name i)
      @ List.filter_map optional ~f:(fun (role, i) ->
        Option.map i ~f:(fun i -> role, name i))
    in
    base
      "OFX primary with CSV companion"
      fields
      layout.header
      (List.map fields ~f:snd)
      [ "the first OFX description (type rules); the CSV companion's columns are not \
         available to rules"
      ]
  | Camt_csv p ->
    let c = p.csv_profile.columns in
    let fields =
      [ "reference", c.reference
      ; "date", c.date
      ; "date and time", c.datetime
      ; "amount", c.amount
      ; "currency", c.currency
      ; "description", c.description
      ; "payment reference", c.payment_reference
      ; "running balance", c.running_balance
      ; "merchant", c.merchant
      ; "fees", c.fees
      ; "direction", c.direction
      ]
      @ List.map p.csv_profile.conflict_columns ~f:(fun n -> "compared for conflicts", n)
    in
    base
      "CAMT.053 primary with CSV companion"
      fields
      p.csv_profile.header
      (List.map fields ~f:snd)
      [ "the first CAMT description (type rules)"
      ; "any column of the CSV companion, with --field (type rules)"
      ]
  | Running_csv p ->
    let c = p.columns in
    let fields =
      [ "type (reported code)", c.transaction_type
      ; "product", c.product
      ; "started", c.started
      ; "completed", c.completed
      ; "description", c.description
      ; "amount", c.amount
      ; "fee", c.fee
      ; "currency", c.currency
      ; "state", c.state
      ; "balance", c.balance
      ]
    in
    base
      "Running-balance CSV (primary)"
      fields
      p.header
      (List.map fields ~f:snd)
      [ Printf.sprintf "the description column %S (type rules)" c.description
      ; Printf.sprintf
          "any column of the file, named by its header, such as %S, with --field (type \
           rules)"
          c.transaction_type
      ]
  | Mt940_csv p ->
    let c = p.columns in
    let fields =
      [ "booked on", c.booked_on
      ; "value date", c.value_on
      ; "amount", c.amount
      ; "account", c.account_id
      ; "counterparty", c.counterparty
      ; "name", c.name
      ; "description", c.description
      ]
    in
    base
      "MT940 primary with CSV companion"
      fields
      p.header
      (List.map fields ~f:snd)
      [ "the first MT940 description (type rules)" ]
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
