open Base
open Institution_type

type statement =
  | Camt of Camt_profile.t
  | Mt940 of Mt940_profile.t
  | Ofx

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
  { institution : string
  ; aliases : string list
  ; statement : statement option
  ; csv : Csv_converter.t list (* Each export layout the institution has. *)
  ; lua_csv : Lua_csv_converter.t list
  ; own_symbols : Adapter.own_symbol list (* Units the institution defines for itself. *)
  ; stored_formats : string list
  ; typ : Institution_type.t
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

let own_symbol_codec =
  Jsont.Object.map (fun symbol asset name : Adapter.own_symbol ->
    (* The asset is the journal's name for it, so it must be one. *)
    ignore (Json.or_error (Commodity.asset asset) : Commodity.t);
    { symbol; asset; name })
  |> Jsont.Object.mem "symbol" Jsont.string ~enc:(fun (o : Adapter.own_symbol) ->
    o.symbol)
  |> Jsont.Object.mem "asset" Jsont.string ~enc:(fun (o : Adapter.own_symbol) -> o.asset)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (o : Adapter.own_symbol) -> o.name)
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
  Jsont.Object.map
    (fun
        institution
         aliases
         camt
         mt940
         ofx
         csv
         lua_csv
         chain_id
         network
         own_symbols
         stored_formats
       ->
       let statement =
         match camt, mt940, ofx with
         | Some profile, None, None -> Some (Camt profile)
         | None, Some profile, None -> Some (Mt940 profile)
         | None, None, Some true -> Some Ofx
         | None, None, (None | Some false) -> None
         | _ -> failwith "a definition has one statement format"
       in
       let csv = Option.value csv ~default:[] in
       let lua_csv =
         List.map lua_csv ~f:(fun source ->
           Lua_csv_converter.create ~institution source |> Json.or_error)
       in
       if (not (List.is_empty csv)) && not (List.is_empty lua_csv)
       then failwith "an institution cannot mix JSON and Lua CSV converters yet";
       if Option.is_none statement && List.is_empty csv && List.is_empty lua_csv
       then failwith "a definition needs a statement format or a CSV layout";
       let typ =
         match chain_id, network with
         | Some id, None -> Institution_type.Wallet (Evm { chain_id = id })
         | None, Some name when not (String.is_empty (String.strip name)) ->
           Institution_type.Wallet (Other name)
         | None, None -> Institution_type.Bank
         | _ -> failwith "network needs one chain_id or a nonempty network name"
       in
       ({ institution
        ; aliases
        ; statement
        ; csv
        ; lua_csv
        ; own_symbols
        ; stored_formats
        ; typ
        ; rules = []
        }
        : t))
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun (r : t) -> r.institution)
  |> Jsont.Object.mem
       "aliases"
       (Jsont.list Jsont.string)
       ~dec_absent:[]
       ~enc:(fun (r : t) -> r.aliases)
  |> Jsont.Object.opt_mem "camt" Camt_profile.codec ~enc:(fun (r : t) ->
    match r.statement with
    | Some (Camt p) -> Some p
    | _ -> None)
  |> Jsont.Object.opt_mem "mt940" Mt940_profile.codec ~enc:(fun (r : t) ->
    match r.statement with
    | Some (Mt940 p) -> Some p
    | _ -> None)
  |> Jsont.Object.opt_mem "ofx" Jsont.bool ~enc:(fun (r : t) ->
    match r.statement with
    | Some Ofx -> Some true
    | _ -> None)
  |> Jsont.Object.opt_mem "csv" layouts_codec ~enc:(fun (r : t) ->
    Option.some_if (not (List.is_empty r.csv)) r.csv)
  |> Jsont.Object.mem
       "lua_csv"
       (Jsont.list Jsont.string)
       ~dec_absent:[]
       ~enc_omit:List.is_empty
       ~enc:(fun r -> List.map r.lua_csv ~f:Lua_csv_converter.source)
  |> Jsont.Object.opt_mem "chain_id" Jsont.int ~enc:(fun (r : t) ->
    match r.typ with
    | Wallet (Evm { chain_id }) -> Some chain_id
    | _ -> None)
  |> Jsont.Object.opt_mem "network" Jsont.string ~enc:(fun (r : t) ->
    match r.typ with
    | Wallet (Other name) -> Some name
    | _ -> None)
  |> Jsont.Object.mem
       "own_symbols"
       (Jsont.list own_symbol_codec)
       ~dec_absent:[]
       ~enc_omit:List.is_empty
       ~enc:(fun (r : t) -> r.own_symbols)
  |> Jsont.Object.mem
       "stored_formats"
       (Jsont.list Jsont.string)
       ~dec_absent:[]
       ~enc_omit:List.is_empty
       ~enc:(fun (r : t) -> r.stored_formats)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let codec =
  Jsont.Object.map (fun () () typ source rules ->
    let typ =
      match typ, source.typ with
      | Institution_type.Wallet _, Wallet network -> Wallet network
      | Wallet _, Bank -> Wallet Unspecified
      | (Bank | Crypto_exchange), Wallet _ ->
        failwith "network metadata is only valid for a wallet"
      | (Bank | Crypto_exchange), Bank -> typ
      | _, Crypto_exchange -> assert false
    in
    { source with typ; rules })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "kind" (Json.tag "records") ~enc:(fun _ -> ())
  |> Jsont.Object.mem
       "type"
       Institution_type.kind_jsont
       ~dec_absent:Institution_type.Bank
       ~enc:(fun p -> p.typ)
  |> Jsont.Object.mem "records" records_codec ~enc:Fn.id
  |> Jsont.Object.mem "rules" (Jsont.list rule_codec) ~dec_absent:[] ~enc:(fun p ->
    p.rules)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let chain_id t =
  match t.typ with
  | Wallet (Evm { chain_id }) -> Some chain_id
  | Bank | Crypto_exchange | Wallet (Other _ | Unspecified) -> None
;;

let institution t = t.institution

(* What can read this institution's files, statement format first. *)
let sources (r : t) =
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
  let scripts =
    List.mapi r.lua_csv ~f:(fun i definition ->
      let index = List.length r.csv + i in
      let format = if index = 0 then "csv" else Printf.sprintf "csv-%d" (index + 1) in
      Records_adapter.statement_source
        format
        ~recognizes:(fun ~filename:_ contents ->
          Lua_csv_converter.recognizes definition contents)
        ~convert:(Lua_csv_converter.convert definition))
  in
  Option.to_list statement @ layouts @ scripts
;;

let overlap (r : t) : Adapter.overlap =
  match r.statement, r.csv with
  | Some (Camt _ | Ofx), _ -> Source_ids
  | Some (Mt940 _), _ -> Day_inventory
  | None, csv :: _ -> Csv_converter.overlap csv
  | None, [] ->
    Option.value_map
      (List.hd r.lua_csv)
      ~default:Adapter.Source_ids
      ~f:Lua_csv_converter.overlap
;;

let adapter t =
  let r = t in
  Records_adapter.make
    ~aliases:r.aliases
    ~stored_formats:r.stored_formats
    ~tickers:(Institution_type.equal t.typ Crypto_exchange)
    ~own_symbols:r.own_symbols
    ~institution:r.institution
    ~overlap:(overlap r)
    (sources r)
;;

let aliases t = t.aliases

let same_sources a b =
  let source_type = function
    | Institution_type.Wallet ((Evm _ | Other _) as network) -> Wallet network
    | Bank | Crypto_exchange | Wallet Unspecified -> Bank
  in
  (* CSV layouts retain the parsed JSON and its source locations. Locations
     depend on surrounding definition text and do not change interpretation. *)
  let same_csv =
    List.equal
      (fun x y ->
         String.equal
           (Json.encode Csv_converter.codec x)
           (Json.encode Csv_converter.codec y))
      a.csv
      b.csv
  in
  same_csv
  && Poly.equal
       { a with typ = source_type a.typ; rules = []; csv = [] }
       { b with typ = source_type b.typ; rules = []; csv = [] }
;;

let names t = institution t :: aliases t
let typ t = t.typ

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
  let r = t in
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
     @ List.map r.lua_csv ~f:(fun _ -> "CSV", "Lua converter"))
    (List.concat_map r.csv ~f:Csv_converter.header)
    (List.concat_map roles ~f:snd)
    [ "the description (type rules)"
    ; "any field of the row, named as the source names it, with --field (type rules)"
    ]
;;
