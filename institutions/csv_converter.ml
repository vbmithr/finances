open Base

type decimal =
  | Dot
  | Comma

type layout =
  | Iso_date
  | Iso_datetime
  | Day_first

type date_spec =
  { column : string
  ; fallback : string option
  ; layout : layout
  ; utc : bool
  }

type quantity_spec =
  { column : string
  ; negate : bool
  ; minus : string option
  }

type commodity_spec =
  | Fiat_column of string
  | Fiat_code of string
  | Reported_column of
      { column : string
      ; provider : string
      }

type sign =
  | Positive
  | Negative
  | Not_positive
  | Not_negative

type test =
  | One_of of string list
  | Sign of sign
  | Filled of bool
  | Matches of Re.re
  | Not_before of string
  | Prefix_of of string

type condition =
  { column : string
  ; test : test
  }

type requirement =
  { applies : condition list
  ; condition : condition
  }

type movement_spec =
  { role : string
  ; quantity : quantity_spec
  ; commodity : commodity_spec
  }

type case =
  { conditions : condition list
  ; movements : movement_spec list
  ; fee : quantity_spec option
  }

type text_spec =
  | Column of string
  | Value of string

type exchange_spec =
  { source : text_spec
  ; target : text_spec
  ; rate : string option
  ; target_quantity : string option
  }

type amount_spec =
  { quantity : string
  ; commodity : commodity_spec
  }

type reference_spec =
  { servicer : string option
  ; normalize : (Camt_profile.hex_normalization * string) option
  ; end_to_end : string option
  ; payment : string option
  ; hash : string option
  }

type reported_spec =
  { quantity : string
  ; symbol : string
  }

type balance_spec =
  { balance_type : Records.balance_type
  ; quantity : string
  ; decimal : decimal option (* When this column differs from the file's. *)
  ; commodity : commodity_spec
  ; as_of : date_spec
  }

type account_spec =
  { id : string
  ; bank : string option
  ; number : string option
  ; account_type : string option
  ; currency : commodity_spec option
  }

type id_spec =
  | Inventory
  | Row_hash
  | Columns of string list

type total_spec =
  { conditions : condition list
  ; quantity : quantity_spec
  ; commodity : commodity_spec
  }

type t =
  { raw : Jsont.json (* As it was written, for printing it back. *)
  ; institution : string
  ; institution_type : Institution_type.t
  ; separator : char
  ; decimal : decimal
  ; header : string list
  ; observation : bool
  ; only : condition list
  ; require : requirement list
  ; total : total_spec option
  ; account : account_spec
  ; id : id_spec
  ; occurred : date_spec
  ; value_on : date_spec option
  ; cases : case list
  ; reported : reported_spec option
  ; descriptions : string list
  ; movement_type : string option
  ; counterparty_name : string list
  ; counterparty_iban : string option
  ; references : reference_spec option
  ; exchange : exchange_spec option
  ; valuations : amount_spec list
  ; balance_after : string option
  ; balances : balance_spec list
  ; journal_tags : (string * string) list
  ; journal_description : string option
  ; conflict_columns : string list
  }

let institution t = t.institution
let header t = t.header
let institution_type t = t.institution_type
let journal_tags t = t.journal_tags
let journal_description t = t.journal_description
let conflict_columns t = t.conflict_columns

(* Columns, in order, under each role a definition gives them. *)
let roles t =
  let commodity = function
    | Fiat_column column | Reported_column { column; _ } -> [ column ]
    | Fiat_code _ -> []
  in
  let date (d : date_spec) = d.column :: Option.to_list d.fallback in
  let quantity (q : quantity_spec) = [ q.column ] in
  let opt = Option.to_list in
  let text = function
    | Column column -> [ column ]
    | Value _ -> []
  in
  let named template =
    List.filter t.header ~f:(fun column ->
      String.is_substring template ~substring:("{" ^ column ^ "}"))
  in
  let account = t.account in
  let movements = List.concat_map t.cases ~f:(fun case -> case.movements) in
  let of_movements f = List.concat_map movements ~f in
  let of_references f = Option.value_map t.references ~default:[] ~f in
  let of_exchange f = Option.value_map t.exchange ~default:[] ~f in
  [ ( "account"
    , List.concat_map
        (account.id
         :: List.filter_opt [ account.bank; account.number; account.account_type ])
        ~f:named
      @ Option.value_map account.currency ~default:[] ~f:commodity )
  ; ( "row ID"
    , match t.id with
      | Columns columns -> List.filter columns ~f:(List.mem t.header ~equal:String.equal)
      | Inventory | Row_hash -> [] )
  ; "date", date t.occurred
  ; "value date", Option.value_map t.value_on ~default:[] ~f:date
  ; "amount", of_movements (fun m -> quantity m.quantity)
  ; "commodity", of_movements (fun m -> commodity m.commodity)
  ; ( "fee"
    , List.concat_map t.cases ~f:(fun case ->
        Option.value_map case.fee ~default:[] ~f:quantity) )
  ; ( "reported"
    , Option.value_map t.reported ~default:[] ~f:(fun r -> [ r.quantity; r.symbol ]) )
  ; "description", t.descriptions
  ; "type", opt t.movement_type
  ; "counterparty", t.counterparty_name @ opt t.counterparty_iban
  ; ( "reference"
    , of_references (fun r ->
        List.filter_opt [ r.servicer; r.end_to_end; r.payment; r.hash ]
        @ Option.value_map r.normalize ~default:[] ~f:(fun (_, column) -> [ column ])) )
  ; ( "exchange"
    , of_exchange (fun e ->
        text e.source @ text e.target @ List.filter_opt [ e.rate; e.target_quantity ]) )
  ; ( "valuation"
    , List.concat_map t.valuations ~f:(fun v -> v.quantity :: commodity v.commodity) )
  ; "balance after", opt t.balance_after
  ; ( "stated balance"
    , List.concat_map t.balances ~f:(fun b ->
        (b.quantity :: commodity b.commodity) @ date b.as_of) )
  ; ( "checked"
    , List.map t.only ~f:(fun (c : condition) -> c.column)
      @ List.concat_map t.require ~f:(fun r ->
        List.map (r.condition :: r.applies) ~f:(fun (c : condition) -> c.column)) )
  ]
  |> List.filter_map ~f:(fun (role, columns) ->
    let columns =
      List.fold columns ~init:[] ~f:(fun seen column ->
        if List.mem seen column ~equal:String.equal then seen else column :: seen)
      |> List.rev
    in
    if List.is_empty columns then None else Some (role, columns))
;;

let overlap t : Adapter.overlap =
  match t.id with
  | Inventory -> Day_inventory
  | Row_hash | Columns _ -> Source_ids
;;

(* Definitions. *)

let finish map = map |> Jsont.Object.error_unknown |> Jsont.Object.finish
let dec_only name codec map = Jsont.Object.mem name codec map
let opt name codec map = Jsont.Object.opt_mem name codec map

let list_or_empty name codec map =
  Jsont.Object.mem name (Jsont.list codec) ~dec_absent:[] map
;;

let flag name map = Jsont.Object.mem name Jsont.bool ~dec_absent:false map
let decimal_codec = Jsont.enum [ "dot", Dot; "comma", Comma ]

let separator_codec =
  Jsont.map
    Jsont.string
    ~dec:(fun text ->
      if String.length text = 1
      then text.[0]
      else Jsont.Error.msgf Jsont.Meta.none "a separator is one character")
    ~enc:String.of_char
;;

let layout_codec =
  Jsont.enum
    [ "iso-date", Iso_date; "iso-datetime", Iso_datetime; "day-first", Day_first ]
;;

let date_codec =
  Jsont.Object.map (fun column fallback layout utc : date_spec ->
    { column; fallback; layout; utc })
  |> dec_only "column" Jsont.string
  |> opt "fallback" Jsont.string
  |> dec_only "layout" layout_codec
  |> flag "utc"
  |> finish
;;

let quantity_codec =
  Jsont.Object.map (fun column negate minus : quantity_spec -> { column; negate; minus })
  |> dec_only "column" Jsont.string
  |> flag "negate"
  |> opt "minus" Jsont.string
  |> finish
;;

let commodity_codec =
  Jsont.Object.map (fun fiat code reported provider ->
    match fiat, code, reported, provider with
    | Some column, None, None, None -> Fiat_column column
    | None, Some code, None, None -> Fiat_code code
    | None, None, Some column, Some provider -> Reported_column { column; provider }
    | _ ->
      Jsont.Error.msgf
        Jsont.Meta.none
        "a commodity is one of fiat, code, or reported with provider")
  |> opt "fiat" Jsont.string
  |> opt "code" Jsont.string
  |> opt "reported" Jsont.string
  |> opt "provider" Jsont.string
  |> finish
;;

let sign_codec =
  Jsont.enum
    [ "positive", Positive
    ; "negative", Negative
    ; "not-positive", Not_positive
    ; "not-negative", Not_negative
    ]
;;

let pattern_codec =
  Jsont.map
    Jsont.string
    ~dec:(fun pattern ->
      try Re.compile (Re.Perl.re pattern) with
      | _ -> Jsont.Error.msgf Jsont.Meta.none "invalid pattern: %s" pattern)
    ~enc:(fun _ -> "")
;;

let test_of one_of sign filled matches not_before prefix_of =
  match one_of, sign, filled, matches, not_before, prefix_of with
  | Some values, None, None, None, None, None -> One_of values
  | None, Some sign, None, None, None, None -> Sign sign
  | None, None, Some filled, None, None, None -> Filled filled
  | None, None, None, Some pattern, None, None -> Matches pattern
  | None, None, None, None, Some column, None -> Not_before column
  | None, None, None, None, None, Some column -> Prefix_of column
  | _ ->
    Jsont.Error.msgf
      Jsont.Meta.none
      "a condition is one of in, sign, filled, matches, not_before or prefix_of"
;;

let test_members map =
  map
  |> opt "in" (Jsont.list Jsont.string)
  |> opt "sign" sign_codec
  |> opt "filled" Jsont.bool
  |> opt "matches" pattern_codec
  |> opt "not_before" Jsont.string
  |> opt "prefix_of" Jsont.string
;;

let condition_codec =
  Jsont.Object.map
    (fun column one_of sign filled matches not_before prefix_of : condition ->
       { column; test = test_of one_of sign filled matches not_before prefix_of })
  |> dec_only "column" Jsont.string
  |> test_members
  |> finish
;;

let requirement_codec =
  Jsont.Object.map
    (fun applies column one_of sign filled matches not_before prefix_of : requirement ->
       { applies
       ; condition =
           { column; test = test_of one_of sign filled matches not_before prefix_of }
       })
  |> list_or_empty "when" condition_codec
  |> dec_only "column" Jsont.string
  |> test_members
  |> finish
;;

let movement_codec =
  Jsont.Object.map (fun role quantity commodity : movement_spec ->
    { role; quantity; commodity })
  |> dec_only "role" Jsont.string
  |> dec_only "quantity" quantity_codec
  |> dec_only "commodity" commodity_codec
  |> finish
;;

let case_codec =
  Jsont.Object.map (fun conditions movements fee : case -> { conditions; movements; fee })
  |> list_or_empty "when" condition_codec
  |> dec_only "movements" (Jsont.list movement_codec)
  |> opt "fee" quantity_codec
  |> finish
;;

let text_codec =
  Jsont.Object.map (fun column value ->
    match column, value with
    | Some column, None -> Column column
    | None, Some value -> Value value
    | _ -> Jsont.Error.msgf Jsont.Meta.none "a text is a column or a value")
  |> opt "column" Jsont.string
  |> opt "value" Jsont.string
  |> finish
;;

let exchange_codec =
  Jsont.Object.map (fun source target rate target_quantity : exchange_spec ->
    { source; target; rate; target_quantity })
  |> dec_only "source" text_codec
  |> dec_only "target" text_codec
  |> opt "rate" Jsont.string
  |> opt "target_quantity" Jsont.string
  |> finish
;;

let amount_codec =
  Jsont.Object.map (fun quantity commodity : amount_spec -> { quantity; commodity })
  |> dec_only "quantity" Jsont.string
  |> dec_only "commodity" commodity_codec
  |> finish
;;

let references_codec =
  Jsont.Object.map (fun servicer normalize column end_to_end payment hash ->
    let normalize =
      match normalize, column with
      | Some config, Some column -> Some (config, column)
      | None, None -> None
      | _ ->
        Jsont.Error.msgf
          Jsont.Meta.none
          "normalize_servicer and its description_column go together"
    in
    { servicer; normalize; end_to_end; payment; hash })
  |> opt "servicer" Jsont.string
  |> opt "normalize_servicer" Camt_profile.hex_codec
  |> opt "description_column" Jsont.string
  |> opt "end_to_end" Jsont.string
  |> opt "payment" Jsont.string
  |> opt "hash" Jsont.string
  |> finish
;;

let reported_codec =
  Jsont.Object.map (fun quantity symbol : reported_spec -> { quantity; symbol })
  |> dec_only "quantity" Jsont.string
  |> dec_only "symbol" Jsont.string
  |> finish
;;

let balance_type_codec =
  Jsont.enum
    [ "opening", Records.Opening
    ; "closing", Records.Closing
    ; "available", Records.Available
    ; "day_end", Records.Day_end
    ]
;;

let balance_codec =
  Jsont.Object.map (fun balance_type quantity decimal commodity as_of : balance_spec ->
    { balance_type; quantity; decimal; commodity; as_of })
  |> dec_only "type" balance_type_codec
  |> dec_only "quantity" Jsont.string
  |> opt "decimal" decimal_codec
  |> dec_only "commodity" commodity_codec
  |> dec_only "as_of" date_codec
  |> finish
;;

let account_codec =
  Jsont.Object.map (fun id bank number account_type currency : account_spec ->
    { id; bank; number; account_type; currency })
  |> dec_only "id" Jsont.string
  |> opt "bank" Jsont.string
  |> opt "number" Jsont.string
  |> opt "type" Jsont.string
  |> opt "currency" commodity_codec
  |> finish
;;

let id_codec =
  Jsont.Object.map (fun columns row_hash ->
    match columns, row_hash with
    | Some columns, false -> Columns columns
    | None, true -> Row_hash
    | _ -> Jsont.Error.msgf Jsont.Meta.none "an id is columns or row_hash")
  |> opt "columns" (Jsont.list Jsont.string)
  |> flag "row_hash"
  |> finish
;;

let total_codec =
  Jsont.Object.map (fun conditions quantity commodity : total_spec ->
    { conditions; quantity; commodity })
  |> list_or_empty "when" condition_codec
  |> dec_only "quantity" quantity_codec
  |> dec_only "commodity" commodity_codec
  |> finish
;;

let typed_codec =
  Jsont.Object.map
    (fun
        ()
         ()
         institution
         institution_type
         separator
         decimal
         header
         observation
         only
         require
         total
         account
         id
         occurred
         value_on
         cases
         reported
         descriptions
         movement_type
         counterparty_name
         counterparty_iban
         references
         exchange
         valuations
         balance_after
         balances
         journal_tags
         journal_description
         conflict_columns
       ->
       { raw = Jsont.Json.null ()
       ; institution
       ; institution_type
       ; separator
       ; decimal
       ; header
       ; observation
       ; only
       ; require
       ; total
       ; account
       ; id = Option.value id ~default:Inventory
       ; occurred
       ; value_on
       ; cases
       ; reported
       ; descriptions
       ; movement_type
       ; counterparty_name
       ; counterparty_iban
       ; references
       ; exchange
       ; valuations
       ; balance_after
       ; balances
       ; journal_tags
       ; journal_description
       ; conflict_columns
       })
  |> dec_only "format" (Json.tag "fin-csv-converter")
  |> dec_only "version" (Json.version 1)
  |> dec_only "institution" Jsont.string
  |> dec_only "institution_type" Institution_type.jsont
  |> Jsont.Object.mem "separator" separator_codec ~dec_absent:','
  |> Jsont.Object.mem "decimal" decimal_codec ~dec_absent:Dot
  |> dec_only "header" (Jsont.list Jsont.string)
  |> flag "observation"
  |> list_or_empty "only" condition_codec
  |> list_or_empty "require" requirement_codec
  |> opt "total" total_codec
  |> dec_only "account" account_codec
  |> opt "id" id_codec
  |> dec_only "occurred" date_codec
  |> opt "value_on" date_codec
  |> dec_only "cases" (Jsont.list case_codec)
  |> opt "reported" reported_codec
  |> list_or_empty "descriptions" Jsont.string
  |> opt "type" Jsont.string
  |> list_or_empty "counterparty_name" Jsont.string
  |> opt "counterparty_iban" Jsont.string
  |> opt "references" references_codec
  |> opt "exchange" exchange_codec
  |> list_or_empty "valuations" amount_codec
  |> opt "balance_after" Jsont.string
  |> list_or_empty "balances" balance_codec
  |> Jsont.Object.mem "journal_tags" Json.string_map ~dec_absent:[]
  |> opt "journal_description" Jsont.string
  |> list_or_empty "conflict_columns" Jsont.string
  |> finish
;;

(* Columns. A header may repeat a name, so [Solde#2] is the second column named Solde. *)
let column_index header name =
  let name, nth =
    match String.rsplit2 name ~on:'#' with
    | Some (base, nth) when Option.is_some (Int.of_string_opt nth) ->
      base, Int.of_string nth
    | _ -> name, 1
  in
  List.filter_mapi header ~f:(fun i column -> Option.some_if (String.equal column name) i)
  |> fun found -> List.nth found (nth - 1)
;;

let template_columns template =
  Re.all (Re.compile (Re.Perl.re {|\{([^}]*)\}|})) template
  |> List.map ~f:(fun group -> Re.Group.get group 1)
;;

(* Every column the definition names, so that a mistake is refused when it is read and not
   on the first row that reaches it. *)
let named_columns t =
  let date (d : date_spec) = d.column :: Option.to_list d.fallback in
  let quantity (q : quantity_spec) = q.column :: Option.to_list q.minus in
  let commodity = function
    | Fiat_column column | Reported_column { column; _ } -> [ column ]
    | Fiat_code _ -> []
  in
  let conditions =
    List.concat_map ~f:(fun (c : condition) ->
      match c.test with
      | Not_before other | Prefix_of other -> [ c.column; other ]
      | One_of _ | Sign _ | Filled _ | Matches _ -> [ c.column ])
  in
  let text = function
    | Column column -> [ column ]
    | Value _ -> []
  in
  List.concat
    [ conditions t.only
    ; List.concat_map t.require ~f:(fun r -> conditions (r.condition :: r.applies))
    ; Option.value_map t.total ~default:[] ~f:(fun total ->
        conditions total.conditions @ quantity total.quantity @ commodity total.commodity)
    ; template_columns t.account.id
    ; Option.value_map t.account.account_type ~default:[] ~f:template_columns
    ; Option.value_map t.account.currency ~default:[] ~f:commodity
    ; (match t.id with
       | Columns columns -> List.filter columns ~f:(Fn.non (String.equal "@reference"))
       | Inventory | Row_hash -> [])
    ; date t.occurred
    ; Option.value_map t.value_on ~default:[] ~f:date
    ; List.concat_map t.cases ~f:(fun case ->
        conditions case.conditions
        @ Option.value_map case.fee ~default:[] ~f:quantity
        @ List.concat_map case.movements ~f:(fun m ->
          quantity m.quantity @ commodity m.commodity))
    ; Option.value_map t.reported ~default:[] ~f:(fun r -> [ r.quantity; r.symbol ])
    ; t.descriptions
    ; Option.to_list t.movement_type
    ; t.counterparty_name
    ; Option.to_list t.counterparty_iban
    ; Option.value_map t.references ~default:[] ~f:(fun r ->
        List.filter_opt
          [ r.servicer; Option.map r.normalize ~f:snd; r.end_to_end; r.payment; r.hash ])
    ; Option.value_map t.exchange ~default:[] ~f:(fun e ->
        text e.source @ text e.target @ List.filter_opt [ e.rate; e.target_quantity ])
    ; List.concat_map t.valuations ~f:(fun v -> v.quantity :: commodity v.commodity)
    ; Option.to_list t.balance_after
    ; List.map t.journal_tags ~f:snd
    ; Option.to_list t.journal_description
    ; t.conflict_columns
    ; List.concat_map t.balances ~f:(fun b ->
        (b.quantity :: commodity b.commodity) @ date b.as_of)
    ]
;;

let check t =
  match
    List.filter (named_columns t) ~f:(fun name ->
      Option.is_none (column_index t.header name))
  with
  | [] -> ()
  | unknown ->
    Jsont.Error.msgf
      Jsont.Meta.none
      "%s: columns not in the header: %s"
      t.institution
      (String.concat ~sep:", " unknown)
;;

let codec =
  Jsont.map
    Jsont.json
    ~dec:(fun raw ->
      match Jsont.Json.decode' typed_codec raw with
      | Ok t ->
        check t;
        { t with raw }
      | Error error -> raise (Jsont.Error error))
    ~enc:(fun t -> t.raw)
;;

let of_string = Json.decode codec

(* Rows. *)

type row =
  { number : int
  ; cells : string list
  }

let raw_cell t row name =
  match column_index t.header name with
  | Some index -> Option.value (List.nth row.cells index) ~default:""
  | None -> Printf.failwithf "column %s is not in the header" name ()
;;

(* Stripped to interpret it. What the record states as text keeps the source's spelling. *)
let cell t row name = String.strip (raw_cell t row name)
let filled text = Option.some_if (not (String.is_empty text)) text

let quantity_in decimal text =
  (* Under a decimal comma, a dot is a group mark or a mistake: 1.234 is not 1.234. *)
  if Poly.equal decimal Comma && String.mem text '.'
  then Printf.failwithf "invalid quantity: %S" text ();
  let text =
    String.concat_map text ~f:(function
      | ' ' -> ""
      | ',' when Poly.equal decimal Comma -> "."
      | c -> String.make 1 c)
    (* Groups digits with a space, no-break space or narrow no-break space. *)
    |> String.substr_replace_all ~pattern:"\xc2\xa0" ~with_:""
    |> String.substr_replace_all ~pattern:"\xe2\x80\xaf" ~with_:""
  in
  if Re.execp (Re.compile (Re.Perl.re {|^-?[0-9]+(\.[0-9]+)?$|})) text
  then Quantity.of_string text
  else Printf.failwithf "invalid quantity: %S" text ()
;;

let quantity_of t text = quantity_in t.decimal text

let quantity_of_spec t row (spec : quantity_spec) =
  let value = quantity_of t (cell t row spec.column) in
  let value =
    match spec.minus with
    | Some column -> Quantity.(value - quantity_of t (cell t row column))
    | None -> value
  in
  if spec.negate then Quantity.neg value else value
;;

let optional_quantity t row name =
  Option.map (filled (cell t row name)) ~f:(quantity_of t)
;;

let commodity_of t row = function
  | Fiat_column column ->
    Validation.currency column (cell t row column) |> Or_error.ok_exn |> Commodity.fiat
  | Fiat_code code ->
    Validation.currency "commodity" code |> Or_error.ok_exn |> Commodity.fiat
  | Reported_column { column; provider } ->
    Commodity.reported ~provider ~symbol:(cell t row column) |> Or_error.ok_exn
;;

let holds t row (condition : condition) =
  let value = cell t row condition.column in
  match condition.test with
  | One_of values -> List.mem values value ~equal:String.equal
  | Filled wanted -> Bool.equal wanted (not (String.is_empty value))
  | Sign sign ->
    (match Option.try_with (fun () -> quantity_of t value), sign with
     | Some quantity, Positive -> Quantity.(quantity > zero)
     | Some quantity, Negative -> Quantity.(quantity < zero)
     | Some quantity, Not_positive -> Quantity.(quantity <= zero)
     | Some quantity, Not_negative -> Quantity.(quantity >= zero)
     | None, _ -> false)
  | Matches pattern -> Re.execp pattern value
  | Not_before other -> String.compare value (cell t row other) >= 0
  | Prefix_of other ->
    (not (String.is_empty value)) && String.is_prefix (cell t row other) ~prefix:value
;;

let holds_all t row = List.for_all ~f:(holds t row)

let temporal_of t row (spec : date_spec) =
  let value =
    match spec.fallback with
    | Some fallback when String.is_empty (cell t row spec.column) -> cell t row fallback
    | _ -> cell t row spec.column
  in
  let date_part = String.prefix value 10 in
  let well_formed =
    match spec.layout with
    | Iso_date -> String.length value = 10
    | Iso_datetime ->
      Re.execp
        (Re.compile
           (Re.Perl.re {|^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}|}))
        value
    | Day_first -> true
  in
  if not well_formed then Printf.failwithf "invalid date: %S" value ();
  let iso =
    match spec.layout with
    | Iso_date | Iso_datetime -> date_part
    | Day_first ->
      (match String.split date_part ~on:'-' with
       | [ day; month; year ] -> String.concat ~sep:"-" [ year; month; day ]
       | _ -> Printf.failwithf "invalid date: %S" value ())
  in
  (* A clock with no stated offset says nothing about the instant, so only its date is kept;
     the clock stays in the fields. *)
  let stated =
    match spec.layout, spec.utc with
    | Iso_datetime, true -> iso ^ "T" ^ String.drop_prefix value 11 ^ "Z"
    | _ -> iso
  in
  Records.temporal_of_string stated |> Or_error.ok_exn
;;

let expand t row template =
  List.fold (template_columns template) ~init:template ~f:(fun text column ->
    String.substr_replace_all
      text
      ~pattern:("{" ^ column ^ "}")
      ~with_:(cell t row column))
;;

let reference t row =
  Option.bind t.references ~f:(fun references ->
    Option.bind references.servicer ~f:(fun column ->
      let value = cell t row column in
      match references.normalize with
      | Some (config, description_column) ->
        Some
          (Camt_source.normalize_hex
             config
             ~description:(cell t row description_column)
             value)
      | None -> filled value))
;;

let details t row : Records.details =
  let stated column = filled (cell t row column) in
  let text = function
    | Column column -> stated column
    | Value value -> Some value
  in
  let name = List.find_map t.counterparty_name ~f:stated in
  let iban =
    Option.bind t.counterparty_iban ~f:stated
    |> Option.filter ~f:(fun value -> Result.is_ok (Iban.of_string value))
  in
  { descriptions =
      List.filter t.descriptions ~f:(fun column -> Option.is_some (stated column))
      |> List.map ~f:(raw_cell t row)
  ; movement_type =
      Option.map (Option.bind t.movement_type ~f:stated) ~f:Movement_type.of_string
  ; counterparty =
      Option.some_if
        (Option.is_some name || Option.is_some iban)
        ({ name; iban } : Records.counterparty)
  ; references =
      Option.map t.references ~f:(fun r : Records.references ->
        { servicer = reference t row
        ; end_to_end = Option.bind r.end_to_end ~f:stated
        ; payment = Option.bind r.payment ~f:stated
        ; hash = Option.bind r.hash ~f:stated
        })
  ; exchange =
      Option.bind t.exchange ~f:(fun e ->
        match text e.source, text e.target with
        | Some source, Some target ->
          Some
            ({ source
             ; target
             ; rate =
                 Option.bind e.rate ~f:(optional_quantity t row)
                 |> Option.filter ~f:Quantity.(fun value -> value >= zero)
             ; target_quantity =
                 Option.bind e.target_quantity ~f:(optional_quantity t row)
             }
             : Records.exchange)
        | _ -> None)
  ; reported = None
  ; balance_after = Option.bind t.balance_after ~f:(optional_quantity t row)
  ; valuations =
      List.filter_map t.valuations ~f:(fun v ->
        Option.map
          (optional_quantity t row v.quantity)
          ~f:(fun quantity : Records.amount ->
            { quantity; commodity = commodity_of t row v.commodity }))
  ; fields =
      List.filter_map (List.zip_exn t.header row.cells) ~f:(fun (name, value) ->
        Option.some_if (not (String.is_empty (String.strip value))) (name, value))
  }
;;

let row_hash row =
  Json.encode (Jsont.list Jsont.string) row.cells
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let record t ~positions ~statements row : Records.record =
  let account = expand t row t.account.id in
  let occurred = temporal_of t row t.occurred in
  let id =
    match t.id with
    | Columns columns ->
      List.map columns ~f:(function
        | "@reference" -> Option.value (reference t row) ~default:""
        | column -> cell t row column)
      |> String.concat ~sep:"/"
    | Row_hash -> "row-sha256-" ^ row_hash row
    | Inventory ->
      let day = Date.to_string (Records.local_date occurred) in
      let key = account ^ "\000" ^ day in
      let position = Hashtbl.find positions key |> Option.value ~default:0 in
      Hashtbl.set positions ~key ~data:(position + 1);
      (* An observation describes a row stated elsewhere, which can use this same scheme. *)
      Printf.sprintf
        "%s%s/%d"
        (if t.observation then "observation/" else "")
        day
        (position + 1)
  in
  let case = List.find t.cases ~f:(fun case -> holds_all t row case.conditions) in
  let movements =
    Option.value_map case ~default:[] ~f:(fun case ->
      List.map case.movements ~f:(fun m : Records.movement ->
        { role = m.role
        ; quantity = quantity_of_spec t row m.quantity
        ; commodity = commodity_of t row m.commodity
        }))
  in
  let details = details t row in
  let details =
    if not (List.is_empty movements)
    then details
    else
      { details with
        reported =
          Option.bind t.reported ~f:(fun r ->
            Option.map
              (filled (cell t row r.symbol))
              ~f:(fun symbol : Records.reported ->
                { quantity = quantity_of t (cell t row r.quantity); symbol }))
      ; balance_after = None
      }
  in
  { id
  ; account
  ; statements
  ; occurred
  ; value_on =
      Option.map t.value_on ~f:(fun spec -> Records.local_date (temporal_of t row spec))
  ; movements
  ; fee =
      Option.bind case ~f:(fun case -> case.fee)
      |> Option.map ~f:(quantity_of_spec t row)
      |> Option.filter ~f:(Fn.non Quantity.is_zero)
  ; group = None
  ; supporting = false
  ; observation = t.observation
  ; details
  }
;;

let account_of t row : Records.account =
  let currency =
    Option.bind t.account.currency ~f:(fun spec ->
      match commodity_of t row spec with
      | Fiat currency -> Some currency
      | Onchain_native _ | Onchain_token _ | Reported _ -> None)
  in
  { id = expand t row t.account.id
  ; bank = t.account.bank
  ; number = t.account.number
  ; account_type = Option.map t.account.account_type ~f:(expand t row)
  ; currency
  ; iban = Unstated
  }
;;

let rows_of t contents =
  Csv_reader.check_utf8 ~name:t.institution contents;
  let input = Csv_reader.channel ~separator:t.separator contents in
  let lines = Csv.input_all input in
  match lines with
  | [] -> failwith "the CSV has no header"
  | header :: rows ->
    if not (List.equal String.equal header t.header)
    then Printf.failwithf "not the %s CSV layout" t.institution ();
    List.filter_mapi rows ~f:(fun index cells ->
      if List.for_all cells ~f:(fun cell -> String.is_empty (String.strip cell))
      then None
      else if List.length cells <> List.length t.header
      then Printf.failwithf "row %d has the wrong number of columns" (index + 1) ()
      else Some { number = index + 1; cells })
;;

let recognizes t contents =
  Csv_reader.recognizes ~separator:t.separator ~trim:false ~header:t.header contents
;;

let filtered_rows t contents =
  Or_error.try_with (fun () ->
    List.count (rows_of t contents) ~f:(fun row -> not (holds_all t row t.only)))
;;

let at_row row f =
  try f () with
  | exn -> Printf.failwithf "row %d: %s" row.number (Exn.to_string exn) ()
;;

let convert t ~name contents =
  Or_error.try_with (fun () ->
    let sha256 = Digestif.SHA256.(digest_string contents |> to_hex) in
    let rows = rows_of t contents |> List.filter ~f:(fun row -> holds_all t row t.only) in
    List.iter rows ~f:(fun row ->
      List.iter t.require ~f:(fun { applies; condition } ->
        if holds_all t row applies && not (holds t row condition)
        then (
          let value = cell t row condition.column in
          let number = row.number
          and column = condition.column in
          match condition.test with
          | Not_before other ->
            Printf.failwithf
              "row %d: %s %S precedes %s %S"
              number
              column
              value
              other
              (cell t row other)
              ()
          | Prefix_of other ->
            Printf.failwithf
              "row %d: %s %S disagrees with %s %S"
              number
              column
              value
              other
              (cell t row other)
              ()
          | One_of _ | Sign _ | Filled _ | Matches _ ->
            Printf.failwithf "row %d: unexpected %s: %S" number column value ())));
    (* A row stating what the others add up to is not a movement. *)
    let totals, rows =
      List.partition_tf rows ~f:(fun row ->
        Option.exists t.total ~f:(fun total -> holds_all t row total.conditions))
    in
    let account_rows =
      List.map rows ~f:(fun row -> at_row row (fun () -> account_of t row))
    in
    let statements : Records.statement list =
      List.filter_map totals ~f:(fun row ->
        at_row row (fun () ->
          match t.total, List.hd account_rows with
          | Some total, Some account ->
            Some
              ({ id = "total-" ^ String.prefix sha256 16
               ; account = account.id
               ; period = None
               ; total =
                   Some
                     { quantity = quantity_of_spec t row total.quantity
                     ; commodity = commodity_of t row total.commodity
                     }
               }
               : Records.statement)
          | _ -> None))
    in
    let listed = List.map statements ~f:(fun s -> s.id) in
    let positions = Hashtbl.create (module String) in
    let records =
      List.map rows ~f:(fun row ->
        at_row row (fun () -> record t ~positions ~statements:listed row))
    in
    let balances : Records.balance list =
      List.concat_map rows ~f:(fun row ->
        at_row row (fun () ->
          List.filter_map t.balances ~f:(fun spec ->
            Option.map
              (Option.map
                 (filled (cell t row spec.quantity))
                 ~f:(quantity_in (Option.value spec.decimal ~default:t.decimal)))
              ~f:(fun quantity : Records.balance ->
                { account = expand t row t.account.id
                ; balance_type = spec.balance_type
                ; quantity
                ; commodity = commodity_of t row spec.commodity
                ; as_of = temporal_of t row spec.as_of
                }))))
    in
    ({ institution = t.institution
     ; identity =
         (match t.id with
          | Inventory -> Inventory
          | Row_hash | Columns _ -> Source)
     ; converter = Some { name = "fin csv " ^ t.institution; version = Some "1" }
     ; sources = [ { name; sha256 } ]
     ; accounts =
         List.dedup_and_sort account_rows ~compare:(fun (a : Records.account) b ->
           String.compare a.id b.id)
     ; statements
     ; balances
     ; records
     }
     : Records.t))
;;
