open Base

type leg =
  { commodity : Commodity.t
  ; amount : Quantity.t
  }

type interpretation =
  | Single
  | Exchange
  | Review of string

type row =
  { file : string
  ; row_number : int
  ; timestamp : Ptime.t
  ; kind : string
  ; fields : (string * string) list
  ; interpretation : interpretation
  ; proposed_legs : leg list
  ; operation : Operation.t
  }

type kind_summary =
  { kind : string
  ; rows : int
  ; single : int
  ; exchange : int
  ; review : int
  }

type row_reference =
  { file : string
  ; row_number : int
  }

type summary =
  { total : int
  ; by_kind : kind_summary list
  ; identical_rows : row_reference list list
  }

let summarize rows =
  let counts = Hashtbl.create (module String) in
  let identical = Hashtbl.Poly.create () in
  List.iter rows ~f:(fun (row : row) ->
    Hashtbl.update counts row.kind ~f:(fun old ->
      let current =
        Option.value
          old
          ~default:{ kind = row.kind; rows = 0; single = 0; exchange = 0; review = 0 }
      in
      match row.interpretation with
      | Single -> { current with rows = current.rows + 1; single = current.single + 1 }
      | Exchange ->
        { current with rows = current.rows + 1; exchange = current.exchange + 1 }
      | Review _ -> { current with rows = current.rows + 1; review = current.review + 1 });
    Hashtbl.add_multi
      identical
      ~key:row.fields
      ~data:{ file = row.file; row_number = row.row_number });
  let by_kind =
    Hashtbl.data counts
    |> List.sort ~compare:(fun a b ->
      match Int.compare b.rows a.rows with
      | 0 -> String.compare a.kind b.kind
      | n -> n)
  in
  let compare_ref a b =
    match String.compare a.file b.file with
    | 0 -> Int.compare a.row_number b.row_number
    | n -> n
  in
  let identical_rows =
    Hashtbl.data identical
    |> List.filter ~f:(fun group -> List.length group > 1)
    |> List.map ~f:(List.sort ~compare:compare_ref)
    |> List.sort ~compare:(List.compare compare_ref)
  in
  { total = List.length rows; by_kind; identical_rows }
;;

(** A CSV export that reports one source row as one or two commodity movements
    (a "from" leg and an optional "to" leg), described entirely by data: the
    header, the columns and the rules that interpret each reported kind. *)
type shape =
  | Single_in
  | Single_out
  | Exchange

type rule =
  { kinds : string list
  ; legs : shape
  }

type columns =
  { timestamp_column : string
  ; currency_column : string
  ; amount_column : string
  ; to_currency_column : string
  ; to_amount_column : string
  ; kind_column : string
  }

type profile =
  { institution : string
  ; institution_type : Institution_type.t
  ; aliases : string list
  ; payload_tag : string
  ; account : string
  ; header : string list
  ; columns : columns
  ; rules : rule list
  }

let shape_codec =
  Jsont.enum [ "single-in", Single_in; "single-out", Single_out; "exchange", Exchange ]
;;

let rule_codec =
  Jsont.Object.map (fun kinds legs -> { kinds; legs })
  |> Jsont.Object.mem "kinds" (Jsont.list Jsont.string) ~enc:(fun r -> r.kinds)
  |> Jsont.Object.mem "legs" shape_codec ~enc:(fun r -> r.legs)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let columns_codec =
  Jsont.Object.map
    (fun
        timestamp_column
         currency_column
         amount_column
         to_currency_column
         to_amount_column
         kind_column
       ->
       { timestamp_column
       ; currency_column
       ; amount_column
       ; to_currency_column
       ; to_amount_column
       ; kind_column
       })
  |> Jsont.Object.mem "timestamp" Jsont.string ~enc:(fun c -> c.timestamp_column)
  |> Jsont.Object.mem "currency" Jsont.string ~enc:(fun c -> c.currency_column)
  |> Jsont.Object.mem "amount" Jsont.string ~enc:(fun c -> c.amount_column)
  |> Jsont.Object.mem "to_currency" Jsont.string ~enc:(fun c -> c.to_currency_column)
  |> Jsont.Object.mem "to_amount" Jsont.string ~enc:(fun c -> c.to_amount_column)
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun c -> c.kind_column)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let profile_codec =
  Jsont.Object.map
    (fun
        ()
         ()
         institution
         institution_type
         aliases
         payload_tag
         account
         header
         columns
         rules
       ->
       let institution = Validation.token "institution" institution |> Json.or_error in
       let aliases =
         List.map aliases ~f:(fun alias ->
           Validation.token "institution alias" alias |> Json.or_error)
       in
       let payload_tag = Validation.token "payload tag" payload_tag |> Json.or_error in
       let account = Validation.token "account" account |> Json.or_error in
       List.iter
         [ columns.timestamp_column
         ; columns.currency_column
         ; columns.amount_column
         ; columns.to_currency_column
         ; columns.to_amount_column
         ; columns.kind_column
         ]
         ~f:(fun column ->
           if not (List.mem header column ~equal:String.equal)
           then
             Json.or_error
               (Or_error.errorf "column %S is not in the profile header" column));
       { institution
       ; institution_type
       ; aliases
       ; payload_tag
       ; account
       ; header
       ; columns
       ; rules
       })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "kind" (Json.tag "movement-csv") ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
  |> Jsont.Object.mem "type" Institution_type.jsont ~enc:(fun p -> p.institution_type)
  |> Jsont.Object.mem "aliases" (Jsont.list Jsont.string) ~enc:(fun p -> p.aliases)
  |> Jsont.Object.mem "payload_tag" Jsont.string ~enc:(fun p -> p.payload_tag)
  |> Jsont.Object.mem "account" Jsont.string ~enc:(fun p -> p.account)
  |> Jsont.Object.mem "header" (Jsont.list Jsont.string) ~enc:(fun p -> p.header)
  |> Jsont.Object.mem "columns" columns_codec ~enc:(fun p -> p.columns)
  |> Jsont.Object.mem "rules" (Jsont.list rule_codec) ~enc:(fun p -> p.rules)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let names profile = profile.institution :: profile.aliases

let interpret profile ~kind ~from_leg ~to_leg =
  let open Quantity in
  let positive = from_leg.amount > zero
  and negative = from_leg.amount < zero in
  let matches rule =
    List.mem rule.kinds kind ~equal:String.equal
    &&
    match rule.legs, to_leg with
    | Single_in, None -> positive
    | Single_out, None -> negative
    | Exchange, Some target ->
      negative
      && target.amount > zero
      && not (Commodity.equal from_leg.commodity target.commodity)
    | (Single_in | Single_out), Some _ | Exchange, None -> false
  in
  match List.find profile.rules ~f:matches with
  | Some { legs = Single_in | Single_out; _ } -> Single, [ from_leg ]
  | Some { legs = Exchange; _ } -> Exchange, from_leg :: Option.to_list to_leg
  | None -> Review "Unrecognized kind or field pattern; no balance effects proposed", []
;;

let cell fields name =
  List.Assoc.find fields name ~equal:String.equal |> Option.map ~f:String.strip
;;

(* The leg and kind columns of one row, as raw fields. *)
let read_row profile ~fields =
  let open Or_error.Let_syntax in
  let parse_leg asset amount =
    let%bind commodity = Commodity.reported ~provider:profile.institution ~symbol:asset in
    let%map amount = Or_error.try_with (fun () -> Quantity.of_string amount) in
    { commodity; amount }
  in
  let columns = profile.columns in
  let required name =
    Or_error.of_option
      (cell fields name)
      ~error:(Error.of_string ("missing column " ^ name))
  in
  let%bind asset = required columns.currency_column in
  let%bind amount = required columns.amount_column in
  let%bind from_leg = parse_leg asset amount in
  let%bind to_currency = required columns.to_currency_column in
  let%bind to_amount = required columns.to_amount_column in
  let%bind kind = required columns.kind_column in
  let%bind to_leg, incomplete_to =
    match String.is_empty to_currency, String.is_empty to_amount with
    | true, true -> Ok (None, false)
    | false, false ->
      let%map leg = parse_leg to_currency to_amount in
      Some leg, false
    | _ -> Ok (None, true)
  in
  Ok (kind, from_leg, to_leg, incomplete_to)
;;

let interpret_row profile ~kind ~from_leg ~to_leg ~incomplete_to =
  if incomplete_to
  then Review "Incomplete To currency/amount pair", []
  else interpret profile ~kind ~from_leg ~to_leg
;;

(** A parsed CSV row without its file context: enough to build both the
    read-only inspection view and the durable source event. *)
type parsed_row =
  { row_number : int
  ; timestamp : Ptime.t
  ; kind : string
  ; fields : (string * string) list
  ; interpretation : interpretation
  ; proposed_legs : leg list
  }

let parse_rows profile contents =
  let header = profile.header in
  let columns = profile.columns in
  Or_error.try_with (fun () ->
    Csv_reader.check_utf8 ~name:profile.institution contents;
    let input = Csv_reader.channel ~separator:',' contents in
    Csv_reader.check_header
      ~trim:false
      ~header
      ~message:("unexpected " ^ profile.institution ^ " CSV header")
      input;
    Csv.input_all input
    |> List.mapi ~f:(fun i values ->
      if List.length values <> List.length header
      then failwith (Printf.sprintf "row %d has the wrong number of columns" (i + 1));
      let fields = List.zip_exn header values in
      let timestamp = cell fields columns.timestamp_column |> Option.value_exn in
      let timestamp =
        Csv_schema.Cell.utc_clock ()
        |> fun clock ->
        let schema =
          Csv_schema.(
            make Fn.id
            |> col_pos 0 clock
            |> create
                 ~name:"timestamp"
                 ~separator:','
                 ~header:[ "timestamp" ]
                 ~trim_cells:true)
        in
        Csv_schema.of_fields schema ~row_number:(i + 1) [ timestamp ] |> Or_error.ok_exn
      in
      let kind, from_leg, to_leg, incomplete_to =
        read_row profile ~fields |> Or_error.ok_exn
      in
      let interpretation, proposed_legs =
        interpret_row profile ~kind ~from_leg ~to_leg ~incomplete_to
      in
      { row_number = i + 1; timestamp; kind; fields; interpretation; proposed_legs }))
  |> Or_error.tag ~tag:(profile.institution ^ " CSV")
;;

let parse profile ~file contents =
  Or_error.map
    (parse_rows profile contents)
    ~f:
      (List.map ~f:(fun (r : parsed_row) ->
         let evidence = [ Operation.File_row { document = file; row = r.row_number } ] in
         let movements =
           List.map r.proposed_legs ~f:(fun leg ->
             Operation.movement ~commodity:leg.commodity ~quantity:leg.amount ~evidence ()
             |> Or_error.ok_exn)
         in
         let operation =
           Operation.create ~occurred:(Temporal.Instant r.timestamp) ~evidence ~movements
           |> Or_error.ok_exn
         in
         { file
         ; row_number = r.row_number
         ; timestamp = r.timestamp
         ; kind = r.kind
         ; fields = r.fields
         ; interpretation = r.interpretation
         ; proposed_legs = r.proposed_legs
         ; operation
         }))
  |> Or_error.tag ~tag:(profile.institution ^ " CSV " ^ file)
;;

(* One durable source event per CSV row; the row's fields are the payload, so
   the proposal can be re-derived from the stored event alone. *)
let source_event profile ~timestamp ~fields =
  let payload = Json.encode Json.string_map fields in
  let source_id = Digestif.SHA256.(to_hex (digest_string payload)) in
  Source_event.create
    ~venue:profile.institution
    ~account:profile.account
    ~kind:profile.payload_tag
    ~source_id
    ~occurred_at:timestamp
    ~payload
    ~fields
    ()
;;

let legs ~interpretation ~proposed_legs =
  let roles =
    match interpretation, proposed_legs with
    | Single, [ leg ] -> [ "primary", leg ]
    | Exchange, [ first; second ] -> [ "from", first; "to", second ]
    | Review _, [] -> []
    | _ -> failwith "invalid movement CSV interpretation"
  in
  List.map roles ~f:(fun (role, leg) ->
    Operation_import.leg ~role ~commodity:leg.commodity ~quantity:leg.amount)
  |> Or_error.combine_errors
;;

let import_rows profile rows =
  List.map rows ~f:(fun row ->
    let open Or_error.Let_syntax in
    let%bind source = source_event profile ~timestamp:row.timestamp ~fields:row.fields in
    let%map legs =
      legs ~interpretation:row.interpretation ~proposed_legs:row.proposed_legs
    in
    Operation_import.row ~source ~legs)
  |> Or_error.combine_errors
;;

(* The source events of a whole export, as a document's loose events. *)
let events profile contents =
  let open Or_error.Let_syntax in
  let%bind rows = parse_rows profile contents in
  List.map rows ~f:(fun (r : parsed_row) ->
    source_event profile ~timestamp:r.timestamp ~fields:r.fields)
  |> Or_error.combine_errors
;;

(* Re-derive the proposed rows of a stored event from its fields. *)
let operation_rows_of_event profile (event : Source_event.t) =
  let open Or_error.Let_syntax in
  let%bind kind, from_leg, to_leg, incomplete_to =
    read_row profile ~fields:event.fields
  in
  let interpretation, proposed_legs =
    interpret_row profile ~kind ~from_leg ~to_leg ~incomplete_to
  in
  let%bind legs = legs ~interpretation ~proposed_legs in
  let%map row = Operation_import.row ~source:event ~legs in
  [ row ]
;;
