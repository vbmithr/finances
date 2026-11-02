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
    (fun () () institution aliases payload_tag account header columns rules ->
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
       { institution; aliases; payload_tag; account; header; columns; rules })
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "kind" (Json.tag "movement-csv") ~enc:(fun _ -> ())
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun p -> p.institution)
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

let parse profile ~file contents =
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
      let cell name =
        List.Assoc.find_exn fields name ~equal:String.equal |> String.strip
      in
      let parse_leg asset amount =
        let commodity =
          Commodity.reported ~provider:profile.institution ~symbol:asset
          |> Or_error.ok_exn
        in
        { commodity; amount = Quantity.of_string amount }
      in
      let timestamp = cell columns.timestamp_column in
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
      let from_leg =
        parse_leg (cell columns.currency_column) (cell columns.amount_column)
      in
      let to_currency, to_amount =
        cell columns.to_currency_column, cell columns.to_amount_column
      in
      let to_leg, incomplete_to =
        match String.is_empty to_currency, String.is_empty to_amount with
        | true, true -> None, false
        | false, false -> Some (parse_leg to_currency to_amount), false
        | _ -> None, true
      in
      let kind = cell columns.kind_column in
      let interpretation, proposed_legs =
        if incomplete_to
        then Review "Incomplete To currency/amount pair", []
        else interpret profile ~kind ~from_leg ~to_leg
      in
      let evidence = [ Operation.File_row { document = file; row = i + 1 } ] in
      let movements =
        List.map proposed_legs ~f:(fun leg ->
          Operation.movement ~commodity:leg.commodity ~quantity:leg.amount ~evidence ()
          |> Or_error.ok_exn)
      in
      let operation =
        Operation.create ~occurred:(Temporal.Instant timestamp) ~evidence ~movements
        |> Or_error.ok_exn
      in
      { file
      ; row_number = i + 1
      ; timestamp
      ; kind
      ; fields
      ; interpretation
      ; proposed_legs
      ; operation
      }))
  |> Or_error.tag ~tag:(profile.institution ^ " CSV " ^ file)
;;

let import_rows profile rows =
  List.map rows ~f:(fun row ->
    let open Or_error.Let_syntax in
    let payload = Json.encode Json.string_map row.fields in
    let source_id = Digestif.SHA256.(to_hex (digest_string payload)) in
    let%bind source =
      Source_event.create
        ~venue:profile.institution
        ~account:profile.account
        ~kind:profile.payload_tag
        ~source_id
        ~occurred_at:row.timestamp
        ~payload
        ~fields:row.fields
        ()
    in
    let roles =
      match row.interpretation, row.proposed_legs with
      | Single, [ leg ] -> [ "primary", leg ]
      | Exchange, [ first; second ] -> [ "from", first; "to", second ]
      | Review _, [] -> []
      | _ -> failwith "invalid movement CSV interpretation"
    in
    let%bind legs =
      List.map roles ~f:(fun (role, leg) ->
        Operation_import.leg ~role ~commodity:leg.commodity ~quantity:leg.amount)
      |> Or_error.combine_errors
    in
    Operation_import.row ~source ~legs)
  |> Or_error.combine_errors
;;
