open Base

(* A user-selected layout; account identity and overlap remain import policy. *)
type account_type =
  | Prepaid_card
  | Credit_card
  | Current
  | Savings

let account_type_name = function
  | Prepaid_card -> "prepaid-card"
  | Credit_card -> "credit-card"
  | Current -> "current"
  | Savings -> "savings"
;;

let account_type_codec =
  Jsont.enum
    [ "prepaid-card", Prepaid_card
    ; "credit-card", Credit_card
    ; "current", Current
    ; "savings", Savings
    ]
;;

type date_format =
  | Iso
  | Iso_datetime
  | Day_first
  | Month_first

type amount_format =
  | Dot_decimal
  | Comma_decimal

let date_format_codec =
  Jsont.enum
    [ "iso", Iso
    ; "iso-datetime", Iso_datetime
    ; "day-first", Day_first
    ; "month-first", Month_first
    ]
;;

let amount_format_codec = Jsont.enum [ "dot", Dot_decimal; "comma", Comma_decimal ]

let date_cell format =
  let pattern = Re.Perl.compile_pat "^[0-9]{2}[/-][0-9]{2}[/-][0-9]{4}$" in
  match format with
  | Iso -> Csv_schema.Cell.iso_date ()
  | Iso_datetime ->
    Csv_schema.Cell.map (Csv_schema.Cell.utc_clock ()) ~f:(fun instant ->
      let year, month, day = Ptime.to_date instant in
      Date.of_string (Printf.sprintf "%04d-%02d-%02d" year month day))
  | Day_first | Month_first ->
    Csv_schema.Cell.create ~name:"date" (fun value ->
      if (not (Re.execp pattern value)) || not (Char.equal value.[2] value.[5])
      then failwith "expected a date with two-digit day and month and a four-digit year";
      let first = String.prefix value 2 in
      let second = String.sub value ~pos:3 ~len:2 in
      let year = String.sub value ~pos:6 ~len:4 in
      let month, day =
        match format with
        | Day_first -> second, first
        | Month_first -> first, second
        | Iso | Iso_datetime -> assert false
      in
      Date.of_string (year ^ "-" ^ month ^ "-" ^ day))
;;

let amount_cell = function
  | Dot_decimal -> Csv_schema.Cell.decimal_balance
  | Comma_decimal -> Csv_schema.Cell.french_amount
;;

type t =
  { separator : char
  ; header : string list
  ; date_column : string
  ; date_format : date_format
  ; amount_format : amount_format
  ; description_column : string
  ; amount_column : string
  ; currency : Iso4217.t
  ; account_type : account_type
  ; invert_amount : bool
  ; skip_when : (string * string) option
  }

type row =
  { row_number : int
  ; fields : string list
  ; date : Date.t
  ; description : string
  ; amount : Quantity.t
  ; currency : Iso4217.t
  ; account_type : string
  }

let column header name =
  match
    List.filter_mapi header ~f:(fun i value -> Option.some_if (String.equal value name) i)
  with
  | [ i ] -> i
  | [] -> failwith (Printf.sprintf "missing column %S" name)
  | _ -> failwith (Printf.sprintf "ambiguous column %S" name)
;;

let parse profile contents =
  Or_error.try_with (fun () ->
    Csv_reader.check_utf8 ~name:"CSV" contents;
    let date = column profile.header profile.date_column in
    let description = column profile.header profile.description_column in
    let amount = column profile.header profile.amount_column in
    let skip =
      Option.map profile.skip_when ~f:(fun (name, value) ->
        column profile.header name, value)
    in
    let schema =
      let open Csv_schema in
      make (fun row_number fields date description amount ->
        { row_number
        ; fields
        ; date
        ; description
        ; amount = (if profile.invert_amount then Quantity.neg amount else amount)
        ; currency = profile.currency
        ; account_type = account_type_name profile.account_type
        })
      |> row_number
      |> raw_fields
      |> col_pos date (date_cell profile.date_format)
      |> col_pos description (Cell.text ~name:"description" ())
      |> col_pos amount (amount_cell profile.amount_format)
      |> create
           ~name:"CSV"
           ~separator:profile.separator
           ~header:profile.header
           ~trim_cells:true
    in
    let input = Csv_reader.channel ~separator:profile.separator contents in
    Csv_reader.check_header
      ~trim:false
      ~header:profile.header
      ~message:"CSV header does not match the saved layout"
      input;
    Csv.input_all input
    |> List.mapi ~f:(fun index fields ->
      if List.length fields <> List.length profile.header
      then
        failwith (Printf.sprintf "CSV row %d has the wrong number of columns" (index + 1));
      match skip with
      | Some (pos, value)
        when String.equal (String.strip (List.nth_exn fields pos)) value
             && String.is_empty (String.strip (List.nth_exn fields date)) -> None
      | _ ->
        Some
          (Csv_schema.of_fields schema ~row_number:(index + 1) fields |> Or_error.ok_exn))
    |> List.filter_opt)
  |> Or_error.tag ~tag:"CSV profile"
;;

let codec =
  Jsont.Object.map
    (fun
        separator
         header
         date_column
         date_format
         amount_format
         description_column
         amount_column
         currency
         account_type
         invert_amount
         skip_column
         skip_value
       ->
       if String.length separator <> 1 then failwith "CSV separator must be one byte";
       let skip_when =
         match skip_column, skip_value with
         | None, None -> None
         | Some column, Some value -> Some (column, value)
         | _ -> failwith "CSV skip column and value must both be present"
       in
       { separator = separator.[0]
       ; header
       ; date_column
       ; date_format
       ; amount_format
       ; description_column
       ; amount_column
       ; currency
       ; account_type
       ; invert_amount
       ; skip_when
       })
  |> Jsont.Object.mem "separator" Jsont.string ~enc:(fun (t : t) ->
    String.of_char t.separator)
  |> Jsont.Object.mem "header" (Jsont.list Jsont.string) ~enc:(fun (t : t) -> t.header)
  |> Jsont.Object.mem "date" Jsont.string ~enc:(fun (t : t) -> t.date_column)
  |> Jsont.Object.mem "date_format" date_format_codec ~dec_absent:Iso ~enc:(fun (t : t) ->
    t.date_format)
  |> Jsont.Object.mem
       "amount_format"
       amount_format_codec
       ~dec_absent:Dot_decimal
       ~enc:(fun (t : t) -> t.amount_format)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun (t : t) ->
    t.description_column)
  |> Jsont.Object.mem "amount" Jsont.string ~enc:(fun (t : t) -> t.amount_column)
  |> Jsont.Object.mem "currency" Json.currency ~enc:(fun (t : t) -> t.currency)
  |> Jsont.Object.mem "account_type" account_type_codec ~enc:(fun (t : t) ->
    t.account_type)
  |> Jsont.Object.mem "invert_amount" Jsont.bool ~enc:(fun (t : t) -> t.invert_amount)
  |> Jsont.Object.mem "skip_column" (Jsont.option Jsont.string) ~enc:(fun (t : t) ->
    Option.map t.skip_when ~f:fst)
  |> Jsont.Object.mem "skip_value" (Jsont.option Jsont.string) ~enc:(fun (t : t) ->
    Option.map t.skip_when ~f:snd)
  |> Jsont.Object.finish
;;
