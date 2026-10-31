open Base

(* Positions are zero-based, so duplicate header names remain distinguishable. *)
type t =
  { name : string
  ; header : string list
  ; separator : char
  ; currency : Iso4217.t option
  ; trim_header : bool
  ; trim_cells : bool
  ; skip_blank_rows : bool
  ; account_id : int
  ; booked_on : int
  ; value_on : int option
  ; amount : int
  ; description : int
  ; suggested_description : int option
  ; category : int option
  ; parent_category : int option
  ; comment : int option
  ; account_label : int option
  ; reported_balance : int option
  ; pointage : int option
  ; date_format : Csv_profile.date_format
  ; amount_format : Csv_profile.amount_format
  ; balance_format : Csv_profile.amount_format
  }

type row =
  { row_number : int
  ; account_id : string
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Quantity.t
  ; currency : Iso4217.t option
  ; description : string
  ; suggested_description : string option
  ; category : string option
  ; parent_category : string option
  ; comment : string option
  ; account_label : string option
  ; reported_balance : Quantity.t option
  ; pointage : string option
  ; fields : string list
  }

let schema profile =
  let open Csv_schema in
  let required pos cell step =
    col ~pos ~name:(List.nth_exn profile.header pos) cell step
  in
  let optional pos cell step =
    match pos with
    | None -> constant None step
    | Some pos -> required pos (Cell.opt cell) step
  in
  make
    (fun
        row_number
         account_id
         booked_on
         value_on
         amount
         description
         suggested_description
         category
         parent_category
         comment
         account_label
         reported_balance
         pointage
         fields
       ->
       { row_number
       ; account_id
       ; booked_on
       ; value_on
       ; amount
       ; currency = profile.currency
       ; description
       ; suggested_description
       ; category
       ; parent_category
       ; comment
       ; account_label
       ; reported_balance
       ; pointage
       ; fields
       })
  |> row_number
  |> required profile.account_id (Cell.text ~name:"CSV account ID" ())
  |> required profile.booked_on (Csv_profile.date_cell profile.date_format)
  |> optional profile.value_on (Csv_profile.date_cell profile.date_format)
  |> required profile.amount (Csv_profile.amount_cell profile.amount_format)
  |> required
       profile.description
       (Cell.required ~message:"CSV original description is required" Cell.string)
  |> optional profile.suggested_description Cell.string
  |> optional profile.category Cell.string
  |> optional profile.parent_category Cell.string
  |> optional profile.comment Cell.string
  |> optional profile.account_label Cell.string
  |> optional profile.reported_balance (Csv_profile.amount_cell profile.balance_format)
  |> optional profile.pointage Cell.string
  |> raw_fields
  |> create
       ~name:profile.name
       ~separator:profile.separator
       ~header:profile.header
       ~trim_header:profile.trim_header
       ~trim_cells:profile.trim_cells
       ~skip_blank_rows:profile.skip_blank_rows
       ~columns_message:(fun expected _ ->
         Printf.sprintf "expected exactly %d CSV columns" expected)
;;

let codec =
  let field name codec (enc : t -> _) = Jsont.Object.mem name codec ~enc in
  Jsont.Object.map
    (fun
        ()
         name
         header
         separator
         currency
         trim_header
         trim_cells
         skip_blank_rows
         account_id
         booked_on
         value_on
         amount
         description
         suggested_description
         category
         parent_category
         comment
         account_label
         reported_balance
         pointage
         date_format
         amount_format
         balance_format
       ->
       if String.length separator <> 1 then failwith "CSV separator must be one byte";
       let indices =
         [ Some account_id
         ; Some booked_on
         ; value_on
         ; Some amount
         ; Some description
         ; suggested_description
         ; category
         ; parent_category
         ; comment
         ; account_label
         ; reported_balance
         ; pointage
         ]
         |> List.filter_opt
       in
       if
         List.is_empty header
         || List.exists indices ~f:(fun i -> i < 0 || i >= List.length header)
       then failwith "companion column index is outside the header";
       if
         List.contains_dup
           [ account_id; booked_on; amount; description ]
           ~compare:Int.compare
       then failwith "companion account, date, amount and description columns must differ";
       { name
       ; header
       ; separator = separator.[0]
       ; currency
       ; trim_header
       ; trim_cells
       ; skip_blank_rows
       ; account_id
       ; booked_on
       ; value_on
       ; amount
       ; description
       ; suggested_description
       ; category
       ; parent_category
       ; comment
       ; account_label
       ; reported_balance
       ; pointage
       ; date_format
       ; amount_format
       ; balance_format
       })
  |> field "version" (Json.version 1) (fun _ -> ())
  |> field "name" Jsont.string (fun (p : t) -> p.name)
  |> field "header" (Jsont.list Jsont.string) (fun p -> p.header)
  |> field "separator" Jsont.string (fun p -> String.of_char p.separator)
  |> Jsont.Object.mem
       "currency"
       (Jsont.option Json.currency)
       ~dec_absent:None
       ~enc:(fun (p : t) -> p.currency)
  |> field "trim_header" Jsont.bool (fun p -> p.trim_header)
  |> field "trim_cells" Jsont.bool (fun p -> p.trim_cells)
  |> field "skip_blank_rows" Jsont.bool (fun p -> p.skip_blank_rows)
  |> field "account_id" Json.integer (fun p -> p.account_id)
  |> field "booked_on" Json.integer (fun p -> p.booked_on)
  |> field "value_on" (Jsont.option Json.integer) (fun p -> p.value_on)
  |> field "amount" Json.integer (fun p -> p.amount)
  |> field "description" Json.integer (fun p -> p.description)
  |> field "suggested_description" (Jsont.option Json.integer) (fun p ->
    p.suggested_description)
  |> field "category" (Jsont.option Json.integer) (fun p -> p.category)
  |> field "parent_category" (Jsont.option Json.integer) (fun p -> p.parent_category)
  |> field "comment" (Jsont.option Json.integer) (fun p -> p.comment)
  |> field "account_label" (Jsont.option Json.integer) (fun p -> p.account_label)
  |> field "reported_balance" (Jsont.option Json.integer) (fun p -> p.reported_balance)
  |> field "pointage" (Jsont.option Json.integer) (fun p -> p.pointage)
  |> field "date_format" Csv_profile.date_format_codec (fun p -> p.date_format)
  |> field "amount_format" Csv_profile.amount_format_codec (fun p -> p.amount_format)
  |> field "balance_format" Csv_profile.amount_format_codec (fun p -> p.balance_format)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;
