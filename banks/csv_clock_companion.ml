open Base

type row =
  { row_number : int
  ; reference : string
  ; booked_at : Ptime.t
  ; amount : Quantity.t
  ; currency : Iso4217.t
  ; description : string
  ; running_balance : Quantity.t
  ; fees : Quantity.t
  ; payment_reference : string option
  ; merchant : string option
  ; payee : string option
  ; details_type : string option
  ; fields : string list
  }

type input =
  { row_id : int64
  ; document_id : int64
  ; row : row
  }

let decimal name =
  Csv_schema.Cell.quantity
    ~sign:`Any
    ~grouping:`None
    ~decimal_mark:`Dot
    ~decimals:`Optional
    ~message:("invalid " ^ name ^ " CSV decimal")
    ()
;;

let schema (profile : Csv_clock_profile.t) camt_profile =
  let open Csv_schema in
  let columns = profile.columns in
  let name = profile.name in
  let decimal = decimal name in
  make
    (fun
        row_number
         raw_id
         day
         booked_at
         amount
         currency
         description
         payment_reference
         running_balance
         payee
         merchant
         fees
         direction
         details_type
         fields
       ->
       if not (Date.equal day (Ptime.to_date ~tz_offset_s:0 booked_at))
       then failwith (name ^ " CSV date and datetime disagree");
       if
         (String.equal direction "DEBIT" && Quantity.(amount > zero))
         || (String.equal direction "CREDIT" && Quantity.(amount < zero))
       then failwith (name ^ " CSV amount and transaction direction disagree");
       let reference = Camt_source.normalize_reference camt_profile raw_id ~description in
       { row_number
       ; reference
       ; booked_at
       ; amount
       ; currency
       ; description
       ; running_balance
       ; fees
       ; payment_reference
       ; merchant
       ; payee
       ; details_type
       ; fields
       })
  |> row_number
  |> col_name columns.reference (Cell.text ~name:(name ^ " ID") ())
  |> col_name
       columns.date
       (Cell.dmy_date ~message:("invalid " ^ name ^ " CSV calendar date") ())
  |> col_name
       columns.datetime
       (Cell.utc_clock
          ~layout:`Dmy_millis
          ~message:(name ^ " CSV clock must include milliseconds")
          ())
  |> col_name columns.amount decimal
  |> col_name columns.currency (Cell.currency ~name:(name ^ " CSV currency") ())
  |> col_name columns.description Cell.string
  |> col_name columns.payment_reference (Cell.opt Cell.string)
  |> col_name columns.running_balance decimal
  |> col_name columns.payee (Cell.opt Cell.string)
  |> col_name columns.merchant (Cell.opt Cell.string)
  |> col_name columns.fees decimal
  |> col_name
       columns.direction
       (Cell.one_of
          ~message:("invalid " ^ name ^ " CSV transaction direction")
          [ "DEBIT"; "CREDIT" ])
  |> col_name columns.details_type (Cell.opt Cell.string)
  |> raw_fields
  |> create
       ~name
       ~separator:','
       ~header:profile.header
       ~trim_header:true
       ~trim_cells:true
       ~skip_blank_rows:true
       ~columns_message:(fun expected _ ->
         Printf.sprintf "expected %d %s CSV columns" expected name)
;;

let recognizes profile camt_profile = Csv_schema.recognizes (schema profile camt_profile)
let of_fields profile camt_profile = Csv_schema.of_fields (schema profile camt_profile)
let parse profile camt_profile = Csv_schema.parse (schema profile camt_profile)

let match_rows (profile : Csv_clock_profile.t) rows candidates =
  let conflict_indices =
    List.map profile.conflict_columns ~f:(fun col ->
      List.findi_exn profile.header ~f:(fun _ name -> String.equal name col) |> fst)
  in
  let name = profile.name in
  let rows =
    List.map rows ~f:(fun (input : input) ->
      let row = input.row in
      { Csv_reference_match.row_id = input.row_id
      ; document_id = input.document_id
      ; reference = row.reference
      ; booked_at = row.booked_at
      ; amount = row.amount
      ; currency = row.currency
      ; conflict_values =
          Quantity.to_string row.fees
          :: List.map conflict_indices ~f:(List.nth_exn row.fields)
      })
  in
  let candidates =
    List.map candidates ~f:(fun (event_id, (tx : Camt_source.transaction)) ->
      { Csv_reference_match.event_id
      ; reference = tx.reference
      ; booked_at = Camt_source.matching_time tx
      ; amount = tx.amount
      ; currency = tx.account.currency
      })
  in
  Csv_reference_match.match_rows
    ~messages:
      { matched = "unique " ^ name ^ " reference/clock/amount/currency match"
      ; unmatched = "no matching " ^ name ^ " CAMT movement"
      ; ambiguous = "multiple " ^ name ^ " CAMT candidates"
      ; duplicate_claim = "multiple " ^ name ^ " CSV rows claim one CAMT movement"
      ; conflict = "conflicting " ^ name ^ " fee or FX metadata"
      }
    rows
    candidates
;;
