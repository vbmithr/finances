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

let header =
  [ "TransferWise ID"
  ; "Date"
  ; "Date Time"
  ; "Amount"
  ; "Currency"
  ; "Description"
  ; "Payment Reference"
  ; "Running Balance"
  ; "Exchange From"
  ; "Exchange To"
  ; "Exchange Rate"
  ; "Payer Name"
  ; "Payee Name"
  ; "Payee Account Number"
  ; "Merchant"
  ; "Card Last Four Digits"
  ; "Card Holder Full Name"
  ; "Attachment"
  ; "Note"
  ; "Total fees"
  ; "Exchange To Amount"
  ; "Transaction Type"
  ; "Transaction Details Type"
  ]
;;

let decimal =
  Csv_schema.Cell.quantity
    ~sign:`Any
    ~grouping:`None
    ~decimal_mark:`Dot
    ~decimals:`Optional
    ~message:"invalid Wise CSV decimal"
    ()
;;

let schema =
  let open Csv_schema in
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
       then failwith "Wise CSV date and datetime disagree";
       if
         (String.equal direction "DEBIT" && Quantity.(amount > zero))
         || (String.equal direction "CREDIT" && Quantity.(amount < zero))
       then failwith "Wise CSV amount and transaction direction disagree";
       let reference = Wise_camt.normalize_reference raw_id ~description in
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
  |> col_name "TransferWise ID" (Cell.text ~name:"Wise ID" ())
  |> col_name "Date" (Cell.dmy_date ~message:"invalid Wise CSV calendar date" ())
  |> col_name
       "Date Time"
       (Cell.utc_clock
          ~layout:`Dmy_millis
          ~message:"Wise CSV clock must include milliseconds"
          ())
  |> col_name "Amount" decimal
  |> col_name "Currency" (Cell.currency ~name:"Wise CSV currency" ())
  |> col_name "Description" Cell.string
  |> col_name "Payment Reference" (Cell.opt Cell.string)
  |> col_name "Running Balance" decimal
  |> col_name "Payee Name" (Cell.opt Cell.string)
  |> col_name "Merchant" (Cell.opt Cell.string)
  |> col_name "Total fees" decimal
  |> col_name
       "Transaction Type"
       (Cell.one_of
          ~message:"invalid Wise CSV transaction direction"
          [ "DEBIT"; "CREDIT" ])
  |> col_name "Transaction Details Type" (Cell.opt Cell.string)
  |> raw_fields
  |> create
       ~name:"Wise"
       ~separator:','
       ~header
       ~trim_header:true
       ~trim_cells:true
       ~skip_blank_rows:true
       ~columns_message:(fun expected _ ->
         Printf.sprintf "expected %d Wise CSV columns" expected)
;;

let recognizes = Csv_schema.recognizes schema
let of_fields = Csv_schema.of_fields schema
let parse = Csv_schema.parse schema

let match_rows rows candidates =
  let amount_key x =
    let s = Quantity.to_string x in
    if Quantity.is_zero x
    then "0"
    else if not (String.contains s '.')
    then s
    else (
      let rec stop i = if Char.equal s.[i] '0' then stop (i - 1) else i in
      let i = stop (String.length s - 1) in
      String.prefix s (if Char.equal s.[i] '.' then i else i + 1))
  in
  let key reference local amount currency =
    Sexp.to_string
      (Sexp.List
         (List.map
            [ reference
            ; Timestamp.to_rfc3339 local
            ; amount_key amount
            ; Iso4217.to_alpha_code currency
            ]
            ~f:(fun s -> Sexp.Atom s)))
  in
  let index = Hashtbl.create (module String) in
  List.iter candidates ~f:(fun (id, tx) ->
    Hashtbl.add_multi
      index
      ~key:
        (key
           tx.Wise_camt.reference
           (Wise_camt.matching_time tx)
           tx.amount
           tx.account.currency)
      ~data:id);
  let initial =
    List.map rows ~f:(fun input ->
      let row = input.row in
      let candidates =
        Option.value
          (Hashtbl.find index (key row.reference row.booked_at row.amount row.currency))
          ~default:[]
        |> List.dedup_and_sort ~compare:Int64.compare
      in
      let event_id, state, reason =
        match candidates with
        | [ id ] ->
          Some id, Bank_match.Matched, "unique Wise reference/clock/amount/currency match"
        | [] -> None, Unmatched, "no matching Wise CAMT movement"
        | _ -> None, Ambiguous, "multiple Wise CAMT candidates"
      in
      input, { Bank_match.row_id = input.row_id; event_id; state; reason; candidates })
  in
  let claims = Hashtbl.Poly.create () in
  List.iter initial ~f:(fun (input, result) ->
    Option.iter result.event_id ~f:(fun id -> Hashtbl.incr claims (input.document_id, id)));
  let unique =
    List.map initial ~f:(fun (input, result) ->
      match result.event_id with
      | Some id when Hashtbl.find_exn claims (input.document_id, id) > 1 ->
        ( input
        , { result with
            event_id = None
          ; state = Ambiguous
          ; reason = "multiple Wise CSV rows claim one CAMT movement"
          } )
      | _ -> input, result)
  in
  let groups = Hashtbl.create (module Int64) in
  List.iter unique ~f:(fun (input, result) ->
    Option.iter result.event_id ~f:(fun id ->
      Hashtbl.add_multi groups ~key:id ~data:input.row));
  let conflict = Hashtbl.create (module Int64) in
  Hashtbl.iteri groups ~f:(fun ~key:id ~data:observations ->
    (* Balance/labels/notes may vary by observation. Fees and FX details must not
       silently disagree for a uniquely identified movement. *)
    let signatures =
      List.map observations ~f:(fun row ->
        Quantity.to_string row.fees
        :: List.map [ 8; 9; 10; 20 ] ~f:(List.nth_exn row.fields))
      |> List.dedup_and_sort ~compare:(List.compare String.compare)
    in
    if List.length signatures > 1 then Hashtbl.set conflict ~key:id ~data:());
  List.map unique ~f:(fun (_, result) ->
    match result.event_id with
    | Some id when Hashtbl.mem conflict id ->
      { result with state = Conflict; reason = "conflicting Wise fee or FX metadata" }
    | _ -> result)
;;
