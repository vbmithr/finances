open Base

type row =
  { row_number : int
  ; reference : string
  ; booked_local : string
  ; amount : Quantity.t
  ; currency : string
  ; description : string
  ; running_balance : Quantity.t
  ; fees : Quantity.t
  ; payment_reference : string option
  ; merchant : string option
  ; payee : string option
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

let channel contents =
  let contents =
    Option.value (String.chop_prefix contents ~prefix:"\239\187\191") ~default:contents
  in
  Csv.of_string
    ~separator:','
    ~strip:false
    ~backslash_escape:false
    ~excel_tricks:false
    ~fix:false
    contents
;;

let recognizes contents =
  match Or_error.try_with (fun () -> Csv.next (channel contents)) with
  | Ok fields -> List.equal String.equal (List.map fields ~f:String.strip) header
  | Error _ -> false
;;

let decimal_pattern = Re.Perl.compile_pat "^[+-]?[0-9]+(\\.[0-9]+)?$"
let date_pattern = Re.Perl.compile_pat "^[0-9]{2}-[0-9]{2}-[0-9]{4}$"

let clock_pattern =
  Re.Perl.compile_pat "^[0-9]{2}-[0-9]{2}-[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}$"
;;

let decimal s =
  if not (Re.execp decimal_pattern s) then failwith "invalid Wise CSV decimal";
  Quantity.of_string s
;;

let date s =
  if not (Re.execp date_pattern s) then failwith "invalid Wise CSV calendar date";
  let date =
    String.sub s ~pos:6 ~len:4
    ^ "-"
    ^ String.sub s ~pos:3 ~len:2
    ^ "-"
    ^ String.prefix s 2
  in
  Date.of_string date
;;

let of_fields ~row_number fields =
  Or_error.try_with (fun () ->
    if row_number <= 0 || List.length fields <> 23
    then failwith "expected 23 Wise CSV columns";
    let a = Array.of_list (List.map fields ~f:String.strip) in
    let optional i = if String.is_empty a.(i) then None else Some a.(i) in
    if not (Re.execp clock_pattern a.(2))
    then failwith "Wise CSV clock must include milliseconds";
    let day = date a.(1) in
    if not (Date.equal day (date (String.prefix a.(2) 10)))
    then failwith "Wise CSV date and datetime disagree";
    let booked_local = Date.to_string day ^ "T" ^ String.drop_prefix a.(2) 11 in
    (* Validate the clock without assigning a timezone to the CSV observation. *)
    ignore (Timestamp.of_rfc3339_exn (booked_local ^ "Z") : Ptime.t);
    let amount = decimal a.(3) in
    if not (List.mem [ "DEBIT"; "CREDIT" ] a.(21) ~equal:String.equal)
    then failwith "invalid Wise CSV transaction direction";
    if
      (String.equal a.(21) "DEBIT" && Quantity.(amount > zero))
      || (String.equal a.(21) "CREDIT" && Quantity.(amount < zero))
    then failwith "Wise CSV amount and transaction direction disagree";
    let reference =
      Validation.text "Wise ID" a.(0)
      |> Or_error.ok_exn
      |> Wise_camt.normalize_reference ~description:a.(5)
    in
    let currency = Validation.token "Wise CSV currency" a.(4) |> Or_error.ok_exn in
    { row_number
    ; reference
    ; booked_local
    ; amount
    ; currency
    ; description = a.(5)
    ; running_balance = decimal a.(7)
    ; fees = decimal a.(19)
    ; payment_reference = optional 6
    ; merchant = optional 14
    ; payee = optional 12
    ; fields
    })
;;

let parse contents =
  Or_error.try_with (fun () ->
    if not (Stdlib.String.is_valid_utf_8 contents) then failwith "Wise CSV must be UTF-8";
    let input = channel contents in
    if not (List.equal String.equal (List.map (Csv.next input) ~f:String.strip) header)
    then failwith "unsupported Wise CSV header";
    Csv.input_all input
    |> List.mapi ~f:(fun i fields ->
      if List.for_all fields ~f:(fun s -> String.is_empty (String.strip s))
      then None
      else Some (of_fields ~row_number:(i + 1) fields |> Or_error.ok_exn))
    |> List.filter_opt)
  |> fun result -> Or_error.tag result ~tag:"Wise CSV"
;;

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
            [ reference; local; amount_key amount; currency ]
            ~f:(fun s -> Sexp.Atom s)))
  in
  let index = Hashtbl.create (module String) in
  List.iter candidates ~f:(fun (id, tx) ->
    Hashtbl.add_multi
      index
      ~key:
        (key
           tx.Wise_camt.reference
           (Wise_camt.local_millisecond tx.booked_at)
           tx.amount
           tx.account.currency)
      ~data:id);
  let initial =
    List.map rows ~f:(fun input ->
      let row = input.row in
      let candidates =
        Option.value
          (Hashtbl.find
             index
             (key row.reference row.booked_local row.amount row.currency))
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
