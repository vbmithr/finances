open Base

type row =
  { row_number : int
  ; account_id : string
  ; booked_on : Date.t
  ; value_on : Date.t
  ; amount : Quantity.t
  ; counterparty : string
  ; name : string
  ; description : string
  ; fields : string list
  }

type input =
  { row_id : int64
  ; document_id : int64
  ; row : row
  }

let header =
  [ "Date"; "Interest Date"; "Amount"; "Account"; "Counterparty"; "Name"; "Description" ]
;;

let schema =
  let open Csv_schema in
  let date = Cell.iso_date ~message:"invalid Bunq CSV date" () in
  make
    (fun
        row_number
         booked_on
         value_on
         amount
         account_id
         counterparty
         name
         description
         fields
       ->
       { row_number
       ; account_id
       ; booked_on
       ; value_on
       ; amount
       ; counterparty
       ; name
       ; description
       ; fields
       })
  |> row_number
  |> col_name "Date" date
  |> col_name "Interest Date" date
  |> col_name "Amount" Cell.bunq_amount
  |> col_name "Account" (Cell.token ~name:"Bunq CSV account" ())
  |> col_name "Counterparty" Cell.string
  |> col_name "Name" Cell.string
  |> col_name "Description" Cell.string
  |> raw_fields
  |> create
       ~name:"Bunq"
       ~separator:','
       ~header
       ~trim_header:false
       ~trim_cells:false
       ~skip_blank_rows:false
       ~columns_message:(fun _ _ -> "expected seven Bunq CSV fields")
;;

let recognizes = Csv_schema.recognizes schema
let of_fields = Csv_schema.of_fields schema
let parse = Csv_schema.parse schema

let key r =
  Bunq_transaction.key
    ~account_id:r.account_id
    ~booked_on:r.booked_on
    ~value_on:r.value_on
    ~amount:r.amount
    ~counterparty:r.counterparty
    ~name:r.name
    ~description:r.description
;;

let match_rows inputs candidates =
  let index = Hashtbl.create (module String) in
  List.iter candidates ~f:(fun (id, tx) ->
    Hashtbl.add_multi index ~key:(Bunq_transaction.csv_key tx) ~data:(id, tx));
  let counts = Hashtbl.Poly.create ()
  and ordinals = Hashtbl.Poly.create () in
  List.iter inputs ~f:(fun input ->
    Hashtbl.incr counts (input.document_id, key input.row));
  List.map inputs ~f:(fun input ->
    let k = key input.row in
    let group = input.document_id, k in
    Hashtbl.incr ordinals group;
    let candidates =
      Option.value (Hashtbl.find index k) ~default:[]
      |> List.sort ~compare:(fun (_, a) (_, b) ->
        Int.compare a.Bunq_transaction.occurrence b.occurrence)
    in
    let ids = List.map candidates ~f:fst in
    let currencies =
      List.map candidates ~f:(fun (_, tx) -> tx.Bunq_transaction.currency)
      |> List.dedup_and_sort ~compare:Iso4217.compare
    in
    let event_id, state, reason =
      match candidates with
      | [] -> None, Bank_match.Unmatched, "no matching Bunq MT940 movement"
      | _
        when List.length candidates = Hashtbl.find_exn counts group
             && List.length currencies = 1 ->
        ( Some (List.nth_exn ids (Hashtbl.find_exn ordinals group - 1))
        , Matched
        , "matching Bunq CSV and MT940 occurrence inventories" )
      | _ ->
        ( None
        , Ambiguous
        , "Bunq CSV and MT940 occurrence counts or currencies are ambiguous" )
    in
    { Bank_match.row_id = input.row_id; event_id; state; reason; candidates = ids })
;;
