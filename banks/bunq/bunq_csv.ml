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

let channel s =
  Csv.of_string
    ~separator:','
    ~strip:false
    ~backslash_escape:false
    ~excel_tricks:false
    ~fix:false
    (Option.value (String.chop_prefix s ~prefix:"\239\187\191") ~default:s)
;;

let recognizes contents =
  match Or_error.try_with (fun () -> Csv.next (channel contents)) with
  | Ok fields -> List.equal String.equal fields header
  | Error _ -> false
;;

let of_fields ~row_number fields =
  Or_error.try_with (fun () ->
    match fields with
    | [ booked; value; amount; account_id; counterparty; name; description ] ->
      let amount = String.substr_replace_all amount ~pattern:"\226\128\175" ~with_:" " in
      if
        not
          (Re.execp
             (Re.Perl.compile_pat "^-?([0-9]+|[0-9]{1,3}( [0-9]{3})+),[0-9]{2}$")
             amount)
      then failwith "invalid Bunq CSV amount";
      let amount =
        String.filter amount ~f:(fun c -> not (Char.equal c ' '))
        |> fun s -> String.tr s ~target:',' ~replacement:'.' |> Quantity.of_string
      in
      let day s =
        if not (Re.execp (Re.Perl.compile_pat "^[0-9]{4}-[0-9]{2}-[0-9]{2}$") s)
        then failwith "invalid Bunq CSV date"
        else Date.of_string s
      in
      let account_id =
        Validation.token "Bunq CSV account" account_id |> Or_error.ok_exn
      in
      { row_number
      ; account_id
      ; booked_on = day booked
      ; value_on = day value
      ; amount
      ; counterparty
      ; name
      ; description
      ; fields
      }
    | _ -> failwith "expected seven Bunq CSV fields")
;;

let parse contents =
  Or_error.try_with (fun () ->
    if not (Stdlib.String.is_valid_utf_8 contents) then failwith "Bunq CSV must be UTF-8";
    let input = channel contents in
    if not (List.equal String.equal (Csv.next input) header)
    then failwith "unsupported Bunq CSV header";
    Csv.input_all input
    |> List.mapi ~f:(fun i fields ->
      of_fields ~row_number:(i + 1) fields |> Or_error.ok_exn))
;;

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
      |> List.dedup_and_sort ~compare:String.compare
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
