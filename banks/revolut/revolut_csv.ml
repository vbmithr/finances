open Base

type transaction =
  { account_id : string
  ; currency : Iso4217.t
  ; started_at : string
  ; completed_at : string
  ; booked_on : Date.t
  ; transaction_type : string
  ; description : string
  ; principal : Quantity.t
  ; fee : Quantity.t
  ; amount : Quantity.t
  ; balance : Quantity.t option
  ; occurrence : int
  ; source_id : string
  ; payload : string
  }

type t =
  { contents : string
  ; transactions : transaction list
  }

let header =
  [ "Type"
  ; "Product"
  ; "Started Date"
  ; "Completed Date"
  ; "Description"
  ; "Amount"
  ; "Fee"
  ; "Currency"
  ; "State"
  ; "Balance"
  ]
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

let account_identity t =
  Yojson.Safe.to_string
    (`List
        [ `String "revolut"
        ; `String t.account_id
        ; `String ("current-" ^ Iso4217.to_alpha_code t.currency)
        ])
;;

let make
      ~account_id
      ~currency
      ~started_at
      ~completed_at
      ~transaction_type
      ~description
      ~principal
      ~fee
      ~balance
      ~occurrence
  =
  ignore (Validation.token "Revolut account_id" account_id |> Or_error.ok_exn : string);
  if occurrence < 1 || Quantity.(fee < zero)
  then failwith "invalid Revolut occurrence or fee";
  let clock s =
    if
      not
        (Re.execp
           (Re.Perl.compile_pat "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$")
           s)
    then failwith "unsupported Revolut datetime"
    else
      ignore
        (Timestamp.of_rfc3339_exn (String.tr s ~target:' ' ~replacement:'T' ^ "Z")
         : Ptime.t)
  in
  clock started_at;
  clock completed_at;
  if String.compare started_at completed_at > 0
  then failwith "Revolut completion precedes start";
  ignore
    (Validation.text "Revolut transaction type" transaction_type |> Or_error.ok_exn
     : string);
  let booked_on = Date.of_string (String.prefix completed_at 10) in
  let amount = Quantity.(principal - fee) in
  let fields =
    [ "format", `String "revolut-v1"
    ; "account_id", `String account_id
    ; "currency", `String (Iso4217.to_alpha_code currency)
    ; "started_at", `String started_at
    ; "completed_at", `String completed_at
    ; "transaction_type", `String transaction_type
    ; "description", `String description
    ; "principal", `String (Quantity.to_string principal)
    ; "fee", `String (Quantity.to_string fee)
    ]
  in
  let base = Yojson.Safe.to_string (`Assoc fields) in
  let source_id =
    Digestif.SHA256.(to_hex (digest_string base)) ^ ":" ^ Int.to_string occurrence
  in
  let payload =
    Yojson.Safe.to_string (`Assoc (fields @ [ "occurrence", `Int occurrence ]))
  in
  { account_id
  ; currency
  ; started_at
  ; completed_at
  ; booked_on
  ; transaction_type
  ; description
  ; principal
  ; fee
  ; amount
  ; balance
  ; occurrence
  ; source_id
  ; payload
  }
;;

let parse ~account_id contents =
  Or_error.try_with (fun () ->
    if not (Stdlib.String.is_valid_utf_8 contents)
    then failwith "Revolut CSV must be UTF-8";
    ignore (Validation.token "Revolut account_id" account_id |> Or_error.ok_exn : string);
    let input = channel contents in
    if not (List.equal String.equal (Csv.next input) header)
    then failwith "unsupported Revolut CSV header";
    let decimal s =
      if not (Re.execp (Re.Perl.compile_pat "^-?[0-9]+(\\.[0-9]+)?$") s)
      then failwith "invalid Revolut decimal"
      else Quantity.of_string s
    in
    let counts = Hashtbl.create (module String) in
    let balances = Hashtbl.create (module Iso4217) in
    let transactions =
      Csv.input_all input
      |> List.map ~f:(fun fields ->
        match fields with
        | [ transaction_type
          ; "Current"
          ; started_at
          ; completed_at
          ; description
          ; principal
          ; fee
          ; currency
          ; "COMPLETED"
          ; balance
          ] ->
          let principal = decimal principal
          and fee = decimal fee
          and balance = decimal balance
          and currency =
            Validation.currency "Revolut currency" currency |> Or_error.ok_exn
          in
          let initial =
            make
              ~account_id
              ~currency
              ~started_at
              ~completed_at
              ~transaction_type
              ~description
              ~principal
              ~fee
              ~balance:(Some balance)
              ~occurrence:1
          in
          Hashtbl.incr counts initial.source_id;
          let tx =
            make
              ~account_id
              ~currency
              ~started_at
              ~completed_at
              ~transaction_type
              ~description
              ~principal
              ~fee
              ~balance:(Some balance)
              ~occurrence:(Hashtbl.find_exn counts initial.source_id)
          in
          Option.iter
            (Hashtbl.find balances currency)
            ~f:(fun (previous_at, previous_balance) ->
              if String.compare completed_at previous_at < 0
              then failwith "Revolut rows are not in completion order";
              if not Quantity.(previous_balance + tx.amount = balance)
              then failwith "Revolut running balance does not agree with amount minus fee");
          Hashtbl.set balances ~key:currency ~data:(completed_at, balance);
          tx
        | _ -> failwith "expected ten-column completed Revolut Current transactions")
    in
    { contents; transactions })
  |> fun r -> Or_error.tag r ~tag:"Revolut CSV"
;;

let source_transaction (event : Source_event.t) =
  Or_error.try_with (fun () ->
    if
      not
        (String.equal event.venue "revolut"
         && String.equal event.kind "revolut-transaction")
    then failwith "not a Revolut event";
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string event.payload in
    let field k = member k json |> to_string in
    if not (String.equal (field "format") "revolut-v1")
    then failwith "unsupported Revolut payload";
    let tx =
      make
        ~account_id:(field "account_id")
        ~currency:
          (Validation.currency "Revolut currency" (field "currency") |> Or_error.ok_exn)
        ~started_at:(field "started_at")
        ~completed_at:(field "completed_at")
        ~transaction_type:(field "transaction_type")
        ~description:(field "description")
        ~principal:(Quantity.of_string (field "principal"))
        ~fee:(Quantity.of_string (field "fee"))
        ~balance:None
        ~occurrence:(member "occurrence" json |> to_int)
    in
    if
      not
        (String.equal event.account (account_identity tx)
         && String.equal event.source_id tx.source_id
         &&
         match event.occurred with
         | On_date d -> Date.equal d tx.booked_on
         | _ -> false)
    then failwith "Revolut source identity disagrees with payload";
    tx)
;;
