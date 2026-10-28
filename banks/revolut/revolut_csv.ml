open Base

type transaction =
  { account_id : string
  ; currency : Iso4217.t
  ; started_at : Ptime.t
  ; completed_at : Ptime.t
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

let channel = Csv_reader.channel ~separator:','
let recognizes = Csv_reader.recognizes ~separator:',' ~trim:false ~header

let account_identity t =
  Json.encode
    (Jsont.list Jsont.string)
    [ "revolut"; t.account_id; "current-" ^ Iso4217.to_alpha_code t.currency ]
;;

let source_clock instant =
  String.prefix (Timestamp.to_rfc3339 instant) 19
  |> String.tr ~target:'T' ~replacement:' '
;;

let payload_members map =
  map
  |> Jsont.Object.mem "format" (Json.tag "revolut-v1") ~enc:(fun _ -> ())
  |> Jsont.Object.mem "account_id" Jsont.string ~enc:(fun (t : transaction) ->
    t.account_id)
  |> Jsont.Object.mem "currency" Json.currency ~enc:(fun t -> t.currency)
  |> Jsont.Object.mem "started_at" Jsont.string ~enc:(fun t -> source_clock t.started_at)
  |> Jsont.Object.mem "completed_at" Jsont.string ~enc:(fun t ->
    source_clock t.completed_at)
  |> Jsont.Object.mem "transaction_type" Jsont.string ~enc:(fun t -> t.transaction_type)
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun t -> t.description)
  |> Jsont.Object.mem "principal" Json.quantity ~enc:(fun t -> t.principal)
  |> Jsont.Object.mem "fee" Json.quantity ~enc:(fun t -> t.fee)
;;

let identity_jsont = Jsont.Object.enc_only () |> payload_members |> Jsont.Object.finish

let rec make
          ~account_id
          ~currency
          ~started_at:started_at_source
          ~completed_at:completed_at_source
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
    else Timestamp.of_rfc3339_exn (String.tr s ~target:' ' ~replacement:'T' ^ "Z")
  in
  let started_at = clock started_at_source
  and completed_at = clock completed_at_source in
  if Ptime.compare started_at completed_at > 0
  then failwith "Revolut completion precedes start";
  ignore
    (Validation.text "Revolut transaction type" transaction_type |> Or_error.ok_exn
     : string);
  let booked_on = Ptime.to_date ~tz_offset_s:0 completed_at in
  let amount = Quantity.(principal - fee) in
  let tx =
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
    ; source_id = ""
    ; payload = ""
    }
  in
  let base = Json.encode identity_jsont tx in
  let source_id =
    Digestif.SHA256.(to_hex (digest_string base)) ^ ":" ^ Int.to_string occurrence
  in
  { tx with source_id; payload = Json.encode (Lazy.force jsont) tx }

and jsont =
  lazy
    (Jsont.Object.map
       (fun
           ()
            account_id
            currency
            started_at
            completed_at
            transaction_type
            description
            principal
            fee
            occurrence
          ->
          Json.validated (fun () ->
            make
              ~account_id
              ~currency
              ~started_at
              ~completed_at
              ~transaction_type
              ~description
              ~principal
              ~fee
              ~balance:None
              ~occurrence))
     |> payload_members
     |> Jsont.Object.mem "occurrence" Json.integer ~enc:(fun t -> t.occurrence)
     |> Jsont.Object.finish)
;;

let jsont = Lazy.force jsont

let parse ~account_id contents =
  Or_error.try_with (fun () ->
    Csv_reader.check_utf8 ~name:"Revolut" contents;
    ignore (Validation.token "Revolut account_id" account_id |> Or_error.ok_exn : string);
    let input = channel contents in
    Csv_reader.check_header
      ~trim:false
      ~header
      ~message:"unsupported Revolut CSV header"
      input;
    let decimal s =
      if not (Re.execp (Re.Perl.compile_pat "^-?[0-9]+(\\.[0-9]+)?$") s)
      then failwith "invalid Revolut decimal"
      else Quantity.of_string s
    in
    let counts = Hashtbl.create (module String) in
    let balances = Hashtbl.create (module Iso4217) in
    let transactions =
      Csv.input_all input
      |> List.filter_map ~f:(fun fields ->
        match fields with
        (* A reverted transaction never completed and never changed the balance:
           Revolut lists it with no completion date and no balance. The document
           keeps it as evidence; it is not a movement. *)
        | [ _; "Current"; _; ""; _; _; _; _; "REVERTED"; "" ] -> None
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
              if Ptime.compare tx.completed_at previous_at < 0
              then failwith "Revolut rows are not in completion order";
              if not Quantity.(previous_balance + tx.amount = balance)
              then failwith "Revolut running balance does not agree with amount minus fee");
          Hashtbl.set balances ~key:currency ~data:(tx.completed_at, balance);
          Some tx
        | _ ->
          failwith
            "expected ten-column completed or reverted Revolut Current transactions")
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
    let tx = Json.decode jsont event.payload |> Or_error.ok_exn in
    if
      not
        (String.equal event.account (account_identity tx)
         && String.equal event.source_id tx.source_id
         &&
         match event.occurred with
         | Temporal.Date d -> Date.equal d tx.booked_on
         | _ -> false)
    then failwith "Revolut source identity disagrees with payload";
    tx)
;;
