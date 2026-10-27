open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : string
  }

type transaction =
  { fitid : string
  ; booked_on : Date.t
  ; user_date : string option
  ; amount : Quantity.t
  ; transaction_type : string
  ; name : string option
  ; memo : string option
  ; source_payload : string
  }

type balance =
  { amount : Quantity.t
  ; as_of : string
  }

type statement =
  { account : account
  ; transactions : transaction list
  ; ledger_balance : balance option
  ; available_balance : balance option
  }

type t =
  { contents : string
  ; statements : statement list
  }

let rec canonical = function
  | Ofx.Text s -> `String (String.strip s)
  | Element (tag, nodes) -> `List [ `String tag; `List (List.map nodes ~f:canonical) ]
;;

let account_identity a =
  Yojson.Safe.to_string
    (`List (List.map [ a.bank_id; a.account_id; a.account_type ] ~f:(fun s -> `String s)))
;;

(* Keep the date in the source's local calendar. A timestamp suffix does not
   turn a bank booking date into a UTC instant. *)
let date s = Ofx.calendar_date s |> Or_error.ok_exn

let parse_transaction account (tx : Ofx.transaction) =
  let fitid = Validation.text "FITID" tx.fitid |> Or_error.ok_exn in
  let booked_on = date tx.posted in
  (* BoursoBank's observed DTUSER is not a valid calendar date. Preserve it
     verbatim as source metadata; it cannot supply a value or booking date. *)
  let user_date = tx.user_date in
  let amount = Quantity.of_string tx.amount in
  if tx.corrects
  then failwith "OFX transaction corrections require an explicit correction workflow";
  (* Keep DTUSER in the archived document and parsed observation, but exclude
     this unreliable metadata from the stable transaction evidence. *)
  let stable_node =
    match tx.tree with
    | Element (tag, nodes) ->
      Ofx.Element
        ( tag
        , List.filter nodes ~f:(function
            | Element ("DTUSER", _) -> false
            | _ -> true) )
    | Text _ -> assert false
  in
  let source_payload =
    Yojson.Safe.to_string
      (`List
          [ `String (account_identity account)
          ; `String account.currency
          ; canonical stable_node
          ])
  in
  { fitid
  ; booked_on
  ; user_date
  ; amount
  ; transaction_type = tx.transaction_type
  ; name = tx.name
  ; memo = tx.memo
  ; source_payload
  }
;;

let parse_balance (balance : Ofx.balance option) =
  Option.map balance ~f:(fun (b : Ofx.balance) ->
    ignore (date b.as_of : Date.t);
    { amount = Quantity.of_string b.amount; as_of = b.as_of })
;;

let tagged label i f =
  Or_error.try_with f
  |> fun result ->
  Or_error.tag result ~tag:(Printf.sprintf "%s %d" label (i + 1)) |> Or_error.ok_exn
;;

let parse_statement (statement : Ofx.statement) =
  let a = statement.account in
  let account =
    { bank_id = a.bank_id |> Validation.text "bank ID" |> Or_error.ok_exn
    ; account_id = a.account_id |> Validation.text "account ID" |> Or_error.ok_exn
    ; account_type = a.account_type |> Validation.text "account type" |> Or_error.ok_exn
    ; currency = a.currency |> Validation.token "currency" |> Or_error.ok_exn
    }
  in
  let transactions =
    List.mapi statement.transactions ~f:(fun i tx ->
      tagged "transaction" i (fun () -> parse_transaction account tx))
  in
  { account
  ; transactions
  ; ledger_balance = parse_balance statement.ledger_balance
  ; available_balance = parse_balance statement.available_balance
  }
;;

let parse contents =
  Or_error.try_with (fun () ->
    let statements = Ofx.parse contents |> Or_error.ok_exn in
    { contents
    ; statements =
        List.mapi statements ~f:(fun i statement ->
          tagged "statement" i (fun () -> parse_statement statement))
    })
  |> fun result -> Or_error.tag result ~tag:"OFX import"
;;

let source_transaction (event : Source_event.t) =
  Or_error.try_with (fun () ->
    if not (String.equal event.kind "bank-transaction")
    then failwith "not a bank source event";
    let identity, currency, tree =
      match Yojson.Safe.from_string event.payload with
      | `List [ `String identity; `String currency; tree ] -> identity, currency, tree
      | _ -> failwith "unsupported bank source payload"
    in
    let bank_id, account_id, account_type =
      match Yojson.Safe.from_string identity with
      | `List [ `String bank_id; `String account_id; `String account_type ] ->
        bank_id, account_id, account_type
      | _ -> failwith "invalid bank account identity"
    in
    let account = { bank_id; account_id; account_type; currency } in
    let rec decode = function
      | `String s -> Ofx.Text s
      | `List [ `String tag; `List nodes ] -> Ofx.Element (tag, List.map nodes ~f:decode)
      | _ -> failwith "invalid canonical bank XML"
    in
    let trn =
      parse_transaction account (Ofx.transaction_of_tree (decode tree) |> Or_error.ok_exn)
    in
    if
      not
        (String.equal identity (account_identity account)
         && String.equal identity event.account
         && String.equal trn.fitid event.source_id)
    then failwith "bank event identity disagrees with payload";
    (match event.occurred with
     | On_date date when Date.equal date trn.booked_on -> ()
     | _ -> failwith "bank event booking date disagrees with payload");
    account, trn)
  |> fun result -> Or_error.tag result ~tag:"stored bank transaction"
;;
