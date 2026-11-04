open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : Iso4217.t
  }

type transaction =
  { fitid : string
  ; booked_on : Date.t
  ; user_date_source : string option
  ; amount : Quantity.t
  ; transaction_type : string
  ; name : string option
  ; memo : string option
  ; source_payload : string
  }

type balance =
  { amount : Quantity.t
  ; as_of : Ptime.t
  }

type statement =
  { account : account
  ; transactions : transaction list
  ; ledger_balance : balance option
  ; available_balance : balance option
  ; period : (Date.t * Date.t) option
  }

type t =
  { contents : string
  ; statements : statement list
  }

(* The archived wire format represents XML elements as [tag, children]. *)
let rec tree_jsont =
  lazy
    (let text =
       Jsont.map
         Jsont.string
         ~dec:(fun s -> Ofx.Text s)
         ~enc:(function
           | Ofx.Text s -> String.strip s
           | _ -> assert false)
     in
     let parts =
       let tag =
         Jsont.map
           Jsont.string
           ~dec:(fun s -> `Tag s)
           ~enc:(function
             | `Tag s -> s
             | _ -> assert false)
       in
       let children =
         Jsont.map
           (Jsont.list (Jsont.rec' tree_jsont))
           ~dec:(fun nodes -> `Children nodes)
           ~enc:(function
             | `Children nodes -> nodes
             | _ -> assert false)
       in
       Jsont.any
         ~dec_string:tag
         ~dec_array:children
         ~enc:(function
           | `Tag _ -> tag
           | `Children _ -> children)
         ()
     in
     let element =
       Jsont.t2
         parts
         ~dec:(fun tag nodes ->
           match tag, nodes with
           | `Tag tag, `Children nodes -> Ofx.Element (tag, nodes)
           | _ -> failwith "invalid canonical bank XML")
         ~enc:(fun tree i ->
           match tree, i with
           | Ofx.Element (tag, _), 0 -> `Tag tag
           | Ofx.Element (_, nodes), 1 -> `Children nodes
           | _ -> assert false)
     in
     Jsont.any
       ~dec_string:text
       ~dec_array:element
       ~enc:(function
         | Ofx.Text _ -> text
         | Ofx.Element _ -> element)
       ())
;;

let account_identity a =
  Json.encode (Jsont.list Jsont.string) [ a.bank_id; a.account_id; a.account_type ]
;;

let payload_jsont =
  let scalar =
    Jsont.map
      Jsont.string
      ~dec:(fun s -> `Scalar s)
      ~enc:(function
        | `Scalar s -> s
        | _ -> assert false)
  in
  let tree =
    Jsont.map
      (Lazy.force tree_jsont)
      ~dec:(fun tree -> `Tree tree)
      ~enc:(function
        | `Tree tree -> tree
        | _ -> assert false)
  in
  let part =
    Jsont.any
      ~dec_string:scalar
      ~dec_array:tree
      ~enc:(function
        | `Scalar _ -> scalar
        | `Tree _ -> tree)
      ()
  in
  Jsont.t3
    part
    ~dec:(fun identity currency tree ->
      match identity, currency, tree with
      | `Scalar identity, `Scalar currency, `Tree tree ->
        let currency = Validation.currency "currency" currency |> Or_error.ok_exn in
        (match Json.decode (Jsont.list Jsont.string) identity |> Or_error.ok_exn with
         | [ bank_id; account_id; account_type ] ->
           { bank_id; account_id; account_type; currency }, tree
         | _ -> failwith "invalid bank account identity")
      | _ -> failwith "unsupported bank source payload")
    ~enc:(fun (account, tree) -> function
      | 0 -> `Scalar (account_identity account)
      | 1 -> `Scalar (Iso4217.to_alpha_code account.currency)
      | 2 -> `Tree tree
      | _ -> assert false)
;;

(* Keep the date in the source's local calendar. A timestamp suffix does not
   turn a bank booking date into a UTC instant. *)
let date s = Ofx.calendar_date s |> Or_error.ok_exn

let parse_transaction account (tx : Ofx.transaction) =
  let fitid = Validation.text "FITID" tx.fitid |> Or_error.ok_exn in
  let booked_on = date tx.posted in
  (* BoursoBank's observed DTUSER is not a valid calendar date. Preserve it
     verbatim as source metadata; it cannot supply a value or booking date. *)
  let user_date_source = tx.user_date in
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
  let source_payload = Json.encode payload_jsont (account, stable_node) in
  { fitid
  ; booked_on
  ; user_date_source
  ; amount
  ; transaction_type = tx.transaction_type
  ; name = tx.name
  ; memo = tx.memo
  ; source_payload
  }
;;

let parse_balance (balance : Ofx.balance option) =
  Option.map balance ~f:(fun (b : Ofx.balance) ->
    { amount = Quantity.of_string b.amount
    ; as_of = Ofx.instant b.as_of |> Or_error.ok_exn
    })
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
    ; currency = a.currency |> Validation.currency "currency" |> Or_error.ok_exn
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
  ; period = Option.map statement.period ~f:(fun (first, last) -> date first, date last)
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
    let account, tree = Json.decode payload_jsont event.payload |> Or_error.ok_exn in
    let identity = account_identity account in
    let trn =
      parse_transaction account (Ofx.transaction_of_tree tree |> Or_error.ok_exn)
    in
    if
      not
        (String.equal identity (account_identity account)
         && String.equal identity event.account
         && String.equal trn.fitid event.source_id)
    then failwith "bank event identity disagrees with payload";
    (match event.occurred with
     | Temporal.Date date when Date.equal date trn.booked_on -> ()
     | _ -> failwith "bank event booking date disagrees with payload");
    account, trn)
  |> fun result -> Or_error.tag result ~tag:"stored bank transaction"
;;
