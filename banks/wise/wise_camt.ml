open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : string
  }

type transaction =
  { account : account
  ; reference : string
  ; booked_at : string
  ; instant_ns : string
  ; occurred_at_ms : int64
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Quantity.t
  ; credit : bool
  ; description : string
  ; source_id : string
  ; payload : string
  }

type statement =
  { account : account
  ; statement_id : string
  ; period_start : string
  ; period_end : string
  ; opening_at : string
  ; closing_at : string
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; transactions : transaction list
  }

type t =
  { contents : string
  ; statements : statement list
  }

type xml =
  | Element of string * ((string * string) * string) list * xml list
  | Text of string

let namespace = "urn:iso:std:iso:20022:tech:xsd:camt.053.001.10"

let children tag = function
  | Element (_, _, children) ->
    List.filter children ~f:(function
      | Element (name, _, _) -> String.equal name tag
      | _ -> false)
  | Text _ -> []
;;

let required tag node =
  match children tag node with
  | [ value ] -> value
  | _ -> Printf.failwithf "expected exactly one %s" tag ()
;;

let rec at node = function
  | [] -> node
  | tag :: rest -> at (required tag node) rest
;;

let text = function
  | Element (_, _, []) -> ""
  | Element (_, _, [ Text text ]) -> String.strip text
  | _ -> failwith "expected text-only CAMT field"
;;

let value node path =
  let result = text (at node path) in
  if String.is_empty result then failwith "required CAMT field is empty";
  result
;;

let optional node path =
  let rec walk node = function
    | [] ->
      let text = text node in
      if String.is_empty text then None else Some text
    | tag :: rest ->
      (match children tag node with
       | [] -> None
       | [ node ] -> walk node rest
       | _ -> failwith "duplicate optional CAMT field")
  in
  walk node path
;;

let account_identity a =
  Yojson.Safe.to_string
    (`List (List.map [ a.bank_id; a.account_id; a.account_type ] ~f:(fun s -> `String s)))
;;

let hex32 text =
  String.length text = 32
  && String.for_all text ~f:(function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false)
;;

let normalize_reference ~description reference =
  match String.chop_prefix reference ~prefix:"BALANCE_CASHBACK-" with
  | Some id ->
    let id = String.filter id ~f:(fun c -> not (Char.equal c '-')) in
    if not (hex32 id) then failwith "invalid Wise cashback ID";
    "BALANCE_CASHBACK-" ^ String.lowercase id
  | None
    when hex32 reference
         && String.equal (String.lowercase (String.strip description)) "cashback" ->
    "BALANCE_CASHBACK-" ^ String.lowercase reference
  | None -> reference
;;

let timestamp value =
  let time = Timestamp.of_rfc3339_exn value in
  let ns = Timestamp.nanoseconds_since_epoch_exn time in
  if Int64.(ns < 0L) then failwith "Wise timestamp is before the supported epoch";
  ns
;;

let local_millisecond text =
  if String.length text < 19 then failwith "Wise booking timestamp lacks a clock time";
  let prefix = String.prefix text 19 in
  let rec fraction i =
    if i < String.length text && Char.is_digit text.[i] then fraction (i + 1) else i
  in
  let digits =
    if String.length text > 19 && Char.equal text.[19] '.'
    then String.sub text ~pos:20 ~len:(fraction 20 - 20)
    else ""
  in
  prefix ^ "." ^ String.prefix (digits ^ "000") 3
;;

let make_transaction account ~reference ~booked_at ~value_on ~amount ~credit ~description =
  if (credit && Quantity.(amount < zero)) || ((not credit) && Quantity.(amount > zero))
  then failwith "Wise amount sign disagrees with direction";
  let ns = timestamp booked_at in
  let instant_ns = Int64.to_string ns in
  let occurred_at_ms = Int64.(ns / 1000000L) in
  let booked_on = Date.of_string (String.prefix booked_at 10) in
  let reference = normalize_reference ~description reference in
  ignore (Validation.text "Wise reference" reference |> Or_error.ok_exn : string);
  let source_id =
    Yojson.Safe.to_string
      (`List
          [ `String reference
          ; `String instant_ns
          ; `String (if credit then "CRDT" else "DBIT")
          ])
  in
  let payload =
    Yojson.Safe.to_string
      (`Assoc
          [ "format", `String "wise-camt-v1"
          ; "account", `String (account_identity account)
          ; "currency", `String account.currency
          ; "reference", `String reference
          ; "booked_at", `String booked_at
          ; ( "value_on"
            , Option.value_map value_on ~default:`Null ~f:(fun date ->
                `String (Date.to_string date)) )
          ; "amount", `String (Quantity.to_string amount)
          ; "credit", `Bool credit
          ; "description", `String description
          ])
  in
  { account
  ; reference
  ; booked_at
  ; instant_ns
  ; occurred_at_ms
  ; booked_on
  ; value_on
  ; amount
  ; credit
  ; description
  ; source_id
  ; payload
  }
;;

let direction node =
  match value node [ "CdtDbtInd" ] with
  | "CRDT" -> true
  | "DBIT" -> false
  | _ -> failwith "invalid CAMT credit/debit indicator"
;;

let signed_amount currency node =
  let amount = required "Amt" node in
  let declared =
    match amount with
    | Element (_, attributes, _) ->
      List.Assoc.find_exn attributes ("", "Ccy") ~equal:Poly.equal
    | _ -> assert false
  in
  if not (String.equal currency declared)
  then failwith "CAMT entry currency differs from balance account currency";
  let amount = Quantity.of_string (text amount) in
  if Quantity.(amount < zero)
  then failwith "CAMT amount must be unsigned before applying credit/debit indicator";
  if direction node then amount else Quantity.neg amount
;;

let parse_statement node =
  let acct = required "Acct" node in
  let servicer = value acct [ "Svcr"; "FinInstnId"; "Nm" ] in
  if not (String.is_substring (String.lowercase servicer) ~substring:"wise")
  then failwith "CAMT account servicer is not Wise";
  let account_id =
    match optional acct [ "Id"; "IBAN" ] with
    | Some id -> id
    | None -> value acct [ "Id"; "Othr"; "Id" ]
  in
  let currency =
    value acct [ "Ccy" ] |> Validation.token "Wise account currency" |> Or_error.ok_exn
  in
  ignore (Validation.text "Wise account ID" account_id |> Or_error.ok_exn : string);
  let account =
    { bank_id = "wise"; account_id; account_type = "balance-" ^ currency; currency }
  in
  let statement_id = value node [ "Id" ] in
  let period_start = value node [ "FrToDt"; "FrDtTm" ]
  and period_end = value node [ "FrToDt"; "ToDtTm" ] in
  let from_ns = timestamp period_start
  and to_ns = timestamp period_end in
  if Int64.(from_ns > to_ns) then failwith "reversed Wise statement interval";
  let balance code =
    let balances =
      List.filter (children "Bal" node) ~f:(fun balance ->
        String.equal (value balance [ "Tp"; "CdOrPrtry"; "Cd" ]) code)
    in
    match balances with
    | [ balance ] ->
      let at = value balance [ "Dt"; "DtTm" ] in
      ignore (timestamp at : int64);
      signed_amount currency balance, at
    | _ ->
      Printf.failwithf "Wise statement requires exactly one %s booked balance" code ()
  in
  let opening, opening_at = balance "OPBD"
  and closing, closing_at = balance "CLBD" in
  let transactions =
    List.map (children "Ntry" node) ~f:(fun entry ->
      if not (String.equal (value entry [ "Sts"; "Cd" ]) "BOOK")
      then failwith "only booked Wise entries are supported";
      let booked_at = value entry [ "BookgDt"; "DtTm" ] in
      let ns = timestamp booked_at in
      if Int64.(ns < from_ns || ns > to_ns)
      then failwith "Wise entry lies outside statement interval";
      let value_on =
        match optional entry [ "ValDt"; "Dt" ] with
        | Some date -> Some (Date.of_string date)
        | None ->
          Option.map
            (optional entry [ "ValDt"; "DtTm" ])
            ~f:(fun date ->
              ignore (timestamp date : int64);
              Date.of_string (String.prefix date 10))
      in
      make_transaction
        account
        ~reference:(value entry [ "BkTxCd"; "Prtry"; "Cd" ])
        ~booked_at
        ~value_on
        ~amount:(signed_amount currency entry)
        ~credit:(direction entry)
        ~description:(value entry [ "AddtlNtryInf" ]))
  in
  if
    List.contains_dup
      (List.map transactions ~f:(fun t -> t.source_id))
      ~compare:String.compare
  then
    failwith
      "Wise statement has indistinguishable entry identities; no entries were collapsed";
  let count = optional node [ "TxsSummry"; "TtlNtries"; "NbOfNtries" ] in
  Option.iter count ~f:(fun count ->
    if Int.of_string count <> List.length transactions
    then failwith "Wise statement entry count mismatch");
  let movements =
    List.fold transactions ~init:Quantity.zero ~f:(fun sum tx ->
      Quantity.(sum + tx.amount))
  in
  if not Quantity.(opening + movements = closing)
  then failwith "Wise booked balances do not reconcile with statement movements";
  { account
  ; statement_id
  ; period_start
  ; period_end
  ; opening_at
  ; closing_at
  ; opening
  ; closing
  ; transactions
  }
;;

let parse contents =
  Or_error.try_with (fun () ->
    let input = Xmlm.make_input ~strip:true (`String (0, contents)) in
    let dtd, root =
      Xmlm.input_doc_tree
        ~el:(fun ((ns, tag), attributes) children ->
          if not (String.equal ns namespace)
          then failwith "expected Wise CAMT.053.001.10 namespace";
          let attributes =
            List.filter attributes ~f:(fun ((ns, _), _) ->
              not (String.equal ns Xmlm.ns_xmlns))
          in
          Element (tag, attributes, children))
        ~data:(fun text -> Text text)
        input
    in
    if Option.is_some dtd || not (Xmlm.eoi input)
    then failwith "unsupported CAMT document structure";
    (match root with
     | Element ("Document", _, _) -> ()
     | _ -> failwith "expected CAMT Document root");
    let message = at root [ "BkToCstmrStmt" ] in
    let statements = List.map (children "Stmt" message) ~f:parse_statement in
    if List.is_empty statements then failwith "CAMT document contains no statements";
    { contents; statements })
  |> fun result -> Or_error.tag result ~tag:"Wise CAMT"
;;

let source_transaction (event : Source_event.t) =
  Or_error.try_with (fun () ->
    if not (String.equal event.kind "wise-transaction")
    then failwith "not a Wise source event";
    let fields =
      match Yojson.Safe.from_string event.payload with
      | `Assoc fields -> fields
      | _ -> failwith "invalid Wise payload"
    in
    let field name = List.Assoc.find_exn fields name ~equal:String.equal in
    let string name =
      match field name with
      | `String s -> s
      | _ -> failwith "invalid Wise payload field"
    in
    if not (String.equal (string "format") "wise-camt-v1")
    then failwith "unsupported Wise payload version";
    let bank_id, account_id, account_type =
      match Yojson.Safe.from_string (string "account") with
      | `List [ `String bank; `String id; `String kind ] -> bank, id, kind
      | _ -> failwith "invalid Wise account identity"
    in
    let currency =
      Validation.token "Wise currency" (string "currency") |> Or_error.ok_exn
    in
    if
      not
        (String.equal bank_id "wise" && String.equal account_type ("balance-" ^ currency))
    then failwith "invalid Wise balance account";
    let account = { bank_id; account_id; account_type; currency } in
    let tx =
      make_transaction
        account
        ~reference:(string "reference")
        ~booked_at:(string "booked_at")
        ~value_on:
          (match field "value_on" with
           | `Null -> None
           | `String s -> Some (Date.of_string s)
           | _ -> failwith "invalid Wise value date")
        ~amount:(Quantity.of_string (string "amount"))
        ~credit:
          (match field "credit" with
           | `Bool b -> b
           | _ -> failwith "invalid Wise debit/credit flag")
        ~description:(string "description")
    in
    if
      (not
         (String.equal event.account (account_identity account)
          && String.equal event.source_id tx.source_id))
      || not
           (match event.occurred with
            | At_ms ms -> Int64.equal ms tx.occurred_at_ms
            | _ -> false)
    then failwith "Wise source identity or booking timestamp disagrees with payload";
    tx)
  |> fun result -> Or_error.tag result ~tag:"stored Wise transaction"
;;
