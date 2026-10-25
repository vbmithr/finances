open Core

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
  ; amount : Bigdecimal.t
  ; transaction_type : string
  ; name : string option
  ; memo : string option
  ; source_payload : string
  }

type balance =
  { amount : Bigdecimal.t
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

type xml =
  | Element of string * xml list
  | Text of string

let date_syntax =
  Re.Perl.compile_pat
    "^[0-9]{8}([0-9]{6}(\\.[0-9]+)?(\\[[+-]?[0-9]+(\\.[0-9]+)?(:[A-Za-z]+)?\\])?)?$"
;;

let children tag = function
  | Element (_, nodes) ->
    List.filter nodes ~f:(function
      | Element (name, _) -> String.equal name tag
      | Text _ -> false)
  | Text _ -> []
;;

let required_child tag node =
  match children tag node with
  | [ child ] -> child
  | _ -> failwithf "expected exactly one %s element" tag ()
;;

let text = function
  | Element (_, [ Text s ]) -> String.strip s
  | Element (_, []) -> ""
  | _ -> failwith "expected a text-only element"
;;

let required tag node =
  let value = text (required_child tag node) in
  if String.is_empty value then failwithf "%s cannot be empty" tag ();
  value
;;

let optional tag node =
  match children tag node with
  | [] -> None
  | [ child ] ->
    let value = text child in
    if String.is_empty value then None else Some value
  | _ -> failwithf "duplicate %s element" tag ()
;;

let date s =
  (* Keep the date in the source's local calendar. A timestamp suffix does not
     turn a bank booking date into a UTC instant. Validate the OFX date syntax. *)
  if not (Re.execp date_syntax s) then failwith "invalid OFX date";
  Date.of_string
    (String.sub s ~pos:0 ~len:4
     ^ "-"
     ^ String.sub s ~pos:4 ~len:2
     ^ "-"
     ^ String.sub s ~pos:6 ~len:2)
;;

let check_status node =
  List.iter (children "STATUS" node) ~f:(fun status ->
    if not (String.equal (required "CODE" status) "0")
    then failwith "OFX response status is not successful")
;;

let rec canonical = function
  | Text s -> `String (String.strip s)
  | Element (tag, nodes) -> `List [ `String tag; `List (List.map nodes ~f:canonical) ]
;;

let account_identity a =
  Yojson.Safe.to_string
    (`List (List.map [ a.bank_id; a.account_id; a.account_type ] ~f:(fun s -> `String s)))
;;

let parse_transaction account node =
  let fitid = required "FITID" node |> Validation.text "FITID" |> Or_error.ok_exn in
  let booked_on = date (required "DTPOSTED" node) in
  (* BoursoBank's observed DTUSER is not a valid calendar date. Preserve it
     verbatim as source metadata; it cannot supply a value or booking date. *)
  let user_date = optional "DTUSER" node in
  let amount = Bigdecimal.of_string (required "TRNAMT" node) in
  let transaction_type = required "TRNTYPE" node in
  if
    List.exists [ "CORRECTFITID"; "CORRECTACTION" ] ~f:(fun tag ->
      not (List.is_empty (children tag node)))
  then failwith "OFX transaction corrections require an explicit correction workflow";
  (* Keep DTUSER in the archived document and parsed observation, but exclude
     this unreliable metadata from the stable transaction evidence. *)
  let stable_node =
    match node with
    | Element (tag, nodes) ->
      Element
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
  ; transaction_type
  ; name = optional "NAME" node
  ; memo = optional "MEMO" node
  ; source_payload
  }
;;

let parse_balance tag node =
  match children tag node with
  | [] -> None
  | [ balance ] ->
    let as_of = required "DTASOF" balance in
    ignore (date as_of : Date.t);
    Some { amount = Bigdecimal.of_string (required "BALAMT" balance); as_of }
  | _ -> failwithf "duplicate %s element" tag ()
;;

let parse_statement node =
  let source_account = required_child "BANKACCTFROM" node in
  let account =
    { bank_id =
        required "BANKID" source_account |> Validation.text "bank ID" |> Or_error.ok_exn
    ; account_id =
        required "ACCTID" source_account
        |> Validation.text "account ID"
        |> Or_error.ok_exn
    ; account_type =
        required "ACCTTYPE" source_account
        |> Validation.text "account type"
        |> Or_error.ok_exn
    ; currency = required "CURDEF" node |> Validation.token "currency" |> Or_error.ok_exn
    }
  in
  let transaction_list = required_child "BANKTRANLIST" node in
  let transactions =
    List.mapi (children "STMTTRN" transaction_list) ~f:(fun i node ->
      Or_error.try_with (fun () -> parse_transaction account node)
      |> fun result ->
      Or_error.tag result ~tag:(Printf.sprintf "transaction %d" (i + 1))
      |> Or_error.ok_exn)
  in
  { account
  ; transactions
  ; ledger_balance = parse_balance "LEDGERBAL" node
  ; available_balance = parse_balance "AVAILBAL" node
  }
;;

let parse contents =
  Or_error.try_with (fun () ->
    let input = Xmlm.make_input ~strip:true (`String (0, contents)) in
    let dtd, root =
      Xmlm.input_doc_tree
        ~el:(fun ((namespace, tag), attributes) nodes ->
          if (not (String.is_empty namespace)) || not (List.is_empty attributes)
          then failwith "OFX namespaced or attributed elements are unsupported";
          Element (tag, nodes))
        ~data:(fun data -> Text data)
        input
    in
    if Option.is_some dtd then failwith "OFX document type declarations are unsupported";
    if not (Xmlm.eoi input) then failwith "unexpected content after OFX document";
    (match root with
     | Element ("OFX", _) -> ()
     | _ -> failwith "expected an OFX XML document");
    List.iter (children "SIGNONMSGSRSV1" root) ~f:(fun signon ->
      List.iter (children "SONRS" signon) ~f:check_status);
    (match root with
     | Element (_, nodes) ->
       List.iter nodes ~f:(function
         | Element (("SIGNONMSGSRSV1" | "BANKMSGSRSV1"), _) -> ()
         | _ -> failwith "unsupported OFX message set")
     | Text _ -> assert false);
    let statements =
      List.concat_map (children "BANKMSGSRSV1" root) ~f:(fun messages ->
        (match messages with
         | Element (_, nodes) ->
           List.iter nodes ~f:(function
             | Element ("STMTTRNRS", _) -> ()
             | _ -> failwith "unsupported OFX bank response")
         | Text _ -> assert false);
        List.concat_map (children "STMTTRNRS" messages) ~f:(fun response ->
          check_status response;
          let statements = children "STMTRS" response in
          if List.is_empty statements then failwith "bank response contains no statement";
          statements))
      |> List.mapi ~f:(fun i statement ->
        Or_error.try_with (fun () -> parse_statement statement)
        |> fun result ->
        Or_error.tag result ~tag:(Printf.sprintf "statement %d" (i + 1))
        |> Or_error.ok_exn)
    in
    if List.is_empty statements then failwith "OFX document contains no bank statement";
    { contents; statements })
  |> fun result -> Or_error.tag result ~tag:"OFX import"
;;
