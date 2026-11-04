open Base

type tree =
  | Element of string * tree list
  | Text of string

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : string
  }

type transaction =
  { tree : tree
  ; fitid : string
  ; transaction_type : string
  ; posted : string
  ; user_date : string option
  ; amount : string
  ; name : string option
  ; memo : string option
  ; corrects : bool
  }

type balance =
  { amount : string
  ; as_of : string
  }

type statement =
  { account : account
  ; transactions : transaction list
  ; ledger_balance : balance option
  ; available_balance : balance option
  ; period : (string * string) option
  }

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
  | _ -> Printf.failwithf "expected exactly one %s element" tag ()
;;

let text = function
  | Element (_, [ Text s ]) -> String.strip s
  | Element (_, []) -> ""
  | _ -> failwith "expected a text-only element"
;;

let required tag node =
  let value = text (required_child tag node) in
  if String.is_empty value then Printf.failwithf "%s cannot be empty" tag ();
  value
;;

let optional tag node =
  match children tag node with
  | [] -> None
  | [ child ] ->
    let value = text child in
    if String.is_empty value then None else Some value
  | _ -> Printf.failwithf "duplicate %s element" tag ()
;;

let checked_date s =
  if not (Re.execp date_syntax s) then failwith "invalid OFX date";
  s
;;

let calendar_date s =
  Or_error.try_with (fun () ->
    let s = checked_date s in
    let number pos len = Int.of_string (String.sub s ~pos ~len) in
    let date = number 0 4, number 4 2, number 6 2 in
    match Ptime.of_date date with
    | Some _ -> date
    | None -> failwith "invalid OFX date")
;;

let check_status node =
  List.iter (children "STATUS" node) ~f:(fun status ->
    if not (String.equal (required "CODE" status) "0")
    then failwith "OFX response status is not successful")
;;

(* OFX 2.2 section 3.2.8: omitted time is midnight, omitted offset is GMT.
   Decimal offsets are hours (e.g. 5.5 means five hours and thirty minutes). *)
let instant value =
  Or_error.try_with (fun () ->
    let value = checked_date value in
    let stamp, offset =
      match String.lsplit2 value ~on:'[' with
      | None -> value, 0
      | Some (stamp, suffix) ->
        let zone =
          String.chop_suffix_exn suffix ~suffix:"]" |> String.split ~on:':' |> List.hd_exn
        in
        let sign, magnitude =
          if String.is_prefix zone ~prefix:"-"
          then -1, String.drop_prefix zone 1
          else 1, Option.value (String.chop_prefix zone ~prefix:"+") ~default:zone
        in
        let hours, fraction =
          match String.lsplit2 magnitude ~on:'.' with
          | None -> magnitude, ""
          | Some (hours, fraction) -> hours, fraction
        in
        if String.length fraction > 2
        then failwith "OFX timezone supports at most two decimal places";
        let hours = Int.of_string hours in
        if hours > 12 then failwith "OFX timezone is outside -12 to +12 hours";
        let hundredths =
          (hours * 100)
          + Int.of_string (fraction ^ String.make (2 - String.length fraction) '0')
        in
        if hundredths > 1200 then failwith "OFX timezone is outside -12 to +12 hours";
        stamp, sign * hundredths * 36
    in
    let day =
      String.sub stamp ~pos:0 ~len:4
      ^ "-"
      ^ String.sub stamp ~pos:4 ~len:2
      ^ "-"
      ^ String.sub stamp ~pos:6 ~len:2
    in
    let clock =
      if String.length stamp = 8
      then "00:00:00"
      else
        String.sub stamp ~pos:8 ~len:2
        ^ ":"
        ^ String.sub stamp ~pos:10 ~len:2
        ^ ":"
        ^ String.drop_prefix stamp 12
    in
    let local =
      match Ptime.of_rfc3339 ~strict:true (day ^ "T" ^ clock ^ "Z") with
      | Ok (time, _, _) -> time
      | Error _ -> failwith "invalid OFX datetime"
    in
    match Ptime.sub_span local (Ptime.Span.of_int_s offset) with
    | Some time -> time
    | None -> failwith "OFX datetime is outside the Ptime range")
;;

let read_transaction node =
  (match node with
   | Element ("STMTTRN", _) -> ()
   | _ -> failwith "expected a bank transaction");
  { tree = node
  ; fitid = required "FITID" node
  ; posted = checked_date (required "DTPOSTED" node)
  ; user_date = optional "DTUSER" node
  ; amount = required "TRNAMT" node
  ; transaction_type = required "TRNTYPE" node
  ; name = optional "NAME" node
  ; memo = optional "MEMO" node
  ; corrects =
      List.exists [ "CORRECTFITID"; "CORRECTACTION" ] ~f:(fun tag ->
        not (List.is_empty (children tag node)))
  }
;;

let transaction_of_tree node = Or_error.try_with (fun () -> read_transaction node)

let read_balance tag node =
  match children tag node with
  | [] -> None
  | [ balance ] ->
    Some
      { amount = required "BALAMT" balance
      ; as_of = checked_date (required "DTASOF" balance)
      }
  | _ -> Printf.failwithf "duplicate %s element" tag ()
;;

let read_statement node =
  let source_account = required_child "BANKACCTFROM" node in
  let account =
    { bank_id = required "BANKID" source_account
    ; account_id = required "ACCTID" source_account
    ; account_type = required "ACCTTYPE" source_account
    ; currency = required "CURDEF" node
    }
  in
  let transaction_list = required_child "BANKTRANLIST" node in
  let transactions =
    List.mapi (children "STMTTRN" transaction_list) ~f:(fun i node ->
      transaction_of_tree node
      |> fun result ->
      Or_error.tag result ~tag:(Printf.sprintf "transaction %d" (i + 1))
      |> Or_error.ok_exn)
  in
  { account
  ; transactions
  ; ledger_balance = read_balance "LEDGERBAL" node
  ; available_balance = read_balance "AVAILBAL" node
  ; period =
      (match optional "DTSTART" transaction_list, optional "DTEND" transaction_list with
       | Some first, Some last -> Some (checked_date first, checked_date last)
       | _ -> None)
  }
;;

let only tags ~otherwise = function
  | Element (_, nodes) ->
    List.iter nodes ~f:(function
      | Element (tag, _) when List.mem tags tag ~equal:String.equal -> ()
      | _ -> failwith otherwise)
  | Text _ -> assert false
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
    only
      [ "SIGNONMSGSRSV1"; "BANKMSGSRSV1" ]
      root
      ~otherwise:"unsupported OFX message set";
    let statements =
      List.concat_map (children "BANKMSGSRSV1" root) ~f:(fun messages ->
        only [ "STMTTRNRS" ] messages ~otherwise:"unsupported OFX bank response";
        List.concat_map (children "STMTTRNRS" messages) ~f:(fun response ->
          check_status response;
          let statements = children "STMTRS" response in
          if List.is_empty statements then failwith "bank response contains no statement";
          statements))
      |> List.mapi ~f:(fun i statement ->
        Or_error.try_with (fun () -> read_statement statement)
        |> fun result ->
        Or_error.tag result ~tag:(Printf.sprintf "statement %d" (i + 1))
        |> Or_error.ok_exn)
    in
    if List.is_empty statements then failwith "OFX document contains no bank statement";
    statements)
;;
