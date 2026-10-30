open Base

type name = string * string

type tree =
  | Element of name * (name * string) list * tree list
  | Text of string

type document =
  { namespace : string
  ; root : tree
  ; statements : tree list
  }

let children ~namespace local = function
  | Text _ -> []
  | Element (_, _, children) ->
    List.filter children ~f:(function
      | Element ((ns, name), _, _) -> String.equal ns namespace && String.equal name local
      | Text _ -> false)
;;

let parse contents =
  Or_error.try_with (fun () ->
    let input = Xmlm.make_input ~strip:true (`String (0, contents)) in
    let dtd, root =
      Xmlm.input_doc_tree
        ~el:(fun (name, attributes) children -> Element (name, attributes, children))
        ~data:(fun text -> Text text)
        input
    in
    if Option.is_some dtd || not (Xmlm.eoi input)
    then failwith "unsupported CAMT document structure";
    let namespace =
      match root with
      | Element ((namespace, "Document"), _, _) -> namespace
      | _ -> failwith "expected CAMT Document root"
    in
    let pattern =
      Re.Perl.compile_pat "^urn:iso:std:iso:20022:tech:xsd:camt\\.053\\.001\\.[0-9]+$"
    in
    if not (Re.execp pattern namespace) then failwith "unsupported CAMT.053 namespace";
    let message =
      match children ~namespace "BkToCstmrStmt" root with
      | [ message ] -> message
      | _ -> failwith "expected exactly one BkToCstmrStmt"
    in
    let statements = children ~namespace "Stmt" message in
    if List.is_empty statements then failwith "CAMT document contains no statements";
    { namespace; root; statements })
  |> Or_error.tag ~tag:"CAMT.053"
;;

type date_value =
  | Date of string
  | Date_time of string
  | Both of string * string

type direction =
  | Credit
  | Debit
  | Other_direction of string

type entry_status =
  | Booked
  | Pending
  | Information
  | Other_status of string

type account_id =
  | Iban of string
  | Other_account_id of string

type balance_code =
  | Standard_balance of string
  | Proprietary_balance of string

type amount =
  { value : string
  ; currency : string option
  }

type account =
  { id : account_id option
  ; other_id : string option
  ; currency : string option
  ; servicer_name : string option
  }

type balance =
  { tree : tree
  ; code : balance_code option
  ; proprietary_code : string option
  ; amount : amount option
  ; direction : direction option
  ; as_of : date_value option
  }

type entry =
  { tree : tree
  ; amount : amount option
  ; direction : direction option
  ; status : entry_status option
  ; booking_date : date_value option
  ; value_date : date_value option
  ; account_servicer_reference : string option
  ; proprietary_code : string option
  ; additional_info : string option
  }

type statement =
  { tree : tree
  ; id : string option
  ; account : account option
  ; period_start : date_value option
  ; period_end : date_value option
  ; total_entries : string option
  ; balances : balance list
  ; entries : entry list
  }

let optional_child ~namespace name node =
  match children ~namespace name node with
  | [] -> None
  | [ child ] -> Some child
  | _ -> Printf.failwithf "duplicate CAMT field %s" name ()
;;

let rec optional_path ~namespace node = function
  | [] -> Some node
  | name :: rest ->
    Option.bind (optional_child ~namespace name node) ~f:(fun child ->
      optional_path ~namespace child rest)
;;

let text = function
  | Element (_, _, []) -> ""
  | Element (_, _, [ Text text ]) -> String.strip text
  | _ -> failwith "expected text-only CAMT field"
;;

let value ~namespace node path =
  Option.map (optional_path ~namespace node path) ~f:text
  |> Option.filter ~f:(Fn.non String.is_empty)
;;

let date_value ~namespace node date_name datetime_name =
  match value ~namespace node [ date_name ], value ~namespace node [ datetime_name ] with
  | Some value, None -> Some (Date value)
  | None, Some value -> Some (Date_time value)
  | None, None -> None
  | Some date, Some datetime -> Some (Both (date, datetime))
;;

let amount ~namespace node =
  Option.map (optional_child ~namespace "Amt" node) ~f:(fun amount ->
    let currency =
      match amount with
      | Element (_, attributes, _) ->
        List.Assoc.find attributes ("", "Ccy") ~equal:Poly.equal
      | Text _ -> None
    in
    { value = text amount; currency })
;;

let direction ~namespace node =
  Option.map (value ~namespace node [ "CdtDbtInd" ]) ~f:(function
    | "CRDT" -> Credit
    | "DBIT" -> Debit
    | other -> Other_direction other)
;;

let account ~namespace node =
  Option.map (optional_child ~namespace "Acct" node) ~f:(fun node ->
    let iban = value ~namespace node [ "Id"; "IBAN" ] in
    let other_id = value ~namespace node [ "Id"; "Othr"; "Id" ] in
    let id =
      match iban, other_id with
      | Some value, _ -> Some (Iban value)
      | None, Some value -> Some (Other_account_id value)
      | None, None -> None
    in
    { id
    ; other_id
    ; currency = value ~namespace node [ "Ccy" ]
    ; servicer_name = value ~namespace node [ "Svcr"; "FinInstnId"; "Nm" ]
    })
;;

let balance ~namespace tree =
  let standard = value ~namespace tree [ "Tp"; "CdOrPrtry"; "Cd" ] in
  let proprietary_code = value ~namespace tree [ "Tp"; "CdOrPrtry"; "Prtry" ] in
  let code =
    match standard, proprietary_code with
    | Some code, _ -> Some (Standard_balance code)
    | None, Some code -> Some (Proprietary_balance code)
    | None, None -> None
  in
  { tree
  ; code
  ; proprietary_code
  ; amount = amount ~namespace tree
  ; direction = direction ~namespace tree
  ; as_of =
      Option.bind (optional_child ~namespace "Dt" tree) ~f:(fun node ->
        date_value ~namespace node "Dt" "DtTm")
  }
;;

let entry ~namespace tree =
  let status =
    Option.bind (optional_child ~namespace "Sts" tree) ~f:(fun node ->
      let code =
        match value ~namespace node [ "Cd" ] with
        | Some _ as code -> code
        | None ->
          (match node with
           | Element (_, _, [ Text _ ]) -> Some (text node)
           | _ -> None)
      in
      Option.map code ~f:(function
        | "BOOK" -> Booked
        | "PDNG" -> Pending
        | "INFO" -> Information
        | other -> Other_status other))
  in
  { tree
  ; amount = amount ~namespace tree
  ; direction = direction ~namespace tree
  ; status
  ; booking_date =
      Option.bind (optional_child ~namespace "BookgDt" tree) ~f:(fun node ->
        date_value ~namespace node "Dt" "DtTm")
  ; value_date =
      Option.bind (optional_child ~namespace "ValDt" tree) ~f:(fun node ->
        date_value ~namespace node "Dt" "DtTm")
  ; account_servicer_reference = value ~namespace tree [ "AcctSvcrRef" ]
  ; proprietary_code = value ~namespace tree [ "BkTxCd"; "Prtry"; "Cd" ]
  ; additional_info = value ~namespace tree [ "AddtlNtryInf" ]
  }
;;

let statement_view ~namespace tree =
  Or_error.try_with (fun () ->
    let period = optional_child ~namespace "FrToDt" tree in
    { tree
    ; id = value ~namespace tree [ "Id" ]
    ; account = account ~namespace tree
    ; period_start =
        Option.bind period ~f:(fun node -> date_value ~namespace node "FrDt" "FrDtTm")
    ; period_end =
        Option.bind period ~f:(fun node -> date_value ~namespace node "ToDt" "ToDtTm")
    ; total_entries = value ~namespace tree [ "TxsSummry"; "TtlNtries"; "NbOfNtries" ]
    ; balances = List.map (children ~namespace "Bal" tree) ~f:(balance ~namespace)
    ; entries = List.map (children ~namespace "Ntry" tree) ~f:(entry ~namespace)
    })
  |> Or_error.tag ~tag:"CAMT statement"
;;
