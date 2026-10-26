open Core

type t =
  { institution : string
  ; bank_id : string
  ; account_id : string
  ; account_type : string
  ; ledger_account : string
  }

let create ~institution ~bank_id ~account_id ~account_type ~ledger_account =
  let open Or_error.Let_syntax in
  let%bind institution = Validation.text "institution" institution in
  let%bind bank_id = Validation.text "bank ID" bank_id in
  let%bind account_id = Validation.text "account ID" account_id in
  let%bind account_type = Validation.text "account type" account_type in
  let%bind ledger_account = Validation.account ledger_account in
  if not (String.is_prefix ledger_account ~prefix:"assets:bank:")
  then Or_error.error_string "bank ledger accounts must be below assets:bank"
  else Ok { institution; bank_id; account_id; account_type; ledger_account }
;;

let source_account t =
  Yojson.Safe.to_string
    (`List (List.map [ t.bank_id; t.account_id; t.account_type ] ~f:(fun s -> `String s)))
;;

let of_json contents =
  let open Or_error.Let_syntax in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string contents) in
  let%bind rows =
    match json with
    | `List rows -> Ok rows
    | _ -> Or_error.error_string "account mappings must be a JSON array"
  in
  let%bind mappings =
    Or_error.combine_errors
      (List.map rows ~f:(fun row ->
         let%bind fields =
           match row with
           | `Assoc fields -> Ok fields
           | _ -> Or_error.error_string "account mapping must be an object"
         in
         let allowed =
           [ "institution"; "bank_id"; "account_id"; "account_type"; "ledger_account" ]
         in
         let names = List.map fields ~f:fst in
         if
           List.length names <> 5
           || not
                (List.equal
                   String.equal
                   (List.sort names ~compare:String.compare)
                   (List.sort allowed ~compare:String.compare))
         then
           Or_error.error_string
             "mapping requires exactly institution, bank_id, account_id, account_type \
              and ledger_account"
         else (
           let field name =
             match List.Assoc.find fields name ~equal:String.equal with
             | Some (`String s) -> Ok s
             | _ -> Or_error.errorf "%s must be a string" name
           in
           let%bind institution = field "institution" in
           let%bind bank_id = field "bank_id" in
           let%bind account_id = field "account_id" in
           let%bind account_type = field "account_type" in
           let%bind ledger_account = field "ledger_account" in
           create ~institution ~bank_id ~account_id ~account_type ~ledger_account)))
  in
  let keys = List.map mappings ~f:(fun t -> t.institution, source_account t) in
  if
    List.contains_dup
      keys
      ~compare:(Tuple2.compare ~cmp1:String.compare ~cmp2:String.compare)
  then Or_error.error_string "duplicate source account in mapping configuration"
  else Ok mappings
;;

let to_json mappings =
  `List
    (List.map mappings ~f:(fun t ->
       `Assoc
         [ "institution", `String t.institution
         ; "bank_id", `String t.bank_id
         ; "account_id", `String t.account_id
         ; "account_type", `String t.account_type
         ; "ledger_account", `String t.ledger_account
         ]))
  |> Yojson.Safe.pretty_to_string
;;
