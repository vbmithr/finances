open Base

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
  Json.encode (Jsont.list Jsont.string) [ t.bank_id; t.account_id; t.account_type ]
;;

let jsont =
  Jsont.Object.map (fun institution bank_id account_id account_type ledger_account ->
    create ~institution ~bank_id ~account_id ~account_type ~ledger_account
    |> Json.or_error)
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun t -> t.institution)
  |> Jsont.Object.mem "bank_id" Jsont.string ~enc:(fun t -> t.bank_id)
  |> Jsont.Object.mem "account_id" Jsont.string ~enc:(fun t -> t.account_id)
  |> Jsont.Object.mem "account_type" Jsont.string ~enc:(fun t -> t.account_type)
  |> Jsont.Object.mem "ledger_account" Jsont.string ~enc:(fun t -> t.ledger_account)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let of_json contents =
  let open Or_error.Let_syntax in
  let%bind mappings = Json.decode (Jsont.list jsont) contents in
  let keys = List.map mappings ~f:(fun t -> t.institution, source_account t) in
  if List.contains_dup keys ~compare:[%compare: string * string]
  then Or_error.error_string "duplicate source account in mapping configuration"
  else Ok mappings
;;

let to_json = Json.encode ~format:Jsont.Indent (Jsont.list jsont)
