open Base

type reference =
  { institution : string
  ; bank_id : string
  ; account_id : string
  ; account_type : string
  }

type 'a update =
  | Keep
  | Set of 'a
  | Clear

type t =
  { reference : reference
  ; ledger_account : string option
  ; iban : Iban.t update
  ; settlement_account : reference update
  }

let reference ~institution ~bank_id ~account_id ~account_type =
  let open Or_error.Let_syntax in
  let%bind institution = Validation.text "institution" institution in
  let%bind bank_id = Validation.text "bank_id" bank_id in
  let%bind account_id = Validation.text "account_id" account_id in
  let%map account_type = Validation.text "account_type" account_type in
  { institution; bank_id; account_id; account_type }
;;

let source_account r =
  Json.encode (Jsont.list Jsont.string) [ r.bank_id; r.account_id; r.account_type ]
;;

let reference_members ~enc map =
  map
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun t -> (enc t).institution)
  |> Jsont.Object.mem "bank_id" Jsont.string ~enc:(fun t -> (enc t).bank_id)
  |> Jsont.Object.mem "account_id" Jsont.string ~enc:(fun t -> (enc t).account_id)
  |> Jsont.Object.mem "account_type" Jsont.string ~enc:(fun t -> (enc t).account_type)
;;

let reference_jsont =
  Jsont.Object.map (fun institution bank_id account_id account_type ->
    reference ~institution ~bank_id ~account_id ~account_type |> Json.or_error)
  |> reference_members ~enc:Fn.id
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let update_jsont codec =
  Jsont.map
    ~dec:(function
      | None -> Clear
      | Some value -> Set value)
    ~enc:(function
      | Set value -> Some value
      | Keep | Clear -> None)
    (Jsont.option codec)
;;

let is_keep = function
  | Keep -> true
  | Set _ | Clear -> false
;;

let jsont =
  Jsont.Object.map
    (fun
        institution
         bank_id
         account_id
         account_type
         ledger_account
         iban
         settlement_account
       ->
       let reference =
         reference ~institution ~bank_id ~account_id ~account_type |> Json.or_error
       in
       let ledger_account =
         Option.map ledger_account ~f:(fun ledger_account ->
           let mapping =
             Bank_account_mapping.create
               ~institution
               ~bank_id
               ~account_id
               ~account_type
               ~ledger_account
             |> Json.or_error
           in
           mapping.ledger_account)
       in
       { reference; ledger_account; iban; settlement_account })
  |> reference_members ~enc:(fun t -> t.reference)
  |> Jsont.Object.opt_mem "ledger_account" Jsont.string ~enc:(fun t -> t.ledger_account)
  |> Jsont.Object.mem
       "iban"
       (update_jsont Json.iban)
       ~dec_absent:Keep
       ~enc:(fun t -> t.iban)
       ~enc_omit:is_keep
  |> Jsont.Object.mem
       "settlement_account"
       (update_jsont reference_jsont)
       ~dec_absent:Keep
       ~enc:(fun t -> t.settlement_account)
       ~enc_omit:is_keep
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let snapshot_jsont =
  Jsont.Object.map (fun () rows -> rows)
  |> Jsont.Object.mem "version" (Json.version 1) ~enc:(fun _ -> ())
  |> Jsont.Object.mem "accounts" (Jsont.list jsont) ~enc:Fn.id
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let of_json contents =
  let open Or_error.Let_syntax in
  let%bind rows = Json.decode snapshot_jsont contents in
  if
    List.contains_dup
      (List.map rows ~f:(fun a -> a.reference.institution, source_account a.reference))
      ~compare:[%compare: string * string]
  then Or_error.error_string "duplicate configured account"
  else Ok rows
;;

let to_json = Json.encode ~format:Jsont.Indent snapshot_jsont
