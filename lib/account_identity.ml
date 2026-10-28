open Base

type t =
  { institution : string
  ; bank_id : string
  ; account_id : string
  ; account_type : string
  ; iban : Iban.t
  }

let inferred_iban account_id = Iban.of_string account_id |> Result.ok

let create ~institution ~bank_id ~account_id ~account_type ~iban =
  let open Or_error.Let_syntax in
  let%bind institution = Validation.text "institution" institution in
  let%bind bank_id = Validation.text "bank ID" bank_id in
  let%bind account_id = Validation.text "account ID" account_id in
  let%map account_type = Validation.text "account type" account_type in
  { institution; bank_id; account_id; account_type; iban }
;;

let source_account t =
  Json.encode (Jsont.list Jsont.string) [ t.bank_id; t.account_id; t.account_type ]
;;

let jsont =
  Jsont.Object.map (fun institution bank_id account_id account_type iban ->
    Option.map iban ~f:(fun iban ->
      create ~institution ~bank_id ~account_id ~account_type ~iban |> Json.or_error))
  |> Jsont.Object.mem "institution" Jsont.string
  |> Jsont.Object.mem "bank_id" Jsont.string
  |> Jsont.Object.mem "account_id" Jsont.string
  |> Jsont.Object.mem "account_type" Jsont.string
  |> Jsont.Object.mem "iban" (Jsont.option Json.iban)
  |> Jsont.Object.error_unknown
  |> Jsont.Object.finish
;;

let of_json contents =
  let open Or_error.Let_syntax in
  let%bind rows = Json.decode (Jsont.list jsont) contents in
  let rows = List.filter_opt rows in
  if
    List.contains_dup
      (List.map rows ~f:(fun r -> r.institution, source_account r))
      ~compare:[%compare: string * string]
  then Or_error.error_string "duplicate account identity"
  else Ok rows
;;
