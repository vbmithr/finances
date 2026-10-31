open Base

type account =
  { bank_id : string
  ; account_id : string
  ; account_type : string
  ; currency : Iso4217.t
  }

type own_iban =
  | Known of Iban.t
  | Unknown
  | Not_applicable

type t =
  { account : account
  ; own_iban : own_iban
  ; booked_on : Date.t
  ; value_on : Date.t option
  ; amount : Quantity.t
  ; fee : Quantity.t
  ; descriptions : string list
  ; counterparty_iban : Iban.t option
  ; movement_type : Movement_type.t option
  ; reported_code : string option
  ; fields : (string * string) list
  ; main_description : int
  }

let create
      ~account
      ~own_iban
      ~booked_on
      ?value_on
      ~amount
      ?(fee = Quantity.zero)
      ?counterparty_iban
      ?movement_type
      ?reported_code
      ?(fields = [])
      ?(main_description = 0)
      descriptions
  =
  let open Or_error.Let_syntax in
  let%bind _ = Validation.text "bank ID" account.bank_id in
  let%bind _ = Validation.text "account ID" account.account_id in
  let%bind _ = Validation.text "account type" account.account_type in
  if Quantity.(fee < zero)
  then Or_error.error_string "movement fee must be non-negative"
  else
    Ok
      { account
      ; own_iban
      ; booked_on
      ; value_on
      ; amount
      ; fee
      ; descriptions
      ; counterparty_iban
      ; movement_type
      ; reported_code
      ; fields
      ; main_description
      }
;;

let principal t = Quantity.(t.amount + t.fee)
let with_movement_type t movement_type = { t with movement_type }

(* [description] is the text the adapter declares as the main one, [text] is any of
   them, and every other name is a field the source carries. *)
let field_values t name =
  let stripped = List.map t.descriptions ~f:String.strip in
  match name with
  | "description" -> Option.to_list (List.nth stripped t.main_description)
  | "text" -> stripped
  | name ->
    List.filter_map t.fields ~f:(fun (field, value) ->
      Option.some_if (String.equal field name) value)
;;
