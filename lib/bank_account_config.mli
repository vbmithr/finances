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

val reference
  :  institution:string
  -> bank_id:string
  -> account_id:string
  -> account_type:string
  -> reference Or_error.t

val source_account : reference -> string
val of_json : string -> t list Or_error.t
val to_json : t list -> string
val reference_jsont : reference Jsont.t
val jsont : t Jsont.t
val snapshot_jsont : t list Jsont.t
