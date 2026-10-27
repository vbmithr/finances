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
  ; iban : string update
  ; settlement_account : reference update
  }

val reference
  :  institution:string
  -> bank_id:string
  -> account_id:string
  -> account_type:string
  -> reference Or_error.t

val source_account : reference -> string
val reference_to_json : reference -> Yojson.Safe.t
val reference_of_json : Yojson.Safe.t -> reference Or_error.t
val of_json : string -> t list Or_error.t
val to_json : t list -> string
