open Base

type t = private
  { institution : string
  ; bank_id : string
  ; account_id : string
  ; account_type : string
  ; ledger_account : string
  }

val create
  :  institution:string
  -> bank_id:string
  -> account_id:string
  -> account_type:string
  -> ledger_account:string
  -> t Or_error.t

val source_account : t -> string
val of_json : string -> t list Or_error.t
val to_json : t list -> string
