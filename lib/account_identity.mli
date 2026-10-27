open Base

type t = private
  { institution : string
  ; bank_id : string
  ; account_id : string
  ; account_type : string
  ; iban : Iban.t
  }

(** A source account identifier that is a valid IBAN is usable as imported IBAN
    evidence. *)
val inferred_iban : string -> Iban.t option

val create
  :  institution:string
  -> bank_id:string
  -> account_id:string
  -> account_type:string
  -> iban:Iban.t
  -> t Or_error.t

val source_account : t -> string

(** Null entries are placeholders, not deletion requests. *)
val of_json : string -> t list Or_error.t
