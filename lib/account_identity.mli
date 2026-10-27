open Base

type t = private
  { institution : string
  ; bank_id : string
  ; account_id : string
  ; account_type : string
  ; iban : string
  }

val normalize_iban : string -> string Or_error.t

(** A checksum-valid source identifier is usable as imported IBAN evidence. *)
val inferred_iban : string -> string option

val create
  :  institution:string
  -> bank_id:string
  -> account_id:string
  -> account_type:string
  -> iban:string
  -> t Or_error.t

val source_account : t -> string

(** Null entries are placeholders, not deletion requests. *)
val of_json : string -> t list Or_error.t
