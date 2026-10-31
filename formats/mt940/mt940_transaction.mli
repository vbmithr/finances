open Base

(** A profile-bound codec and identity policy, created once and reused. *)
type engine

type t = private
  { account_id : string
  ; currency : Iso4217.t
  ; booked_on : Date.t
  ; value_on : Date.t
  ; amount : Quantity.t
  ; counterparty : string
  ; name : string
  ; description : string
  ; occurrence : int
  ; source_id : string
  ; payload : string
  }

val make : Mt940_profile.t -> engine
val profile : engine -> Mt940_profile.t
val normalize : string -> string
val account_identity : engine -> t -> string

val key
  :  account_id:string
  -> booked_on:Date.t
  -> value_on:Date.t
  -> amount:Quantity.t
  -> counterparty:string
  -> name:string
  -> description:string
  -> string

val csv_key : t -> string

val create
  :  engine
  -> account_id:string
  -> currency:Iso4217.t
  -> booked_on:Date.t
  -> value_on:Date.t
  -> amount:Quantity.t
  -> counterparty:string
  -> name:string
  -> description:string
  -> occurrence:int
  -> t Or_error.t

val codec : engine -> t Jsont.t
val source_transaction : engine -> Source_event.t -> t Or_error.t
