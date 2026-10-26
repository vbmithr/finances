open Core

type t = private
  { account_id : string
  ; currency : string
  ; booked_on : Date.t
  ; value_on : Date.t
  ; amount : Bigdecimal.t
  ; counterparty : string
  ; name : string
  ; description : string
  ; occurrence : int
  ; source_id : string
  ; payload : string
  }

val normalize : string -> string

val key
  :  account_id:string
  -> booked_on:Date.t
  -> value_on:Date.t
  -> amount:Bigdecimal.t
  -> counterparty:string
  -> name:string
  -> description:string
  -> string

val csv_key : t -> string
val account_identity : t -> string

val create
  :  account_id:string
  -> currency:string
  -> booked_on:Date.t
  -> value_on:Date.t
  -> amount:Bigdecimal.t
  -> counterparty:string
  -> name:string
  -> description:string
  -> occurrence:int
  -> t Or_error.t

val source_transaction : Source_event.t -> t Or_error.t
