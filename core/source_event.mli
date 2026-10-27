open Base

(** Immutable source evidence. Identity is scoped to venue, account and kind. *)
type t = private
  { venue : string
  ; account : string
  ; kind : string
  ; source_id : string
  ; occurred : Temporal.t
  ; payload : string
  }

val create
  :  venue:string
  -> account:string
  -> kind:string
  -> source_id:string
  -> occurred_at:Ptime.t
  -> payload:string
  -> t Or_error.t

(** Bank booking dates do not assert a time of day or timezone. *)
val create_dated
  :  venue:string
  -> account:string
  -> kind:string
  -> source_id:string
  -> occurred_on:Date.t
  -> payload:string
  -> t Or_error.t
