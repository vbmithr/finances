open Base

(** Immutable source evidence. Identity is scoped to venue, account and kind. *)
type t = private
  { venue : string
  ; account : string
  ; kind : string
  ; source_id : string
  ; occurred : Temporal.t
  ; payload : string
  ; fields : (string * string) list
    (** Every column or tag the source carried, by name, in source order. Kept
            beside the payload so rules can match them; not part of the identity. *)
  }

val create
  :  ?fields:(string * string) list
  -> venue:string
  -> account:string
  -> kind:string
  -> source_id:string
  -> occurred_at:Ptime.t
  -> payload:string
  -> unit
  -> t Or_error.t

(** Bank booking dates do not assert a time of day or timezone. *)
val create_dated
  :  ?fields:(string * string) list
  -> venue:string
  -> account:string
  -> kind:string
  -> source_id:string
  -> occurred_on:Date.t
  -> payload:string
  -> unit
  -> t Or_error.t

(** The same event with its fields replaced. *)
val with_fields : t -> (string * string) list -> t
