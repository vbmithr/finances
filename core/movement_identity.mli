open Base

(** A stable source identity plus a source-defined leg role. A role is a
    semantic identifier such as [primary], [from], or [to], not a list index.
    Source evidence must still be checked for changes and ambiguity. *)
type t = private
  { venue : string
  ; source_account : string
  ; source_kind : string
  ; source_id : string
  ; role : string
  }

val create
  :  venue:string
  -> source_account:string
  -> source_kind:string
  -> source_id:string
  -> role:string
  -> t Or_error.t

val of_event : Source_event.t -> role:string -> t Or_error.t
val equal : t -> t -> bool
val compare : t -> t -> int
val codec : t Jsont.t
