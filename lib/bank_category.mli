open Core

(** Explicit reviewed counter-account for one immutable bank movement. *)
type t = private
  { event_id : int64
  ; account : string
  ; reason : string
  }

val create : event_id:int64 -> account:string -> reason:string -> t Or_error.t
val of_json : string -> t list Or_error.t
