open Base

(** The banks a [fin] build knows. The kernel names no bank: everything it
    learns about bank events comes through a registry. *)
type t

(** Rejects invalid or duplicate institutions, adapters without kinds, repeated
    formats and companions for formats the adapter does not declare. Fallbacks
    decode specified kinds for unregistered institutions and for explicitly
    named fallback institutions whose current adapter does not claim that kind. *)
val create
  :  ?fallback:string * (Source_event.t -> Movement.t Or_error.t)
  -> ?fallbacks:(string * (Source_event.t -> Movement.t Or_error.t)) list
  -> ?fallback_institutions:string list
  -> ?source_typing:Source_typing.t list
  -> ?source_evidence:Source_evidence.default list
  -> (module Adapter.S) list
  -> t Or_error.t

(** Sorted. *)
val institutions : t -> string list

val is_registered : t -> string -> bool

(** The institution identifier and its adapter's aliases; just the identifier
    for an unregistered institution. *)
val aliases : t -> string -> string list

(** Whether some adapter claims the event's kind. Other events (trading venues,
    future sources) are not bank movements. *)
val is_movement : t -> Source_event.t -> bool

(** Look up a bundled automatic source-typing profile. *)
val default_source_typing : t -> string -> Source_typing.t option

val default_source_evidence : t -> Source_evidence.default list

(** Decode with the venue adapter or an applicable format fallback. *)
val movement
  :  ?source_typing:Source_typing.t
  -> t
  -> Source_event.t
  -> Movement.t Or_error.t

val find : t -> string -> (module Adapter.S) option

(** Every registered (institution, format) that recognizes the file. *)
val probe : t -> filename:string -> string -> (string * string) list

(** Every companion format, with the institution that owns it. *)
val companions : t -> (string * Companion.t) list
