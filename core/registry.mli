open Base

(** The banks a [fin] build knows. The kernel names no bank: everything it
    learns about bank events comes through a registry. *)
type t

(** Rejects invalid or duplicate institutions and adapters without kinds. *)
val create : (module Adapter.S) list -> t Or_error.t

(** Sorted. *)
val institutions : t -> string list

val is_registered : t -> string -> bool

(** The institution identifier and its adapter's aliases; just the identifier
    for an unregistered institution. *)
val aliases : t -> string -> string list

(** Whether some adapter claims the event's kind. Other events (trading venues,
    future sources) are not bank movements. *)
val is_movement : t -> Source_event.t -> bool

(** Decodes a movement with the adapter registered for the event's venue. A
    bank kind under a venue no adapter claims is an error, never skipped. *)
val movement : t -> Source_event.t -> Movement.t Or_error.t
