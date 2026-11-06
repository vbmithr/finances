open Base

(** What restore and export compare a source event by, computed once when it is imported
    and stored in [source_event_digests], so that neither decodes payloads again. *)

type t =
  { payload : string
    (** A records row's identity, else the payload, as SHA-256: says the row is the same. *)
  ; fields : string
    (** A records row's identity, else its fields: its fields can grow with its converter. *)
  ; evidence : string
    (** What tells it from other rows of its day when its institution has no row IDs:
        ["payload:"] or ["fields:"] and a digest. *)
  }

val compute : Source_event.t -> t
val store : Handle.t -> int64 -> Source_event.t -> unit Or_error.t

(** Every event's digests, by event ID, in one query. *)
val load : Handle.t -> (int64, t) Hashtbl.t Or_error.t
