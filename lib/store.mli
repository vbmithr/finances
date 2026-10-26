open Core

(** SQLite is authoritative. One connection must not be shared between threads.
    Opening applies versioned migrations atomically and rejects newer schemas. *)
type t

type ingest =
  | Inserted of int64
  | Existing of int64

val with_open : string -> (t -> 'a Or_error.t) -> 'a Or_error.t

(** Identical retries return [Existing]; changed evidence for the same identity
    returns an error rather than silently overwriting the original. *)
val ingest : t -> Source_event.t -> ingest Or_error.t

val events : t -> (int64 * Source_event.t) list Or_error.t

type bank_import =
  { document_id : int64
  ; statements : int
  ; inserted : int
  ; existing : int
  }

(** Archives the original OFX bytes, imports every statement's transactions, and
    links evidence to the document in one transaction. Overlapping exports reuse
    transaction IDs scoped by institution and bank account. Conflicts roll back
    the complete import, including the document archive. No postings are created. *)
val import_ofx : t -> institution:string -> Bank_ofx.t -> bank_import Or_error.t

(** [key] is the stable projection identity within a source event. Recording the
    entry and all postings is atomic. Identical retries are harmless; changed
    projections require an explicit future correction/rebuild workflow. *)
val record_entry
  :  t
  -> event_id:int64
  -> key:string
  -> Accounting.entry
  -> ingest Or_error.t

val entries : t -> (int64 * string * Accounting.entry) list Or_error.t

type projection_report =
  { inserted : int
  ; existing : int
  }

(** Project every stored bank event against equity:unclassified, atomically.
    Source documents are not needed again. Repeating this operation is harmless;
    a conflict or invalid event rolls back the complete projection batch. *)
val project_bank : t -> projection_report Or_error.t
