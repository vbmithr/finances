(** How repeated observations are reconciled across documents. *)
type overlap =
  | Source_ids (** The bank supplies stable movement identifiers. *)
  | Day_inventory
  (** No bank IDs: per account and day, the sorted movement payloads must agree
      across documents; a partial or corrected day is refused for review. *)

(** User-supplied import hints. *)
type context = { account_label : string option }

(** A bank, as the kernel knows it. Adapters are pure: they never access the
    database, and they decode only the events they produced. *)
module type S = sig
  (** Stable institution identifier; the [venue] of every event the adapter
      produced. *)
  val institution : string

  (** Other words, in lower case, that name the institution in other banks'
      descriptions. The identifier itself always counts. *)
  val aliases : string list

  (** Event kinds the adapter decodes. Several institutions may share a kind
      (every OFX bank stores [bank-transaction]); dispatch is by venue and
      kind. *)
  val kinds : string list

  val decode : Source_event.t -> Movement.t Base.Or_error.t

  (** Format identifiers, recorded in import history. *)
  val formats : string list

  (** The format a file is in, if it is one of the adapter's. A recognizable
      but malformed file is still recognized, so that parsing explains what is
      wrong. *)
  val probe : filename:string -> string -> string option

  (** Whether imports need [--account-id]: the export does not identify its
      account. *)
  val takes_account_label : bool

  val parse : context:context -> format:string -> string -> Document.t Base.Or_error.t
  val overlap : overlap
  val companions : Companion.t list
end
