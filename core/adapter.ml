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
end
