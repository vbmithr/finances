open Base
module Db = Rel_sqlite3
open Handle

type category_restore_preview =
  { categories_to_add : string list
  ; rules_to_add : Classification_rule.t list
  ; decisions_to_apply : Bank_category.t list
  ; types_to_apply : (int64 * Movement_type.t) list
  ; resolutions_to_apply : (int64 * string * string * Commodity.t * string) list
  ; unchanged : int
  ; problems : string list
  }

val category_snapshot : t -> Category_portable.t Or_error.t

val preview_category_restore
  :  t
  -> Category_portable.t
  -> category_restore_preview Or_error.t

(** Applies a preview made in the same transaction, without making it again. *)
val restore_categories : t -> category_restore_preview -> int Or_error.t

val observed_provider_movement_types : t -> string list Or_error.t

val set_movement_type_overrides
  :  t
  -> event_ids:int64 list
  -> movement_type:Movement_type.t option
  -> int Or_error.t

val set_movement_type_override
  :  t
  -> event_id:int64
  -> movement_type:Movement_type.t option
  -> bool Or_error.t

type movement_type_history_entry =
  { id : int64
  ; event_id : int64
  ; previous_type : Movement_type.t option
  ; movement_type : Movement_type.t option
  ; reason : string
  ; changed_at : Ptime.t
  }

val movement_type_history : t -> movement_type_history_entry list Or_error.t
val bank_transfer_decisions : t -> Bank_transfer.decision list Or_error.t

val verify_bank_entry
  :  Handle.t
  -> int64
  -> Source_event.t
  -> (int64 option, Error.t) Result.t

val find_transfer_decision
  :  int64
  -> int64
  -> (int64 * int64 * string * string) Rel_sql.Stmt.t

(** Every account's IBAN, for [transfer_iban_evidence]. It visits every movement: read it
    once per batch. *)
val known_ibans
  :  Handle.t
  -> (Source_event.t * Movement.account * Iban.t option) list Or_error.t

val transfer_iban_evidence
  :  Handle.t
  -> (Source_event.t * Movement.account * Iban.t option) list
  -> Source_event.t
  -> Movement.t
  -> Source_event.t
  -> Movement.t
  -> (bool * bool * String.t List.t, Error.t) Result.t

val known_account_ibans
  :  Handle.t
  -> ('a * Source_event.t) List.t
  -> movement:('a -> (Movement.t, Error.t) Result.t)
  -> (Source_event.t * Movement.account * Iban.t option) list Or_error.t

val write_movement_type_override
  :  Handle.t
  -> event_id:int64
  -> previous:Movement_type.t Option.t
  -> movement_type:Movement_type.t Option.t
  -> reason:string
  -> (bool, Error.t) Result.t

val account_iban
  :  Handle.t
  -> Source_event.t
  -> Movement.t
  -> (Iban.t option * string, Error.t) Result.t

val transfer_iban_evidence_with
  :  (Source_event.t * Movement.account * Iban.t Option.t) List.t
  -> a_iban:Iban.t option
  -> b_iban:Iban.t option
  -> Source_event.t
  -> Movement.t
  -> Source_event.t
  -> Movement.t
  -> (bool * bool * String.t List.t, 'a) Result.t

val own_iban
  :  Handle.t
  -> (int64 * Source_event.t) List.t
  -> Bank_account_mapping.t
  -> Movement.own_iban Or_error.t

val find_account_iban : string -> string -> string Rel_sql.Stmt.t

val bank_operation_anchors
  :  Handle.t
  -> ( (Int64.t, string * string) Hashtbl.t * (Int64.t, Int64.comparator_witness) Set.t
       , Error.t )
       Result.t

val verify_bank_operation_anchor
  :  (int64, string * string) Hashtbl.t * (int64, 'a) Set.t
  -> Review.bank_review
  -> unit Or_error.t

(** A source event's stored digests ([Event_digests.load]), by event ID. *)
val category_fields_hash : (int64, Event_digests.t) Hashtbl.t -> int64 -> string

(** A digest that says a saved decision still names this row. For a records row it covers only what
    identifies the row, so that it survives a converter keeping more detail. *)
val payload_hash : (int64, Event_digests.t) Hashtbl.t -> int64 -> string

(** How many rows of this event's place and time share its [evidence_key], counted in SQL. *)
val evidence_count
  :  Handle.t
  -> (int64, Event_digests.t) Hashtbl.t
  -> int64
  -> Source_event.t
  -> int Or_error.t

(** What tells this row from others of its day, for matching saved decisions. *)
val evidence_key
  :  Handle.t
  -> (int64, Event_digests.t) Hashtbl.t
  -> int64
  -> Source_event.t
  -> string
