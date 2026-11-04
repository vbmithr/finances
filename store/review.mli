open Base
module Db = Rel_sqlite3
open Handle
open Ledger

(** A statement's stated balances against the movements it lists. Period boundaries are
    typed calendar dates or Ptime instants. *)
type statement_reconciliation =
  { statement_id : int64
  ; document_id : int64
  ; institution : string
  ; source_account : string
  ; reference : string option
  ; currency : Iso4217.t
  ; period_start : Temporal.t
  ; period_end : Temporal.t
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; movements : Quantity.t
  ; entries : int
  ; difference : Quantity.t
  }

val statement_reconciliations
  :  ?institution:string
  -> t
  -> statement_reconciliation list Or_error.t

(** Atomically apply explicit category decisions and audit their reasons. Verify
    existing generated postings before changing only the counter-account.
    Replays are idempotent. New decisions also apply before first projection.
    When [only_unclassified] is set, abort the batch if any selected movement
    already has a classification, checking inside the transaction. *)
val set_bank_categories
  :  ?only_unclassified:bool
  -> t
  -> Bank_category.t list
  -> int Or_error.t

(** Atomically add catalog entries and apply reviewed decisions only if each
    prior account and reason still matches the snapshot. *)
val review_bank_categories
  :  ?categories:string list
  -> t
  -> expected:(int64 * (string * string) option) list
  -> Bank_category.t list
  -> int Or_error.t

type category_change =
  { id : int64
  ; event_id : int64
  ; previous_account : string option
  ; account : string
  ; reason : string
  ; changed_at : Ptime.t
  }

val bank_category_history : t -> category_change list Or_error.t

type review_classification =
  | Unclassified
  | Category
  | Transfer
  | Settlement
  | Reversal

type transaction_status =
  { total : int
  ; categorized : int
  ; transfers : int
  ; settlements : int
  ; reversals : int
  ; unclassified : int
  ; by_category : (string * int) list
  ; by_type : (string * int) list
  }

(** Summarize stored bank movements with SQL aggregation, including effective
    movement types and accounting decision precedence. *)
val transaction_status : t -> transaction_status Or_error.t

type bank_review =
  { event_id : int64
  ; institution : string
  ; occurred : Temporal.t
  ; transaction : Movement.t
  ; description : string (** Effective description, selected from available sources. *)
  ; movement_type : Movement_type.t option
  ; imported_movement_type : Movement_type.t option
  ; movement_type_override : Movement_type.t option
  ; source_evidence : Source_evidence.t
  ; detail_sources : int64 list
  ; classification : review_classification
  ; counter_account : string
  ; reason : string option
  }

(** Choose a consistent nonempty companion display name, or the movement's
    primary description when companion names are absent or disagree. *)
val effective_description : string -> Companion.annotation list -> string

val bank_review : t -> bank_review list Or_error.t

val bank_review_ordered
  :  t
  -> columns:bank_review_order_column list
  -> ascending:bool
  -> bank_review list Or_error.t

(** One stored operation with the review row of each of its movements. A bank
    movement is a one-movement operation and carries its bank review row; any
    other movement carries only its identity, account, commodity and quantity.
    A confirmed bank transfer is one operation with two movements. [review] is
    the primary movement's bank review row, when the operation has one. *)
type operation_movement_review =
  { role : string
  ; account : string option
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  ; anchor_event_id : int64
  ; review : bank_review option
  }

type operation_review =
  { operation_id : int64
  ; occurred : Temporal.t
  ; institution : string
  ; description : string
    (** The primary movement's review description, or the
                            provider's own description column when there is none. *)
  ; reported_amount : string option
  ; reported_currency : string option
  ; exchange_reviewed : bool
  ; review : bank_review option
  ; movements : operation_movement_review list
  }

(** A persisted reviewed transfer expressed using stable movement identities. *)
type operation_transfer_link =
  { first_operation_id : int64
  ; second_operation_id : int64
  ; first_event_id : int64
  ; second_event_id : int64
  ; first_movement : Movement_identity.t
  ; second_movement : Movement_identity.t
  ; reason : string
  }

val operation_transfer_links : t -> operation_transfer_link list Or_error.t

(** Which stored operations read as one: confirmed transfer links, and the groups
    institutions state through their adapters. *)
val operation_groups : t -> Operation_groups.t Or_error.t

(** The operation carrying each bank movement, from its primary movement anchor.
    Unknown events are omitted. *)
val operation_ids_for_events : t -> int64 list -> (int64 * int64) list Or_error.t

(** Every stored operation, ordered by occurrence then operation ID. A bank
    movement appears exactly once, as the movement of its one-movement
    operation; a confirmed transfer appears once, with both movements. *)
val operation_review : t -> operation_review list Or_error.t

type operation_type_filter =
  | Untyped
  | Typed of Movement_type.t

(** Each is a condition on the operation's group: [only_account] names a movement's
    institution and source account. *)
type operation_filter =
  { only_account : (string * string) option
  ; only_institution : string option
  ; only_type : operation_type_filter option
  ; only_decision : review_classification option
  ; only_counter_account : string option
  ; from_date : Date.t option
  ; to_date : Date.t option
  ; search : string option
  }

val no_operation_filter : operation_filter

type operation_order =
  | By_id
  | By_occurrence
  | By_date
  | By_amount
  | By_currency
  | By_institution
  | By_account
  | By_type
  | By_counter_account
  | By_description
  | By_event

(** The operations a filter selects, in SQL, ending with the operation ID in the same
    direction. Only the rows returned are read in full. [accounts] gives, as institution,
    source account and ID, what [By_account] sorts on. *)
val operations
  :  ?filter:operation_filter
  -> ?accounts:(string * string * string) list
  -> ?order:operation_order list
  -> ?ascending:bool
  -> ?limit:int
  -> t
  -> operation_review list Or_error.t

val resolve_operation_movement
  :  t
  -> operation_id:int64
  -> role:string
  -> account:string
  -> commodity:Commodity.t
  -> symbol:string
  -> bool Or_error.t

val validate_commodity_symbol : Commodity.t -> String.t -> (String.t, Error.t) Result.t

val set_bank_categories_in_transaction
  :  ?only_unclassified:bool
  -> Handle.t
  -> Bank_category.t List.t
  -> (Base__Int.t, Error.t) Result.t

val resolve_operation_movement_internal
  :  ?in_transaction:bool
  -> Handle.t
  -> operation_id:int64
  -> role:String.t
  -> account:String.t
  -> commodity:Commodity.t
  -> symbol:String.t
  -> (bool, Error.t) Result.t

val movement_source : Handle.t -> ((int64 * Source_event.t) List.t, Error.t) Result.t

val review_metadata
  :  Handle.t
  -> ( (Int64.t, string * string) Hashtbl.t
       * (Int64.t, int64 * int64 * string * string option) Hashtbl.t
       * (Int64.t, string * string) Hashtbl.t
       * (Int64.t, int64 * int64 * string) Hashtbl.t
       , Error.t )
       Result.t

val operation_fields : Handle.t -> int64 -> ((string * string) List.t, Error.t) Result.t
