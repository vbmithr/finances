open Base
module Db = Rel_sqlite3
open Handle

(** Persist an operation and its movements atomically. Every source must already
    exist; every movement needs a stable identity anchored to one of its sources.
    File-row references are inspection-only and cannot be persisted. *)
val add_operation : t -> Operation.t -> int64 Or_error.t

(** The operations that read as one with this one, as its group's canonical ID and its
    members, canonical first: a confirmed transfer link joins two, and an institution's
    records may state a group of any size, which a link outranks. *)
val operation_group : t -> int64 -> (int64 * int64 list) Or_error.t

(** Reload an operation, validating its commodities, quantities and references.
    Confirmed transfers combine both bank operations using the first event's
    operation ID; either component ID retrieves the grouped operation. *)
val get_operation : t -> int64 -> Operation.t Or_error.t

(** Find operations supported by a source event; confirmed bank transfer
    components resolve to the same canonical operation ID. *)
val operations_for_event : t -> int64 -> (int64 * Operation.t) list Or_error.t

type general_transfer_leg =
  { operation_id : int64
  ; movement_id : int64
  ; event_id : int64
  ; role : string
  ; account : string
  ; commodity : Commodity.t
  ; symbol : string
  ; quantity : Quantity.t
  ; date : Date.t
  ; bank : bool
  }

type general_transfer_preview =
  { first : general_transfer_leg
  ; second : general_transfer_leg
  ; entry : Accounting.entry
  }

val preview_general_transfer
  :  t
  -> first_operation:int64
  -> first_role:string
  -> second_operation:int64
  -> second_role:string
  -> general_transfer_preview Or_error.t

val general_transfer_entries : t -> general_transfer_preview list Or_error.t
val confirm_general_transfer : t -> expected:general_transfer_preview -> unit Or_error.t
val undo_general_transfer : t -> int64 -> unit Or_error.t
val operation_exchange_preview : t -> int64 -> (int64 * Accounting.entry) Or_error.t

val set_operation_exchange
  :  t
  -> operation_id:int64
  -> confirm:bool
  -> expected:(int64 * Accounting.entry) option
  -> bool Or_error.t

val operation_exchange_entries : t -> (int64 * string * Accounting.entry) list Or_error.t

type exchange_restore_preview =
  { exchange_decisions : (int64 * (int64 * Accounting.entry)) list
  ; exchange_unchanged : int
  ; exchange_problems : string list
  }

val exchange_snapshot : t -> Exchange_portable.t Or_error.t

val preview_exchange_restore
  :  t
  -> Exchange_portable.t
  -> exchange_restore_preview Or_error.t

val restore_exchanges
  :  t
  -> expected:exchange_restore_preview
  -> Exchange_portable.t
  -> int Or_error.t

type portable_transfer_preview =
  { decisions : Bank_transfer.decision list
  ; general_decisions : general_transfer_preview list
  ; unchanged : int
  ; problems : string list
  }

val transfer_snapshot : t -> Transfer_portable.t Or_error.t

val preview_transfer_restore
  :  t
  -> Transfer_portable.t
  -> portable_transfer_preview Or_error.t

val restore_transfers
  :  t
  -> expected:portable_transfer_preview
  -> Transfer_portable.t
  -> int Or_error.t
