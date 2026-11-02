open Base

(** An immutable reference to evidence. File rows identify locations for
    inspection only; they are not stable cross-import transaction identities. *)
type evidence =
  | Stored_event of int64
  | File_row of
      { document : string
      ; row : int
      }

type movement = private
  { identity : Movement_identity.t option
  ; account : string option
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  ; evidence : evidence list
  }

type t = private
  { occurred : Temporal.t
  ; evidence : evidence list
  ; movements : movement list
  }

(** A missing account is a proposed effect, not a reconciled balance. A zero
    quantity preserves a reported movement with no balance effect. *)
val movement
  :  ?identity:Movement_identity.t
  -> ?account:string
  -> commodity:Commodity.t
  -> quantity:Quantity.t
  -> evidence:evidence list
  -> unit
  -> movement Or_error.t

(** Every movement must be supported by evidence linked to the operation.
    An operation may have no movements when its meaning is still unknown. *)
val create
  :  occurred:Temporal.t
  -> evidence:evidence list
  -> movements:movement list
  -> t Or_error.t
