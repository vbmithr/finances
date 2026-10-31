open Base

(** Why a movement has the type and category it has. Plain data built by
    [Store.explain_movement] and printed by [fin rule explain]. *)

type verdict =
  | Decides of Movement_type.t option
  (** The first matching rule decides; [None] leaves the movement untyped. *)
  | Also_matches (** Matches too, but an earlier rule already decided. *)
  | Does_not_match of string

type check =
  { name : string
  ; pattern : string
  ; input : string (** The text the rule's conditions were tried on. *)
  ; verdict : verdict
  }

type source =
  | Manual_override
  | Import_rules
  | Companion_code
  | Untyped

type t =
  { event_id : int64
  ; institution : string
  ; booked_on : string
  ; amount : string
  ; currency : string
  ; account_type : string
  ; descriptions : string list
  ; override : Movement_type.t option
  ; description_checks : check list (** Type rules on the first description. *)
  ; rules_type : Movement_type.t option (** What the two stages above give. *)
  ; companion : (string * Movement_type.t option) list
    (** Codes of matched companion rows and the type each maps to. *)
  ; companion_type : Movement_type.t option
    (** The type those rows agree on, if they agree on one. *)
  ; effective : Movement_type.t option
  ; source : source
  ; stored : Movement_type.t option (** The type saved at import or last refresh. *)
  ; category : (string * string) option (** Recorded account and reason. *)
  }
