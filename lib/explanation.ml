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
  | Rules
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
  ; checks : check list
    (** The type rules, over the movement's fields and its companion rows' columns. *)
  ; rules_type : Movement_type.t option (** What the rules give, before any override. *)
  ; effective : Movement_type.t option
  ; source : source
  ; category : (string * string) option (** Recorded account and reason. *)
  }
