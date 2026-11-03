open Base
module Db = Rel_sqlite3
open Handle

(** What a draft import-time type rule would do to one movement it matches. *)
type type_rule_outcome =
  | Newly_typed
  | Changed
  | Unchanged
  | Shadowed of string
  (** A rule that runs earlier already decides the type; it is named. *)
  | Overridden (** A manual type override takes precedence over every rule. *)

type type_rule_row =
  { event_id : int64
  ; institution : string
  ; description : string
  ; before : Movement_type.t option
  ; after : Movement_type.t option
  ; outcome : type_rule_outcome
  }

type type_rule_test =
  { scanned : int (** Movements in scope: the rule's institution, or all. *)
  ; matches : type_rule_row list
  ; untyped_before : int (** Movements no rule or override types, as saved. *)
  ; untyped_after : int (** The same with the draft added. *)
  }

(** Evaluate a draft type rule as an automatic import-time rule, against every
    stored movement, without saving anything. The draft takes the place of a
    saved rule with the same name. Only rules that set a movement type can be
    tested. *)
val test_type_rule : t -> Classification_rule.t -> type_rule_test Or_error.t

(** Movements that have no type, grouped by the first words of their first
    description. *)
type description_gap =
  { institution : string
  ; key : string
  ; count : int
  ; first_on : Date.t
  ; last_on : Date.t
  ; debits : int
  ; credits : int
  ; example : string
  ; pattern : string (** {!Description_key.pattern} of the key. *)
  }

val description_gaps : ?institution:string -> t -> description_gap list Or_error.t

(** Imported movements of one institution: how many, over what dates, and by
    effective type (a manual override, else the primary's type, else the type the
    matched companion rows agree on; [None] is untyped). It reads stored rows with
    three queries and decodes no payloads, so it is cheap on a large database. *)
type institution_summary =
  { movements : int
  ; first_on : string option
  ; last_on : string option
  ; by_type : (Movement_type.t option * int) list (** Largest first. *)
  }

val institution_summary : t -> string -> institution_summary Or_error.t

(** Trace how a movement got its type, step by step, and report its recorded
    category. Reads one movement; nothing is changed. *)
val explain_movement : t -> int64 -> Explanation.t Or_error.t

(** Everything stored about one movement: its source event and payload, the
    documents and files it was imported from, statements, matched companion rows
    with every column, the type trace, and category and transfer decisions.
    Nothing is changed. *)
val show_movement : t -> int64 -> Transaction_detail.t Or_error.t
