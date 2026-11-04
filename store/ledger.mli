open Base
module Db = Rel_sqlite3
open Handle

val set_institution_definition
  :  ?dry_run:bool
  -> t
  -> name:string
  -> definition:string
  -> expected:string option
  -> expected_rules:Classification_rule.t list
  -> rules:Classification_rule.t list
  -> source_unchanged:bool
  -> int Or_error.t

val add_classification_rule : t -> Classification_rule.t -> unit Or_error.t
val replace_classification_rule : t -> Classification_rule.t -> unit Or_error.t
val delete_classification_rule : t -> string -> unit Or_error.t

val set_classification_rule_mode
  :  t
  -> string
  -> Classification_rule.mode
  -> unit Or_error.t

val rename_classification_rule
  :  t
  -> old_name:string
  -> new_name:string
  -> unit Or_error.t

val events : t -> (int64 * Source_event.t) list Or_error.t

(** Every bank event with its decoded movement, in event order. *)
val movements : t -> (int64 * Source_event.t * Movement.t) list Or_error.t

type companion_observation =
  { row : Companion.row
  ; matching : Bank_match.result option
  }

type companion_report =
  { matched : int
  ; unmatched : int
  ; ambiguous : int
  ; conflicts : int
  ; changed : int
  }

(** Companion observations enrich existing movements; they never create events
    or postings. Each format is matched by its adapter; results are derived,
    repeatable and audited, including rows that become ambiguous when more
    evidence arrives. *)
val match_companions : ?institution:string -> t -> companion_report Or_error.t

val companion_observations
  :  ?institution:string
  -> t
  -> companion_observation list Or_error.t

(** A balance a source stated, by the source's own name for the account. *)
type stated_balance =
  { institution : string
  ; account : string
  ; balance_type : string
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  ; as_of : Temporal.t
  }

(** Oldest first, for one institution or all. *)
val stated_balances : ?institution:string -> t -> stated_balance list Or_error.t

(** Matched companion rows and review counts needed for journal export. *)
type journal_companions =
  { rows : (int64 * Companion.row) list
  ; total : int
  ; review : int
  }

val journal_companions : t -> journal_companions Or_error.t
val bank_accounts : t -> Bank_account_mapping.t list Or_error.t

(** Store mappings and audit changes, renaming only verified existing bank
    projections. Unknown source identities or changed projections roll back all
    mappings and posting renames. Omitted mappings are retained. *)
val set_bank_account_mappings : t -> Bank_account_mapping.t list -> int Or_error.t

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

(** Movement fields needed for journal export, without decoding source payloads. *)
type journal_movement =
  { event_id : int64
  ; value_on : Date.t option
  ; description : string
  }

val journal_movements : t -> journal_movement list Or_error.t

type projection_report =
  { inserted : int
  ; existing : int
  }

(** Project every stored bank event against its reviewed counter-account or
    equity:unclassified by default, atomically.
    Source documents are not needed again. Repeating this operation is harmless;
    a conflict or invalid event rolls back the complete projection batch. *)
val project_bank : t -> projection_report Or_error.t

(** Columns whose effective values can be ordered in SQLite without decoding
    companion data. Event ID is appended as a deterministic tie-breaker. *)
type bank_review_order_column =
  | Review_date
  | Review_currency
  | Review_institution
  | Review_type
  | Review_event

type source_evidence_change =
  { event_id : int64
  ; institution : string
  ; description : string
  ; before : Source_evidence.t
  ; after : Source_evidence.t
  }

val preview_source_evidence
  :  ?institution:string
  -> t
  -> source_evidence_change list Or_error.t

val refresh_source_evidence
  :  ?institution:string
  -> t
  -> expected:source_evidence_change list
  -> int Or_error.t

(** Recompute the type of every movement from the current rules, over its own
    fields and the columns of its matched companion rows, and return how many
    changed. This happens by itself after a rule changes and after companion rows
    match. With [reason], each change is also recorded in the type history. *)
val retype : ?institution:string -> ?reason:string -> t -> int Or_error.t

val linked_transfer
  :  Handle.t
  -> int64
  -> ((int64 * int64 * string) option, Error.t) Result.t

val card_settlement : Handle.t -> int64 -> ((string * string) option, Error.t) Result.t

val linked_reversal
  :  Handle.t
  -> int64
  -> ((int64 * int64 * string) option, Error.t) Result.t

val find_category : int64 -> (string * string) Rel_sql.Stmt.t
val find_entry : int64 -> string -> (int64 * string * string) Rel_sql.Stmt.t
val counter_account : Handle.t -> int64 -> (string Option.t, Error.t) Result.t

val read_entry
  :  Handle.t
  -> int64
  -> string
  -> string
  -> (Accounting.entry, Error.t) Result.t

val mapped_bank_entry
  :  Handle.t
  -> int64
  -> Source_event.t
  -> (Accounting.entry, Error.t) Result.t

val update_posting : string -> int64 -> int -> unit Rel_sql.Stmt.t
val same_entry : Accounting.entry -> Accounting.entry -> bool
val stored : (int64, 'a) Hashtbl.t -> int64 Hashtbl.key -> 'a Or_error.t
val find_mapping : string -> string -> string Rel_sql.Stmt.t
val movement : Handle.t -> int64 -> (Movement.t, Error.t) Result.t
val reversal_account : int64 -> int64 -> string

val events_with_order
  :  ?order:bank_review_order_column Base__List.t * bool
  -> Handle.t
  -> ((int64 * Source_event.t) List.t, Error.t) Result.t

val movement_table : Handle.t -> ((Int64.t, Movement.t) Hashtbl.t, Error.t) Result.t
val category_account : Handle.t -> int64 -> (string Option.t, Error.t) Result.t

val add_classification_rule_in_transaction
  :  Handle.t
  -> Classification_rule.t
  -> (unit, Error.t) Result.t

val companion_columns
  :  ?institution:string
  -> Handle.t
  -> ((Int64.t, (string * string) List.t) Hashtbl.t, Error.t) Result.t

val bank_accounts_from
  :  Handle.t
  -> ('a * Source_event.t) List.t
  -> movement:('a -> (Movement.t, Error.t) Result.t)
  -> Bank_account_mapping.t list Or_error.t

val event_by_id : Handle.t -> int64 -> (Source_event.t, Error.t) Result.t
val movement_rows : Handle.t -> String.t -> ((int64 * Movement.t) list, Error.t) Result.t

val set_bank_account_mappings_in_transaction
  :  Handle.t
  -> Bank_account_mapping.t list
  -> (Base__Int.t, Error.t) Result.t

val companion_rows
  :  Handle.t
  -> institution:string
  -> format:string
  -> Companion.row list Or_error.t

val companion_matches
  :  Handle.t
  -> institution:string
  -> format:string
  -> (Bank_match.result * string) list Or_error.t
