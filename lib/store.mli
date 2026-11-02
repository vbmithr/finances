open Base

(** SQLite is authoritative. One connection must not be shared between threads.
    Opening initializes the latest experimental schema atomically and rejects
    any other existing schema version. Original records can be reimported. *)
type t

type ingest =
  | Inserted of int64
  | Existing of int64

(** Persist an operation and its movements atomically. Every source must already
    exist; every movement needs a stable identity anchored to one of its sources.
    File-row references are inspection-only and cannot be persisted. *)
val add_operation : t -> Operation.t -> int64 Or_error.t

(** Reload an operation, validating its commodities, quantities and references.
    Confirmed transfers combine both bank operations using the first event's
    operation ID; either component ID retrieves the grouped operation. *)
val get_operation : t -> int64 -> Operation.t Or_error.t

(** Find operations supported by a source event; confirmed bank transfer
    components resolve to the same canonical operation ID. *)
val operations_for_event : t -> int64 -> (int64 * Operation.t) list Or_error.t

type operation_document_preview =
  { rows : int
  ; already_imported : bool
  ; fingerprint : string
  ; problems : string list
  }

(** Inspect a complete source document without writing. Identical rows or
    overlapping records in a different document are reported as ambiguous. *)
val preview_operation_document
  :  t
  -> institution:string
  -> contents:string
  -> rows:Operation_import.row list
  -> operation_document_preview Or_error.t

(** Recheck the preview and atomically archive/import the entire document. *)
val import_operation_document
  :  t
  -> institution:string
  -> contents:string
  -> rows:Operation_import.row list
  -> expected:operation_document_preview
  -> int Or_error.t

(** The registry supplies every bank the store can decode; the store itself
    names none. *)
val with_open : registry:Registry.t -> string -> (t -> 'a Or_error.t) -> 'a Or_error.t

(** [backup source path] writes a consistent copy of the database file [source] to
    [path], a new file, with SQLite's [VACUUM INTO]; other connections may be using
    the database meanwhile. It does not check the schema version, so it works on
    databases this build cannot open. *)
val backup : string -> string -> unit Or_error.t

(** Restore a SQLite backup after checking its schema, integrity and foreign
    keys. The source is opened read-only. SQLite replaces the destination in
    one write transaction, preserving it on copy failure and managing its
    sidecars. A missing destination and its parents are created privately.
    Source and destination must be different files. *)
val restore : source:string -> destination:string -> unit Or_error.t

(** Immutable contexts attached to accepted documents for an institution. *)
val document_contexts : t -> institution:string -> string list Or_error.t

val registry : t -> Registry.t

(** User-added institutions using generic CSV layouts. *)
val institutions : t -> string list Or_error.t

val institution_definitions : t -> (string * string) list Or_error.t

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

val institution_companion_profiles : t -> (string * string) list Or_error.t
val institution_companion_profile : t -> string -> string option Or_error.t

val set_institution_companion_profile
  :  ?dry_run:bool
  -> t
  -> string
  -> expected:string option
  -> string
  -> unit Or_error.t

val clear_institution_companion_profile
  :  ?dry_run:bool
  -> t
  -> string
  -> expected:string
  -> unit Or_error.t

val add_institution : ?profile:string -> t -> string -> unit Or_error.t
val institution_profile : t -> string -> string option Or_error.t
val set_institution_profile_if_missing : t -> string -> string -> unit Or_error.t

(** Delete a user-added institution only when it has no imported movements. *)
val delete_institution : ?dry_run:bool -> t -> string -> unit Or_error.t

(** Saved, explicitly reviewed regular-expression rules. *)
val classification_rules : t -> Classification_rule.t list Or_error.t

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

(** Identical retries return [Existing]; changed evidence for the same identity
    returns an error rather than silently overwriting the original. *)
val ingest : t -> Source_event.t -> ingest Or_error.t

val events : t -> (int64 * Source_event.t) list Or_error.t

(** Every bank event with its decoded movement, in event order. *)
val movements : t -> (int64 * Source_event.t * Movement.t) list Or_error.t

(** Import one readable inbox snapshot and record its outcome. The institution's
    adapter recognizes and parses the file; a file another bank recognizes is
    rejected with a pointer to that institution. Unsupported and rejected bytes
    are archived, too. Successful evidence and its attempt commit together; a
    source conflict rolls back before a separate rejection is saved. *)
val import_bank_file
  :  ?account_label:string
  -> t
  -> institution:string
  -> path:string
  -> contents:string
  -> Bank_import.file_report Or_error.t

val record_bank_read_error
  :  t
  -> institution:string
  -> path:string
  -> message:string
  -> Bank_import.file_report Or_error.t

val bank_import_history : ?limit:int -> t -> Bank_import.file_report list Or_error.t

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

(** Personal income and expense category accounts, including unused names. *)
val categories : t -> string list Or_error.t

val add_categories : t -> string list -> unit Or_error.t

(** Remove an unused catalog entry. Current transaction decisions and category
    rules must be changed first; historical decisions are retained. *)
val delete_category : t -> string -> unit Or_error.t

type category_restore_preview =
  { categories_to_add : string list
  ; rules_to_add : Classification_rule.t list
  ; decisions_to_apply : Bank_category.t list
  ; types_to_apply : (int64 * Movement_type.t) list
  ; unchanged : int
  ; problems : string list
  }

val category_snapshot : t -> Category_portable.t Or_error.t

val preview_category_restore
  :  t
  -> Category_portable.t
  -> category_restore_preview Or_error.t

val restore_categories
  :  t
  -> expected:category_restore_preview
  -> Category_portable.t
  -> int Or_error.t

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

(** Columns whose effective values can be ordered in SQLite without decoding
    companion data. Event ID is appended as a deterministic tie-breaker. *)
type bank_review_order_column =
  | Review_date
  | Review_currency
  | Review_institution
  | Review_type
  | Review_event

val bank_review_ordered
  :  t
  -> columns:bank_review_order_column list
  -> ascending:bool
  -> bank_review list Or_error.t

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

type reversal_candidate =
  { original : bank_review
  ; reversal : bank_review
  }

val reversal_candidates : t -> reversal_candidate list Or_error.t

val reversal_change
  :  t
  -> original:int64
  -> reversal:int64
  -> undo:bool
  -> unit Or_error.t

type reversal_action =
  | Reversal_confirmed
  | Reversal_undone

type reversal_history_entry =
  { id : int64
  ; original : int64
  ; reversal : int64
  ; action : reversal_action
  ; changed_at : Ptime.t
  }

val reversal_history : t -> reversal_history_entry list Or_error.t

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

(** Explicit same-currency transfer decisions; a confirmation asserts ownership
    of both accounts. Each movement can have at most one confirmed counterpart.
    Rejection of a confirmed pair restores its prior category. All projection
    changes and audit rows commit atomically after verifying stored entries. *)
val set_bank_transfers
  :  ?require_unreviewed:bool
  -> ?expected_previous:Bank_transfer.decision
  -> t
  -> Bank_transfer.decision list
  -> int Or_error.t

val bank_transfer_decisions : t -> Bank_transfer.decision list Or_error.t

type transfer_change =
  { id : int64
  ; first_event_id : int64
  ; second_event_id : int64
  ; previous_state : string option
  ; state : Bank_transfer.state
  ; reason : string
  ; changed_at : Ptime.t
  }

val bank_transfer_history : t -> transfer_change list Or_error.t

type transfer_candidate =
  { first_event_id : int64
  ; second_event_id : int64
  ; first : Source_event.t
  ; second : Source_event.t
  ; first_transaction : Movement.t
  ; second_transaction : Movement.t
  ; days_apart : int
  ; state : Bank_transfer.state option
  ; reason : string option
  ; unique : bool
  ; iban_compatible : bool
  ; iban_match : bool
  ; iban_evidence : string list
  }

val transfer_candidates
  :  ?include_reviewed:bool
  -> t
  -> transfer_candidate list Or_error.t

type bank_account_info =
  { mapping : Bank_account_mapping.t
  ; iban : Iban.t option
  ; iban_source : string
  ; currency : Iso4217.t
  ; movements : int
  ; first_on : Date.t
  ; last_on : Date.t
  ; settlement_account : Bank_account_config.reference option
  }

val bank_account_info : t -> bank_account_info list Or_error.t

(** Preview and confirm a single bank debit against its linked credit card. *)
val card_settlement_preview
  :  ?card_id:string
  -> t
  -> int64
  -> (bank_review * bank_account_info) Or_error.t

val confirm_card_settlement
  :  ?card_id:string
  -> ?expected_ledger:string
  -> t
  -> int64
  -> unit Or_error.t

(** Apply a reviewed card setup and settlement in one transaction, checking the
    setup still matches the preview before changing anything. *)
val confirm_card_settlement_with_setup
  :  t
  -> event_id:int64
  -> card_id:string
  -> expected_ledger:string
  -> expected_link:Bank_account_config.reference option
  -> setup:Bank_account_config.t
  -> unit Or_error.t

val undo_card_settlement : t -> int64 -> unit Or_error.t

type card_settlement_action =
  | Confirmed
  | Undone

type card_settlement_change =
  { id : int64
  ; event_id : int64
  ; card_institution : string
  ; card_source_account : string
  ; ledger_account : string
  ; action : card_settlement_action
  ; changed_at : Ptime.t
  }

val card_settlement_history : t -> card_settlement_change list Or_error.t

type classification_rule_match =
  { transaction : bank_review
  ; eligible : bool
  ; apply_type : bool
  ; apply_account : bool
  ; settlement_ledger : string option
  ; explanation : string option
  }

val preview_classification_rule
  :  ?force:bool
  -> t
  -> Classification_rule.t
  -> classification_rule_match list Or_error.t

(** Recheck the saved rule and every selected match, then apply atomically. *)
val apply_classification_rule
  :  ?force:bool
  -> t
  -> expected:Classification_rule.t
  -> selected:classification_rule_match list
  -> int Or_error.t

(** Account information, movement count, unclassified count and candidate count.
    SQL computes counts and date ranges; only candidate legs and representative
    account movements are decoded. Nothing is cached across calls. *)
val bank_overview : t -> (bank_account_info list * int * int * int) Or_error.t

(** Audited registration; rejects contradictions with imported identifiers or
    confirmed transfers. Null template rows are ignored by Account_identity. *)
val set_bank_account_identities : t -> Account_identity.t list -> int Or_error.t

type account_change =
  { id : int64
  ; kind : string
  ; institution : string
  ; source_account : string
  ; previous_value : string option
  ; value : string option
  ; changed_at : Ptime.t
  }

type account_config_report =
  { mapping_changes : int
  ; identity_changes : int
  ; settlement_changes : int
  ; changes : account_change list
  }

val export_bank_accounts : t -> Bank_account_config.t list Or_error.t

val apply_bank_accounts
  :  ?dry_run:bool
  -> t
  -> Bank_account_config.t list
  -> account_config_report Or_error.t

val bank_account_history : t -> account_change list Or_error.t

(** Delete one account's exclusively owned records atomically. Refuses shared
    documents, transfer decisions and references from another account. *)
type account_deletion =
  { transactions : int
  ; documents : int
  }

val delete_account
  :  ?dry_run:bool
  -> t
  -> Bank_account_mapping.t
  -> account_deletion Or_error.t

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

(** Recompute the type of every movement from the current rules, over its own
    fields and the columns of its matched companion rows, and return how many
    changed. This happens by itself after a rule changes and after companion rows
    match. With [reason], each change is also recorded in the type history. *)
val retype : ?institution:string -> ?reason:string -> t -> int Or_error.t
