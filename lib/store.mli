open Base

(** SQLite is authoritative. One connection must not be shared between threads.
    Opening initializes the latest experimental schema atomically and rejects
    any other existing schema version. Original records can be reimported. *)
type t

type ingest =
  | Inserted of int64
  | Existing of int64

(** The registry supplies every bank the store can decode; the store itself
    names none. *)
val with_open : registry:Registry.t -> string -> (t -> 'a Or_error.t) -> 'a Or_error.t

(** Writes a consistent copy of the database to [path], a new file, with SQLite's
    [VACUUM INTO]; other connections may be using the database meanwhile. *)
val backup : t -> string -> unit Or_error.t

val registry : t -> Registry.t

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

type projection_report =
  { inserted : int
  ; existing : int
  }

(** Project every stored bank event against its reviewed counter-account or
    equity:unclassified by default, atomically.
    Source documents are not needed again. Repeating this operation is harmless;
    a conflict or invalid event rolls back the complete projection batch. *)
val project_bank : t -> projection_report Or_error.t

(** A statement's stated balances against the movements it lists. Period and
    balance dates are the bank's text: a date or a timestamp with offset. *)
type statement_reconciliation =
  { statement_id : int64
  ; document_id : int64
  ; institution : string
  ; source_account : string
  ; reference : string option
  ; currency : Iso4217.t
  ; period_start : string
  ; period_end : string
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
    Replays are idempotent. New decisions also apply before first projection. *)
val set_bank_categories : t -> Bank_category.t list -> int Or_error.t

type category_change =
  { id : int64
  ; event_id : int64
  ; previous_account : string option
  ; account : string
  ; reason : string
  ; changed_at : string
  }

val bank_category_history : t -> category_change list Or_error.t

type bank_review =
  { event_id : int64
  ; institution : string
  ; transaction : Movement.t
  ; counter_account : string
  ; reason : string option
  }

val bank_review : t -> bank_review list Or_error.t

(** Explicit same-currency transfer decisions; a confirmation asserts ownership
    of both accounts. Each movement can have at most one confirmed counterpart.
    Rejection of a confirmed pair restores its prior category. All projection
    changes and audit rows commit atomically after verifying stored entries. *)
val set_bank_transfers : t -> Bank_transfer.decision list -> int Or_error.t

val bank_transfer_decisions : t -> Bank_transfer.decision list Or_error.t

type transfer_change =
  { id : int64
  ; first_event_id : int64
  ; second_event_id : int64
  ; previous_state : string option
  ; state : Bank_transfer.state
  ; reason : string
  ; changed_at : string
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
  ; changed_at : string
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
