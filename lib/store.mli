open Core

(** SQLite is authoritative. One connection must not be shared between threads.
    Opening applies versioned migrations atomically and rejects newer schemas. *)
type t

type ingest =
  | Inserted of int64
  | Existing of int64

val with_open : string -> (t -> 'a Or_error.t) -> 'a Or_error.t

(** Identical retries return [Existing]; changed evidence for the same identity
    returns an error rather than silently overwriting the original. *)
val ingest : t -> Source_event.t -> ingest Or_error.t

val events : t -> (int64 * Source_event.t) list Or_error.t

type bank_import =
  { document_id : int64
  ; statements : int
  ; inserted : int
  ; existing : int
  }

(** Archives the original OFX bytes, imports every statement's transactions, and
    links evidence to the document in one transaction. Overlapping exports reuse
    transaction IDs scoped by institution and bank account. Conflicts roll back
    the complete import, including the document archive. No postings are created. *)
val import_ofx : t -> institution:string -> Bank_ofx.t -> bank_import Or_error.t

(** Import one readable inbox snapshot and record its outcome. Unsupported and
    rejected bytes are archived, too. Successful evidence and its attempt commit
    together; a source conflict rolls back before a separate rejection is saved. *)
val import_bank_file
  :  ?account_id:string
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

type csv_observation =
  { row_id : int64
  ; document_id : int64
  ; institution : string
  ; row : Bank_csv.row
  ; matching : Bank_csv_match.result option
  }

type csv_report =
  { matched : int
  ; unmatched : int
  ; ambiguous : int
  ; conflicts : int
  ; changed : int
  }

(** CSV observations enrich existing OFX movements; they never create events or
    postings. Matching is derived, repeatable, and audited, including rows that
    become ambiguous when additional OFX evidence arrives. *)
val match_bank_csv : t -> csv_report Or_error.t

val bank_csv_observations : t -> csv_observation list Or_error.t
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

val wise_transactions : t -> (int64 * Wise_camt.transaction) list Or_error.t
val match_wise_csv : t -> csv_report Or_error.t

type wise_csv_observation =
  { row_id : int64
  ; document_id : int64
  ; row : Wise_csv.row
  ; matching : Bank_csv_match.result option
  }

val wise_csv_observations : t -> wise_csv_observation list Or_error.t

type wise_reconciliation =
  { statement_id : int64
  ; document_id : int64
  ; source_account : string
  ; currency : string
  ; opening : Bigdecimal.t
  ; closing : Bigdecimal.t
  ; movements : Bigdecimal.t
  ; entries : int
  ; difference : Bigdecimal.t
  }

val wise_reconciliations : t -> wise_reconciliation list Or_error.t

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
  ; transaction : Bank_transaction.t
  ; counter_account : string
  ; reason : string option
  }

val bank_review : t -> bank_review list Or_error.t
val bunq_transactions : t -> (int64 * Bunq_transaction.t) list Or_error.t
val match_bunq_csv : t -> csv_report Or_error.t

type bunq_csv_observation =
  { row_id : int64
  ; document_id : int64
  ; row : Bunq_csv.row
  ; matching : Bank_csv_match.result option
  }

val bunq_csv_observations : t -> bunq_csv_observation list Or_error.t

type bunq_reconciliation =
  { document_id : int64
  ; account_id : string
  ; currency : string
  ; entries : int
  ; difference : Bigdecimal.t
  }

val bunq_reconciliations : t -> bunq_reconciliation list Or_error.t

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
  ; first_transaction : Bank_transaction.t
  ; second_transaction : Bank_transaction.t
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
  ; iban : string option
  ; iban_source : string
  ; currency : string
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
