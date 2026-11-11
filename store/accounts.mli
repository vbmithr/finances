open Base
module Db = Rel_sqlite3
open Handle
open Review
open Transfers

val registry : t -> Registry.t

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

(** Delete a nonempty batch atomically, refusing duplicate accounts and the
    same shared evidence as [delete_account]. Results follow input order. *)
val delete_accounts
  :  ?dry_run:bool
  -> t
  -> Bank_account_mapping.t list
  -> account_deletion list Or_error.t
