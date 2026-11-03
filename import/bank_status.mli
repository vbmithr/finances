open Base
open Finances
open Finances_store

(** Import and review progress. Counts are movements and candidate pairs, not
    monetary balances. Account date ranges describe observed records only. *)
type t =
  { accounts : Transfers.bank_account_info list
  ; movements : int
  ; unclassified_movements : int
  ; transfer_candidate_pairs : int
  ; operations : (string * Explain.operation_summary) list
    (** Institutions whose rows are operations with no bank account. *)
  ; latest_import : Bank_import.file_report option
  }

val read : Handle.t -> t Or_error.t
