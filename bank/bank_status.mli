open Base
open Finances

(** Import and review progress. Counts are movements and candidate pairs, not
    monetary balances. Account date ranges describe observed records only. *)
type t =
  { accounts : Store.bank_account_info list
  ; movements : int
  ; unclassified_movements : int
  ; transfer_candidate_pairs : int
  ; latest_import : Bank_import.file_report option
  }

val empty : t
val read : Store.t -> t Or_error.t
