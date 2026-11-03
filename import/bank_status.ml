open Base
open Finances
open Finances_store
open Or_error.Let_syntax

type t =
  { accounts : Transfers.bank_account_info list
  ; movements : int
  ; unclassified_movements : int
  ; transfer_candidate_pairs : int
  ; latest_import : Bank_import.file_report option
  }

let read db =
  let%bind accounts, movements, unclassified_movements, transfer_candidate_pairs =
    Accounts.bank_overview db
  in
  let%bind attempts = Import.bank_import_history ~limit:1 db in
  Ok
    { accounts
    ; movements
    ; unclassified_movements
    ; transfer_candidate_pairs
    ; latest_import = List.hd attempts
    }
;;
