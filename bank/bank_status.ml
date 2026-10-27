open Base
open Finances
open Or_error.Let_syntax

type t =
  { accounts : Store.bank_account_info list
  ; movements : int
  ; unclassified_movements : int
  ; transfer_candidate_pairs : int
  ; latest_import : Bank_import.file_report option
  }

let read db =
  let%bind accounts, movements, unclassified_movements, transfer_candidate_pairs =
    Store.bank_overview db
  in
  let%bind attempts = Store.bank_import_history ~limit:1 db in
  Ok
    { accounts
    ; movements
    ; unclassified_movements
    ; transfer_candidate_pairs
    ; latest_import = List.hd attempts
    }
;;
