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
  let%bind accounts = Store.bank_account_info db in
  let%bind movements = Store.bank_review db in
  let%bind candidates = Store.transfer_candidates db in
  let%bind attempts = Store.bank_import_history ~limit:1 db in
  Ok
    { accounts
    ; movements = List.length movements
    ; unclassified_movements =
        List.count movements ~f:(fun r ->
          String.equal r.Store.counter_account "equity:unclassified")
    ; transfer_candidate_pairs = List.length candidates
    ; latest_import = List.hd attempts
    }
;;
