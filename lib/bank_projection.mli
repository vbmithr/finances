open Core

(** Stable bank projection identity. The default counter-account is
    equity:unclassified; reviewed categories explicitly replace it. *)
val key : string

val ledger_account : institution:string -> Bank_ofx.account -> string Or_error.t

val entry
  :  ?bank_account:string
  -> ?counter_account:string
  -> Source_event.t
  -> Accounting.entry Or_error.t

val ledger_account_for
  :  institution:string
  -> Bank_transaction.account
  -> string Or_error.t
