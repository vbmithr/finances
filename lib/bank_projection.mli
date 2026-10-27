open Base

(** Stable bank projection identity. The default counter-account is
    equity:unclassified; reviewed categories explicitly replace it. *)
val key : string

val ledger_account : institution:string -> Movement.account -> string Or_error.t

val entry
  :  ?bank_account:string
  -> ?counter_account:string
  -> institution:string
  -> Movement.t
  -> Accounting.entry Or_error.t
