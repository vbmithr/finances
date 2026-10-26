open Core

(** Initial accounting policy: bank movement against equity:unclassified.
    This deliberately makes no income, expense, or transfer classification. *)
val key : string

val ledger_account : institution:string -> Bank_ofx.account -> string Or_error.t
val entry : Source_event.t -> Accounting.entry Or_error.t
