open Base

(** Credit-card settlement review. *)

val setup_card
  :  Finances_store.Handle.t
  -> card:Finances_store.Transfers.bank_account_info
  -> bank:Finances_store.Transfers.bank_account_info
  -> yes:bool
  -> allow_yes:bool
  -> bool Or_error.t

val cmd : unit Cmdliner.Cmd.t
