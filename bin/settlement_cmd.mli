open Base

(** Credit-card settlement review. *)

type settlement_suggestion =
  { transaction : Finances_store.Review.bank_review
  ; card : Finances_store.Transfers.bank_account_info
  ; bank : Finances_store.Transfers.bank_account_info
  }

(** Card spending that settles exactly one card account, in listing order. *)
val settlement_suggestions
  :  Finances_store.Handle.t
  -> settlement_suggestion list Or_error.t

val setup_card
  :  Finances_store.Handle.t
  -> card:Finances_store.Transfers.bank_account_info
  -> bank:Finances_store.Transfers.bank_account_info
  -> yes:bool
  -> allow_yes:bool
  -> bool Or_error.t

val cmd : unit Cmdliner.Cmd.t
