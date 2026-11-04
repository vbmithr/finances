open Base

(** Interactive setup of a CSV layout and of new accounts during import. *)

val import_account_label
  :  Finances_core.Registry.t
  -> string
  -> String.t Option.t
  -> (String.t Option.t, Error.t) Result.t

val configure_new_accounts
  :  Finances_store.Handle.t
  -> Finances_store.Transfers.bank_account_info List.t
  -> Int.t Or_error.t

val csv_import_profile
  :  ?setup_only:bool
  -> String.t
  -> string
  -> Int.t
  -> institution:string
  -> Finances_institutions.Csv_profile.t Or_error.t

val builtin_institution_name : Finances_core.Registry.t -> string -> bool
val ensure_generic_source : String.t -> string -> String.t -> unit Or_error.t
