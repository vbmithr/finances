(** Saving and re-applying everything the user decided. *)

val decisions_json : Finances_store.Handle.t -> string Base.Or_error.t
val decisions_cmd : unit Cmdliner.Cmd.t
