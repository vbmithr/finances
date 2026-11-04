open Base

(** Adding rules. *)

val find_rule
  :  Finances_store.Handle.t
  -> String.t
  -> Finances.Classification_rule.t Or_error.t

val field_arg : settlement:bool -> string option Cmdliner.Term.t
val rule_add_cmd : settlement:bool -> unit Cmdliner.Cmd.t
val rule_add_evidence_cmd : unit Cmdliner.Cmd.t
