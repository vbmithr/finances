open Base

(** Testing and explaining rules, and rule coverage gaps. *)

val rule_test_cmd : unit Cmdliner.Cmd.t
val type_name : Finances_core.Movement_type.t Option.t -> string
val rule_explain_cmd : unit Cmdliner.Cmd.t
val rule_gaps_cmd : unit Cmdliner.Cmd.t
