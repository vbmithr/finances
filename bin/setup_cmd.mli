open Base

(** Creating, saving, restoring and deleting fin's state. *)

val init : unit Cmdliner.Cmd.t
val backup_cmd : unit Cmdliner.Cmd.t
val restore_cmd : unit Cmdliner.Cmd.t
val delete_cmd : unit Cmdliner.Cmd.t
val welcome : unit -> unit
val setup_cmd : unit Cmdliner.Cmd.t
val paths_cmd : unit Cmdliner.Cmd.t
