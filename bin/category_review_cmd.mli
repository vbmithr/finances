open Base

(** Reviewing and setting operation categories. *)

val bank_event_ids_for_operations
  :  ?source_event_id:Int64.t
  -> Finances_store.Handle.t
  -> Int64.t List.t
  -> Int64.t list Or_error.t

val category_set_cmd : unit Cmdliner.Cmd.t
val categorize_bank_cmd : unit Cmdliner.Cmd.t
val category_similar_cmd : unit Cmdliner.Cmd.t
val category_queue_cmd : unit Cmdliner.Cmd.t
val category_undo_cmd : unit Cmdliner.Cmd.t
val category_review_cmd : unit Cmdliner.Cmd.t
val category_history_cmd : unit Cmdliner.Cmd.t
