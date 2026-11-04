open Base

(** Listing operations. *)

val parse_movement_type_filter
  :  String.t
  -> Finances_store.Review.operation_type_filter Or_error.t

val matches_movement_type_filter
  :  Finances_store.Review.operation_type_filter
  -> Finances_core.Movement_type.t Option.t
  -> bool

val transaction_list_cmd : unit Cmdliner.Cmd.t
