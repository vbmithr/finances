open Base

(** Listing operations. *)

type movement_type_filter =
  | Unknown_type
  | Known_type of Finances_core.Movement_type.t

val parse_movement_type_filter : String.t -> movement_type_filter Or_error.t

val matches_movement_type_filter
  :  movement_type_filter
  -> Finances_core.Movement_type.t Option.t
  -> bool

val transaction_list_cmd : unit Cmdliner.Cmd.t
