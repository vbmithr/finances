open Base

val builtin_institution_name : Finances_core.Registry.t -> string -> bool
val ensure_generic_source : String.t -> string -> String.t -> unit Or_error.t
