open Base

(** What every command shares: the database option, confirmation, file access. *)

val prompt : string -> string option
val confirm : yes:bool -> destructive:bool -> string -> bool Or_error.t

(** [confirm] when standard input is in use, such as by a pipe: asks on the terminal, and Enter
    declines. *)
val confirm_on_terminal : yes:bool -> string -> bool Or_error.t

val paths : Finances.App_paths.t
val require_database : String.t -> unit Or_error.t

val create_database
  :  String.t
  -> (Finances_store.Handle.t -> 'a Or_error.t)
  -> 'a Or_error.t

val companion_profiles
  :  string
  -> (string * Finances_institutions.Csv_companion_layout.t) list Or_error.t

val institution_definitions
  :  string
  -> Finances_institutions.Institution_definition.t list Or_error.t

val registry_for : string -> Finances_core.Registry.t Or_error.t

val with_database
  :  String.t
  -> (Finances_store.Handle.t -> 'a Or_error.t)
  -> 'a Or_error.t

val write_file : String.t -> string -> unit Or_error.t
val read_file : String.t -> string Or_error.t
val database : string Cmdliner.Term.t
val required : string -> string -> string Cmdliner.Term.t
val json_output : bool Cmdliner.Term.t

val command
  :  ?man:Cmdliner.Manpage.block list
  -> string
  -> string
  -> ('a, Error.t) Result.t Cmdliner.Term.t
  -> 'a Cmdliner.Cmd.t

val group : string -> string -> 'a Cmdliner.Cmd.t list -> 'a Cmdliner.Cmd.t
