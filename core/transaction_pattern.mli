open Base

(** A compiled Perl-style regular expression for transaction text. *)
type t

val compile : case_sensitive:bool -> string -> t Or_error.t

(** Search the display name and each original description separately. *)
val matches : t -> display_name:string option -> descriptions:string list -> bool
