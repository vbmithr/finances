open Base

(** Statement CSVs as records, described by a config file instead of code. A definition says
    which columns mean what; it holds no logic beyond conditions on a cell. A source it cannot
    describe needs a converter of its own, whose output gets the same checks.
    See docs/records-format.md. *)
type t

val of_string : string -> t Or_error.t
val institution : t -> string
val institution_type : t -> Institution_type.t

(** Journal tags, as tag name and column, that a paired row of this layout adds. *)
val journal_tags : t -> (string * string) list

(** The column whose value, if any, a paired row of this layout gives its journal entry as
    description. Otherwise the counted record's stays. *)
val journal_description : t -> string option

(** How overlapping files of this layout are reconciled. *)
val overlap : t -> Adapter.overlap

(** Whether this is a CSV the definition describes, judged from its header. *)
val recognizes : t -> string -> bool

(** How many rows [only] leaves out of this file, so that they do not vanish unnoticed. *)
val filtered_rows : t -> string -> int Or_error.t

(** Every row of the CSV as a record. [name] is the source's name in the result. *)
val convert : t -> name:string -> string -> Records.t Or_error.t

(** The bundled definitions: every CSV layout fin knows, with its institution. *)
val bundled : t list Lazy.t
