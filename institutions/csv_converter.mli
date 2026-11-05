open Base

(** Statement CSVs as records, described by a config file instead of code. A definition says
    which columns mean what; it holds no logic beyond conditions on a cell. A source it cannot
    describe needs a converter of its own, whose output gets the same checks.
    See docs/records-format.md. *)
type t

val codec : t Jsont.t
val of_string : string -> t Or_error.t
val institution : t -> string
val header : t -> string list
val institution_type : t -> Institution_type.t

(** What the definition reads: each role with its columns, in order. *)
val roles : t -> (string * string list) list

(** Journal tags, as tag name and column, that a paired row of this layout adds. *)
val journal_tags : t -> (string * string) list

(** The column whose value, if any, a paired row of this layout gives its journal entry as
    description. Otherwise the counted record's stays. *)
val journal_description : t -> string option

(** Columns two exports of one row must agree on. Others, such as a user's note, may differ. *)
val conflict_columns : t -> string list

(** How overlapping files of this layout are reconciled. *)
val overlap : t -> Adapter.overlap

(** Whether this is a CSV the definition describes, judged from its header. *)
val recognizes : t -> string -> bool

(** Why this looks like a CSV of this layout that went wrong, for a file {!recognizes} refused:
    which columns differ, or the separator. [None] if nothing suggests it was meant to be one. *)
val header_difference : t -> string -> string option

(** How many rows [only] leaves out of this file, so that they do not vanish unnoticed. *)
val filtered_rows : t -> string -> int Or_error.t

(** Every row of the CSV as a record. [name] is the source's name in the result. *)
val convert : t -> name:string -> string -> Records.t Or_error.t
