open Base
module Db = Rel_sqlite3
open Handle

(** The registry supplies every bank the store can decode; the store itself
    names none. *)
val with_open : registry:Registry.t -> string -> (t -> 'a Or_error.t) -> 'a Or_error.t

(** Compiles every query under [store/sql/] without running it, and runs every script
    in a savepoint it then undoes, so that a typo or a renamed column fails a test, not a
    command. *)
val check_queries : t -> unit Or_error.t

(** The SQL that creates an empty database: its tables, indexes and triggers, and its
    schema version. [store/sql/schema.sql] is a copy, checked in for reading. *)
val schema_sql : string

(** Whether this build can open the database at [path]: it reads the schema version and
    nothing else, so it works on any database. *)
val schema_supported : string -> bool Or_error.t

(** [backup source path] writes a consistent copy of the database file [source] to
    [path], a new file, with SQLite's [VACUUM INTO]; other connections may be using
    the database meanwhile. It does not check the schema version, so it works on
    databases this build cannot open. *)
val backup : string -> string -> unit Or_error.t

(** Restore a SQLite backup after checking its schema, integrity and foreign
    keys. The source is opened read-only. SQLite replaces the destination in
    one write transaction, preserving it on copy failure and managing its
    sidecars. A missing destination and its parents are created privately.
    Source and destination must be different files. *)
val restore : source:string -> destination:string -> unit Or_error.t

(** User-added institutions using generic CSV layouts. *)
val institutions : t -> string list Or_error.t

val institution_definitions : t -> (string * string) list Or_error.t
val institution_companion_profiles : t -> (string * string) list Or_error.t
val institution_companion_profile : t -> string -> string option Or_error.t

val set_institution_companion_profile
  :  ?dry_run:bool
  -> t
  -> string
  -> expected:string option
  -> string
  -> unit Or_error.t

val clear_institution_companion_profile
  :  ?dry_run:bool
  -> t
  -> string
  -> expected:string
  -> unit Or_error.t

val add_institution : ?profile:string -> t -> string -> unit Or_error.t
val institution_profile : t -> string -> string option Or_error.t
val set_institution_profile_if_missing : t -> string -> string -> unit Or_error.t

(** Delete a user-added institution only when it has no imported movements. *)
val delete_institution : ?dry_run:bool -> t -> string -> unit Or_error.t

(** Personal income and expense category accounts, including unused names. *)
val categories : t -> string list Or_error.t

val add_categories : t -> string list -> unit Or_error.t

(** Remove an unused catalog entry. Current transaction decisions and category
    rules must be changed first; historical decisions are retained. *)
val delete_category : t -> string -> unit Or_error.t

val read_column : ('a, 'b) Rel.Col.t -> ('c, 'b) Rel.Col.t

val select_by_column
  :  'a Rel.Table.t
  -> ('a, 'b) Rel.Col.t
  -> ('a, 'c) Rel.Col.t
  -> 'b
  -> 'c Rel_sql.Stmt.t

val select_by_pair
  :  'a Rel.Table.t
  -> ('a, 'b) Rel.Col.t
  -> ('a, 'c) Rel.Col.t
  -> ('a, 'd) Rel.Col.t
  -> 'b
  -> 'c
  -> 'd Rel_sql.Stmt.t
