open Base
module Db = Rel_sqlite3

(** SQLite is authoritative. One connection must not be shared between threads.
    Opening initializes the latest experimental schema atomically and rejects
    any other existing schema version. Original records can be reimported. *)
type t =
  { conn : Db.t
  ; mutable in_transaction : bool
  ; registry : Registry.t
  ; source_typing_cache : (string, Source_typing.t) Hashtbl.t
  ; source_evidence_cache : (string, Source_evidence.rule list) Hashtbl.t
  }

type ingest =
  | Inserted of int64
  | Existing of int64

val db_result : ('a, Db.Error.t) Result.t -> ('a, Error.t) Result.t
val statement : string -> 'a Rel.Row.t -> 'a Rel_sql.Stmt.t
val ( let* ) : ('a, 'b) Result.t -> ('a -> ('c, 'b) Result.t) -> ('c, 'b) Result.t
val transaction : t -> (t -> ('a, Error.t) Result.t) -> ('a, Error.t) Result.t
val integer : int Rel.Row.t
val traverse : ('a -> 'b Or_error.t) -> 'a List.t -> 'b list Or_error.t
val operation_event_id : Operation.evidence -> (int64, Error.t) Result.t
val collect : t -> 'a Rel_sql.Stmt.t -> ('a List.t, Error.t) Result.t
