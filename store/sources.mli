open Base
module Db = Rel_sqlite3
open Handle

(** Identical retries return [Existing]; changed evidence for the same identity
    returns an error rather than silently overwriting the original. *)
val ingest : t -> Source_event.t -> ingest Or_error.t

val movement_columns : string
val movement_of_row : Db_schema.Movements.t -> Movement.t Or_error.t
val source_row : (Db_schema.Source_events.t, Db_schema.Source_events.t) Rel.Row.prod

val source_evidence_rules
  :  Handle.t
  -> string Hashtbl.key
  -> (Source_evidence.rule list, Error.t) Result.t

val write_source_evidence
  :  Handle.t
  -> int64
  -> Source_evidence.t
  -> (unit, Error.t) Result.t

val ingest_in_transaction
  :  Handle.t
  -> Source_event.t
  -> (Handle.ingest, Error.t) Result.t

val find_source
  :  string
  -> string
  -> string
  -> string
  -> Db_schema.Source_events.t Rel_sql.Stmt.t

val bank_primary_movement_id : Handle.t -> int64 -> (int64, Error.t) Result.t

val source_typing_profile
  :  Handle.t
  -> string Hashtbl.key
  -> (Source_typing.t, Error.t) Result.t

val add_operation_with_sources
  :  ?source_event_id:int64
  -> Handle.t
  -> (int64, Source_event.t) Hashtbl.t
  -> Operation.t
  -> (int64, Error.t) Result.t
