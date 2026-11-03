open Base
open Rel
module Db = Rel_sqlite3

type t =
  { conn : Db.t
  ; registry : Registry.t
  ; source_typing_cache : (string, Source_typing.t) Hashtbl.t
  ; source_evidence_cache : (string, Source_evidence.rule list) Hashtbl.t
  }

type ingest =
  | Inserted of int64
  | Existing of int64

let ( let* ) x f = Result.bind x ~f

let db_result result =
  Result.map_error result ~f:(fun e -> Error.of_string (Db.Error.message e))
;;

let transaction db f =
  match Db.with_transaction `Immediate db.conn (fun _ -> f db) with
  | Ok result -> result
  | Error e -> Error (Error.of_string (Db.Error.message e))
;;

let statement sql result = Rel_sql.Stmt.v sql ~rev_args:[] ~result
let integer = Row.(t1 (int "value"))
let traverse f xs = Or_error.combine_errors (List.map xs ~f)

let operation_event_id = function
  | Operation.Stored_event id -> Ok id
  | Operation.File_row _ ->
    Or_error.error_string "file-row references must be imported as source events first"
;;

let collect db stmt =
  let* rows = db_result (Db.fold db.conn stmt (fun row rows -> row :: rows) []) in
  Ok (List.rev rows)
;;
