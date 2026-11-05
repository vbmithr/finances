open Base
open Rel
module Db = Rel_sqlite3

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

let ( let* ) x f = Result.bind x ~f

let db_result result =
  Result.map_error result ~f:(fun e -> Error.of_string (Db.Error.message e))
;;

(* Inside a transaction, a savepoint: the inner work rolls back alone on error, and the
   outer transaction decides the rest. *)
let transaction db f =
  if db.in_transaction
  then (
    let run sql = db_result (Db.exec_sql db.conn sql) in
    let* () = run "SAVEPOINT fin_nested" in
    match f db with
    | Ok _ as result ->
      let* () = run "RELEASE fin_nested" in
      result
    | Error _ as result ->
      let* () = run "ROLLBACK TO fin_nested" in
      let* () = run "RELEASE fin_nested" in
      result)
  else (
    db.in_transaction <- true;
    Exn.protect
      ~f:(fun () ->
        match Db.with_transaction `Immediate db.conn (fun _ -> f db) with
        | Ok result -> result
        | Error e -> Error (Error.of_string (Db.Error.message e)))
      ~finally:(fun () -> db.in_transaction <- false))
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
