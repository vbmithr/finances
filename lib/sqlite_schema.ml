open Core
open Rel

(** SQLite features missing from the pinned Rel DDL generator. Checks refer to
    typed columns; table structure, keys, defaults and indexes remain Rel's job. *)
module Check : sig
  type 'r t

  val positive : ('r, int) Col.t -> 'r t
  val nonnegative : ('r, int) Col.t -> 'r t
  val nonnegative_int64_option : ('r, int64 option) Col.t -> 'r t
  val one_of : ('r, string) Col.t -> string list -> 'r t
  val present : ('r, 'a option) Col.t -> 'r t
  val absent : ('r, 'a option) Col.t -> 'r t
  val text_equal : ('r, string) Col.t -> string -> 'r t
  val same : ('r, 'a) Col.t -> ('r, 'a) Col.t -> 'r t
  val less : ('r, int64) Col.t -> ('r, int64) Col.t -> 'r t
  val equivalent : 'r t -> 'r t -> 'r t
  val different : 'r t -> 'r t -> 'r t
  val either : 'r t -> 'r t -> 'r t
  val sql : 'r t -> string
end = struct
  type 'r t = Check of string

  let column c = Rel_sql.Syntax.id (Col.name c)
  let literal = Rel_sql.Syntax.string_to_literal
  let positive c = Check (column c ^ " > 0")
  let nonnegative c = Check (column c ^ " >= 0")
  let nonnegative_int64_option c = Check (column c ^ " >= 0")

  let one_of c values =
    Check (column c ^ " IN (" ^ String.concat ~sep:"," (List.map values ~f:literal) ^ ")")
  ;;

  let present c = Check (column c ^ " IS NOT NULL")
  let absent c = Check (column c ^ " IS NULL")
  let text_equal c value = Check (column c ^ " = " ^ literal value)
  let same a b = Check (column a ^ " = " ^ column b)
  let less a b = Check (column a ^ " < " ^ column b)
  let combine op (Check a) (Check b) = Check ("(" ^ a ^ ") " ^ op ^ " (" ^ b ^ ")")
  let equivalent a b = combine "=" a b
  let different a b = combine "!=" a b
  let either a b = combine "OR" a b
  let sql (Check s) = s
end

type t = Definition : 'r Table.t * 'r Check.t list * string option -> t

let table ?(checks = []) ?immutable table = Definition (table, checks, immutable)
let rel_table (Definition (table, _, _)) = Table.Def table

let statements (Definition (table, checks, immutable)) =
  let dialect = Rel_sqlite3.dialect in
  let sql = Rel_sql.Stmt.src (Rel_sql.create_table dialect table) |> String.strip in
  (* Rel emits BIGINT for int64. SQLite STRICT requires INTEGER, which also
     preserves rowid allocation for single integer primary keys. Rewrite only
     column declaration prefixes, never defaults or other SQL expressions. *)
  let sql =
    String.split_lines sql
    |> List.map ~f:(fun line ->
      List.fold (Table.cols table) ~init:line ~f:(fun line (Col.Def col) ->
        let prefix = Rel_sql.Syntax.id (Col.name col) ^ " BIGINT" in
        let stripped = String.lstrip line in
        match String.chop_prefix stripped ~prefix with
        | None -> line
        | Some suffix ->
          String.prefix line (String.length line - String.length stripped)
          ^ Rel_sql.Syntax.id (Col.name col)
          ^ " INTEGER"
          ^ suffix))
    |> String.concat ~sep:"\n"
  in
  let body =
    match String.chop_suffix sql ~suffix:");" with
    | Some body -> body
    | None -> failwith "Rel CREATE TABLE output no longer ends with );"
  in
  let checks = List.map checks ~f:(fun c -> ",\nCHECK (" ^ Check.sql c ^ ")") in
  let create = body ^ String.concat checks ^ "\n) STRICT;" in
  let indexes =
    List.map (Table.indices table) ~f:(fun index ->
      Rel_sql.Stmt.src (Rel_sql.create_index dialect table index))
  in
  let triggers =
    match immutable with
    | None -> []
    | Some message ->
      List.map
        [ "UPDATE", "no_update"; "DELETE", "no_delete" ]
        ~f:(fun (op, suffix) ->
          Printf.sprintf
            "CREATE TRIGGER %s BEFORE %s ON %s BEGIN SELECT RAISE(ABORT, %s); END;"
            (Rel_sql.Syntax.id (Table.name table ^ "_" ^ suffix))
            op
            (Rel_sql.Syntax.id (Table.name table))
            (Rel_sql.Syntax.string_to_literal message))
  in
  (create :: indexes) @ triggers
;;
