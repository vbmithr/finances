open Base
open Rel

(** What Rel's DDL lacks: the triggers that keep evidence immutable and histories
    append-only. [Check] writes Rel check constraints from typed columns. *)
module Check : sig
  type 'r t = 'r Table.Check.t

  val positive : ('r, int) Col.t -> 'r t
  val nonnegative : ('r, int) Col.t -> 'r t
  val canonical_instant : ('r, Ptime.t option) Col.t -> 'r t
  val one_of : ('r, string) Col.t -> string list -> 'r t
  val present : ('r, 'a option) Col.t -> 'r t
  val absent : ('r, 'a option) Col.t -> 'r t
  val text_equal : ('r, string) Col.t -> string -> 'r t
  val same : ('r, 'a) Col.t -> ('r, 'a) Col.t -> 'r t
  val less : ('r, int64) Col.t -> ('r, int64) Col.t -> 'r t
  val equivalent : 'r t -> 'r t -> 'r t
  val different : 'r t -> 'r t -> 'r t
  val either : 'r t -> 'r t -> 'r t
end = struct
  type 'r t = 'r Table.Check.t

  let check sql = Table.Check.make sql
  let column c = Rel_sql.Syntax.id (Col.name c)
  let literal = Rel_sql.Syntax.string_to_literal
  let positive c = check (column c ^ " > 0")
  let nonnegative c = check (column c ^ " >= 0")

  let canonical_instant c =
    let name = column c in
    let digit = "[0-9]" in
    let digits n = String.concat (List.init n ~f:(fun _ -> digit)) in
    let pattern =
      digits 4
      ^ "-"
      ^ digits 2
      ^ "-"
      ^ digits 2
      ^ "T"
      ^ digits 2
      ^ ":"
      ^ digits 2
      ^ ":"
      ^ digits 2
      ^ "."
      ^ digits 12
      ^ "Z"
    in
    check
      (name
       ^ " IS NULL OR (length("
       ^ name
       ^ ") = 33 AND "
       ^ name
       ^ " GLOB "
       ^ literal pattern
       ^ ")")
  ;;

  let one_of c values =
    check (column c ^ " IN (" ^ String.concat ~sep:"," (List.map values ~f:literal) ^ ")")
  ;;

  let present c = check (column c ^ " IS NOT NULL")
  let absent c = check (column c ^ " IS NULL")
  let text_equal c value = check (column c ^ " = " ^ literal value)
  let same a b = check (column a ^ " = " ^ column b)
  let less a b = check (column a ^ " < " ^ column b)

  let combine op a b =
    check ("(" ^ Table.Check.expr a ^ ") " ^ op ^ " (" ^ Table.Check.expr b ^ ")")
  ;;

  let equivalent a b = combine "=" a b
  let different a b = combine "!=" a b
  let either a b = combine "OR" a b
end

type t = Definition : 'r Table.t * (string * string list) option -> t

(* [mutable_columns] are exempt from the update trigger of an immutable table: the
   rest of each row is evidence, but derived values such as a type may be recomputed. *)
let table ?immutable ?(mutable_columns = []) table =
  Definition (table, Option.map immutable ~f:(fun message -> message, mutable_columns))
;;

let rel_table (Definition (table, _)) = Table.Def table

let statements (Definition (table, immutable)) =
  let dialect = Rel_sqlite3.dialect in
  let create = Rel_sql.Stmt.src (Rel_sql.create_table dialect table) |> String.strip in
  let indexes =
    List.map (Table.indices table) ~f:(fun index ->
      Rel_sql.Stmt.src (Rel_sql.create_index dialect table index))
  in
  let triggers =
    match immutable with
    | None -> []
    | Some (message, mutable_columns) ->
      let guarded =
        List.filter_map (Table.cols table) ~f:(fun (Col.Def col) ->
          Option.some_if
            (not (List.mem mutable_columns (Col.name col) ~equal:String.equal))
            (Rel_sql.Syntax.id (Col.name col)))
      in
      List.map
        [ "UPDATE", "no_update"; "DELETE", "no_delete" ]
        ~f:(fun (op, suffix) ->
          let op =
            if String.equal op "UPDATE" && not (List.is_empty mutable_columns)
            then "UPDATE OF " ^ String.concat ~sep:", " guarded
            else op
          in
          Printf.sprintf
            "CREATE TRIGGER %s BEFORE %s ON %s BEGIN SELECT RAISE(ABORT, %s); END;"
            (Rel_sql.Syntax.id (Table.name table ^ "_" ^ suffix))
            op
            (Rel_sql.Syntax.id (Table.name table))
            (Rel_sql.Syntax.string_to_literal message))
  in
  (create :: indexes) @ triggers
;;
