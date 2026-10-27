open Base
open Finances

let ok = Or_error.ok_exn
let bos r = Result.map_error r ~f:(fun (`Msg m) -> Error.of_string m) |> ok
let now = Option.value_exn (Ptime.of_date_time ((2026, 10, 27), ((17, 22, 3), 0)))
let touch dir name = bos (Bos.OS.File.write Fpath.(dir / name) name)
let exists dir name = bos (Bos.OS.File.exists Fpath.(dir / name))

let archive () =
  let dir = bos (Bos.OS.Dir.tmp "fin-reset-%s") in
  let database = Fpath.(to_string (dir / "fin.sqlite3")) in
  Alcotest.(check int)
    "nothing to move"
    0
    (List.length (ok (Database_reset.archive ~now ~database)));
  List.iter
    [ "fin.sqlite3"; "fin.sqlite3-wal"; "bank.journal"; "accounts.json" ]
    ~f:(touch dir);
  let moved = ok (Database_reset.archive ~now ~database) in
  Alcotest.(check (list string))
    "moved, database last"
    [ "bank.journal"; "fin.sqlite3-wal"; "fin.sqlite3" ]
    (List.map moved ~f:(fun m -> Stdlib.Filename.basename m.from));
  List.iter [ "fin.sqlite3"; "fin.sqlite3-wal"; "bank.journal" ] ~f:(fun name ->
    Alcotest.(check bool) (name ^ " moved") false (exists dir name);
    Alcotest.(check bool)
      (name ^ " archived")
      true
      (exists dir (name ^ ".20261027T172203Z.old")));
  Alcotest.(check bool) "other files untouched" true (exists dir "accounts.json");
  touch dir "fin.sqlite3";
  Alcotest.(check bool)
    "never overwrites an archive"
    true
    (Result.is_error (Database_reset.archive ~now ~database));
  Alcotest.(check bool) "database left in place" true (exists dir "fin.sqlite3")
;;

let () =
  Alcotest.run
    "database reset"
    [ "reset", [ Alcotest.test_case "archive" `Quick archive ] ]
;;
