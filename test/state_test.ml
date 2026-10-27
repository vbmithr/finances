open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let bos r = Result.map_error r ~f:(fun (`Msg m) -> Error.of_string m) |> ok
let join = Stdlib.Filename.concat
let touch path = Out_channel.write_all path ~data:"x"

let files () =
  let root = Fpath.to_string (bos (Bos.OS.Dir.tmp "fin-state-%s")) in
  let data = join root "data"
  and config_dir = join root "config" in
  List.iter [ data; config_dir ] ~f:(fun d -> Unix.mkdir d 0o700);
  let database = join data "fin.sqlite3"
  and config = join config_dir "accounts.json" in
  Alcotest.(check (list string))
    "nothing yet"
    []
    (ok (Fin_state.files ~database ~config:(Some config)));
  List.iter
    [ "fin.sqlite3"
    ; "fin.sqlite3-wal"
    ; "bank.journal"
    ; "fin.sqlite3.20261027T172203Z.old"
    ; "bank.journal.20261027T172203Z.old"
    ; "fin.sqlite3.backup"
    ; "notes.txt"
    ]
    ~f:(fun name -> touch (join data name));
  touch config;
  let state = ok (Fin_state.files ~database ~config:(Some config)) in
  Alcotest.(check (list string))
    "fin's files only"
    (List.map
       [ "bank.journal"
       ; "bank.journal.20261027T172203Z.old"
       ; "fin.sqlite3"
       ; "fin.sqlite3-wal"
       ; "fin.sqlite3.20261027T172203Z.old"
       ]
       ~f:(join data)
     @ [ config ])
    state;
  Alcotest.(check int)
    "configuration only when given"
    5
    (List.length (ok (Fin_state.files ~database ~config:None)));
  ok (Fin_state.delete state);
  Alcotest.(check bool)
    "other files kept"
    true
    (Stdlib.Sys.file_exists (join data "notes.txt"));
  Alcotest.(check bool) "non-empty directory kept" true (Stdlib.Sys.file_exists data);
  Alcotest.(check bool)
    "empty directory removed"
    false
    (Stdlib.Sys.file_exists config_dir)
;;

let backup () =
  let root = Fpath.to_string (bos (Bos.OS.Dir.tmp "fin-backup-%s")) in
  let database = join root "fin.sqlite3"
  and copy = join root "copy.sqlite3" in
  let count db = List.length (ok (Store.events db)) in
  let original =
    ok
      (Store.with_open ~registry:Test_support.registry database (fun db ->
         ignore
           (ok
              (Test_support.import_ofx
                 db
                 ~institution:"boursobank"
                 (In_channel.read_all "fixtures/bank.ofx")));
         let%map.Or_error () = Store.backup db copy in
         Alcotest.(check bool)
           "never overwrites"
           true
           (Result.is_error (Store.backup db copy));
         count db))
  in
  Alcotest.(check bool) "events imported" true (original > 0);
  Alcotest.(check int)
    "same events in the copy"
    original
    (ok (Store.with_open ~registry:Test_support.registry copy (fun db -> Ok (count db))))
;;

let () =
  Alcotest.run
    "fin state"
    [ ( "state"
      , [ Alcotest.test_case "files and delete" `Quick files
        ; Alcotest.test_case "backup" `Quick backup
        ] )
    ]
;;
