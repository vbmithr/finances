open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let bos r = Result.map_error r ~f:(fun (`Msg m) -> Error.of_string m) |> ok
let join = Stdlib.Filename.concat
let touch path = Out_channel.write_all path ~data:"x"

let files () =
  let root = Fpath.to_string (bos (Bos.OS.Dir.tmp "fin-state-%s")) in
  let data = join root "data" in
  Unix.mkdir data 0o700;
  let database = join data "fin.sqlite3" in
  let snapshot = join data "accounts.json" in
  Alcotest.(check (list string)) "nothing yet" [] (ok (Fin_state.files ~database));
  List.iter
    [ "fin.sqlite3"
    ; "fin.sqlite3.edit"
    ; "fin.sqlite3-wal"
    ; "bank.journal"
    ; "fin.sqlite3.20261027T172203Z.old"
    ; "bank.journal.20261027T172203Z.old"
    ; "fin.sqlite3.backup"
    ; "notes.txt"
    ]
    ~f:(fun name -> touch (join data name));
  touch snapshot;
  let state = ok (Fin_state.files ~database) in
  Alcotest.(check (list string))
    "fin's files only"
    (List.map
       [ "bank.journal"
       ; "bank.journal.20261027T172203Z.old"
       ; "fin.sqlite3"
       ; "fin.sqlite3-wal"
       ; "fin.sqlite3.20261027T172203Z.old"
       ; "fin.sqlite3.edit"
       ]
       ~f:(join data))
    state;
  ok (Fin_state.delete state);
  Alcotest.(check bool)
    "other files kept"
    true
    (Stdlib.Sys.file_exists (join data "notes.txt"));
  Alcotest.(check bool) "non-empty directory kept" true (Stdlib.Sys.file_exists data);
  Alcotest.(check bool) "exported snapshot kept" true (Stdlib.Sys.file_exists snapshot)
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

let raw path f =
  let db =
    Rel_sqlite3.open' ~mode:Read_write_create path
    |> Result.map_error ~f:Rel_sqlite3.Error.message
    |> Result.ok_or_failwith
  in
  Exn.protect ~f:(fun () -> f db) ~finally:(fun () -> ignore (Rel_sqlite3.close db))
;;

let sql db text =
  Rel_sqlite3.exec_sql db text
  |> Result.map_error ~f:Rel_sqlite3.Error.message
  |> Result.ok_or_failwith
;;

let seed path =
  ok
    (Store.with_open ~registry:Test_support.registry path (fun db ->
       let%map.Or_error _ =
         Test_support.import_ofx
           db
           ~institution:"boursobank"
           (In_channel.read_all "fixtures/bank.ofx")
       in
       ()))
;;

let restore () =
  let root = Fpath.to_string (bos (Bos.OS.Dir.tmp "fin-restore-%s")) in
  let source = join root "backup.sqlite3" in
  seed source;
  let bytes = In_channel.read_all source in
  let target = join root "new/nested/fin.sqlite3" in
  ok (Store.restore ~source ~destination:target);
  Alcotest.(check int)
    "private restored file"
    0o600
    ((Unix.stat target).st_perm land 0o777);
  let count path =
    ok
      (Store.with_open ~registry:Test_support.registry path (fun db ->
         Result.map (Store.events db) ~f:List.length))
  in
  Alcotest.(check int) "restored evidence" (count source) (count target);
  Alcotest.(check string) "source unchanged" bytes (In_channel.read_all source);
  let rejected source destination =
    Alcotest.(check bool)
      "refused"
      true
      (Result.is_error (Store.restore ~source ~destination))
  in
  rejected source source;
  let alias = join root "alias.sqlite3" in
  Unix.link source alias;
  rejected source alias;
  let link = join root "link.sqlite3" in
  Unix.symlink target link;
  rejected source link;
  let original = In_channel.read_all target in
  List.iter
    [ "not a database"; "old schema"; "missing tables"; "foreign-key violation" ]
    ~f:(fun kind ->
      let invalid = join root kind in
      if String.equal kind "not a database"
      then Out_channel.write_all invalid ~data:kind
      else if String.equal kind "foreign-key violation"
      then (
        Out_channel.write_all invalid ~data:bytes;
        raw invalid (fun db ->
          sql
            db
            "PRAGMA foreign_keys=OFF; INSERT INTO \
             postings(entry_id,ordinal,account,commodity,quantity) VALUES \
             (999999,0,'assets:test','EUR','1')"))
      else
        raw invalid (fun db ->
          sql
            db
            (if String.equal kind "old schema"
             then "PRAGMA user_version=12"
             else "PRAGMA user_version=24"));
      rejected invalid target;
      Alcotest.(check string)
        "existing destination preserved"
        original
        (In_channel.read_all target);
      let missing = join root "absent/fin.sqlite3" in
      rejected invalid missing;
      Alcotest.(check bool)
        "invalid source creates nothing"
        false
        (Stdlib.Sys.file_exists missing))
;;

let restore_wal () =
  let root = Fpath.to_string (bos (Bos.OS.Dir.tmp "fin-restore-wal-%s")) in
  let target = join root "target.sqlite3" in
  let source = join root "source.sqlite3" in
  raw target (fun db -> sql db "PRAGMA page_size=4096; VACUUM");
  seed target;
  raw source (fun db -> sql db "PRAGMA page_size=8192; VACUUM");
  ok (Store.with_open ~registry:Test_support.registry source (fun _ -> Ok ()));
  raw target (fun db ->
    sql db "PRAGMA journal_mode=WAL";
    let count () =
      let query =
        Rel_sql.Stmt.v
          "SELECT count(*) FROM source_events"
          ~rev_args:[]
          ~result:Rel.Row.(t1 (int "count"))
      in
      Rel_sqlite3.first db query
      |> Result.map_error ~f:Rel_sqlite3.Error.message
      |> Result.ok_or_failwith
      |> Option.value_exn
    in
    let original = count () in
    Alcotest.(check bool)
      "page-size mismatch rejected"
      true
      (Result.is_error (Store.restore ~source ~destination:target));
    Alcotest.(check int) "failed copy rolls back" original (count ());
    (* Match the page sizes, then restore through a live WAL connection. *)
    raw source (fun db -> sql db "PRAGMA page_size=4096; VACUUM");
    ok (Store.restore ~source ~destination:target);
    Alcotest.(check int) "open connection sees restored database" 0 (count ()))
;;

let () =
  Alcotest.run
    "fin state"
    [ ( "state"
      , [ Alcotest.test_case "files and delete" `Quick files
        ; Alcotest.test_case "backup" `Quick backup
        ; Alcotest.test_case "restore validation and fresh destination" `Quick restore
        ; Alcotest.test_case "restore WAL and rollback on copy failure" `Quick restore_wal
        ] )
    ]
;;
