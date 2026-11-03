open Base
open Stdio
open Finances_store

let ok = Or_error.ok_exn
let join = Stdlib.Filename.concat
let zip_ok = Result.map_error ~f:Error.of_string
let write path contents = Out_channel.write_all path ~data:contents
let fixture name = In_channel.read_all ("fixtures/" ^ name)

let rec remove path =
  if Stdlib.Sys.is_directory path
  then (
    Array.iter (Stdlib.Sys.readdir path) ~f:(fun name -> remove (join path name));
    Unix.rmdir path)
  else Unix.unlink path
;;

let workspace f =
  let root = Stdlib.Filename.temp_dir ~temp_dir:(Sys.getenv_exn "TMPDIR") "fin-zip-" "" in
  Exn.protect
    ~finally:(fun () -> remove root)
    ~f:(fun () ->
      ok
        (Store.with_open ~registry:Builtin.registry (join root "fin.sqlite3") (fun db ->
           f root db;
           Ok ())))
;;

let file ?(compress = false) contents =
  ok
    (zip_ok
       (if compress
        then Zipc.File.deflate_of_binary_string contents
        else Zipc.File.stored_of_binary_string contents))
;;

let member ?mode path file = ok (zip_ok (Zipc.Member.make ?mode ~path (File file)))

let archive members =
  List.fold members ~init:Zipc.empty ~f:(fun zip member -> Zipc.add member zip)
  |> Zipc.to_binary_string
  |> zip_ok
  |> ok
;;

let scan ?max_file_bytes ?(institution = "boursobank") db paths =
  ok (Finances_import.Files.scan ?max_file_bytes db ~institution ~paths)
;;

let count report outcome =
  List.count report.Finances_import.Files.files ~f:(fun file ->
    Poly.equal file.outcome outcome)
;;

let check label expected actual = Alcotest.(check int) label expected actual
let events db = List.length (ok (Store.events db))

let import_and_replay () =
  List.iter [ false; true ] ~f:(fun compress ->
    workspace (fun root db ->
      let path = join root "export.ZIP" in
      let contents = fixture "bank.ofx" in
      write path (archive [ member "folder/statement.ofx" (file ~compress contents) ]);
      let report = scan db [ path ] in
      check "one member imported" 1 (count report Imported);
      Alcotest.(check string)
        "member provenance"
        (path ^ "/folder/statement.ofx")
        (List.hd_exn report.files).path;
      let n = events db in
      check "repeat unchanged" 1 (count (scan db [ path ]) Unchanged);
      let extracted = join root "extracted.ofx" in
      write extracted contents;
      check "extracted copy unchanged" 1 (count (scan db [ extracted ]) Unchanged);
      check "movements unchanged" n (events db);
      ignore (ok (Store.project_bank db));
      let journal = ok (Finances_hledger.export db) in
      ignore (scan db [ path; extracted ]);
      Alcotest.(check string) "journal stable" journal (ok (Finances_hledger.export db))))
;;

let companions_and_filters () =
  workspace (fun root db ->
    let path = join root "export.zip" in
    write
      path
      (archive
         [ member "b.ofx" (file (fixture "bank.ofx"))
         ; member "a.csv" (file ~compress:true (fixture "bank.csv"))
         ; member ".hidden/b.ofx" (file "ignored")
         ; member "download.part" (file "ignored")
         ; member "README.txt" (file "unrelated")
         ]);
    let report = scan db [ path ] in
    check "primary and companion imported" 2 (count report Imported);
    check "unrelated member bytes reported" 1 (count report Unsupported);
    check "hidden and partial members ignored" 2 (List.length report.ignored);
    ignore (ok (Store.match_companions db));
    ignore (ok (Store.project_bank db));
    let journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "companion enrichment retained"
      true
      (String.is_substring journal ~substring:"bank-category");
    Alcotest.(check bool)
      "no extraction"
      false
      (Stdlib.Sys.file_exists (join root "b.ofx")))
;;

let expanded_limits () =
  workspace (fun root db ->
    let path = join root "bomb.zip" in
    write
      path
      (archive [ member "large.txt" (file ~compress:true (String.make 100000 'x')) ]);
    check
      "expanded member rejected"
      1
      (count (scan ~max_file_bytes:4096L db [ path ]) Unreadable);
    check "nothing imported" 0 (events db);
    write
      path
      (archive
         [ member "a.txt" (file ~compress:true (String.make 2500 'x'))
         ; member "b.txt" (file ~compress:true (String.make 2500 'x'))
         ]);
    check
      "expanded sum rejected"
      1
      (count (scan ~max_file_bytes:4096L db [ path ]) Unreadable);
    let forged =
      ok
        (zip_ok
           (Zipc.File.make
              ~compression:Stored
              (String.make 3500 'x')
              ~decompressed_size:1
              ~decompressed_crc_32:
                (Zipc.File.decompressed_crc_32 (file (String.make 3500 'x')))))
    in
    write path (archive [ member "forged.txt" forged ]);
    check
      "stored size metadata checked"
      1
      (count (scan ~max_file_bytes:4096L db [ path ]) Unreadable);
    write path (archive (List.init 1025 ~f:(fun i -> member (Int.to_string i) (file ""))));
    check "member count bounded" 1 (count (scan db [ path ]) Unreadable))
;;

let unsafe_paths () =
  workspace (fun root db ->
    let path = join root "unsafe.zip" in
    write
      path
      (archive
         [ member "../escape.ofx" (file (fixture "bank.ofx"))
         ; member "/absolute.ofx" (file (fixture "bank.ofx"))
         ; member "good.ofx" (file (fixture "bank.ofx"))
         ]);
    let report = scan db [ path ] in
    check "unsafe paths rejected" 2 (count report Unreadable);
    check "valid member survives" 1 (count report Imported);
    Alcotest.(check bool)
      "nothing written outside archive"
      false
      (Stdlib.Sys.file_exists (join root "escape.ofx")))
;;

let bad_archives () =
  workspace (fun root db ->
    let path = join root "bad.zip" in
    write path "PK broken";
    check "malformed archive" 1 (count (scan db [ path ]) Unreadable);
    write path (archive []);
    check "empty archive" 1 (count (scan db [ path ]) Unreadable);
    let plain = fixture "bank.ofx" in
    let unavailable compression gp_flags =
      ok
        (zip_ok
           (Zipc.File.make
              ~compression
              ~gp_flags
              plain
              ~decompressed_size:(String.length plain)
              ~decompressed_crc_32:(Zipc.File.decompressed_crc_32 (file plain))))
    in
    write
      path
      (archive
         [ member "encrypted.ofx" (unavailable Stored 1)
         ; member "unsupported.ofx" (unavailable Zstd 0)
         ]);
    check "encrypted and unsupported members" 2 (count (scan db [ path ]) Unreadable);
    let bad_crc =
      Zipc.File.make
        ~compression:Stored
        plain
        ~decompressed_size:(String.length plain)
        ~decompressed_crc_32:(Zipc.File.decompressed_crc_32 (file "different"))
    in
    write
      path
      (archive
         [ member "bad.ofx" (ok (zip_ok bad_crc))
         ; member "good.ofx" (file plain)
         ; member "nested.zip" (file (archive []))
         ]);
    let report = scan db [ path ] in
    check "CRC failure and nested archive reported" 2 (count report Unreadable);
    check "good member still imports" 1 (count report Imported))
;;

let wise_archive () =
  workspace (fun root db ->
    let path = join root "wise.zip" in
    write
      path
      (archive [ member "statement.xml" (file ~compress:true (fixture "wise.xml")) ]);
    let report = scan ~institution:"wise" db [ path ] in
    check "Wise XML imported" 1 (count report Imported);
    check
      "Wise replay unchanged"
      1
      (count (scan ~institution:"wise" db [ path ]) Unchanged);
    let other = scan ~institution:"boursobank" db [ path ] in
    check "wrong bank rejected" 1 (count other Rejected))
;;

let () =
  Alcotest.run
    "ZIP inbox"
    [ ( "archive"
      , List.map
          [ "stored and deflated, provenance and replay", import_and_replay
          ; "companions and ignored members", companions_and_filters
          ; "expanded size and member count limits", expanded_limits
          ; "unsafe member paths", unsafe_paths
          ; "malformed, empty, corrupt and nested", bad_archives
          ; "Wise archive and institution scope", wise_archive
          ]
          ~f:(fun (name, test) -> Alcotest.test_case name `Quick test) )
    ]
;;
