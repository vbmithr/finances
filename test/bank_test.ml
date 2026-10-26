open Core
open Rel
open Finances

let ok = Or_error.ok_exn
let error result = Alcotest.(check bool) "rejected" true (Result.is_error result)
let fixture () = In_channel.read_all "fixtures/bank.ofx"
let parse contents = ok (Bank_ofx.parse contents)

let replace s old replacement =
  String.substr_replace_first s ~pattern:old ~with_:replacement
;;

let with_database f =
  let path =
    Stdlib.Filename.temp_file
      ~temp_dir:(Sys.getenv_exn "TMPDIR")
      "finances-bank-"
      ".sqlite3"
  in
  Exn.protect ~finally:(fun () -> Stdlib.Sys.remove path) ~f:(fun () -> f path)
;;

let with_store path f =
  ok
    (Store.with_open path (fun db ->
       f db;
       Ok ()))
;;

let with_raw path f =
  let db =
    Rel_sqlite3.open' ~mode:Read_write path
    |> Rel_sqlite3.string_error
    |> Result.ok_or_failwith
  in
  Exn.protect ~finally:(fun () -> ignore (Rel_sqlite3.close db)) ~f:(fun () -> f db)
;;

let sql db text =
  Rel_sqlite3.exec_sql db text |> Rel_sqlite3.string_error |> Result.ok_or_failwith
;;

let scalar db query =
  let stmt = Rel_sql.Stmt.v query ~rev_args:[] ~result:Row.(t1 (int "value")) in
  Rel_sqlite3.first db stmt
  |> Rel_sqlite3.string_error
  |> Result.ok_or_failwith
  |> Option.value_exn
;;

let check_counts path documents events links =
  with_raw path (fun db ->
    List.iter
      [ "import_documents", documents
      ; "source_events", events
      ; "import_document_events", links
      ]
      ~f:(fun (table, expected) ->
        Alcotest.(check int) table expected (scalar db ("SELECT count(*) FROM " ^ table))))
;;

let import path ?(institution = "boursobank") contents =
  let document = parse contents in
  ok (Store.with_open path (fun db -> Store.import_ofx db ~institution document))
;;

let multi_account_parser () =
  let document = parse (fixture ()) in
  Alcotest.(check int) "both statements" 2 (List.length document.statements);
  let first = List.hd_exn document.statements in
  Alcotest.(check int) "first account records" 2 (List.length first.transactions);
  let debit = List.hd_exn first.transactions in
  Alcotest.(check string)
    "decoded unicode and XML entity"
    "CAFÉ & CO"
    (Option.value_exn debit.name);
  Alcotest.(check bool)
    "exact debit"
    true
    (Bigdecimal.equal debit.amount (Bigdecimal.of_string "-0.10"));
  let credit = List.nth_exn first.transactions 1 in
  Alcotest.(check bool)
    "all decimal digits"
    true
    (Bigdecimal.equal
       credit.amount
       (Bigdecimal.of_string "12345678901234567890.1234567890123456789"));
  Alcotest.(check string)
    "opaque invalid DTUSER retained"
    "00009999"
    (Option.value_exn debit.user_date);
  Alcotest.(check string)
    "booking date retained"
    "2026-01-03"
    (Date.to_string debit.booked_on);
  Alcotest.(check bool)
    "available balance is not a ledger balance"
    true
    (Option.is_none first.ledger_balance);
  Alcotest.(check string)
    "balance observation time preserved"
    "20260105123000"
    (Option.value_exn first.available_balance).as_of
;;

let parser_rejections () =
  let original = fixture () in
  List.iter
    [ replace original "<TRNAMT>-0.10</TRNAMT>" "<TRNAMT>NaN</TRNAMT>"
    ; replace original "20260103" "20260230"
    ; replace original "<FITID>shared-id</FITID>" ""
    ; replace
        original
        "<FITID>shared-id</FITID>"
        "<FITID>shared-id</FITID><FITID>duplicate</FITID>"
    ; replace original "<CODE>0</CODE>" "<CODE>2000</CODE>"
    ; replace original "<DTPOSTED>20260103</DTPOSTED>" "<DTPOSTED>20260103oops</DTPOSTED>"
    ; replace
        original
        "<TRNTYPE>DEBIT</TRNTYPE>"
        "<TRNTYPE>DEBIT</TRNTYPE><CORRECTFITID>old</CORRECTFITID>"
    ; replace original "<BANKMSGSRSV1>" "<CREDITCARDMSGSRSV1>"
    ; replace original "<BANKMSGSRSV1>" "<BANKMSGSRSV1><UNKNOWN/>"
    ; "OFXHEADER:100\n\n<OFX>"
    ; original ^ "<OFX/>"
    ]
    ~f:(fun contents -> error (Bank_ofx.parse contents))
;;

let timestamp_date () =
  let document = parse (replace (fixture ()) "20260103" "20260103003000[-5:EST]") in
  let trn = List.hd_exn (List.hd_exn document.statements).transactions in
  Alcotest.(check string)
    "source calendar date, not shifted UTC"
    "2026-01-03"
    (Date.to_string trn.booked_on)
;;

let replay_and_archive () =
  with_database (fun path ->
    let contents = fixture () in
    let first = import path contents in
    Alcotest.(check int) "all accounts imported" 3 first.inserted;
    let second = import path contents in
    Alcotest.(check int) "repeat adds nothing" 0 second.inserted;
    Alcotest.(check int) "repeat reuses all records" 3 second.existing;
    Alcotest.(check int64) "same archived document" first.document_id second.document_id;
    with_store path (fun db ->
      let events = ok (Store.events db) in
      List.iter events ~f:(fun (_, e) ->
        match e.occurred with
        | On_date _ -> ()
        | At_ms _ -> failwith "bank date became a fabricated instant");
      Alcotest.(check int)
        "no automatic accounting classification"
        0
        (List.length (ok (Store.entries db))));
    with_raw path (fun db ->
      let stmt =
        Rel_sql.Stmt.v
          "SELECT contents FROM import_documents"
          ~rev_args:[]
          ~result:Row.(t1 (blob "contents"))
      in
      let archived =
        Rel_sqlite3.first db stmt
        |> Rel_sqlite3.string_error
        |> Result.ok_or_failwith
        |> Option.value_exn
      in
      Alcotest.(check string) "original file bytes archived" contents archived;
      error (Rel_sqlite3.exec_sql db "DELETE FROM import_documents"));
    check_counts path 1 3 3)
;;

let overlapping_exports () =
  with_database (fun path ->
    ignore (import path (fixture ()));
    let changed =
      fixture ()
      |> fun s ->
      replace s "100.01" "101.01"
      |> fun s ->
      String.substr_replace_all s ~pattern:"><" ~with_:">\n<"
      |> fun s -> String.substr_replace_all s ~pattern:"00009999" ~with_:"20260106"
    in
    let result = import path changed in
    Alcotest.(check int) "new statement observation" 0 result.inserted;
    Alcotest.(check int)
      "DTUSER and formatting do not duplicate transactions"
      3
      result.existing;
    check_counts path 2 3 6)
;;

let atomic_conflict () =
  with_database (fun path ->
    ignore (import path (fixture ()));
    let changed =
      fixture ()
      |> fun s ->
      replace
        s
        "<STMTTRN>"
        "<STMTTRN><DTPOSTED>20260102</DTPOSTED><TRNTYPE>CREDIT</TRNTYPE><TRNAMT>5.00</TRNAMT><FITID>new-record</FITID></STMTTRN><STMTTRN>"
      |> fun s -> replace s "<TRNAMT>-0.10</TRNAMT>" "<TRNAMT>-0.11</TRNAMT>"
    in
    let document = parse changed in
    error
      (Store.with_open path (fun db ->
         Store.import_ofx db ~institution:"boursobank" document));
    check_counts path 1 3 3)
;;

let institution_scoping () =
  with_database (fun path ->
    ignore (import path (fixture ()));
    let result = import path ~institution:"other-bank" (fixture ()) in
    Alcotest.(check int) "institution scopes identity" 3 result.inserted;
    check_counts path 2 6 6)
;;

let migration_v1 () =
  with_database (fun path ->
    with_raw path (fun db ->
      sql db (In_channel.read_all "fixtures/schema-v1.sql");
      sql
        db
        {|
INSERT INTO source_events(id,venue,account,kind,source_id,occurred_at_ms,payload)
VALUES(42,'paradex','main','transfer','legacy',1234,x'726177007061796c6f6164');
INSERT INTO journal_entries(id,event_id,projection_key,date,description)
VALUES(9,42,'legacy:v1','2026-01-01','Legacy entry');
INSERT INTO postings(entry_id,ordinal,account,commodity,quantity)
VALUES(9,0,'assets:cash','EUR','0.1234567890123456789'),
      (9,1,'equity:opening','EUR','-0.1234567890123456789');
|});
    with_store path (fun db ->
      let id, e = List.hd_exn (ok (Store.events db)) in
      Alcotest.(check int64) "legacy ID unchanged" 42L id;
      Alcotest.(check string) "legacy bytes unchanged" "raw\000payload" e.payload;
      (match e.occurred with
       | At_ms ms -> Alcotest.(check int64) "legacy timestamp" 1234L ms
       | On_date _ -> failwith "lost legacy timestamp");
      let _, _, entry = List.hd_exn (ok (Store.entries db)) in
      Alcotest.(check bool)
        "legacy exact decimal"
        true
        (Bigdecimal.equal
           (List.hd_exn entry.postings).quantity
           (Bigdecimal.of_string "0.1234567890123456789")));
    with_raw path (fun db ->
      Alcotest.(check int) "version advanced" 5 (scalar db "PRAGMA user_version");
      let check =
        Rel_sql.Stmt.v
          "PRAGMA foreign_key_check"
          ~rev_args:[]
          ~result:Row.(t4 (text "table") (int64 "rowid") (text "parent") (int "fkid"))
      in
      Alcotest.(check bool)
        "foreign keys remain valid"
        true
        (Rel_sqlite3.first db check
         |> Rel_sqlite3.string_error
         |> Result.ok_or_failwith
         |> Option.is_none));
    let result = import path (fixture ()) in
    Alcotest.(check int) "bank import after migration" 3 result.inserted;
    check_counts path 1 4 3)
;;

let failed_migration () =
  with_database (fun path ->
    with_raw path (fun db ->
      sql db (In_channel.read_all "fixtures/schema-v1.sql");
      sql db "CREATE TABLE postings_v2(collision TEXT)");
    error (Store.with_open path (fun _ -> Ok ()));
    with_raw path (fun db ->
      Alcotest.(check int)
        "version unchanged after rollback"
        1
        (scalar db "PRAGMA user_version");
      Alcotest.(check int)
        "new table creation rolled back"
        0
        (scalar db "SELECT count(*) FROM sqlite_master WHERE name='source_events_v2'")))
;;

let () =
  Alcotest.run
    "bank importer"
    [ ( "OFX"
      , List.map
          [ "multiple accounts and exact amounts", multi_account_parser
          ; "malformed and unsupported input", parser_rejections
          ; "calendar date from timestamp", timestamp_date
          ; "replay and original archive", replay_and_archive
          ; "overlapping exports", overlapping_exports
          ; "atomic conflict rollback", atomic_conflict
          ; "institution scoping", institution_scoping
          ; "version 1 migration", migration_v1
          ; "failed migration rollback", failed_migration
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
