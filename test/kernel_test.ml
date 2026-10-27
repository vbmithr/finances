open Base
open Finances

let ok = Or_error.ok_exn
let decimal = Quantity.of_string
let error result = Alcotest.(check bool) "rejected" true (Result.is_error result)

let equal_decimal expected actual =
  Alcotest.(check bool) "exact amount" true (Quantity.equal (decimal expected) actual)
;;

let with_database f =
  let path =
    Stdlib.Filename.temp_file ~temp_dir:(Sys.getenv_exn "TMPDIR") "finances-" ".sqlite3"
  in
  Exn.protect ~finally:(fun () -> Stdlib.Sys.remove path) ~f:(fun () -> f path)
;;

let with_store path f =
  ok
    (Store.with_open ~registry:Builtin.registry path (fun db ->
       f db;
       Ok ()))
;;

let raw path f =
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

let event
      ?(account = "main")
      ?(kind = "transfer")
      ?(at = 1L)
      ?(payload = "{\"amount\":\"0.1\"}")
      id
  =
  ok
    (Source_event.create
       ~venue:"paradex"
       ~account
       ~kind
       ~source_id:id
       ~occurred_at_ms:at
       ~payload)
;;

let inserted = function
  | Store.Inserted id -> id
  | Store.Existing _ -> failwith "expected a new record"
;;

let existing expected = function
  | Store.Existing id -> Alcotest.(check int64) "same record" expected id
  | Store.Inserted _ -> failwith "expected an existing record"
;;

let posting ?(commodity = "USDC") account quantity =
  ok (Accounting.posting ~account ~commodity (decimal quantity))
;;

let entry ?(description = "Collateral transfer") () =
  ok
    (Accounting.entry
       ~date:(Date.of_string "2026-10-25")
       ~description
       [ posting "assets:paradex:collateral" "12345678901234567890.123456789012345678901"
       ; posting "assets:wallet" "-12345678901234567890.123456789012345678901"
       ])
;;

let replay () =
  with_database (fun path ->
    let original = event ~payload:"raw\000payload\n" "deposit-1" in
    let id =
      ok
        (Store.with_open ~registry:Builtin.registry path (fun db ->
           Store.ingest db original))
      |> inserted
    in
    with_store path (fun db ->
      existing id (ok (Store.ingest db original));
      error (Store.ingest db (event ~payload:"changed" "deposit-1"));
      error (Store.ingest db (event ~at:2L "deposit-1"));
      let stored_id, stored = List.hd_exn (ok (Store.events db)) in
      Alcotest.(check int64) "identity survived reopening" id stored_id;
      Alcotest.(check string)
        "payload preserved byte for byte"
        original.payload
        stored.payload))
;;

let source_scoping () =
  with_database (fun path ->
    with_store path (fun db ->
      List.iter
        [ event "same"; event ~account:"subaccount" "same"; event ~kind:"funding" "same" ]
        ~f:(fun e -> ignore (inserted (ok (Store.ingest db e))));
      Alcotest.(check int)
        "three distinct identities"
        3
        (List.length (ok (Store.events db)))))
;;

let chronology () =
  with_database (fun path ->
    with_store path (fun db ->
      List.iter
        [ event ~at:9L "later"; event ~at:1L "earlier" ]
        ~f:(fun e -> ignore (ok (Store.ingest db e)));
      Alcotest.(check (list string))
        "event time ordering"
        [ "earlier"; "later" ]
        (List.map (ok (Store.events db)) ~f:(fun (_, e) -> e.source_id))))
;;

let accounting_invariants () =
  let date = Date.of_string "2026-10-25" in
  let make = Accounting.entry ~date ~description:"Test" in
  error (make [ posting "assets:cash" "1" ]);
  error
    (make
       [ posting "assets:cash" "0.1"; posting "equity:opening" "-0.100000000000000001" ]);
  error
    (make
       [ posting ~commodity:"USD" "assets:cash" "1"
       ; posting ~commodity:"EUR" "assets:cash" "-1"
       ]);
  ignore
    (ok
       (make
          [ posting "assets:cash" "0.1"
          ; posting "assets:cash" "0.2"
          ; posting "equity:opening" "-0.3"
          ]));
  error (Accounting.posting ~account:"assets::cash" ~commodity:"USD" Quantity.one)
;;

let journal_replay () =
  with_database (fun path ->
    let event_id =
      ok
        (Store.with_open ~registry:Builtin.registry path (fun db ->
           Store.ingest db (event "deposit")))
      |> inserted
    in
    let original = entry () in
    let entry_id =
      ok
        (Store.with_open ~registry:Builtin.registry path (fun db ->
           Store.record_entry db ~event_id ~key:"collateral:v1" original))
      |> inserted
    in
    with_store path (fun db ->
      existing
        entry_id
        (ok (Store.record_entry db ~event_id ~key:"collateral:v1" original));
      error
        (Store.record_entry
           db
           ~event_id
           ~key:"collateral:v1"
           (entry ~description:"Changed" ()));
      let entries = ok (Store.entries db) in
      Alcotest.(check int) "one journal entry" 1 (List.length entries);
      let _, _, stored = List.hd_exn entries in
      List.iter2_exn original.postings stored.postings ~f:(fun a b ->
        Alcotest.(check bool)
          "all decimal digits preserved"
          true
          (Quantity.equal a.quantity b.quantity))))
;;

let foreign_keys () =
  with_database (fun path ->
    with_store path (fun db ->
      error (Store.record_entry db ~event_id:999L ~key:"missing" (entry ()));
      Alcotest.(check int) "no orphaned entry" 0 (List.length (ok (Store.entries db)))))
;;

let rollback () =
  with_database (fun path ->
    let event_id =
      ok
        (Store.with_open ~registry:Builtin.registry path (fun db ->
           Store.ingest db (event "deposit")))
      |> inserted
    in
    raw path (fun db ->
      sql
        db
        {|
CREATE TRIGGER reject_second BEFORE INSERT ON postings WHEN NEW.ordinal = 1
BEGIN SELECT RAISE(ABORT, 'injected failure'); END;
|});
    with_store path (fun db ->
      error (Store.record_entry db ~event_id ~key:"collateral:v1" (entry ()));
      Alcotest.(check int)
        "header and postings rolled back"
        0
        (List.length (ok (Store.entries db))));
    raw path (fun db -> sql db "DROP TRIGGER reject_second");
    with_store path (fun db ->
      ignore
        (inserted (ok (Store.record_entry db ~event_id ~key:"collateral:v1" (entry ()))))))
;;

let immutable_evidence () =
  with_database (fun path ->
    with_store path (fun db -> ignore (ok (Store.ingest db (event "deposit"))));
    raw path (fun db ->
      error (Rel_sqlite3.exec_sql db "UPDATE source_events SET payload=x'00'");
      error (Rel_sqlite3.exec_sql db "DELETE FROM source_events"));
    with_store path (fun db ->
      Alcotest.(check int) "evidence survives" 1 (List.length (ok (Store.events db)))))
;;

let future_schema () =
  with_database (fun path ->
    with_store path ignore;
    raw path (fun db -> sql db "PRAGMA user_version=99");
    error (Store.with_open ~registry:Builtin.registry path (fun _ -> Ok ())))
;;

let linear_perpetual () =
  let instrument =
    ok
      (Instrument.linear_perpetual
         ~venue:"paradex"
         ~market:"BTC-USD-PERP"
         ~base:"BTC"
         ~quote:"USD"
         ~settlement:"USDC"
         ~multiplier:(decimal "0.001"))
  in
  let position quantity =
    ok
      (Position.create
         ~instrument
         ~quantity:(decimal quantity)
         ~entry_price:(decimal "60000"))
  in
  let mark = decimal "65000" in
  equal_decimal "10" (ok (Position.unrealized_pnl_in_quote ~mark (position "2")));
  equal_decimal "-10" (ok (Position.unrealized_pnl_in_quote ~mark (position "-2")));
  error (Position.unrealized_pnl_in_quote ~mark:Quantity.zero (position "2"));
  error
    (Instrument.linear_perpetual
       ~venue:"paradex"
       ~market:"invalid"
       ~base:"BTC"
       ~quote:"USD"
       ~settlement:"USDC"
       ~multiplier:Quantity.zero)
;;

let () =
  Alcotest.run
    "finances kernel"
    [ ( "storage"
      , List.map
          [ "source replay and conflicts", replay
          ; "scoped source IDs", source_scoping
          ; "event chronology", chronology
          ; "journal replay and exact decimals", journal_replay
          ; "foreign keys", foreign_keys
          ; "atomic rollback", rollback
          ; "immutable evidence", immutable_evidence
          ; "future schema", future_schema
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ; ( "model"
      , [ Alcotest.test_case "accounting invariants" `Quick accounting_invariants
        ; Alcotest.test_case "linear perpetual valuation" `Quick linear_perpetual
        ] )
    ]
;;
