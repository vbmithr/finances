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
    (Store.with_open ~registry:Test_support.registry path (fun db ->
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
       ~occurred_at:(Timestamp.of_unix_ms_exn at)
       ~payload
       ())
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
        (Store.with_open ~registry:Test_support.registry path (fun db ->
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
        (Store.with_open ~registry:Test_support.registry path (fun db ->
           Store.ingest db (event "deposit")))
      |> inserted
    in
    let original = entry () in
    let entry_id =
      ok
        (Store.with_open ~registry:Test_support.registry path (fun db ->
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
        (Store.with_open ~registry:Test_support.registry path (fun db ->
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
    error (Store.with_open ~registry:Test_support.registry path (fun _ -> Ok ())))
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

let precise_timestamps () =
  with_database (fun path ->
    let samples =
      [ "latest", "9999-12-31T23:59:59.999999999999Z"
      ; "second", "2026-10-27T00:00:00.000000000002Z"
      ; "before_epoch", "1969-12-31T23:59:59.999000000000Z"
      ; "first", "2026-10-27T00:00:00.000000000001Z"
      ]
    in
    let make id text =
      ok
        (Source_event.create
           ~venue:"clock"
           ~account:"main"
           ~kind:"observation"
           ~source_id:id
           ~occurred_at:(Timestamp.of_rfc3339_exn text)
           ~payload:"raw"
           ())
    in
    with_store path (fun db ->
      List.iter samples ~f:(fun (id, text) ->
        ignore (ok (Store.ingest db (make id text))));
      let equivalent = make "first" "2026-10-27T01:00:00.000000000001+01:00" in
      (match ok (Store.ingest db equivalent) with
       | Existing _ -> ()
       | Inserted _ -> Alcotest.fail "equivalent offset duplicated an instant");
      error (Store.ingest db (make "first" "2026-10-27T00:00:00.000000000002Z")));
    with_store path (fun db ->
      let rows = ok (Store.events db) in
      Alcotest.(check (list string))
        "SQL chronology preserves picoseconds"
        [ "before_epoch"; "first"; "second"; "latest" ]
        (List.map rows ~f:(fun (_, e) -> e.Source_event.source_id));
      List.iter rows ~f:(fun (_, e) ->
        match e.Source_event.occurred with
        | Temporal.Instant instant ->
          let expected = List.Assoc.find_exn samples e.source_id ~equal:String.equal in
          Alcotest.(check string)
            "full precision survives reopen"
            expected
            (Timestamp.to_rfc3339 instant)
        | Temporal.Date _ -> Alcotest.fail "instant changed into a date")))
;;

let temporal_codec () =
  let date = Temporal.of_string_exn "2026-10-27" in
  let instant = Temporal.of_string_exn "2026-10-27T01:00:00.000000000001+01:00" in
  (match date, instant with
   | Temporal.Date d, Temporal.Instant t ->
     Alcotest.(check string) "date stays a date" "2026-10-27" (Date.to_string d);
     Alcotest.(check string)
       "instant preserves precision and normalizes offset"
       "2026-10-27T00:00:00.000000000001Z"
       (Timestamp.to_rfc3339 t)
   | _ -> Alcotest.fail "temporal precision changed");
  List.iter [ date; instant ] ~f:(fun value ->
    Alcotest.(check bool)
      "temporal roundtrip"
      true
      (Poly.equal value (Temporal.of_string_exn (Temporal.to_string value))));
  List.iter [ "2026-02-30"; "2026-10-27T25:00:00Z"; "garbage" ] ~f:(fun text ->
    error (Or_error.try_with (fun () -> Temporal.of_string_exn text)))
;;

let commodity_identity () =
  let usd = Iso4217.of_alpha_code "USD" |> Option.value_exn in
  let fiat = Commodity.fiat usd in
  let reported = ok (Commodity.reported ~provider:"cdc" ~symbol:"USD") in
  let other = ok (Commodity.reported ~provider:"other" ~symbol:"USD") in
  let eth = ok (Commodity.onchain_native ~network:"ethereum-mainnet") in
  let token =
    ok
      (Commodity.onchain_token
         ~network:"ethereum-mainnet"
         ~contract:"0x123"
         ~token_id:None)
  in
  let other_network =
    ok
      (Commodity.onchain_token
         ~network:"polygon-mainnet"
         ~contract:"0x123"
         ~token_id:None)
  in
  let multi_token =
    ok
      (Commodity.onchain_token
         ~network:"ethereum-mainnet"
         ~contract:"0x123"
         ~token_id:(Some "1"))
  in
  Alcotest.(check bool)
    "fiat differs from reported symbol"
    false
    (Commodity.equal fiat reported);
  Alcotest.(check bool) "provider scopes symbol" false (Commodity.equal reported other);
  Alcotest.(check bool) "identity equals itself" true (Commodity.equal reported reported);
  Alcotest.(check bool) "network scopes token" false (Commodity.equal token other_network);
  Alcotest.(check bool) "token ID scopes token" false (Commodity.equal token multi_token);
  List.iter
    [ fiat; reported; other; eth; token; other_network; multi_token ]
    ~f:(fun commodity ->
      let restored =
        Json.decode Commodity.codec (Json.encode Commodity.codec commodity) |> ok
      in
      Alcotest.(check bool)
        "commodity identity survives JSON"
        true
        (Commodity.equal commodity restored));
  error (Json.decode Commodity.codec {|{"kind":"onchain-token","network":"ethereum"}|});
  error (Commodity.reported ~provider:"cdc" ~symbol:"")
;;

let operation_model () =
  let usd = Commodity.fiat (Option.value_exn (Iso4217.of_alpha_code "USD")) in
  let eur = Commodity.fiat (Option.value_exn (Iso4217.of_alpha_code "EUR")) in
  let source = Operation.Stored_event 1L
  and other = Operation.Stored_event 2L in
  let identity = ok (Movement_identity.of_event (event "trade-1") ~role:"from") in
  let identity_to = ok (Movement_identity.of_event (event "trade-1") ~role:"to") in
  let roundtrip =
    ok
      (Json.decode Movement_identity.codec (Json.encode Movement_identity.codec identity))
  in
  Alcotest.(check bool)
    "portable identity roundtrip"
    true
    (Movement_identity.equal identity roundtrip);
  Alcotest.(check bool)
    "movement roles differ"
    false
    (Movement_identity.equal identity identity_to);
  error (Movement_identity.of_event (event "trade-1") ~role:"");
  let leg ?account commodity quantity evidence =
    ok (Operation.movement ?account ~commodity ~quantity:(decimal quantity) ~evidence ())
  in
  let date = Temporal.Date (Date.of_string "2026-11-02") in
  let debit = leg ~account:"assets:bank:eur" eur "-100" [ source ] in
  let credit = leg ~account:"assets:bank:usd" usd "108" [ source ] in
  let conversion =
    ok (Operation.create ~occurred:date ~evidence:[ source ] ~movements:[ debit; credit ])
  in
  Alcotest.(check int)
    "one source can describe two effects"
    2
    (List.length conversion.movements);
  let incoming = leg ~account:"assets:bank:usd" usd "10" [ other ] in
  let outgoing = leg ~account:"assets:bank:other" usd "-10" [ source ] in
  let transfer =
    ok
      (Operation.create
         ~occurred:date
         ~evidence:[ source; other ]
         ~movements:[ outgoing; incoming ])
  in
  Alcotest.(check int) "two records support a transfer" 2 (List.length transfer.evidence);
  let purchase = leg ~account:"assets:bank:card" eur "-4.30" [ source ] in
  let card =
    ok (Operation.create ~occurred:date ~evidence:[ source ] ~movements:[ purchase ])
  in
  Alcotest.(check int)
    "card purchase has one observed movement"
    1
    (List.length card.movements);
  let cro = ok (Commodity.reported ~provider:"cdc" ~symbol:"CRO") in
  let cashback = leg cro "0.0123456789" [ source ] in
  Alcotest.(check bool)
    "cashback account is unresolved"
    true
    (Option.is_none cashback.account);
  Alcotest.(check bool)
    "cashback quantity is exact"
    true
    (Quantity.equal cashback.quantity (decimal "0.0123456789"));
  let top_up = leg cro "-20" [ source ] in
  let partial =
    ok (Operation.create ~occurred:date ~evidence:[ source ] ~movements:[ top_up ])
  in
  Alcotest.(check int)
    "top-up does not invent a destination"
    1
    (List.length partial.movements);
  error (Operation.create ~occurred:date ~evidence:[ source ] ~movements:[ incoming ]);
  error (Operation.create ~occurred:date ~evidence:[ source; source ] ~movements:[]);
  let identified =
    ok
      (Operation.movement
         ~identity
         ~commodity:usd
         ~quantity:Quantity.one
         ~evidence:[ source ]
         ())
  in
  error
    (Operation.create
       ~occurred:date
       ~evidence:[ source ]
       ~movements:[ identified; identified ]);
  let zero =
    ok (Operation.movement ~commodity:usd ~quantity:Quantity.zero ~evidence:[ source ] ())
  in
  Alcotest.(check bool)
    "zero source movement is retained"
    true
    (Quantity.is_zero zero.quantity);
  error
    (Operation.movement
       ~account:"bad account"
       ~commodity:usd
       ~quantity:Quantity.one
       ~evidence:[ source ]
       ());
  error
    (Operation.create
       ~occurred:date
       ~evidence:[ Operation.File_row { document = "x"; row = 0 } ]
       ~movements:[]);
  let unknown = ok (Operation.create ~occurred:date ~evidence:[ source ] ~movements:[]) in
  Alcotest.(check int) "unknown meaning is retained" 0 (List.length unknown.movements)
;;

let operation_storage () =
  with_database (fun path ->
    let operation_id = ref 0L in
    with_store path (fun db ->
      let from_event = event "conversion" in
      let second_event = event "confirmation" in
      let first = inserted (ok (Store.ingest db from_event)) in
      let second = inserted (ok (Store.ingest db second_event)) in
      let evidence = [ Operation.Stored_event first; Operation.Stored_event second ] in
      let from = ok (Movement_identity.of_event from_event ~role:"from") in
      let into = ok (Movement_identity.of_event from_event ~role:"to") in
      let eur = Commodity.fiat (Option.value_exn (Iso4217.of_alpha_code "EUR")) in
      let btc = ok (Commodity.reported ~provider:"cdc" ~symbol:"BTC") in
      let from_leg =
        ok
          (Operation.movement
             ~identity:from
             ~account:"assets:bank:cash"
             ~commodity:eur
             ~quantity:(decimal "-100")
             ~evidence:[ Operation.Stored_event first ]
             ())
      in
      let to_leg =
        ok
          (Operation.movement
             ~identity:into
             ~commodity:btc
             ~quantity:(decimal "0.00123456789")
             ~evidence
             ())
      in
      let op =
        ok
          (Operation.create
             ~occurred:from_event.occurred
             ~evidence
             ~movements:[ from_leg; to_leg ])
      in
      operation_id := ok (Store.add_operation db op);
      error (Store.add_operation db op);
      let bad_id = ok (Movement_identity.of_event second_event ~role:"wrong") in
      let bad_leg =
        ok
          (Operation.movement
             ~identity:bad_id
             ~commodity:btc
             ~quantity:Quantity.one
             ~evidence:[ Operation.Stored_event first ]
             ())
      in
      let bad =
        ok
          (Operation.create
             ~occurred:from_event.occurred
             ~evidence
             ~movements:[ bad_leg ])
      in
      error (Store.add_operation db bad);
      let temporary =
        ok
          (Operation.create
             ~occurred:from_event.occurred
             ~evidence:[ Operation.File_row { document = "sample.csv"; row = 1 } ]
             ~movements:[])
      in
      error (Store.add_operation db temporary));
    with_store path (fun db ->
      let restored = ok (Store.get_operation db !operation_id) in
      Alcotest.(check int) "both sources survive reopen" 2 (List.length restored.evidence);
      Alcotest.(check int) "both legs survive reopen" 2 (List.length restored.movements);
      let incoming = List.nth_exn restored.movements 1 in
      equal_decimal "0.00123456789" incoming.quantity;
      Alcotest.(check bool)
        "unassigned account remains unknown"
        true
        (Option.is_none incoming.account);
      Alcotest.(check string)
        "reported asset survives"
        "BTC"
        (Commodity.display incoming.commodity);
      error (Store.get_operation db 999999L));
    raw path (fun db ->
      Alcotest.(check bool)
        "failed operations rolled back"
        true
        (match
           Rel_sqlite3.first
             db
             (Rel_sql.Stmt.v
                "SELECT count(*) FROM operations"
                ~rev_args:[]
                ~result:Rel.Row.(t1 (int "n")))
         with
         | Ok (Some 1) -> true
         | _ -> false)))
;;

let operation_document_safety () =
  with_database (fun path ->
    let source = event "document-row" in
    let commodity = ok (Commodity.reported ~provider:"demo" ~symbol:"BTC") in
    let make quantity =
      let leg =
        ok (Operation_import.leg ~role:"primary" ~commodity ~quantity:(decimal quantity))
      in
      ok (Operation_import.row ~source ~legs:[ leg ])
    in
    with_store path (fun db ->
      let rows = [ make "1.25" ] in
      let preview =
        ok
          (Store.preview_operation_document
             db
             ~institution:"paradex"
             ~contents:"document"
             ~rows)
      in
      error
        (Store.import_operation_document
           db
           ~institution:"paradex"
           ~contents:"document"
           ~rows:[ make "9" ]
           ~expected:preview);
      Alcotest.(check int)
        "changed preview stores no source"
        0
        (List.length (ok (Store.events db)));
      Alcotest.(check int)
        "document imported"
        1
        (ok
           (Store.import_operation_document
              db
              ~institution:"paradex"
              ~contents:"document"
              ~rows
              ~expected:preview)));
    raw path (fun db ->
      sql db "UPDATE operation_movements SET quantity='9' WHERE role='primary'");
    with_store path (fun db ->
      let rows = [ make "1.25" ] in
      let preview =
        ok
          (Store.preview_operation_document
             db
             ~institution:"paradex"
             ~contents:"document"
             ~rows)
      in
      Alcotest.(check bool)
        "corrupt replay is blocked"
        true
        (not (List.is_empty preview.problems));
      error
        (Store.import_operation_document
           db
           ~institution:"paradex"
           ~contents:"document"
           ~rows
           ~expected:preview)))
;;

let exchange_accounting () =
  let eur = Commodity.fiat (Option.value_exn (Iso4217.of_alpha_code "EUR")) in
  let usd = Commodity.fiat (Option.value_exn (Iso4217.of_alpha_code "USD")) in
  let btc = ok (Commodity.onchain_native ~network:"bitcoin") in
  let date = Date.of_string "2026-11-03" in
  let example first second second_symbol =
    ok
      (Operation_accounting.exchange
         ~date
         ~description:"Exchange"
         [ "assets:wallet", first, "EUR", decimal "-100.1234567890123456789"
         ; "assets:wallet", second, second_symbol, decimal "0.00000000000012345678"
         ])
  in
  List.iter
    [ example eur usd "USD"; example eur btc "BTC" ]
    ~f:(fun entry ->
      Alcotest.(check int)
        "two movements and two trading postings"
        4
        (List.length entry.Accounting.postings);
      let a, b, c, d =
        match entry.postings with
        | [ a; b; c; d ] -> a, b, c, d
        | _ -> assert false
      in
      equal_decimal "-100.1234567890123456789" a.quantity;
      equal_decimal "0.00000000000012345678" b.quantity;
      Alcotest.(check string) "trading account" "equity:trading" c.account;
      Alcotest.(check bool)
        "first commodity balances"
        true
        (Quantity.is_zero (Quantity.add a.quantity c.quantity));
      Alcotest.(check bool)
        "second commodity balances"
        true
        (Quantity.is_zero (Quantity.add b.quantity d.quantity)));
  error
    (Operation_accounting.exchange
       ~date
       ~description:"bad"
       [ "assets:a", eur, "EUR", decimal "-1"; "assets:b", eur, "EUR", decimal "1" ]);
  error
    (Operation_accounting.exchange
       ~date
       ~description:"bad"
       [ "assets:a", eur, "EUR", decimal "-1"; "assets:b", usd, "USD", decimal "-2" ]);
  error
    (Operation_accounting.exchange
       ~date
       ~description:"bad"
       [ "assets:a", eur, "EUR", decimal "-1"
       ; ( "assets:b"
         , ok (Commodity.reported ~provider:"example" ~symbol:"BTC")
         , "BTC"
         , decimal "1" )
       ])
;;

let transfer_accounting () =
  let eur = Commodity.fiat (Option.value_exn (Iso4217.of_alpha_code "EUR")) in
  let btc = ok (Commodity.onchain_native ~network:"bitcoin") in
  let other_btc = ok (Commodity.onchain_native ~network:"other") in
  let date = Date.of_string "2026-11-03" in
  let make commodity symbol =
    [ "assets:bank", commodity, symbol, decimal "-0.00000000000001"
    ; "assets:wallet", commodity, symbol, decimal "0.00000000000001"
    ]
  in
  List.iter
    [ make eur "EUR"; make btc "BTC" ]
    ~f:(fun movements ->
      let entry =
        ok (Operation_accounting.transfer ~date ~description:"Transfer" movements)
      in
      Alcotest.(check int) "direct postings" 2 (List.length entry.Accounting.postings));
  error
    (Operation_accounting.transfer
       ~date
       ~description:"Transfer"
       [ "assets:bank", btc, "BTC", decimal "-1"
       ; "assets:wallet", other_btc, "BTC", decimal "1"
       ]);
  error
    (Operation_accounting.transfer
       ~date
       ~description:"Transfer"
       [ "assets:bank", btc, "BTC", decimal "-1"
       ; "assets:wallet", btc, "BTC", decimal "0.9"
       ])
;;

let () =
  Alcotest.run
    "finances kernel"
    [ ( "storage"
      , List.map
          [ "source replay and conflicts", replay
          ; "scoped source IDs", source_scoping
          ; "event chronology", chronology
          ; "Ptime precision and chronological SQL ordering", precise_timestamps
          ; "journal replay and exact decimals", journal_replay
          ; "foreign keys", foreign_keys
          ; "atomic rollback", rollback
          ; "immutable evidence", immutable_evidence
          ; "future schema", future_schema
          ; "operation storage and rollback", operation_storage
          ; "operation document replay and preview safety", operation_document_safety
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ; ( "model"
      , [ Alcotest.test_case "shared temporal codec" `Quick temporal_codec
        ; Alcotest.test_case "commodity identity" `Quick commodity_identity
        ; Alcotest.test_case "multi-movement operations" `Quick operation_model
        ; Alcotest.test_case "accounting invariants" `Quick accounting_invariants
        ; Alcotest.test_case
            "fiat and crypto exchange accounting"
            `Quick
            exchange_accounting
        ; Alcotest.test_case
            "fiat and crypto transfer accounting"
            `Quick
            transfer_accounting
        ; Alcotest.test_case "linear perpetual valuation" `Quick linear_perpetual
        ] )
    ]
;;
