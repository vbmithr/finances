open Base
open Finances
open Finances_store

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
    (Setup.with_open ~registry:Test_support.registry path (fun db ->
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
  | Handle.Inserted id -> id
  | Handle.Existing _ -> failwith "expected a new record"
;;

let existing expected = function
  | Handle.Existing id -> Alcotest.(check int64) "same record" expected id
  | Handle.Inserted _ -> failwith "expected an existing record"
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
        (Setup.with_open ~registry:Test_support.registry path (fun db ->
           Sources.ingest db original))
      |> inserted
    in
    with_store path (fun db ->
      existing id (ok (Sources.ingest db original));
      error (Sources.ingest db (event ~payload:"changed" "deposit-1"));
      error (Sources.ingest db (event ~at:2L "deposit-1"));
      let stored_id, stored = List.hd_exn (ok (Ledger.events db)) in
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
        ~f:(fun e -> ignore (inserted (ok (Sources.ingest db e))));
      Alcotest.(check int)
        "three distinct identities"
        3
        (List.length (ok (Ledger.events db)))))
;;

let chronology () =
  with_database (fun path ->
    with_store path (fun db ->
      List.iter
        [ event ~at:9L "later"; event ~at:1L "earlier" ]
        ~f:(fun e -> ignore (ok (Sources.ingest db e)));
      Alcotest.(check (list string))
        "event time ordering"
        [ "earlier"; "later" ]
        (List.map (ok (Ledger.events db)) ~f:(fun (_, e) -> e.source_id))))
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
        (Setup.with_open ~registry:Test_support.registry path (fun db ->
           Sources.ingest db (event "deposit")))
      |> inserted
    in
    let original = entry () in
    let entry_id =
      ok
        (Setup.with_open ~registry:Test_support.registry path (fun db ->
           Ledger.record_entry db ~event_id ~key:"collateral:v1" original))
      |> inserted
    in
    with_store path (fun db ->
      existing
        entry_id
        (ok (Ledger.record_entry db ~event_id ~key:"collateral:v1" original));
      error
        (Ledger.record_entry
           db
           ~event_id
           ~key:"collateral:v1"
           (entry ~description:"Changed" ()));
      let entries = ok (Ledger.entries db) in
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
      error (Ledger.record_entry db ~event_id:999L ~key:"missing" (entry ()));
      Alcotest.(check int) "no orphaned entry" 0 (List.length (ok (Ledger.entries db)))))
;;

let rollback () =
  with_database (fun path ->
    let event_id =
      ok
        (Setup.with_open ~registry:Test_support.registry path (fun db ->
           Sources.ingest db (event "deposit")))
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
      error (Ledger.record_entry db ~event_id ~key:"collateral:v1" (entry ()));
      Alcotest.(check int)
        "header and postings rolled back"
        0
        (List.length (ok (Ledger.entries db))));
    raw path (fun db -> sql db "DROP TRIGGER reject_second");
    with_store path (fun db ->
      ignore
        (inserted (ok (Ledger.record_entry db ~event_id ~key:"collateral:v1" (entry ()))))))
;;

let immutable_evidence () =
  with_database (fun path ->
    with_store path (fun db -> ignore (ok (Sources.ingest db (event "deposit"))));
    raw path (fun db ->
      error (Rel_sqlite3.exec_sql db "UPDATE source_events SET payload=x'00'");
      error (Rel_sqlite3.exec_sql db "DELETE FROM source_events"));
    with_store path (fun db ->
      Alcotest.(check int) "evidence survives" 1 (List.length (ok (Ledger.events db)))))
;;

let future_schema () =
  with_database (fun path ->
    with_store path ignore;
    raw path (fun db -> sql db "PRAGMA user_version=99");
    error (Setup.with_open ~registry:Test_support.registry path (fun _ -> Ok ())))
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
        ignore (ok (Sources.ingest db (make id text))));
      let equivalent = make "first" "2026-10-27T01:00:00.000000000001+01:00" in
      (match ok (Sources.ingest db equivalent) with
       | Existing _ -> ()
       | Inserted _ -> Alcotest.fail "equivalent offset duplicated an instant");
      error (Sources.ingest db (make "first" "2026-10-27T00:00:00.000000000002Z")));
    with_store path (fun db ->
      let rows = ok (Ledger.events db) in
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
      let first = inserted (ok (Sources.ingest db from_event)) in
      let second = inserted (ok (Sources.ingest db second_event)) in
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
      operation_id := ok (Operations.add_operation db op);
      error (Operations.add_operation db op);
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
      error (Operations.add_operation db bad);
      let temporary =
        ok
          (Operation.create
             ~occurred:from_event.occurred
             ~evidence:[ Operation.File_row { document = "sample.csv"; row = 1 } ]
             ~movements:[])
      in
      error (Operations.add_operation db temporary));
    with_store path (fun db ->
      let restored = ok (Operations.get_operation db !operation_id) in
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
      error (Operations.get_operation db 999999L));
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

let cdc_header =
  "Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native \
   Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n"
;;

let cdc_row n kind amount to_currency to_amount =
  Printf.sprintf
    "2026-01-0%d 10:0%d:00,Row %d,CRO,%s,%s,%s,EUR,1,1,%s,\n"
    n
    n
    n
    amount
    to_currency
    to_amount
    kind
;;

(* CDC rows import through the same registry adapter and store path as
   every other venue: source events plus proposed operations. *)
let cdc_import_unified () =
  with_database (fun path ->
    let contents =
      cdc_header
      ^ cdc_row 1 "crypto_deposit" "1.5" "" ""
      ^ cdc_row 2 "crypto_viban_exchange" "-2" "EUR" "0.20"
    in
    with_store path (fun db ->
      let report =
        ok (Import.import_bank_file db ~institution:"cdc" ~path:"cdc.csv" ~contents)
      in
      Alcotest.(check bool)
        "imported"
        true
        (match report.Bank_import.outcome with
         | Bank_import.Imported -> true
         | _ -> false);
      Alcotest.(check int) "two rows inserted" 2 report.inserted;
      let events = ok (Ledger.events db) in
      Alcotest.(check int) "two source events" 2 (List.length events);
      List.iter events ~f:(fun (_, (e : Source_event.t)) ->
        Alcotest.(check string) "venue" "cdc" e.venue;
        Alcotest.(check string) "operation kind" "cdc-csv-v1" e.kind);
      let exchange =
        List.find_exn events ~f:(fun (_, e) ->
          String.equal
            (List.Assoc.find_exn
               e.Source_event.fields
               "Transaction Kind"
               ~equal:String.equal)
            "crypto_viban_exchange")
      in
      let operations = ok (Operations.operations_for_event db (fst exchange)) in
      Alcotest.(check int) "one proposed operation" 1 (List.length operations);
      let operation = snd (List.hd_exn operations) in
      Alcotest.(check int)
        "exchange has two movements"
        2
        (List.length operation.Operation.movements);
      let roles =
        List.sort
          (List.map operation.movements ~f:(fun m ->
             (Option.value_exn m.Operation.identity).Movement_identity.role))
          ~compare:String.compare
      in
      Alcotest.(check (list string)) "roles" [ "from"; "to" ] roles;
      (* Reimporting the identical file is unchanged. *)
      let again =
        ok (Import.import_bank_file db ~institution:"cdc" ~path:"cdc.csv" ~contents)
      in
      Alcotest.(check bool)
        "unchanged"
        true
        (match again.Bank_import.outcome with
         | Bank_import.Unchanged -> true
         | _ -> false);
      Alcotest.(check int) "nothing new inserted" 0 again.inserted;
      Alcotest.(check int) "both rows existing" 2 again.existing;
      (* A different document overlapping an imported row is refused. *)
      let overlap =
        cdc_header
        ^ cdc_row 1 "crypto_deposit" "1.5" "" ""
        ^ cdc_row 3 "crypto_deposit" "9" "" ""
      in
      let blocked =
        Import.import_bank_file
          db
          ~institution:"cdc"
          ~path:"overlap.csv"
          ~contents:overlap
      in
      Alcotest.(check bool)
        "partial overlap refused"
        true
        (match blocked with
         | Error _ -> true
         | Ok report ->
           (match report.Bank_import.outcome with
            | Bank_import.Rejected -> true
            | _ -> false));
      (* Identical rows inside one document are ambiguous. *)
      let duplicated =
        cdc_header
        ^ cdc_row 1 "crypto_deposit" "1.5" "" ""
        ^ cdc_row 1 "crypto_deposit" "1.5" "" ""
      in
      let ambiguous =
        Import.import_bank_file db ~institution:"cdc" ~path:"dup.csv" ~contents:duplicated
      in
      Alcotest.(check bool)
        "identical rows refused"
        true
        (match ambiguous with
         | Error _ -> true
         | Ok report ->
           (match report.Bank_import.outcome with
            | Bank_import.Rejected -> true
            | _ -> false));
      (* A header with no transaction rows is not a successful import. *)
      let header_only =
        Import.import_bank_file
          db
          ~institution:"cdc"
          ~path:"empty.csv"
          ~contents:cdc_header
      in
      Alcotest.(check bool)
        "header-only file refused"
        true
        (match header_only with
         | Error _ -> true
         | Ok report ->
           (match report.Bank_import.outcome with
            | Bank_import.Rejected -> true
            | _ -> false));
      (* A file the adapter does not recognize is unsupported. *)
      let foreign = "a,b\n1,2\n" in
      let unsupported =
        Import.import_bank_file db ~institution:"cdc" ~path:"other.csv" ~contents:foreign
      in
      Alcotest.(check bool)
        "foreign CSV unsupported"
        true
        (match unsupported with
         | Error _ -> true
         | Ok report ->
           (match report.Bank_import.outcome with
            | Bank_import.Unsupported -> true
            | _ -> false))))
;;

let cdc_reimport_validates_movements () =
  with_database (fun path ->
    let contents = cdc_header ^ cdc_row 2 "crypto_viban_exchange" "-2" "EUR" "0.20" in
    with_store path (fun db ->
      ignore (ok (Import.import_bank_file db ~institution:"cdc" ~path:"a.csv" ~contents)));
    raw path (fun conn ->
      sql conn "UPDATE operation_movements SET quantity='-999' WHERE quantity='-2'");
    with_store path (fun db ->
      let replay =
        Import.import_bank_file db ~institution:"cdc" ~path:"a.csv" ~contents
      in
      Alcotest.(check bool)
        "tampered movement refused"
        true
        (match replay with
         | Error _ -> true
         | Ok report ->
           (match report.Bank_import.outcome with
            | Bank_import.Rejected -> true
            | _ -> false)));
    with_store path (fun db ->
      let events = ok (Ledger.events db) in
      let operations =
        ok (Operations.operations_for_event db (fst (List.hd_exn events)))
      in
      let quantities =
        List.concat_map operations ~f:(fun (_, (o : Operation.t)) ->
          List.map o.movements ~f:(fun m -> Quantity.to_string m.Operation.quantity))
      in
      Alcotest.(check bool)
        "stored quantity left untouched"
        true
        (List.mem quantities "-999" ~equal:String.equal)))
;;

let operation_groups () =
  let groups = ok (Operation_groups.of_links [ 9L, 4L; 2L, 3L ]) in
  let check = Alcotest.(check (list int64)) in
  check "an unlinked operation is alone" [ 7L ] (Operation_groups.members groups 7L);
  Alcotest.(check int64) "alone reads as itself" 7L (Operation_groups.canonical groups 7L);
  (* The canonical ID is the link's first, even when it is not the smallest. *)
  Alcotest.(check int64) "member reads as first" 9L (Operation_groups.canonical groups 4L);
  check "first comes first" [ 9L; 4L ] (Operation_groups.members groups 4L);
  check "same group from either side" [ 9L; 4L ] (Operation_groups.members groups 9L);
  let refused links =
    Alcotest.(check bool)
      "an operation belongs to one group"
      true
      (Result.is_error (Operation_groups.of_links links))
  in
  refused [ 1L, 2L; 2L, 3L ];
  refused [ 1L, 2L; 3L, 2L ];
  refused [ 1L, 2L; 1L, 3L ];
  refused [ 1L, 1L ]
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
          ; "cdc imports through the unified path", cdc_import_unified
          ; "cdc reimport validates stored movements", cdc_reimport_validates_movements
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ; ( "model"
      , [ Alcotest.test_case "operation groups" `Quick operation_groups
        ; Alcotest.test_case "shared temporal codec" `Quick temporal_codec
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
