open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let error result = Alcotest.(check bool) "rejected" true (Result.is_error result)
let fixture () = In_channel.read_all "fixtures/bank.ofx"

let with_database f =
  let path =
    Stdlib.Filename.temp_file
      ~temp_dir:(Sys.getenv_exn "TMPDIR")
      "finances-journal-"
      ".sqlite3"
  in
  Exn.protect ~finally:(fun () -> Stdlib.Sys.remove path) ~f:(fun () -> f path)
;;

let with_store path f =
  ok
    (Store.with_open ~registry:Test_support.registry path (fun db ->
       f db;
       Ok ()))
;;

let import path contents =
  ok
    (Store.with_open ~registry:Test_support.registry path (fun db ->
       Test_support.import_ofx db ~institution:"boursobank" contents))
;;

let journal_replay () =
  with_database (fun path ->
    ignore (import path (fixture ()));
    let first =
      ok (Store.with_open ~registry:Test_support.registry path Store.project_bank)
    in
    Alcotest.(check int) "all bank events projected" 3 first.inserted;
    let journal =
      ok (Store.with_open ~registry:Test_support.registry path Finances_hledger.export)
    in
    let second =
      ok (Store.with_open ~registry:Test_support.registry path Store.project_bank)
    in
    Alcotest.(check int) "replay creates no postings" 0 second.inserted;
    Alcotest.(check int) "replay recognizes all entries" 3 second.existing;
    let repeated =
      ok (Store.with_open ~registry:Test_support.registry path Finances_hledger.export)
    in
    Alcotest.(check string) "byte-identical export" journal repeated;
    let transactions =
      Hledger.Parse.journal_exn journal |> Hledger.Journal.transactions
    in
    Alcotest.(check int) "one transaction per bank event" 3 (List.length transactions);
    let quantities =
      List.concat_map transactions ~f:(fun transaction ->
        Alcotest.(check int) "two balanced postings" 2 (List.length transaction.postings);
        Alcotest.(check bool) "event provenance" true (Option.is_some transaction.code);
        List.map transaction.postings ~f:(fun posting ->
          match posting.amount with
          | Some (Amount amount) -> amount.quantity
          | _ -> failwith "missing explicit posting amount"))
    in
    Alcotest.(check bool)
      "exact quantities balance"
      true
      (Quantity.is_zero (List.fold quantities ~init:Quantity.zero ~f:Quantity.( + )));
    Alcotest.(check bool)
      "huge precise amount survives journal parsing"
      true
      (List.exists
         quantities
         ~f:
           (Quantity.equal
              (Quantity.of_string "12345678901234567890.1234567890123456789")));
    Alcotest.(check bool)
      "not guessed as expenses"
      false
      (String.is_substring journal ~substring:"expenses:");
    Alcotest.(check bool)
      "unclassified counteraccount"
      true
      (String.is_substring journal ~substring:"equity:unclassified"))
;;

let descriptions () =
  with_database (fun path ->
    let contents =
      String.substr_replace_first
        (fixture ())
        ~pattern:"CAFÉ &amp; CO"
        ~with_:"(code) * ; note&#10;2026-01-01 | more"
    in
    ignore (import path contents);
    ignore (ok (Store.with_open ~registry:Test_support.registry path Store.project_bank));
    let journal =
      ok (Store.with_open ~registry:Test_support.registry path Finances_hledger.export)
    in
    let transactions =
      Hledger.Parse.journal_exn journal |> Hledger.Journal.transactions
    in
    Alcotest.(check int)
      "description cannot inject transactions"
      3
      (List.length transactions);
    let first = List.hd_exn transactions in
    Alcotest.(check bool)
      "semicolon preserved visibly"
      true
      (String.is_substring first.description ~substring:"； note");
    Alcotest.(check bool)
      "description does not become code"
      true
      (Option.equal String.equal first.code (Some "event-1"));
    Alcotest.(check bool)
      "bank text pipe does not become payee separator"
      true
      (String.is_substring first.description ~substring:"｜ more"))
;;

let malformed_source_rollback () =
  with_database (fun path ->
    ignore (import path (fixture ()));
    with_store path (fun db ->
      let _, last = List.last_exn (ok (Store.events db)) in
      let fake =
        ok
          (Source_event.create_dated
             ~venue:last.venue
             ~account:last.account
             ~kind:last.kind
             ~source_id:"does-not-match-payload"
             ~occurred_on:(Date.of_string "2026-01-06")
             ~payload:last.payload)
      in
      let before = List.length (ok (Store.events db)) in
      (* Movements are decoded when they are stored, so an event whose payload
         disagrees with its identity never enters the database. *)
      error (Store.ingest db fake);
      Alcotest.(check int)
        "inconsistent event not stored"
        before
        (List.length (ok (Store.events db)));
      ignore (ok (Store.project_bank db));
      Alcotest.(check bool)
        "projection unaffected"
        true
        (not (List.is_empty (ok (Store.entries db))))))
;;

let existing_conflict_rollback () =
  with_database (fun path ->
    ignore (import path (fixture ()));
    with_store path (fun db ->
      let event_id, event = List.last_exn (ok (Store.events db)) in
      let movement = ok (Registry.movement Builtin.registry event) in
      let expected = ok (Bank_projection.entry ~institution:event.venue movement) in
      let wrong =
        ok
          (Accounting.entry
             ~date:expected.date
             ~description:"conflicting policy"
             expected.postings)
      in
      ignore (ok (Store.record_entry db ~event_id ~key:Bank_projection.key wrong));
      error (Store.project_bank db);
      Alcotest.(check int)
        "previous entry retained, no partial new entries"
        1
        (List.length (ok (Store.entries db)))))
;;

let masked_accounts () =
  let account source_id =
    let contents =
      String.substr_replace_all (fixture ()) ~pattern:"SAVINGS" ~with_:source_id
    in
    let statement = List.last_exn (ok (Bank_ofx.parse contents)).statements in
    let a = statement.account in
    ok
      (Bank_projection.ledger_account
         ~institution:"boursobank"
         { bank_id = a.bank_id
         ; account_id = a.account_id
         ; account_type = a.account_type
         ; currency = a.currency
         })
  in
  let masked = account "****4321" in
  Alcotest.(check bool)
    "masked identifier encoded"
    false
    (String.is_substring masked ~substring:"*");
  let star = account "*" in
  let literal_encoding = account "x-2a" in
  Alcotest.(check bool)
    "encoding cannot collide with literal account IDs"
    false
    (String.equal star literal_encoding)
;;

let () =
  Alcotest.run
    "bank journals"
    [ ( "projection"
      , List.map
          [ "stored events, exact export and replay", journal_replay
          ; "description syntax safety", descriptions
          ; "invalid source rollback", malformed_source_rollback
          ; "projection conflict rollback", existing_conflict_rollback
          ; "masked account identifiers", masked_accounts
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
