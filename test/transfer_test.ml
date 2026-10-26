open Core
open Rel
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)
let csv () = In_channel.read_all "fixtures/revolut.csv"
let mt () = In_channel.read_all "fixtures/bunq.mt940"
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b

let accepted report =
  Alcotest.(check bool)
    "accepted"
    true
    (Poly.equal report.Bank_import.outcome Imported || Poly.equal report.outcome Unchanged)
;;

let rejected report =
  Alcotest.(check bool)
    "rejected input"
    true
    (Poly.equal report.Bank_import.outcome Rejected)
;;

let import_revolut ?(account_id = "main") db contents =
  ok
    (Store.import_bank_file
       ~account_id
       db
       ~institution:"revolut"
       ~path:"revolut.csv"
       ~contents)
;;

let import_bunq db contents =
  ok (Store.import_bank_file db ~institution:"bunq" ~path:"bunq.mt940" ~contents)
;;

let seed db =
  accepted (import_bunq db (mt ()));
  let report = import_revolut db (csv ()) in
  accepted report;
  Alcotest.(check bool)
    "first Revolut import is imported"
    true
    (Poly.equal report.outcome Imported)
;;

let memory f =
  ok
    (Store.with_open ":memory:" (fun db ->
       seed db;
       f db;
       Ok ()))
;;

let project db = ignore (ok (Store.project_bank db))

let candidate db =
  List.find_exn (ok (Store.transfer_candidates db)) ~f:(fun c -> c.Store.unique)
;;

let decision ?(state = Bank_transfer.Confirmed) c =
  ok
    (Bank_transfer.create
       ~first_event_id:c.Store.first_event_id
       ~second_event_id:c.second_event_id
       ~state
       ~reason:"Reviewed both owned accounts")
;;

let entries db id =
  let _, _, entry =
    List.find_exn (ok (Store.entries db)) ~f:(fun (event, _, _) -> Int64.equal event id)
  in
  entry
;;

let counter db id = (List.nth_exn (entries db id).postings 1).account

let parser () =
  let doc = ok (Revolut_csv.parse ~account_id:"main" (csv ())) in
  check "duplicates preserved" 4 (List.length doc.transactions);
  check
    "identities distinct"
    4
    (List.map doc.transactions ~f:(fun t -> t.source_id)
     |> List.dedup_and_sort ~compare:String.compare
     |> List.length);
  let tx = List.last_exn doc.transactions in
  Alcotest.(check bool)
    "bank debit includes fee"
    true
    (Bigdecimal.equal tx.amount (Bigdecimal.of_string "-100.50"));
  error (Revolut_csv.parse ~account_id:"main" (replace (csv ()) "69.50" "70.00"));
  error (Revolut_csv.parse ~account_id:"main" (replace (csv ()) "COMPLETED" "PENDING"));
  error (Revolut_csv.parse ~account_id:"main" (replace (csv ()) "0.50" "-0.50"));
  error (Revolut_csv.parse ~account_id:"" (csv ()))
;;

let import_and_fees () =
  memory (fun db ->
    let replay = import_revolut db (csv ()) in
    accepted replay;
    check "reused four events" 4 replay.existing;
    accepted (import_revolut db ("\239\187\191" ^ csv ()));
    check "no new source events" 8 (List.length (ok (Store.events db)));
    rejected (import_revolut ~account_id:"other" db (csv ()));
    rejected
      (import_revolut db (replace (csv ()) "Initial deposit" "Changed initial deposit"));
    rejected
      (ok
         (Store.import_bank_file
            db
            ~institution:"revolut"
            ~path:"no-context.csv"
            ~contents:(csv ())));
    rejected
      (ok
         (Store.import_bank_file
            ~account_id:"main"
            db
            ~institution:"boursobank"
            ~path:"wrong.csv"
            ~contents:(csv ())));
    project db;
    let c = candidate db in
    let fee_leg =
      if Bigdecimal.(c.first_transaction.fee > zero)
      then c.first_event_id
      else c.second_event_id
    in
    let ps = (entries db fee_leg).postings in
    check "fee has its own posting" 3 (List.length ps);
    Alcotest.(check string) "fee account" "expenses:bank-fees" (List.nth_exn ps 2).account;
    Alcotest.(check bool)
      "fee quantity exact"
      true
      (Bigdecimal.equal (List.nth_exn ps 2).quantity (Bigdecimal.of_string "0.50")))
;;

let candidates () =
  memory (fun db ->
    let cs = ok (Store.transfer_candidates db) in
    check "five candidates including ambiguous repetitions" 5 (List.length cs);
    check "one unique pair" 1 (List.count cs ~f:(fun c -> c.Store.unique));
    let c = candidate db in
    check
      "reject candidate"
      1
      (ok (Store.set_bank_transfers db [ decision ~state:Rejected c ]));
    check "rejected pair hidden" 4 (List.length (ok (Store.transfer_candidates db)));
    check
      "all includes rejection"
      5
      (List.length (ok (Store.transfer_candidates ~include_reviewed:true db))))
;;

let confirm_and_reject () =
  memory (fun db ->
    project db;
    let c = candidate db in
    let d = decision c in
    check "confirm" 1 (ok (Store.set_bank_transfers db [ d ]));
    check "confirm replay" 0 (ok (Store.set_bank_transfers db [ d ]));
    let account = Bank_transfer.account c.first_event_id c.second_event_id in
    List.iter [ c.first_event_id; c.second_event_id ] ~f:(fun id ->
      Alcotest.(check string) "shared transfer counter" account (counter db id));
    let sum =
      List.fold
        [ c.first_event_id; c.second_event_id ]
        ~init:Bigdecimal.zero
        ~f:(fun total id ->
          Bigdecimal.(total + (List.nth_exn (entries db id).postings 1).quantity))
    in
    Alcotest.(check bool)
      "transfer clears exactly despite fee"
      true
      (Bigdecimal.is_zero sum);
    check "no duplicate projection" 0 (ok (Store.project_bank db)).inserted;
    let journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "review provenance"
      true
      (String.is_substring journal ~substring:"transfer-pair:");
    error
      (Store.set_bank_categories
         db
         [ ok
             (Bank_category.create
                ~event_id:c.first_event_id
                ~account:"expenses:food"
                ~reason:"cannot overwrite linked event")
         ]);
    let mapping = List.hd_exn (ok (Store.bank_accounts db)) in
    let alias =
      ok
        (Bank_account_mapping.create
           ~institution:mapping.institution
           ~bank_id:mapping.bank_id
           ~account_id:mapping.account_id
           ~account_type:mapping.account_type
           ~ledger_account:"assets:bank:daily")
    in
    ignore (ok (Store.set_bank_account_mappings db [ alias ]));
    project db;
    accepted (import_revolut db (csv ()));
    project db;
    check
      "undo confirmation"
      1
      (ok (Store.set_bank_transfers db [ decision ~state:Rejected c ]));
    List.iter [ c.first_event_id; c.second_event_id ] ~f:(fun id ->
      Alcotest.(check string)
        "restored unclassified"
        "equity:unclassified"
        (counter db id));
    project db;
    check "two audit records" 2 (List.length (ok (Store.bank_transfer_history db)));
    check "reconfirm" 1 (ok (Store.set_bank_transfers db [ d ]));
    let review = ok (Store.bank_review db) in
    check
      "review shows two transfer legs"
      2
      (List.count review ~f:(fun r -> String.equal r.Store.counter_account account)))
;;

let before_projection () =
  memory (fun db ->
    let c = candidate db in
    ignore (ok (Store.set_bank_transfers db [ decision c ]));
    project db;
    Alcotest.(check string)
      "decision before projection"
      (Bank_transfer.account c.first_event_id c.second_event_id)
      (counter db c.first_event_id))
;;

let safeguards () =
  memory (fun db ->
    project db;
    let c = candidate db in
    ignore
      (ok
         (Store.set_bank_categories
            db
            [ ok
                (Bank_category.create
                   ~event_id:c.first_event_id
                   ~account:"income:work"
                   ~reason:"Previously reviewed")
            ]));
    error (Store.set_bank_transfers db [ decision c ]);
    ignore
      (ok
         (Store.set_bank_categories
            db
            [ ok
                (Bank_category.create
                   ~event_id:c.first_event_id
                   ~account:"equity:unclassified"
                   ~reason:"Reconsidered")
            ]));
    let wrong =
      ok
        (Bank_transfer.create
           ~first_event_id:c.first_event_id
           ~second_event_id:99999L
           ~state:Confirmed
           ~reason:"Invalid")
    in
    error (Store.set_bank_transfers db [ decision c; wrong ]);
    check "batch audit rolled back" 0 (List.length (ok (Store.bank_transfer_history db)));
    Alcotest.(check string)
      "counter rolled back"
      "equity:unclassified"
      (counter db c.first_event_id);
    error (Store.set_bank_transfers db [ decision c; decision c ]);
    error
      (Bank_transfer.create
         ~first_event_id:1L
         ~second_event_id:1L
         ~state:Confirmed
         ~reason:"same");
    accepted (import_bunq db (replace (mt ()) "SYNTHETICBUNQ" "SECONDACCOUNT"));
    project db;
    ignore (ok (Store.set_bank_transfers db [ decision c ]));
    let competing =
      List.find_exn
        (ok (Store.transfer_candidates ~include_reviewed:true db))
        ~f:(fun d ->
          List.exists [ d.first_event_id; d.second_event_id ] ~f:(fun id ->
            Int64.equal id c.first_event_id || Int64.equal id c.second_event_id)
          && not
               (Int64.equal d.first_event_id c.first_event_id
                && Int64.equal d.second_event_id c.second_event_id))
    in
    error (Store.set_bank_transfers db [ decision competing ]);
    ignore (ok (Store.set_bank_transfers db [ decision ~state:Rejected competing ]));
    Alcotest.(check string)
      "unrelated rejection preserves confirmed link"
      (Bank_transfer.account c.first_event_id c.second_event_id)
      (counter db c.first_event_id))
;;

let raw path f =
  let db =
    Rel_sqlite3.open' ~mode:Read_write_create path
    |> Rel_sqlite3.string_error
    |> Result.ok_or_failwith
  in
  Exn.protect ~finally:(fun () -> ignore (Rel_sqlite3.close db)) ~f:(fun () -> f db)
;;

let sql db s =
  Rel_sqlite3.exec_sql db s |> Rel_sqlite3.string_error |> Result.ok_or_failwith
;;

let scalar db s =
  Rel_sqlite3.first db (Rel_sql.Stmt.v s ~rev_args:[] ~result:Row.(t1 (int "value")))
  |> Rel_sqlite3.string_error
  |> Result.ok_or_failwith
  |> Option.value_exn
;;

let workspace f =
  let path =
    Filename_unix.temp_file ~in_dir:(Sys.getenv_exn "TMPDIR") "transfer-tests" ".sqlite3"
  in
  Exn.protect ~finally:(fun () -> Core_unix.unlink path) ~f:(fun () -> f path)
;;

let conflict_and_reopen () =
  workspace (fun path ->
    let d =
      ok
        (Store.with_open path (fun db ->
           seed db;
           project db;
           let d = decision (candidate db) in
           ignore (ok (Store.set_bank_transfers db [ d ]));
           Ok d))
    in
    ok
      (Store.with_open path (fun db ->
         project db;
         check
           "reopened decisions"
           1
           (List.length (ok (Store.bank_transfer_decisions db)));
         Ok ()));
    raw path (fun db ->
      sql
        db
        (sprintf
           "UPDATE postings SET quantity='999' WHERE ordinal=1 AND entry_id IN (SELECT \
            id FROM journal_entries WHERE event_id=%Ld)"
           d.second_event_id));
    ok
      (Store.with_open path (fun db ->
         let reject =
           ok
             (Bank_transfer.create
                ~first_event_id:d.first_event_id
                ~second_event_id:d.second_event_id
                ~state:Rejected
                ~reason:"Undo")
         in
         error (Store.set_bank_transfers db [ reject ]);
         check
           "conflict audit unchanged"
           1
           (List.length (ok (Store.bank_transfer_history db)));
         Ok ())))
;;

let eligibility () =
  memory (fun db ->
    let c = candidate db in
    let other = replace (csv ()) "EUR" "USD" in
    accepted (import_revolut db other);
    let cs = ok (Store.transfer_candidates db) in
    check "FX legs not proposed" 5 (List.length cs);
    let event =
      ok
        (Source_event.create_dated
           ~venue:"test"
           ~account:"opaque"
           ~kind:"opaque"
           ~source_id:"x"
           ~occurred_on:(Date.of_string "2026-01-01")
           ~payload:"{}")
    in
    let id =
      match ok (Store.ingest db event) with
      | Inserted id | Existing id -> id
    in
    error
      (Store.set_bank_transfers
         db
         [ ok
             (Bank_transfer.create
                ~first_event_id:id
                ~second_event_id:c.first_event_id
                ~state:Confirmed
                ~reason:"not a bank event")
         ]))
;;

let migrations () =
  workspace (fun path ->
    raw path (fun db ->
      sql db (In_channel.read_all "fixtures/schema-v7.sql");
      sql
        db
        "INSERT INTO \
         bank_import_attempts(institution,path,format,outcome,document_id,inserted_count,existing_count) \
         VALUES('old','missing','unknown','unreadable',NULL,0,0)");
    ok
      (Store.with_open path (fun db ->
         check "history preserved" 1 (List.length (ok (Store.bank_import_history db)));
         seed db;
         ignore (ok (Store.set_bank_transfers db [ decision (candidate db) ]));
         Ok ()));
    raw path (fun db ->
      check "version eight" 8 (scalar db "PRAGMA user_version");
      Alcotest.(check bool)
        "audit immutable"
        true
        (Result.is_error (Rel_sqlite3.exec_sql db "DELETE FROM bank_transfer_changes"))));
  workspace (fun path ->
    raw path (fun db ->
      sql db (In_channel.read_all "fixtures/schema-v7.sql");
      sql db "CREATE TABLE bank_transfer_changes(collision TEXT)");
    error (Store.with_open path (fun _ -> Ok ()));
    raw path (fun db ->
      check "rollback version" 7 (scalar db "PRAGMA user_version");
      check
        "memberships rolled back"
        0
        (scalar
           db
           "SELECT count(*) FROM sqlite_master WHERE name='bank_transfer_memberships'")))
;;

let () =
  Alcotest.run
    "Transfers and Revolut"
    [ ( "workflow"
      , List.map
          [ "CSV validation and duplicate identities", parser
          ; "import replay and fee accounting", import_and_fees
          ; "candidate ambiguity and rejections", candidates
          ; "confirm, reject, remap and reimport", confirm_and_reject
          ; "review before projection", before_projection
          ; "category, batch and one-to-one safeguards", safeguards
          ; "reopen and projection conflict", conflict_and_reopen
          ; "currency and source eligibility", eligibility
          ; "migration and rollback", migrations
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
