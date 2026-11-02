open Base
open Stdio
open Finances

module Revolut_csv = Csv_running_source.Make (struct
    let profile = Option.value_exn (Builtin.running_profile "revolut")
  end)

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
       ~account_label:account_id
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
    (Store.with_open ~registry:Test_support.registry ":memory:" (fun db ->
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

let reusable_running_csv () =
  let original = Option.value_exn (Builtin.running_profile "revolut") in
  let profile =
    { original with
      institution = "example"
    ; name = "Example"
    ; kind = "example-transaction"
    ; payload_tag = "example-v1"
    ; format = "example_csv"
    ; header =
        List.map original.header ~f:(fun h -> if String.equal h "Type" then "Kind" else h)
    ; columns = { original.columns with transaction_type = "Kind" }
    }
  in
  let profile =
    Json.decode Csv_running_profile.codec (Json.encode Csv_running_profile.codec profile)
    |> ok
  in
  let module A = (val Csv_running_adapter.make profile : Adapter.S) in
  let contents = replace (csv ()) "Type,Product" "Kind,Product" in
  let document =
    ok (A.parse ~context:{ account_label = Some "main" } ~format:profile.format contents)
  in
  Alcotest.(check bool)
    "other institution produces events"
    true
    (not (List.is_empty document.events));
  List.iter document.events ~f:(fun event ->
    let movement = ok (A.decode event) in
    Alcotest.(check string) "configured institution" "example" movement.account.bank_id)
;;

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
    (Quantity.equal tx.amount (Quantity.of_string "-100.50"));
  error (Revolut_csv.parse ~account_id:"main" (replace (csv ()) "69.50" "70.00"));
  error (Revolut_csv.parse ~account_id:"main" (replace (csv ()) "COMPLETED" "PENDING"));
  error (Revolut_csv.parse ~account_id:"main" (replace (csv ()) "0.50" "-0.50"));
  error (Revolut_csv.parse ~account_id:"" (csv ()))
;;

let utc_clocks () =
  let tx =
    (ok (Revolut_csv.parse ~account_id:"main" (csv ()))).transactions |> List.hd_exn
  in
  Alcotest.(check string)
    "start is UTC"
    "2026-01-01T09:00:00.000000000000Z"
    (Timestamp.to_rfc3339 tx.started_at);
  Alcotest.(check string)
    "completion is UTC"
    "2026-01-01T09:00:01.000000000000Z"
    (Timestamp.to_rfc3339 tx.completed_at);
  Alcotest.(check bool)
    "source clock text retained"
    true
    (String.is_substring tx.payload ~substring:"2026-01-01 09:00:01");
  error
    (Revolut_csv.parse
       ~account_id:"main"
       (replace (csv ()) "2026-01-01 09:00:01" "2026-01-01 08:59:59"));
  error
    (Revolut_csv.parse
       ~account_id:"main"
       (replace (csv ()) "2026-01-01 09:00:00" "2026-02-30 09:00:00"));
  memory (fun db ->
    let event =
      List.find_exn
        (ok (Store.events db))
        ~f:(fun (_, e) ->
          String.equal e.Source_event.venue "revolut"
          && String.equal e.source_id tx.source_id)
      |> snd
    in
    let decoded = ok (Revolut_csv.source_transaction event) in
    Alcotest.(check bool)
      "stored start decodes as same instant"
      true
      (Ptime.equal tx.started_at decoded.started_at);
    Alcotest.(check bool)
      "stored completion decodes as same instant"
      true
      (Ptime.equal tx.completed_at decoded.completed_at);
    Alcotest.(check string) "source payload unchanged" tx.payload decoded.payload;
    check
      "UTC assumption does not duplicate movements"
      4
      (import_revolut db (csv ())).existing)
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
            ~account_label:"main"
            db
            ~institution:"boursobank"
            ~path:"wrong.csv"
            ~contents:(csv ())));
    project db;
    let c = candidate db in
    let fee_leg =
      if Quantity.(c.first_transaction.fee > zero)
      then c.first_event_id
      else c.second_event_id
    in
    let ps = (entries db fee_leg).postings in
    check "fee has its own posting" 3 (List.length ps);
    Alcotest.(check string) "fee account" "expenses:bank-fees" (List.nth_exn ps 2).account;
    Alcotest.(check bool)
      "fee quantity exact"
      true
      (Quantity.equal (List.nth_exn ps 2).quantity (Quantity.of_string "0.50")))
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

let categorized_candidates () =
  memory (fun db ->
    let c = candidate db in
    let same_pair pair =
      Int64.equal pair.Store.first_event_id c.first_event_id
      && Int64.equal pair.second_event_id c.second_event_id
    in
    let classify id account =
      ignore
        (ok
           (Store.set_bank_categories
              db
              [ ok (Bank_category.create ~event_id:id ~account ~reason:"Test review") ]))
    in
    classify c.first_event_id "expenses:food";
    Alcotest.(check bool)
      "categorized leg excluded"
      false
      (List.exists
         (ok (Store.transfer_candidates db))
         ~f:(fun pair ->
           Int64.equal pair.first_event_id c.first_event_id
           || Int64.equal pair.second_event_id c.first_event_id));
    Alcotest.(check bool)
      "unreviewed classified pair absent from all"
      false
      (List.exists
         (ok (Store.transfer_candidates ~include_reviewed:true db))
         ~f:same_pair);
    let _, _, _, count = ok (Store.bank_overview db) in
    check
      "status count matches eligible candidates"
      (List.length (ok (Store.transfer_candidates db)))
      count;
    classify c.first_event_id "equity:unclassified";
    Alcotest.(check bool)
      "reset restores candidacy"
      true
      (List.exists (ok (Store.transfer_candidates db)) ~f:same_pair);
    classify c.second_event_id "income:other";
    Alcotest.(check bool)
      "second categorized leg excluded"
      false
      (List.exists (ok (Store.transfer_candidates db)) ~f:same_pair);
    classify c.second_event_id "equity:unclassified";
    ignore (ok (Store.set_bank_transfers db [ decision ~state:Rejected c ]));
    classify c.first_event_id "expenses:food";
    Alcotest.(check bool)
      "reviewed rejection remains in all"
      true
      (List.exists
         (ok (Store.transfer_candidates ~include_reviewed:true db))
         ~f:(fun pair ->
           same_pair pair && Poly.equal pair.state (Some Bank_transfer.Rejected))))
;;

let confirm_and_reject () =
  memory (fun db ->
    project db;
    let c = candidate db in
    let d = decision c in
    let first_op =
      ok (Store.operations_for_event db c.first_event_id) |> List.hd_exn |> fst
    in
    let second_op =
      ok (Store.operations_for_event db c.second_event_id) |> List.hd_exn |> fst
    in
    Alcotest.(check bool)
      "unreviewed operations are distinct"
      false
      (Int64.equal first_op second_op);
    check "confirm" 1 (ok (Store.set_bank_transfers db [ d ]));
    let grouped_first = ok (Store.operations_for_event db c.first_event_id) in
    let grouped_second = ok (Store.operations_for_event db c.second_event_id) in
    let id, operation = List.hd_exn grouped_first in
    Alcotest.(check int64) "canonical operation belongs to first event" first_op id;
    Alcotest.(check int64)
      "both records resolve to one operation"
      id
      (fst (List.hd_exn grouped_second));
    check "both source records preserved" 2 (List.length operation.evidence);
    check "both movements preserved" 2 (List.length operation.movements);
    check
      "old component ID remains resolvable"
      2
      (List.length (ok (Store.get_operation db second_op)).movements);
    check "confirm replay" 0 (ok (Store.set_bank_transfers db [ d ]));
    let account = Bank_transfer.account c.first_event_id c.second_event_id in
    List.iter [ c.first_event_id; c.second_event_id ] ~f:(fun id ->
      Alcotest.(check string) "shared transfer counter" account (counter db id));
    let sum =
      List.fold
        [ c.first_event_id; c.second_event_id ]
        ~init:Quantity.zero
        ~f:(fun total id ->
          Quantity.(total + (List.nth_exn (entries db id).postings 1).quantity))
    in
    Alcotest.(check bool)
      "transfer clears exactly despite fee"
      true
      (Quantity.is_zero sum);
    check "no duplicate projection" 0 (ok (Store.project_bank db)).inserted;
    let journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "no per-pair clearing account in export"
      false
      (String.is_substring journal ~substring:account);
    Alcotest.(check bool)
      "both source ids retained"
      true
      (String.is_substring
         journal
         ~substring:
           (Printf.sprintf "source-event-ids: %Ld %Ld" c.first_event_id c.second_event_id));
    let journal_transactions =
      Hledger.Parse.journal_exn journal |> Hledger.Journal.transactions
    in
    let direct =
      List.find_exn journal_transactions ~f:(fun tx ->
        Option.equal
          String.equal
          tx.Hledger.Journal.code
          (Some (Printf.sprintf "event-%Ld" c.first_event_id)))
    in
    let first_entry = entries db c.first_event_id in
    let second_entry = entries db c.second_event_id in
    let accounts = List.map direct.postings ~f:(fun p -> p.Hledger.Journal.account) in
    Alcotest.(check bool)
      "both real bank accounts are direct postings"
      true
      (List.mem accounts (List.hd_exn first_entry.postings).account ~equal:String.equal
       && List.mem
            accounts
            (List.hd_exn second_entry.postings).account
            ~equal:String.equal);
    let reviews = ok (Store.bank_review db) in
    let find_review id =
      List.find_exn reviews ~f:(fun r -> Int64.equal r.Store.event_id id)
    in
    Alcotest.(check string)
      "first movement shows the other account"
      (List.hd_exn second_entry.postings).account
      (find_review c.first_event_id).counter_account;
    Alcotest.(check string)
      "second movement shows the other account"
      (List.hd_exn first_entry.postings).account
      (find_review c.second_event_id).counter_account;
    Alcotest.(check int)
      "bank legs and fees appear exactly once"
      (List.length first_entry.postings + List.length second_entry.postings - 2)
      (List.length direct.postings);
    let later =
      if Date.compare first_entry.date second_entry.date > 0
      then first_entry.date
      else second_entry.date
    in
    if not (Date.equal first_entry.date second_entry.date)
    then
      Alcotest.(check bool)
        "later leg keeps posting date"
        true
        (List.exists direct.postings ~f:(fun p ->
           Option.equal
             String.equal
             p.Hledger.Journal.comment.same_line
             (Some ("[" ^ Date.to_string later ^ "]"))));
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
    Alcotest.(check int64)
      "first operation restored"
      first_op
      (fst (List.hd_exn (ok (Store.operations_for_event db c.first_event_id))));
    Alcotest.(check int64)
      "second operation restored"
      second_op
      (fst (List.hd_exn (ok (Store.operations_for_event db c.second_event_id))));
    check
      "undo leaves a single movement"
      1
      (List.length (ok (Store.get_operation db second_op)).movements);
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
      (List.count review ~f:(fun r ->
         Poly.equal r.Store.classification Store.Transfer
         && not (String.is_prefix r.counter_account ~prefix:"assets:transfers:pair-"))))
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

let workspace f =
  let path =
    Stdlib.Filename.temp_file
      ~temp_dir:(Sys.getenv_exn "TMPDIR")
      "transfer-tests"
      ".sqlite3"
  in
  Exn.protect ~finally:(fun () -> Unix.unlink path) ~f:(fun () -> f path)
;;

let conflict_and_reopen () =
  workspace (fun path ->
    let d =
      ok
        (Store.with_open ~registry:Test_support.registry path (fun db ->
           seed db;
           project db;
           let d = decision (candidate db) in
           ignore (ok (Store.set_bank_transfers db [ d ]));
           Ok d))
    in
    ok
      (Store.with_open ~registry:Test_support.registry path (fun db ->
         project db;
         check
           "reopened decisions"
           1
           (List.length (ok (Store.bank_transfer_decisions db)));
         let first = ok (Store.operations_for_event db d.first_event_id) |> List.hd_exn in
         let second =
           ok (Store.operations_for_event db d.second_event_id) |> List.hd_exn
         in
         Alcotest.(check int64)
           "grouped operation survives reopen"
           (fst first)
           (fst second);
         check
           "reopened group retains both source events"
           2
           (List.length (snd first).evidence);
         Ok ()));
    raw path (fun db ->
      sql
        db
        (Printf.sprintf
           "UPDATE postings SET quantity='999' WHERE ordinal=1 AND entry_id IN (SELECT \
            id FROM journal_entries WHERE event_id=%Ld)"
           d.second_event_id));
    ok
      (Store.with_open ~registry:Test_support.registry path (fun db ->
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
           ~payload:"{}"
           ())
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

let () =
  Alcotest.run
    "Transfers and Revolut"
    [ ( "workflow"
      , List.map
          [ "CSV validation and duplicate identities", parser
          ; "reusable running CSV profile", reusable_running_csv
          ; "UTC clock interpretation and replay", utc_clocks
          ; "import replay and fee accounting", import_and_fees
          ; "candidate ambiguity and rejections", candidates
          ; "categorized movements are excluded", categorized_candidates
          ; "confirm, reject, remap and reimport", confirm_and_reject
          ; "review before projection", before_projection
          ; "category, batch and one-to-one safeguards", safeguards
          ; "reopen and projection conflict", conflict_and_reopen
          ; "currency and source eligibility", eligibility
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
