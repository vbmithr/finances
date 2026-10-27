open Base
open Stdio
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)
let document () = ok (Bank_ofx.parse (In_channel.read_all "fixtures/bank.ofx"))
let import db = ignore (ok (Store.import_ofx db ~institution:"boursobank" (document ())))

let memory f =
  ok
    (Store.with_open ~registry:Builtin.registry ":memory:" (fun db ->
       import db;
       f db;
       Ok ()))
;;

let decision ?(reason = "Reviewed receipt") event_id account =
  ok (Bank_category.create ~event_id ~account ~reason)
;;

let id db = fst (List.hd_exn (ok (Store.events db)))

let account db event_id =
  let _, _, entry =
    List.find_exn (ok (Store.entries db)) ~f:(fun (id, _, _) -> Int64.equal id event_id)
  in
  (List.nth_exn entry.postings 1).account
;;

let classify db decisions = ok (Store.set_bank_categories db decisions)
let project db = ignore (ok (Store.project_bank db))

let replay () =
  memory (fun db ->
    let event_id = id db in
    let d = decision event_id "expenses:groceries" in
    check "before projection" 1 (classify db [ d ]);
    project db;
    Alcotest.(check string) "category applied" "expenses:groceries" (account db event_id);
    check "replay decision" 0 (classify db [ d ]);
    import db;
    project db;
    Alcotest.(check string) "survives reimport" "expenses:groceries" (account db event_id);
    check "one audit row" 1 (List.length (ok (Store.bank_category_history db)));
    let journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "audit provenance"
      true
      (String.is_substring journal ~substring:"category-decision-id:");
    check
      "correct decision"
      1
      (classify db [ decision ~reason:"Corrected" event_id "expenses:food" ]);
    project db;
    check
      "reset"
      1
      (classify db [ decision ~reason:"Needs review" event_id "equity:unclassified" ]);
    project db;
    Alcotest.(check string) "reset applied" "equity:unclassified" (account db event_id);
    check "history retained" 3 (List.length (ok (Store.bank_category_history db))))
;;

let after_projection () =
  memory (fun db ->
    project db;
    let before = ok (Store.entries db) in
    let event_id = id db in
    ignore (classify db [ decision event_id "income:salary" ]);
    let after = ok (Store.entries db) in
    check "no extra entries" (List.length before) (List.length after);
    List.iter2_exn before after ~f:(fun (_, _, a) (_, _, b) ->
      Alcotest.(check bool)
        "amounts unchanged"
        true
        (List.equal
           (fun a b -> Quantity.equal a.Accounting.quantity b.Accounting.quantity)
           a.postings
           b.postings));
    let wise = In_channel.read_all "fixtures/wise.xml" in
    ignore
      (ok (Store.import_bank_file db ~institution:"wise" ~path:"wise.xml" ~contents:wise));
    let wise_id, _ = List.hd_exn (ok (Store.wise_transactions db)) in
    ignore (classify db [ decision wise_id "expenses:travel" ]);
    project db;
    Alcotest.(check string) "Wise classified" "expenses:travel" (account db wise_id);
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
    Alcotest.(check string)
      "mapping retains category"
      "income:salary"
      (account db event_id))
;;

let invalid_batch () =
  memory (fun db ->
    project db;
    let event_id = id db in
    let d = decision event_id "expenses:food" in
    error (Store.set_bank_categories db [ d; decision 999999L "expenses:other" ]);
    check "history rolled back" 0 (List.length (ok (Store.bank_category_history db)));
    Alcotest.(check string)
      "postings rolled back"
      "equity:unclassified"
      (account db event_id);
    error (Store.set_bank_categories db [ d; d ]);
    error
      (Bank_category.create
         ~event_id
         ~account:"assets:bank:other"
         ~reason:"not a category");
    error (Bank_category.create ~event_id ~account:"expenses:food" ~reason:" ");
    error
      (Bank_category.of_json
         "[{\"event_id\":1,\"account\":\"expenses:food\",\"reason\":\"reviewed\",\"extra\":1}]");
    let event =
      ok
        (Source_event.create
           ~venue:"test"
           ~account:"main"
           ~kind:"opaque"
           ~source_id:"one"
           ~occurred_at_ms:1L
           ~payload:"{}")
    in
    let other =
      match ok (Store.ingest db event) with
      | Inserted id | Existing id -> id
    in
    error (Store.set_bank_categories db [ decision other "income:other" ]))
;;

let with_raw path f =
  let db =
    Rel_sqlite3.open' ~mode:Read_write_create path
    |> Rel_sqlite3.string_error
    |> Result.ok_or_failwith
  in
  Exn.protect ~finally:(fun () -> ignore (Rel_sqlite3.close db)) ~f:(fun () -> f db)
;;

let sql db query =
  Rel_sqlite3.exec_sql db query |> Rel_sqlite3.string_error |> Result.ok_or_failwith
;;

let workspace f =
  let path =
    Stdlib.Filename.temp_file ~temp_dir:(Sys.getenv_exn "TMPDIR") "categories" ".sqlite3"
  in
  Exn.protect ~finally:(fun () -> Unix.unlink path) ~f:(fun () -> f path)
;;

let conflicts () =
  workspace (fun path ->
    ok
      (Store.with_open ~registry:Builtin.registry path (fun db ->
         import db;
         project db;
         Ok ()));
    with_raw path (fun db -> sql db "UPDATE postings SET quantity='123' WHERE ordinal=1");
    ok
      (Store.with_open ~registry:Builtin.registry path (fun db ->
         error (Store.set_bank_categories db [ decision (id db) "expenses:food" ]);
         check
           "no decision after conflict"
           0
           (List.length (ok (Store.bank_category_history db)));
         Ok ())))
;;

let () =
  Alcotest.run
    "Bank categories"
    [ ( "review"
      , List.map
          [ "persistent replay and reset", replay
          ; "post-projection, multi-bank and mappings", after_projection
          ; "invalid atomic batches", invalid_batch
          ; "projection conflicts", conflicts
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
