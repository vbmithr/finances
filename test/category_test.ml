open Base
open Stdio
open Finances
open Finances_store

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let error r = Alcotest.(check bool) "rejected" true (Result.is_error r)

let import db =
  ignore
    (ok
       (Test_support.import_ofx
          db
          ~institution:"boursobank"
          (In_channel.read_all "fixtures/bank.ofx")))
;;

let memory f =
  ok
    (Setup.with_open ~registry:Test_support.registry ":memory:" (fun db ->
       import db;
       f db;
       Ok ()))
;;

let decision ?(reason = "Reviewed receipt") event_id account =
  ok (Bank_category.create ~event_id ~account ~reason)
;;

let id db = fst (List.hd_exn (ok (Ledger.events db)))

let account db event_id =
  let _, _, entry =
    List.find_exn (ok (Ledger.entries db)) ~f:(fun (id, _, _) -> Int64.equal id event_id)
  in
  (List.nth_exn entry.postings 1).account
;;

let classify db decisions = ok (Review.set_bank_categories db decisions)
let project db = ignore (ok (Ledger.project_bank db))

let rereview_atomic () =
  memory (fun db ->
    let first, second =
      match ok (Ledger.events db) with
      | (a, _) :: (b, _) :: _ -> a, b
      | _ -> failwith "expected two events"
    in
    ignore
      (classify db [ decision first "expenses:food"; decision second "expenses:food" ]);
    let expected =
      [ first, Some ("expenses:food", "Reviewed receipt")
      ; second, Some ("expenses:food", "Reviewed receipt")
      ]
    in
    ignore (classify db [ decision ~reason:"Changed elsewhere" second "expenses:other" ]);
    error
      (Review.review_bank_categories
         db
         ~expected
         [ decision first "expenses:coffee"; decision second "expenses:coffee" ]);
    let rows = ok (Review.bank_review db) in
    let category id =
      (List.find_exn rows ~f:(fun r -> Int64.equal r.Review.event_id id)).counter_account
    in
    Alcotest.(check string)
      "first unchanged after stale batch"
      "expenses:food"
      (category first);
    Alcotest.(check string)
      "concurrent change retained"
      "expenses:other"
      (category second))
;;

let staged_catalog_rollback () =
  memory (fun db ->
    let missing = 999999L in
    error
      (Review.review_bank_categories
         ~categories:[ "expenses:staged" ]
         db
         ~expected:[ missing, None ]
         [ decision missing "expenses:staged" ]);
    Alcotest.(check bool)
      "catalog insert rolls back when a decision fails"
      false
      (List.mem (ok (Setup.categories db)) "expenses:staged" ~equal:String.equal))
;;

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
    check "one audit row" 1 (List.length (ok (Review.bank_category_history db)));
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
    check "history retained" 3 (List.length (ok (Review.bank_category_history db))))
;;

let after_projection () =
  memory (fun db ->
    project db;
    let before = ok (Ledger.entries db) in
    let event_id = id db in
    ignore (classify db [ decision event_id "income:salary" ]);
    let after = ok (Ledger.entries db) in
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
      (ok
         (Import.import_bank_file db ~institution:"wise" ~path:"wise.xml" ~contents:wise));
    let wise_id, _, _ =
      List.find_exn
        (ok (Ledger.movements db))
        ~f:(fun (_, e, _) -> String.equal e.Source_event.venue "wise")
    in
    ignore (classify db [ decision wise_id "expenses:travel" ]);
    project db;
    Alcotest.(check string) "Wise classified" "expenses:travel" (account db wise_id);
    let mapping = List.hd_exn (ok (Ledger.bank_accounts db)) in
    let alias =
      ok
        (Bank_account_mapping.create
           ~institution:mapping.institution
           ~bank_id:mapping.bank_id
           ~account_id:mapping.account_id
           ~account_type:mapping.account_type
           ~ledger_account:"assets:bank:daily")
    in
    ignore (ok (Ledger.set_bank_account_mappings db [ alias ]));
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
    error (Review.set_bank_categories db [ d; decision 999999L "expenses:other" ]);
    check "history rolled back" 0 (List.length (ok (Review.bank_category_history db)));
    Alcotest.(check string)
      "postings rolled back"
      "equity:unclassified"
      (account db event_id);
    error (Review.set_bank_categories db [ d; d ]);
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
           ~occurred_at:(Timestamp.of_unix_ms_exn 1L)
           ~payload:"{}"
           ())
    in
    let other =
      match ok (Sources.ingest db event) with
      | Inserted id | Existing id -> id
    in
    error (Review.set_bank_categories db [ decision other "income:other" ]))
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
      (Setup.with_open ~registry:Test_support.registry path (fun db ->
         import db;
         project db;
         Ok ()));
    with_raw path (fun db -> sql db "UPDATE postings SET quantity='123' WHERE ordinal=1");
    ok
      (Setup.with_open ~registry:Test_support.registry path (fun db ->
         error (Review.set_bank_categories db [ decision (id db) "expenses:food" ]);
         check
           "no decision after conflict"
           0
           (List.length (ok (Review.bank_category_history db)));
         Ok ())))
;;

let unclassified_batch_is_atomic () =
  memory (fun db ->
    project db;
    let rows = ok (Review.bank_review db) in
    let first = (List.nth_exn rows 0).event_id in
    let second = (List.nth_exn rows 1).event_id in
    ignore (classify db [ decision second "expenses:existing" ]);
    let before = ok (Review.bank_category_history db) in
    error
      (Review.set_bank_categories
         ~only_unclassified:true
         db
         [ decision first "expenses:new"; decision second "expenses:new" ]);
    let after = ok (Review.bank_category_history db) in
    check "no partial audit entries" (List.length before) (List.length after);
    Alcotest.(check string)
      "first remains unclassified"
      "equity:unclassified"
      (account db first);
    Alcotest.(check string)
      "existing classification retained"
      "expenses:existing"
      (account db second))
;;

let rule_batch_is_atomic () =
  memory (fun db ->
    project db;
    let rule =
      ok
        (Classification_rule.create
           ~mode:Review
           ~account_type:None
           ~priority:1000
           ~name:"all"
           ~pattern:(Some ".")
           ~institution:None
           ~match_type:None
           ~case_sensitive:false
           ~action:(Type_and_category (Movement_type.Interest, "expenses:food"))
           ())
    in
    ok (Ledger.add_classification_rule db rule);
    let matches = ok (Accounts.preview_classification_rule db rule) in
    let ids =
      List.filter_map matches ~f:(fun m ->
        Option.some_if m.Accounts.eligible m.transaction.event_id)
    in
    Alcotest.(check bool) "at least two matches" true (List.length ids >= 2);
    let first = List.hd_exn ids
    and last = List.last_exn ids in
    ignore (classify db [ decision last "expenses:existing" ]);
    error
      (Accounts.apply_classification_rule
         db
         ~expected:rule
         ~selected:(List.filter matches ~f:(fun m -> m.Accounts.eligible)));
    Alcotest.(check string)
      "no partial classification"
      "equity:unclassified"
      (account db first);
    check "no partial audit" 1 (List.length (ok (Review.bank_category_history db)));
    check
      "no partial type changes"
      0
      (List.length (ok (Restore.movement_type_history db)));
    let forced = ok (Accounts.preview_classification_rule ~force:true db rule) in
    ignore (classify db [ decision last "expenses:changed-again" ]);
    error
      (Accounts.apply_classification_rule
         ~force:true
         db
         ~expected:rule
         ~selected:(List.filter forced ~f:(fun m -> m.Accounts.eligible)));
    Alcotest.(check string)
      "forced batch rejects stale preview"
      "equity:unclassified"
      (account db first);
    ok (Ledger.delete_classification_rule db rule.name);
    let replacement =
      ok
        (Classification_rule.create
           ~mode:Review
           ~account_type:None
           ~priority:1000
           ~name:"all"
           ~pattern:(Some "C")
           ~institution:None
           ~match_type:None
           ~case_sensitive:false
           ~action:(Category "expenses:food")
           ())
    in
    ok (Ledger.add_classification_rule db replacement);
    error
      (Accounts.apply_classification_rule
         db
         ~expected:rule
         ~selected:
           (List.filter matches ~f:(fun m ->
              Int64.equal m.Accounts.transaction.event_id first)));
    Alcotest.(check string)
      "changed rule is refused"
      "equity:unclassified"
      (account db first))
;;

let similarity () =
  let currency = ok (Validation.currency "currency" "EUR") in
  let movement description =
    ok
      (Movement.create
         ~account:
           { bank_id = "provider"
           ; account_id = "main"
           ; account_type = "current"
           ; currency
           }
         ~own_iban:Unknown
         ~booked_on:(Date.of_string "2026-10-01")
         ~amount:(Quantity.of_string "-1")
         [ description ])
  in
  let row event_id institution description counter_account reason : Review.bank_review =
    { event_id
    ; institution
    ; occurred = Temporal.Date (Date.of_string "2026-10-01")
    ; transaction = movement description
    ; description
    ; movement_type = None
    ; imported_movement_type = None
    ; movement_type_override = None
    ; source_evidence = Source_evidence.empty
    ; detail_sources = []
    ; classification = Unclassified
    ; counter_account
    ; reason
    }
  in
  let seed = row 1L "provider" "Coffee  SHOP" "expenses:food" (Some "reviewed") in
  let candidate = row 2L "provider" " coffee\tshop " "equity:unclassified" None in
  let conflict = row 3L "provider" "COFFEE SHOP" "expenses:work" (Some "reviewed") in
  let other_provider = row 4L "other" "coffee shop" "equity:unclassified" None in
  let transfer =
    row 5L "provider" "coffee shop" "assets:transfers:pair" (Some "reviewed")
  in
  let no_match = row 6L "provider" "coffee shop two" "equity:unclassified" None in
  let other_conflict =
    row 10L "other" "coffee shop" "expenses:travel" (Some "reviewed")
  in
  let rows =
    [ seed; candidate; conflict; other_provider; transfer; no_match; other_conflict ]
  in
  let result = ok (Category_similarity.find ~seed:1L rows) in
  Alcotest.(check (list int64))
    "unclassified matches"
    [ 2L; 4L ]
    (List.map result.candidates ~f:(fun r -> r.event_id));
  Alcotest.(check (list int64))
    "conflicting decisions"
    [ 3L; 10L ]
    (List.map result.conflicts ~f:(fun r -> r.event_id));
  error (Category_similarity.find ~seed:2L rows);
  error (Category_similarity.find ~seed:99L rows);
  let empty = row 7L "provider" "   " "expenses:food" (Some "reviewed") in
  error (Category_similarity.find ~seed:7L [ empty ]);
  let card r name =
    { r with Review.description = name; movement_type = Some Movement_type.Card }
  in
  let card_seed = card seed "Coffee Shop" in
  let card_candidate =
    card (row 8L "provider" "Different amount" "equity:unclassified" None) "coffee  shop"
  in
  let no_merchant = row 9L "other" "Coffee  SHOP" "equity:unclassified" None in
  let result =
    ok (Category_similarity.find ~seed:1L [ card_seed; card_candidate; no_merchant ])
  in
  Alcotest.(check (list int64))
    "card display name matches another display name or a single description"
    [ 8L; 9L ]
    (List.map result.candidates ~f:(fun r -> r.event_id));
  let close = row 11L "other" "Coffee-Shop Paris" "equity:unclassified" None in
  let unrelated = row 12L "other" "Bakery Toulouse" "equity:unclassified" None in
  let fuzzy =
    Category_similarity.fuzzy ~seed [ seed; candidate; close; unrelated; transfer ]
  in
  Alcotest.(check (list int64))
    "fuzzy ranking includes a near match, excluding exact matches and transfers"
    [ 11L ]
    (List.map fuzzy ~f:(fun (r, _) -> r.Review.event_id));
  let many =
    List.init 35 ~f:(fun i ->
      row (Int64.of_int (100 + i)) "other" "Coffee-Shop Paris" "equity:unclassified" None)
  in
  Alcotest.(check int)
    "fuzzy suggestions are not capped"
    35
    (List.length (Category_similarity.fuzzy ~seed many));
  let short = row 13L "other" "AB" "expenses:food" (Some "reviewed") in
  Alcotest.(check int)
    "short text has no fuzzy suggestions"
    0
    (List.length (Category_similarity.fuzzy ~seed:short [ close ]));
  let pattern =
    ok (Transaction_pattern.compile ~case_sensitive:false "coffee[ \\t]+shop")
  in
  Alcotest.(check bool)
    "regex matches original description"
    true
    (Transaction_pattern.matches
       pattern
       ~display_name:None
       ~descriptions:candidate.transaction.descriptions);
  Alcotest.(check bool)
    "regex matches merchant"
    true
    (Transaction_pattern.matches
       pattern
       ~display_name:(Some card_candidate.description)
       ~descriptions:card_candidate.transaction.descriptions);
  let exact = ok (Transaction_pattern.compile ~case_sensitive:true "^Coffee Shop$") in
  Alcotest.(check bool)
    "case-sensitive regex honors case"
    false
    (Transaction_pattern.matches
       exact
       ~display_name:(Some card_candidate.description)
       ~descriptions:card_candidate.transaction.descriptions);
  error (Transaction_pattern.compile ~case_sensitive:false "[");
  error (Transaction_pattern.compile ~case_sensitive:false " ")
;;

let () =
  Alcotest.run
    "Bank categories"
    [ ( "review"
      , List.map
          [ "staged category and decisions roll back together", staged_catalog_rollback
          ; "rereview checks all previous decisions atomically", rereview_atomic
          ; "unclassified batch is atomic", unclassified_batch_is_atomic
          ; "rule batches reject stale previews atomically", rule_batch_is_atomic
          ; "similarity preview", similarity
          ; "persistent replay and reset", replay
          ; "post-projection, multi-bank and mappings", after_projection
          ; "invalid atomic batches", invalid_batch
          ; "projection conflicts", conflicts
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
