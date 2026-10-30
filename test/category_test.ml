open Base
open Stdio
open Finances

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
    (Store.with_open ~registry:Test_support.registry ":memory:" (fun db ->
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
    let wise_id, _, _ =
      List.find_exn
        (ok (Store.movements db))
        ~f:(fun (_, e, _) -> String.equal e.Source_event.venue "wise")
    in
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
           ~occurred_at:(Timestamp.of_unix_ms_exn 1L)
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
      (Store.with_open ~registry:Test_support.registry path (fun db ->
         import db;
         project db;
         Ok ()));
    with_raw path (fun db -> sql db "UPDATE postings SET quantity='123' WHERE ordinal=1");
    ok
      (Store.with_open ~registry:Test_support.registry path (fun db ->
         error (Store.set_bank_categories db [ decision (id db) "expenses:food" ]);
         check
           "no decision after conflict"
           0
           (List.length (ok (Store.bank_category_history db)));
         Ok ())))
;;

let unclassified_batch_is_atomic () =
  memory (fun db ->
    project db;
    let rows = ok (Store.bank_review db) in
    let first = (List.nth_exn rows 0).event_id in
    let second = (List.nth_exn rows 1).event_id in
    ignore (classify db [ decision second "expenses:existing" ]);
    let before = ok (Store.bank_category_history db) in
    error
      (Store.set_bank_categories
         ~only_unclassified:true
         db
         [ decision first "expenses:new"; decision second "expenses:new" ]);
    let after = ok (Store.bank_category_history db) in
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
           ~name:"all"
           ~pattern:"."
           ~institution:None
           ~case_sensitive:false
           ~action:(Category "expenses:food"))
    in
    ok (Store.add_classification_rule db rule);
    let matches = ok (Store.preview_classification_rule db rule) in
    let ids =
      List.filter_map matches ~f:(fun m ->
        Option.some_if m.Store.eligible m.transaction.event_id)
    in
    Alcotest.(check bool) "at least two matches" true (List.length ids >= 2);
    let first = List.hd_exn ids
    and last = List.last_exn ids in
    ignore (classify db [ decision last "expenses:existing" ]);
    error (Store.apply_classification_rule db ~expected:rule ~event_ids:ids);
    Alcotest.(check string)
      "no partial classification"
      "equity:unclassified"
      (account db first);
    check "no partial audit" 1 (List.length (ok (Store.bank_category_history db)));
    ok (Store.delete_classification_rule db rule.name);
    let replacement =
      ok
        (Classification_rule.create
           ~name:"all"
           ~pattern:"C"
           ~institution:None
           ~case_sensitive:false
           ~action:(Category "expenses:food"))
    in
    ok (Store.add_classification_rule db replacement);
    error (Store.apply_classification_rule db ~expected:rule ~event_ids:[ first ]);
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
  let row event_id institution description counter_account reason : Store.bank_review =
    { event_id
    ; institution
    ; occurred = Temporal.Date (Date.of_string "2026-10-01")
    ; transaction = movement description
    ; display_name = None
    ; movement_type = None
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
  let rows = [ seed; candidate; conflict; other_provider; transfer; no_match ] in
  let result = ok (Category_similarity.find ~seed:1L rows) in
  Alcotest.(check (list int64))
    "unclassified matches"
    [ 2L ]
    (List.map result.candidates ~f:(fun r -> r.event_id));
  Alcotest.(check (list int64))
    "conflicting decisions"
    [ 3L ]
    (List.map result.conflicts ~f:(fun r -> r.event_id));
  error (Category_similarity.find ~seed:2L rows);
  error (Category_similarity.find ~seed:99L rows);
  let empty = row 7L "provider" "   " "expenses:food" (Some "reviewed") in
  error (Category_similarity.find ~seed:7L [ empty ]);
  let card r name =
    { r with Store.display_name = Some name; movement_type = Some Movement_type.Card }
  in
  let card_seed = card seed "Coffee Shop" in
  let card_candidate =
    card (row 8L "provider" "Different amount" "equity:unclassified" None) "coffee  shop"
  in
  let no_merchant = row 9L "provider" "Coffee  SHOP" "equity:unclassified" None in
  let result =
    ok (Category_similarity.find ~seed:1L [ card_seed; card_candidate; no_merchant ])
  in
  Alcotest.(check (list int64))
    "merchant matches without amount and does not mix fallback"
    [ 8L ]
    (List.map result.candidates ~f:(fun r -> r.event_id));
  let pattern =
    ok (Transaction_pattern.compile ~case_sensitive:false "coffee[ \\t]+shop")
  in
  Alcotest.(check bool)
    "regex matches original description"
    true
    (Transaction_pattern.matches
       pattern
       ~display_name:candidate.display_name
       ~descriptions:candidate.transaction.descriptions);
  Alcotest.(check bool)
    "regex matches merchant"
    true
    (Transaction_pattern.matches
       pattern
       ~display_name:card_candidate.display_name
       ~descriptions:card_candidate.transaction.descriptions);
  let exact = ok (Transaction_pattern.compile ~case_sensitive:true "^Coffee Shop$") in
  Alcotest.(check bool)
    "case-sensitive regex honors case"
    false
    (Transaction_pattern.matches
       exact
       ~display_name:card_candidate.display_name
       ~descriptions:card_candidate.transaction.descriptions);
  error (Transaction_pattern.compile ~case_sensitive:false "[");
  error (Transaction_pattern.compile ~case_sensitive:false " ")
;;

let () =
  Alcotest.run
    "Bank categories"
    [ ( "review"
      , List.map
          [ "unclassified batch is atomic", unclassified_batch_is_atomic
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
