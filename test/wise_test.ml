open Core
open Rel
open Finances

let ok = Or_error.ok_exn
let check = Alcotest.(check int)
let replace s a b = String.substr_replace_all s ~pattern:a ~with_:b
let xml () = In_channel.read_all "fixtures/wise.xml"
let csv () = In_channel.read_all "fixtures/wise.csv"

let import db ?(institution = "wise") path contents =
  ok (Store.import_bank_file db ~institution ~path ~contents)
;;

let accepted result =
  Alcotest.(check bool)
    "accepted"
    true
    (Poly.equal result.Bank_import.outcome Imported || Poly.equal result.outcome Unchanged)
;;

let rejected result =
  Alcotest.(check bool) "rejected" true (Poly.equal result.Bank_import.outcome Rejected)
;;

let memory f =
  ok
    (Store.with_open ":memory:" (fun db ->
       f db;
       Ok ()))
;;

let error result = Alcotest.(check bool) "invalid document" true (Result.is_error result)

let parser () =
  let doc = ok (Wise_camt.parse (xml ())) in
  let tx = (List.hd_exn doc.statements).transactions in
  check "reversal preserved" 3 (List.length tx);
  check
    "unique identities"
    3
    (List.length
       (List.dedup_and_sort
          (List.map tx ~f:(fun t -> t.source_id))
          ~compare:String.compare));
  Alcotest.(check string)
    "local calendar"
    "2025-01-02"
    (Date.to_string (List.hd_exn tx).booked_on);
  error
    (Wise_camt.parse
       (replace (xml ()) "<Amt Ccy=\"EUR\">105.00</Amt>" "<Amt Ccy=\"EUR\">106.00</Amt>"));
  error (Wise_camt.parse (replace (xml ()) "<Cd>BOOK</Cd>" "<Cd>PDNG</Cd>"));
  error (Wise_camt.parse (replace (xml ()) "camt.053.001.10" "camt.053.001.08"));
  let original = xml () in
  let start = String.substr_index_exn original ~pattern:"<Ntry>" in
  let finish = String.substr_index_exn original ~pattern:"</Ntry>" + 7 in
  let entry = String.sub original ~pos:start ~len:(finish - start) in
  error (Wise_camt.parse (replace original "</Stmt>" (entry ^ "</Stmt>")));
  error (Wise_csv.parse (replace (csv ()) "02-01-2025" "32-01-2025"))
;;

let multi_bank () =
  memory (fun db ->
    accepted (import db "wise.csv" (csv ()));
    check "CSV creates no movements" 0 (List.length (ok (Store.events db)));
    check "CSV waiting" 3 (ok (Store.match_wise_csv db)).unmatched;
    accepted
      (import
         db
         ~institution:"boursobank"
         "bank.ofx"
         (In_channel.read_all "fixtures/bank.ofx"));
    accepted (import db "wise.xml" (xml ()));
    check "both institutions" 6 (List.length (ok (Store.events db)));
    check "all Wise rows matched" 3 (ok (Store.match_wise_csv db)).matched;
    check "all projected" 6 (ok (Store.project_bank db)).inserted;
    let accounts = ok (Store.bank_accounts db) in
    Alcotest.(check bool)
      "separate bank accounts"
      true
      (List.exists accounts ~f:(fun a ->
         String.equal a.Bank_account_mapping.institution "wise")
       && List.exists accounts ~f:(fun a ->
         String.equal a.Bank_account_mapping.institution "boursobank"));
    List.iter
      (ok (Store.wise_reconciliations db))
      ~f:(fun r ->
        Alcotest.(check bool)
          "stored balance reconciled"
          true
          (Bigdecimal.is_zero r.Store.difference));
    let journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "merchant metadata exported"
      true
      (String.is_substring journal ~substring:"wise-merchant: Synthetic shop");
    Alcotest.(check bool)
      "nonzero fees retained as metadata"
      true
      (String.is_substring journal ~substring:"wise-reported-fees: 0.5");
    accepted (import db "replay.xml" (xml ()));
    accepted (import db "overlap.xml" (replace (xml ()) "statement-a" "statement-b"));
    check "overlap no new projections" 0 (ok (Store.project_bank db)).inserted;
    check "repeated matching stable" 0 (ok (Store.match_wise_csv db)).changed;
    check "replay source count" 6 (List.length (ok (Store.events db)));
    let wise_account =
      List.find_exn accounts ~f:(fun a ->
        String.equal a.Bank_account_mapping.institution "wise")
    in
    let mapped =
      ok
        (Bank_account_mapping.create
           ~institution:wise_account.institution
           ~bank_id:wise_account.bank_id
           ~account_id:wise_account.account_id
           ~account_type:wise_account.account_type
           ~ledger_account:"assets:bank:wise:daily")
    in
    check "Wise mapping applied" 1 (ok (Store.set_bank_account_mappings db [ mapped ]));
    check "mapping replay" 0 (ok (Store.project_bank db)).inserted;
    let mapped_journal = ok (Finances_hledger.export db) in
    Alcotest.(check bool)
      "Wise alias exported"
      true
      (String.is_substring mapped_journal ~substring:"assets:bank:wise:daily");
    let changed =
      replace
        (replace (xml ()) "<Amt Ccy=\"EUR\">5.00</Amt>" "<Amt Ccy=\"EUR\">6.00</Amt>")
        "<Amt Ccy=\"EUR\">105.00</Amt>"
        "<Amt Ccy=\"EUR\">106.00</Amt>"
    in
    rejected (import db "conflict.xml" changed);
    check "conflict atomic" 6 (List.length (ok (Store.events db)));
    rejected (import db ~institution:"boursobank" "wrong.xml" (xml ())))
;;

let currencies () =
  memory (fun db ->
    accepted (import db "eur.xml" (xml ()));
    accepted (import db "usd.xml" (replace (xml ()) "EUR" "USD"));
    check "currencies independent" 6 (List.length (ok (Store.events db)));
    accepted (import db "eur.csv" (csv ()));
    check "currency match" 3 (ok (Store.match_wise_csv db)).matched;
    accepted (import db "fees.csv" (replace (csv ()) "0.50" "0.75"));
    check "fee conflict audit" 2 (ok (Store.match_wise_csv db)).conflicts;
    check "postings only movements" 6 (ok (Store.project_bank db)).inserted)
;;

let duplicate_rows () =
  memory (fun db ->
    accepted (import db "wise.xml" (xml ()));
    let lines = String.split_lines (csv ()) in
    accepted (import db "dup.csv" (csv () ^ List.nth_exn lines 1 ^ "\n"));
    check "ambiguous duplicates" 2 (ok (Store.match_wise_csv db)).ambiguous)
;;

let migration () =
  let path =
    Filename_unix.temp_file ~in_dir:(Sys.getenv_exn "TMPDIR") "wise-migration" ".sqlite3"
  in
  Exn.protect
    ~finally:(fun () -> Core_unix.unlink path)
    ~f:(fun () ->
      let db =
        Rel_sqlite3.open' ~mode:Read_write_create path
        |> Rel_sqlite3.string_error
        |> Result.ok_or_failwith
      in
      let sql s =
        Rel_sqlite3.exec_sql db s |> Rel_sqlite3.string_error |> Result.ok_or_failwith
      in
      sql (In_channel.read_all "fixtures/schema-v4.sql");
      sql
        "INSERT INTO \
         bank_import_attempts(institution,path,format,outcome,document_id,inserted_count,existing_count,error) \
         VALUES('old','missing','unknown','unreadable',NULL,0,0,'missing')";
      ignore (Rel_sqlite3.close db);
      ok
        (Store.with_open path (fun db ->
           check "history preserved" 1 (List.length (ok (Store.bank_import_history db)));
           accepted (import db "wise.xml" (xml ()));
           Ok ())))
;;

let migration_rollback () =
  let path =
    Filename_unix.temp_file ~in_dir:(Sys.getenv_exn "TMPDIR") "wise-rollback" ".sqlite3"
  in
  Exn.protect
    ~finally:(fun () -> Core_unix.unlink path)
    ~f:(fun () ->
      let raw f =
        let db =
          Rel_sqlite3.open' ~mode:Read_write_create path
          |> Rel_sqlite3.string_error
          |> Result.ok_or_failwith
        in
        Exn.protect ~finally:(fun () -> ignore (Rel_sqlite3.close db)) ~f:(fun () -> f db)
      in
      raw (fun db ->
        let sql s =
          Rel_sqlite3.exec_sql db s |> Rel_sqlite3.string_error |> Result.ok_or_failwith
        in
        sql (In_channel.read_all "fixtures/schema-v4.sql");
        sql "CREATE TABLE wise_statements(collision TEXT)");
      error (Store.with_open path (fun _ -> Ok ()));
      raw (fun db ->
        let query sql =
          Rel_sqlite3.first
            db
            (Rel_sql.Stmt.v sql ~rev_args:[] ~result:Row.(t1 (int "value")))
          |> Rel_sqlite3.string_error
          |> Result.ok_or_failwith
          |> Option.value_exn
        in
        check "schema version unchanged" 4 (query "PRAGMA user_version");
        check
          "history rebuild rolled back"
          0
          (query
             "SELECT count(*) FROM sqlite_master WHERE name='bank_import_attempts_v5'")))
;;

let () =
  Alcotest.run
    "Wise and multiple banks"
    [ ( "adapter"
      , List.map
          [ "parser and balances", parser
          ; "two banks, replay, conflict", multi_bank
          ; "currency and fee conflicts", currencies
          ; "duplicate CSV claims", duplicate_rows
          ; "schema four migration", migration
          ; "Wise migration rollback", migration_rollback
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
