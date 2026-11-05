open Base
open Stdio
open Rel
open Finances
open Finances_store

let ok = Or_error.ok_exn
let error result = Alcotest.(check bool) "rejected" true (Result.is_error result)
let fixture () = In_channel.read_all "fixtures/bank.ofx"
let join = Stdlib.Filename.concat
let database root = join root "bank.sqlite3"
let input_directory root = join root "statements"
let journal root = join root "bank.journal"

let rec remove path =
  match (Unix.LargeFile.lstat path).st_kind with
  | S_DIR ->
    Array.iter (Stdlib.Sys.readdir path) ~f:(fun name -> remove (join path name));
    Unix.rmdir path
  | _ -> Unix.unlink path
;;

let workspace f =
  let root =
    Stdlib.Filename.temp_dir ~temp_dir:(Sys.getenv_exn "TMPDIR") "finances-import" ""
  in
  Exn.protect
    ~finally:(fun () -> remove root)
    ~f:(fun () ->
      Unix.mkdir (input_directory root) 0o700;
      f root)
;;

let with_store root f =
  ok
    (Setup.with_open ~registry:Test_support.registry (database root) (fun db ->
       f db;
       Ok ()))
;;

let run_workflow root =
  Finances_import.Workflow.run
    ~registry:Builtin.registry
    ~database:(database root)
    ~institution:"boursobank"
    ~paths:[ input_directory root ]
    ~journal:(journal root)
    ()
;;

let write path contents = Out_channel.write_all path ~data:contents

let count outcome report =
  List.count report.Finances_import.Files.files ~f:(fun file ->
    Poly.equal file.outcome outcome)
;;

let raw path f =
  let db =
    Rel_sqlite3.open' ~mode:Read_write_create path
    |> Rel_sqlite3.string_error
    |> Result.ok_or_failwith
  in
  Exn.protect ~finally:(fun () -> ignore (Rel_sqlite3.close db)) ~f:(fun () -> f db)
;;

let sql db text =
  Rel_sqlite3.exec_sql db text |> Rel_sqlite3.string_error |> Result.ok_or_failwith
;;

let scalar db text =
  Rel_sqlite3.first db (Rel_sql.Stmt.v text ~rev_args:[] ~result:Row.(t1 (int "value")))
  |> Rel_sqlite3.string_error
  |> Result.ok_or_failwith
  |> Option.value_exn
;;

let source_count root expected =
  with_store root (fun db ->
    Alcotest.(check int) "source count" expected (List.length (ok (Ledger.events db))))
;;

let entry_count root expected =
  with_store root (fun db ->
    Alcotest.(check int) "entry count" expected (List.length (ok (Ledger.entries db))))
;;

let overlap () =
  String.substr_replace_first
    (fixture ())
    ~pattern:"<FITID>shared-id</FITID>"
    ~with_:"<FITID>another-real-payment</FITID>"
;;

let recursive_and_repeat () =
  workspace (fun root ->
    write (join (input_directory root) "a-renamed.txt") (fixture ());
    write (join (input_directory root) "m-broken.ofx") "not an OFX document";
    write (join (input_directory root) "categories.csv") "date;amount\n2026-01-03;-0.10\n";
    let nested = join (input_directory root) "nested" in
    Unix.mkdir nested 0o755;
    write (join nested "z-overlap.ofx") (overlap ());
    write (join nested "copy.ofx.part") (fixture ());
    write (join (input_directory root) ".hidden.ofx") (fixture ());
    Unix.symlink
      (join (input_directory root) "a-renamed.txt")
      (join (input_directory root) "link.ofx");
    let first = ok (run_workflow root) in
    Alcotest.(check int) "accepted files" 2 (count Imported first.inputs);
    Alcotest.(check int) "bad file isolated" 1 (count Rejected first.inputs);
    Alcotest.(check int) "CSV pending" 1 (count Unsupported first.inputs);
    Alcotest.(check int) "ignored paths" 3 (List.length first.inputs.ignored);
    Alcotest.(check int)
      "legitimate same-date/amount transaction retained"
      4
      first.projection.inserted;
    let original_journal = In_channel.read_all (journal root) in
    let second = ok (run_workflow root) in
    Alcotest.(check int) "identical files recognized" 2 (count Unchanged second.inputs);
    Alcotest.(check int) "no duplicate entries" 0 second.projection.inserted;
    Alcotest.(check string)
      "stable journal"
      original_journal
      (In_channel.read_all (journal root));
    source_count root 4;
    with_store root (fun db ->
      let history = ok (Import.bank_import_history db) in
      Alcotest.(check int) "all attempts persisted" 8 (List.length history);
      Alcotest.(check int)
        "history limit"
        2
        (List.length (ok (Import.bank_import_history ~limit:2 db)));
      error (Import.bank_import_history ~limit:0 db));
    raw (database root) (fun db ->
      Alcotest.(check int)
        "rejected and unsupported evidence archived without duplicate bytes"
        4
        (scalar db "SELECT count(*) FROM import_documents");
      error (Rel_sqlite3.exec_sql db "DELETE FROM bank_import_attempts");
      error (Rel_sqlite3.exec_sql db "UPDATE bank_import_attempts SET outcome='rejected'"));
    Alcotest.(check string)
      "input untouched"
      (fixture ())
      (In_channel.read_all (join (input_directory root) "a-renamed.txt")))
;;

let renamed_duplicates () =
  workspace (fun root ->
    write (join (input_directory root) "first.bin") (fixture ());
    write (join (input_directory root) "second.bin") (fixture ());
    let report = ok (run_workflow root) in
    Alcotest.(check int) "one accepted document" 1 (count Imported report.inputs);
    Alcotest.(check int) "renamed duplicate" 1 (count Unchanged report.inputs);
    source_count root 3;
    raw (database root) (fun db ->
      Alcotest.(check int)
        "one archive"
        1
        (scalar db "SELECT count(*) FROM import_documents")))
;;

let conflict_then_valid () =
  workspace (fun root ->
    write (join (input_directory root) "a.ofx") (fixture ());
    let conflict =
      fixture ()
      |> fun s ->
      String.substr_replace_first
        s
        ~pattern:"<STMTTRN>"
        ~with_:
          "<STMTTRN><DTPOSTED>20260102</DTPOSTED><TRNTYPE>CREDIT</TRNTYPE><TRNAMT>5</TRNAMT><FITID>must-roll-back</FITID></STMTTRN><STMTTRN>"
      |> fun s ->
      String.substr_replace_first
        s
        ~pattern:"<TRNAMT>-0.10</TRNAMT>"
        ~with_:"<TRNAMT>-0.11</TRNAMT>"
    in
    write (join (input_directory root) "b-conflict.ofx") conflict;
    write (join (input_directory root) "z.ofx") (overlap ());
    let report = ok (run_workflow root) in
    Alcotest.(check int) "conflict is durable" 1 (count Rejected report.inputs);
    Alcotest.(check int) "later valid file imported" 2 (count Imported report.inputs);
    source_count root 4;
    with_store root (fun db ->
      Alcotest.(check bool)
        "partial conflicting transactions absent"
        false
        (List.exists
           (ok (Ledger.events db))
           ~f:(fun (_, event) -> String.equal event.source_id "must-roll-back")));
    raw (database root) (fun db ->
      Alcotest.(check int)
        "conflicting bytes retained separately"
        3
        (scalar db "SELECT count(*) FROM import_documents")))
;;

let read_limit () =
  workspace (fun root ->
    write (join (input_directory root) "large.ofx") (fixture ());
    with_store root (fun db ->
      let report =
        ok
          (Finances_import.Files.scan
             ~max_file_bytes:32L
             db
             ~institution:"boursobank"
             ~paths:[ input_directory root ])
      in
      Alcotest.(check int) "oversize file recorded" 1 (count Unreadable report);
      let row = List.hd_exn report.files in
      Alcotest.(check bool)
        "unread bytes are not falsely archived"
        true
        (Option.is_none row.document_id && Option.is_none row.sha256)))
;;

let remap name (mapping : Bank_account_mapping.t) =
  ok
    (Bank_account_mapping.create
       ~institution:mapping.institution
       ~bank_id:mapping.bank_id
       ~account_id:mapping.account_id
       ~account_type:mapping.account_type
       ~ledger_account:("assets:bank:" ^ name))
;;

let config_json mappings =
  Bank_account_config.to_json
    (List.map mappings ~f:(fun (a : Bank_account_mapping.t) ->
       { Bank_account_config.reference =
           { institution = a.institution
           ; bank_id = a.bank_id
           ; account_id = a.account_id
           ; account_type = a.account_type
           }
       ; ledger_account = Some a.ledger_account
       ; iban = Keep
       ; settlement_account = Keep
       }))
;;

let import_settings root contents =
  Result.bind (Bank_account_config.of_json contents) ~f:(fun rows ->
    Setup.with_open ~registry:Test_support.registry (database root) (fun db ->
      Accounts.apply_bank_accounts db rows))
;;

let mappings_after_projection () =
  workspace (fun root ->
    write (join (input_directory root) "bank.ofx") (fixture ());
    ignore (ok (run_workflow root));
    let initial =
      ok
        (Setup.with_open
           ~registry:Test_support.registry
           (database root)
           Ledger.bank_accounts)
    in
    let mappings =
      List.mapi initial ~f:(fun i mapping ->
        remap (Printf.sprintf "account-%d" i) mapping)
    in
    let changes = ok (import_settings root (config_json mappings)) in
    let result = ok (run_workflow root) in
    Alcotest.(check int) "mappings persisted" 2 changes.mapping_changes;
    Alcotest.(check int) "renamed entries replay" 0 result.projection.inserted;
    Alcotest.(check int) "all entries reused" 3 result.projection.existing;
    let restored =
      ok
        (Setup.with_open
           ~registry:Test_support.registry
           (database root)
           Ledger.bank_accounts)
    in
    Alcotest.(check string)
      "mapping survives reopening"
      (Bank_account_mapping.to_json mappings)
      (Bank_account_mapping.to_json restored);
    Alcotest.(check bool)
      "journal has readable names"
      true
      (String.is_substring
         (In_channel.read_all (journal root))
         ~substring:"assets:bank:account-0");
    let repeated = ok (import_settings root (config_json mappings)) in
    Alcotest.(check int) "mapping replay is harmless" 0 repeated.mapping_changes;
    raw (database root) (fun db ->
      Alcotest.(check int)
        "audit only real changes"
        2
        (scalar db "SELECT count(*) FROM bank_account_mapping_changes"));
    with_store root (fun db ->
      ignore
        (ok
           (Ledger.set_bank_account_mappings
              db
              [ remap "renamed-again" (List.hd_exn mappings) ]));
      let current = ok (Ledger.bank_accounts db) in
      Alcotest.(check string)
        "omitted mapping retained"
        (List.last_exn mappings).ledger_account
        (List.last_exn current).ledger_account;
      let aliases = List.map current ~f:(remap "reviewed-alias") in
      ignore (ok (Ledger.set_bank_account_mappings db aliases));
      Alcotest.(check int)
        "reviewed aliases do not duplicate events"
        3
        (ok (Ledger.project_bank db)).existing);
    entry_count root 3)
;;

let mapping_errors () =
  workspace (fun root ->
    write (join (input_directory root) "bank.ofx") (fixture ());
    ignore (ok (run_workflow root));
    let mappings =
      ok
        (Setup.with_open
           ~registry:Test_support.registry
           (database root)
           Ledger.bank_accounts)
    in
    let first = List.hd_exn mappings in
    error (Bank_account_mapping.of_json (Bank_account_mapping.to_json [ first; first ]));
    let unknown =
      ok
        (Bank_account_mapping.create
           ~institution:first.institution
           ~bank_id:first.bank_id
           ~account_id:"unknown"
           ~account_type:first.account_type
           ~ledger_account:"assets:bank:unknown")
    in
    with_store root (fun db ->
      error (Ledger.set_bank_account_mappings db [ remap "first" first; unknown ]));
    raw (database root) (fun db ->
      (* Every account has its generated name from import; none may have been renamed. *)
      Alcotest.(check int)
        "unknown mapping rolls back earlier mapping"
        0
        (scalar
           db
           "SELECT count(*) FROM bank_account_mappings WHERE ledger_account LIKE \
            '%first%'"));
    error
      (Bank_account_mapping.create
         ~institution:first.institution
         ~bank_id:first.bank_id
         ~account_id:first.account_id
         ~account_type:first.account_type
         ~ledger_account:"expenses:wrong"))
;;

let mapping_conflict_preserves_journal () =
  workspace (fun root ->
    write (join (input_directory root) "bank.ofx") (fixture ());
    ignore (ok (run_workflow root));
    let before = In_channel.read_all (journal root) in
    let mappings =
      ok
        (Setup.with_open
           ~registry:Test_support.registry
           (database root)
           Ledger.bank_accounts)
      |> List.mapi ~f:(fun i m -> remap (Printf.sprintf "changed-%d" i) m)
    in
    raw (database root) (fun db ->
      sql
        db
        "UPDATE journal_entries SET description='manual change' WHERE id=(SELECT max(id) \
         FROM journal_entries)");
    error (import_settings root (config_json mappings));
    Alcotest.(check string)
      "previous journal intact"
      before
      (In_channel.read_all (journal root));
    raw (database root) (fun db ->
      Alcotest.(check int)
        "mapping transaction rolled back"
        0
        (scalar
           db
           "SELECT count(*) FROM bank_account_mappings WHERE ledger_account LIKE \
            '%changed-%'");
      Alcotest.(check int)
        "audit rolled back"
        0
        (scalar db "SELECT count(*) FROM bank_account_mapping_changes");
      Alcotest.(check int)
        "earlier posting renames rolled back"
        0
        (scalar
           db
           "SELECT count(*) FROM postings WHERE account LIKE 'assets:bank:changed-%'")))
;;

let path_protection () =
  workspace (fun root ->
    write (join (input_directory root) "bank.ofx") (fixture ());
    let run database directory journal =
      Finances_import.Workflow.run
        ~registry:Builtin.registry
        ~database
        ~institution:"boursobank"
        ~paths:[ directory ]
        ~journal
        ()
    in
    error (run (database root) (input_directory root) (database root));
    error
      (run
         (join (input_directory root) "database.sqlite3")
         (input_directory root)
         (journal root));
    error
      (run
         (database root)
         (input_directory root)
         (join (input_directory root) "out.journal"));
    ignore (ok (run_workflow root));
    let linked = join root "linked.journal" in
    Unix.link (database root) linked;
    error (run (database root) (input_directory root) linked);
    let input_link = join (input_directory root) "database-copy.bin" in
    Unix.link (database root) input_link;
    let report = ok (run_workflow root) in
    Alcotest.(check bool)
      "database hard links are not ingested"
      true
      (List.exists report.inputs.ignored ~f:(fun row ->
         String.equal row.reason "control file alias"));
    let symlink = join root "linked-statements" in
    Unix.symlink (input_directory root) symlink;
    error (run (database root) symlink (journal root));
    source_count root 3)
;;

let output_failure () =
  workspace (fun root ->
    write (join (input_directory root) "bank.ofx") (fixture ());
    write (journal root) "previous journal";
    let original = Sys.getenv_exn "TMPDIR" in
    Exn.protect
      ~finally:(fun () -> Unix.putenv "TMPDIR" original)
      ~f:(fun () ->
        Unix.putenv "TMPDIR" (join root "absent-staging");
        error (run_workflow root));
    Alcotest.(check string)
      "failed publication leaves old output"
      "previous journal"
      (In_channel.read_all (journal root));
    source_count root 3;
    entry_count root 3;
    let retry = ok (run_workflow root) in
    Alcotest.(check int) "publication retry reuses entries" 0 retry.projection.inserted;
    Alcotest.(check int)
      "eventual journal valid"
      3
      (Hledger.Parse.journal_exn (In_channel.read_all (journal root))
       |> Hledger.Journal.transactions
       |> List.length))
;;

let explicit_paths () =
  workspace (fun root ->
    let file = join (input_directory root) "bank.ofx" in
    write file (fixture ());
    let run paths =
      Finances_import.Workflow.run
        ~registry:Builtin.registry
        ~database:(database root)
        ~institution:"boursobank"
        ~paths
        ~journal:(journal root)
        ()
    in
    let report = ok (run [ input_directory root; file ]) in
    Alcotest.(check int) "imported once" 1 (count Imported report.inputs);
    Alcotest.(check (list string))
      "second visit reported"
      [ "already listed" ]
      (List.map report.inputs.ignored ~f:(fun i -> i.reason));
    let hidden = join root ".export.ofx" in
    write hidden (fixture ());
    let report = ok (run [ hidden ]) in
    Alcotest.(check (list string))
      "explicit hidden file processed"
      [ hidden ]
      (List.map report.inputs.files ~f:(fun f -> f.path));
    let link = join root "link" in
    Unix.symlink (input_directory root) link;
    error (run [ link ]);
    error (run [ join root "missing.ofx" ]);
    error (run [ database root ]);
    error (run []))
;;

let () =
  Alcotest.run
    "import workflow"
    [ ( "workflow"
      , List.map
          [ "nested files, failures and replay", recursive_and_repeat
          ; "renamed duplicate files", renamed_duplicates
          ; "conflicting file between valid files", conflict_then_valid
          ; "read limits", read_limit
          ; "persistent account mapping and aliases", mappings_after_projection
          ; "unknown and duplicate mappings", mapping_errors
          ; "mapping conflict preserves output", mapping_conflict_preserves_journal
          ; "control path protection", path_protection
          ; "explicit files and directories", explicit_paths
          ; "publication failure and retry", output_failure
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
