open Core
open Rel
open Finances

let ok = Or_error.ok_exn
let error result = Alcotest.(check bool) "rejected" true (Result.is_error result)
let fixture () = In_channel.read_all "fixtures/bank.ofx"
let join = Filename.concat
let database root = join root "bank.sqlite3"
let inbox root = join root "inbox"
let journal root = join root "bank.journal"

let rec remove path =
  match (Core_unix.lstat path).st_kind with
  | S_DIR ->
    Array.iter (Stdlib.Sys.readdir path) ~f:(fun name -> remove (join path name));
    Core_unix.rmdir path
  | _ -> Core_unix.unlink path
;;

let workspace f =
  let root =
    Filename_unix.temp_dir ~in_dir:(Sys.getenv_exn "TMPDIR") "finances-inbox" ""
  in
  Exn.protect
    ~finally:(fun () -> remove root)
    ~f:(fun () ->
      Core_unix.mkdir ~perm:0o700 (inbox root);
      f root)
;;

let with_store root f =
  ok
    (Store.with_open (database root) (fun db ->
       f db;
       Ok ()))
;;

let sync ?accounts root =
  Finances_bank.Sync.run
    ?accounts
    ~database:(database root)
    ~institution:"boursobank"
    ~directory:(inbox root)
    ~journal:(journal root)
    ()
;;

let write path contents = Out_channel.write_all path ~data:contents

let count outcome report =
  List.count report.Finances_bank.Inbox.files ~f:(fun file ->
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
    Alcotest.(check int) "source count" expected (List.length (ok (Store.events db))))
;;

let entry_count root expected =
  with_store root (fun db ->
    Alcotest.(check int) "entry count" expected (List.length (ok (Store.entries db))))
;;

let overlap () =
  String.substr_replace_first
    (fixture ())
    ~pattern:"<FITID>shared-id</FITID>"
    ~with_:"<FITID>another-real-payment</FITID>"
;;

let recursive_and_repeat () =
  workspace (fun root ->
    write (join (inbox root) "a-renamed.txt") (fixture ());
    write (join (inbox root) "m-broken.ofx") "not an OFX document";
    write (join (inbox root) "categories.csv") "date;amount\n2026-01-03;-0.10\n";
    let nested = join (inbox root) "nested" in
    Core_unix.mkdir nested;
    write (join nested "z-overlap.ofx") (overlap ());
    write (join nested "copy.ofx.part") (fixture ());
    write (join (inbox root) ".hidden.ofx") (fixture ());
    Core_unix.symlink
      ~target:(join (inbox root) "a-renamed.txt")
      ~link_name:(join (inbox root) "link.ofx");
    let first = ok (sync root) in
    Alcotest.(check int) "accepted files" 2 (count Imported first.inbox);
    Alcotest.(check int) "bad file isolated" 1 (count Rejected first.inbox);
    Alcotest.(check int) "CSV pending" 1 (count Unsupported first.inbox);
    Alcotest.(check int) "ignored paths" 3 (List.length first.inbox.ignored);
    Alcotest.(check int)
      "legitimate same-date/amount transaction retained"
      4
      first.projection.inserted;
    let original_journal = In_channel.read_all (journal root) in
    let second = ok (sync root) in
    Alcotest.(check int) "identical files recognized" 2 (count Unchanged second.inbox);
    Alcotest.(check int) "no duplicate entries" 0 second.projection.inserted;
    Alcotest.(check string)
      "stable journal"
      original_journal
      (In_channel.read_all (journal root));
    source_count root 4;
    with_store root (fun db ->
      let history = ok (Store.bank_import_history db) in
      Alcotest.(check int) "all attempts persisted" 8 (List.length history);
      Alcotest.(check int)
        "history limit"
        2
        (List.length (ok (Store.bank_import_history ~limit:2 db)));
      error (Store.bank_import_history ~limit:0 db));
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
      (In_channel.read_all (join (inbox root) "a-renamed.txt")))
;;

let renamed_duplicates () =
  workspace (fun root ->
    write (join (inbox root) "first.bin") (fixture ());
    write (join (inbox root) "second.bin") (fixture ());
    let report = ok (sync root) in
    Alcotest.(check int) "one accepted document" 1 (count Imported report.inbox);
    Alcotest.(check int) "renamed duplicate" 1 (count Unchanged report.inbox);
    source_count root 3;
    raw (database root) (fun db ->
      Alcotest.(check int)
        "one archive"
        1
        (scalar db "SELECT count(*) FROM import_documents")))
;;

let conflict_then_valid () =
  workspace (fun root ->
    write (join (inbox root) "a.ofx") (fixture ());
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
    write (join (inbox root) "b-conflict.ofx") conflict;
    write (join (inbox root) "z.ofx") (overlap ());
    let report = ok (sync root) in
    Alcotest.(check int) "conflict is durable" 1 (count Rejected report.inbox);
    Alcotest.(check int) "later valid file imported" 2 (count Imported report.inbox);
    source_count root 4;
    with_store root (fun db ->
      Alcotest.(check bool)
        "partial conflicting transactions absent"
        false
        (List.exists
           (ok (Store.events db))
           ~f:(fun (_, event) -> String.equal event.source_id "must-roll-back")));
    raw (database root) (fun db ->
      Alcotest.(check int)
        "conflicting bytes retained separately"
        3
        (scalar db "SELECT count(*) FROM import_documents")))
;;

let read_limit () =
  workspace (fun root ->
    write (join (inbox root) "large.ofx") (fixture ());
    with_store root (fun db ->
      let report =
        ok
          (Finances_bank.Inbox.scan
             ~max_file_bytes:32L
             db
             ~institution:"boursobank"
             ~directory:(inbox root))
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

let mappings_after_projection () =
  workspace (fun root ->
    write (join (inbox root) "bank.ofx") (fixture ());
    ignore (ok (sync root));
    let initial = ok (Store.with_open (database root) Store.bank_accounts) in
    let mappings =
      List.mapi initial ~f:(fun i mapping -> remap (sprintf "account-%d" i) mapping)
    in
    let config = join root "accounts.json" in
    write config (Bank_account_mapping.to_json mappings);
    let result = ok (sync ~accounts:config root) in
    Alcotest.(check int) "mappings persisted" 2 result.mapping_changes;
    Alcotest.(check int) "renamed entries replay" 0 result.projection.inserted;
    Alcotest.(check int) "all entries reused" 3 result.projection.existing;
    let restored = ok (Store.with_open (database root) Store.bank_accounts) in
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
    let repeated = ok (sync ~accounts:config root) in
    Alcotest.(check int) "mapping replay is harmless" 0 repeated.mapping_changes;
    raw (database root) (fun db ->
      Alcotest.(check int)
        "audit only real changes"
        2
        (scalar db "SELECT count(*) FROM bank_account_mapping_changes"));
    with_store root (fun db ->
      ignore
        (ok
           (Store.set_bank_account_mappings
              db
              [ remap "renamed-again" (List.hd_exn mappings) ]));
      let current = ok (Store.bank_accounts db) in
      Alcotest.(check string)
        "omitted mapping retained"
        (List.last_exn mappings).ledger_account
        (List.last_exn current).ledger_account;
      let aliases = List.map current ~f:(remap "reviewed-alias") in
      ignore (ok (Store.set_bank_account_mappings db aliases));
      Alcotest.(check int)
        "reviewed aliases do not duplicate events"
        3
        (ok (Store.project_bank db)).existing);
    entry_count root 3)
;;

let mapping_errors () =
  workspace (fun root ->
    write (join (inbox root) "bank.ofx") (fixture ());
    ignore (ok (sync root));
    let mappings = ok (Store.with_open (database root) Store.bank_accounts) in
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
      error (Store.set_bank_account_mappings db [ remap "first" first; unknown ]));
    raw (database root) (fun db ->
      Alcotest.(check int)
        "unknown mapping rolls back earlier mapping"
        0
        (scalar db "SELECT count(*) FROM bank_account_mappings"));
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
    write (join (inbox root) "bank.ofx") (fixture ());
    ignore (ok (sync root));
    let before = In_channel.read_all (journal root) in
    let mappings =
      ok (Store.with_open (database root) Store.bank_accounts)
      |> List.mapi ~f:(fun i m -> remap (sprintf "changed-%d" i) m)
    in
    let config = join root "accounts.json" in
    write config (Bank_account_mapping.to_json mappings);
    raw (database root) (fun db ->
      sql
        db
        "UPDATE journal_entries SET description='manual change' WHERE id=(SELECT max(id) \
         FROM journal_entries)");
    error (sync ~accounts:config root);
    Alcotest.(check string)
      "previous journal intact"
      before
      (In_channel.read_all (journal root));
    raw (database root) (fun db ->
      Alcotest.(check int)
        "mapping transaction rolled back"
        0
        (scalar db "SELECT count(*) FROM bank_account_mappings");
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
    write (join (inbox root) "bank.ofx") (fixture ());
    let run database directory journal =
      Finances_bank.Sync.run ~database ~institution:"boursobank" ~directory ~journal ()
    in
    error (run (database root) (inbox root) (database root));
    error (run (join (inbox root) "database.sqlite3") (inbox root) (journal root));
    error (run (database root) (inbox root) (join (inbox root) "out.journal"));
    ignore (ok (sync root));
    let linked = join root "linked.journal" in
    Core_unix.link ~target:(database root) ~link_name:linked ();
    error (run (database root) (inbox root) linked);
    let input_link = join (inbox root) "database-copy.bin" in
    Core_unix.link ~target:(database root) ~link_name:input_link ();
    let report = ok (sync root) in
    Alcotest.(check bool)
      "database hard links are not ingested"
      true
      (List.exists report.inbox.ignored ~f:(fun row ->
         String.equal row.reason "control file alias"));
    let symlink = join root "linked-inbox" in
    Core_unix.symlink ~target:(inbox root) ~link_name:symlink;
    error (run (database root) symlink (journal root));
    source_count root 3)
;;

let invalid_config_before_import () =
  workspace (fun root ->
    write (join (inbox root) "bank.ofx") (fixture ());
    write (journal root) "previous journal";
    let config = join root "accounts.json" in
    write config "not JSON";
    error (sync ~accounts:config root);
    Alcotest.(check bool)
      "no import before validating config"
      false
      (Stdlib.Sys.file_exists (database root));
    Alcotest.(check string)
      "existing output preserved"
      "previous journal"
      (In_channel.read_all (journal root)))
;;

let output_failure () =
  workspace (fun root ->
    write (join (inbox root) "bank.ofx") (fixture ());
    write (journal root) "previous journal";
    let original = Sys.getenv_exn "TMPDIR" in
    Exn.protect
      ~finally:(fun () -> Core_unix.putenv ~key:"TMPDIR" ~data:original)
      ~f:(fun () ->
        Core_unix.putenv ~key:"TMPDIR" ~data:(join root "absent-staging");
        error (sync root));
    Alcotest.(check string)
      "failed publication leaves old output"
      "previous journal"
      (In_channel.read_all (journal root));
    source_count root 3;
    entry_count root 3;
    let retry = ok (sync root) in
    Alcotest.(check int) "publication retry reuses entries" 0 retry.projection.inserted;
    Alcotest.(check int)
      "eventual journal valid"
      3
      (Hledger.Parse.journal_exn (In_channel.read_all (journal root))
       |> Hledger.Journal.transactions
       |> List.length))
;;

let migration_v2 () =
  workspace (fun root ->
    raw (database root) (fun db ->
      sql db (In_channel.read_all "fixtures/schema-v2.sql");
      sql
        db
        "INSERT INTO \
         source_events(id,venue,account,kind,source_id,occurred_at_ms,payload) \
         VALUES(42,'paradex','main','test','legacy',1234,x'00ff')");
    with_store root (fun db ->
      let id, event = List.hd_exn (ok (Store.events db)) in
      Alcotest.(check int64) "legacy ID preserved" 42L id;
      Alcotest.(check string) "legacy raw bytes preserved" "\000\255" event.payload;
      Alcotest.(check int)
        "new history starts empty"
        0
        (List.length (ok (Store.bank_import_history db))));
    raw (database root) (fun db ->
      Alcotest.(check int) "version 8" 8 (scalar db "PRAGMA user_version")))
;;

let failed_migration_v2 () =
  workspace (fun root ->
    raw (database root) (fun db ->
      sql db (In_channel.read_all "fixtures/schema-v2.sql");
      sql db "CREATE TABLE bank_account_mappings(collision TEXT)");
    error (Store.with_open (database root) (fun _ -> Ok ()));
    raw (database root) (fun db ->
      Alcotest.(check int) "version unchanged" 2 (scalar db "PRAGMA user_version");
      Alcotest.(check int)
        "new history creation rolled back"
        0
        (scalar db "SELECT count(*) FROM sqlite_master WHERE name='bank_import_attempts'")))
;;

let () =
  Alcotest.run
    "bank inbox"
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
          ; "invalid configuration before import", invalid_config_before_import
          ; "publication failure and retry", output_failure
          ; "version 2 migration", migration_v2
          ; "version 2 migration rollback", failed_migration_v2
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
