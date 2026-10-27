open Base
open Stdio
open Rel
open Finances

let ok = Or_error.ok_exn
let error value = Alcotest.(check bool) "rejected" true (Result.is_error value)
let ofx () = In_channel.read_all "fixtures/bank.ofx"
let csv () = In_channel.read_all "fixtures/bank.csv"
let fields () = List.map (ok (Bank_csv.parse (csv ()))) ~f:(fun r -> r.fields)

let set row column value =
  List.mapi row ~f:(fun i old -> if i = column then value else old)
;;

let encode rows =
  let quote s =
    if String.exists s ~f:(fun c -> List.mem [ ';'; '"'; '\r'; '\n' ] c ~equal:Char.equal)
    then "\"" ^ String.substr_replace_all s ~pattern:"\"" ~with_:"\"\"" ^ "\""
    else s
  in
  let header = List.hd_exn (String.split_lines (csv ())) in
  header
  ^ "\n"
  ^ String.concat
      ~sep:"\n"
      (List.map rows ~f:(fun row -> String.concat ~sep:";" (List.map row ~f:quote)))
  ^ "\n"
;;

let rec remove path =
  match (Unix.LargeFile.lstat path).st_kind with
  | S_DIR ->
    Array.iter (Stdlib.Sys.readdir path) ~f:(fun name ->
      remove (Stdlib.Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path
;;

let workspace f =
  let root =
    Stdlib.Filename.temp_dir ~temp_dir:(Sys.getenv_exn "TMPDIR") "finances-csv" ""
  in
  Exn.protect ~finally:(fun () -> remove root) ~f:(fun () -> f root)
;;

let database root = Stdlib.Filename.concat root "bank.sqlite3"

let with_store root f =
  ok
    (Store.with_open ~registry:Builtin.registry (database root) (fun db ->
       f db;
       Ok ()))
;;

let import_ofx ?(institution = "boursobank") root contents =
  let document = ok (Bank_ofx.parse contents) in
  ok
    (Store.with_open ~registry:Builtin.registry (database root) (fun db ->
       Store.import_ofx db ~institution document))
;;

let import_csv root contents =
  ok
    (Store.with_open ~registry:Builtin.registry (database root) (fun db ->
       Store.import_bank_file db ~institution:"boursobank" ~path:"export.csv" ~contents))
;;

let match_csv root =
  ok (Store.with_open ~registry:Builtin.registry (database root) Store.match_bank_csv)
;;

let export root =
  ignore
    (ok (Store.with_open ~registry:Builtin.registry (database root) Store.project_bank));
  ok (Store.with_open ~registry:Builtin.registry (database root) Finances_hledger.export)
;;

let raw root f =
  let db =
    Rel_sqlite3.open' ~mode:Read_write_create (database root)
    |> Rel_sqlite3.string_error
    |> Result.ok_or_failwith
  in
  Exn.protect ~finally:(fun () -> ignore (Rel_sqlite3.close db)) ~f:(fun () -> f db)
;;

let scalar db text =
  Rel_sqlite3.first db (Rel_sql.Stmt.v text ~rev_args:[] ~result:Row.(t1 (int "value")))
  |> Rel_sqlite3.string_error
  |> Result.ok_or_failwith
  |> Option.value_exn
;;

let count_sources root expected =
  with_store root (fun db ->
    Alcotest.(check int)
      "only OFX creates movements"
      expected
      (List.length (ok (Store.events db))))
;;

let transactions journal =
  Hledger.Parse.journal_exn journal |> Hledger.Journal.transactions
;;

let parser () =
  let contents =
    "\239\187\191" ^ String.substr_replace_all (csv ()) ~pattern:"\n" ~with_:"\r\n"
  in
  let rows = ok (Bank_csv.parse contents) in
  Alcotest.(check int) "quoted multiline records" 3 (List.length rows);
  let row = List.hd_exn rows in
  Alcotest.(check bool)
    "first Solde is amount"
    true
    (Quantity.equal row.amount (Quantity.of_string "-0.10"));
  Alcotest.(check bool)
    "second Solde is balance"
    true
    (Quantity.equal (Option.value_exn row.reported_balance) (Quantity.of_string "100.01"));
  Alcotest.(check string)
    "value date"
    "2026-01-04"
    (Date.to_string (Option.value_exn row.value_on));
  Alcotest.(check bool)
    "quoted semicolon retained"
    true
    (String.is_substring (Option.value_exn row.comment) ~substring:"source; note");
  let large = List.nth_exn rows 1 in
  Alcotest.(check bool)
    "all amount digits preserved"
    true
    (Quantity.equal
       large.amount
       (Quantity.of_string "12345678901234567890.1234567890123456789"));
  let grouped =
    fields ()
    |> List.mapi ~f:(fun i r -> if i = 0 then set r 6 "-1\226\128\175234,56" else r)
    |> encode
    |> Bank_csv.parse
    |> ok
  in
  Alcotest.(check bool)
    "French digit grouping"
    true
    (Quantity.equal (List.hd_exn grouped).amount (Quantity.of_string "-1234.56"))
;;

let malformed () =
  List.iter
    [ 6, "1 2,00"; 6, "=1+1"; 6, "1.234,56"; 1, "2026-02-30"; 10, "100,01" ]
    ~f:(fun (column, value) ->
      let changed =
        fields ()
        |> List.mapi ~f:(fun i row -> if i = 0 then set row column value else row)
        |> encode
      in
      error (Bank_csv.parse changed));
  error (Bank_csv.parse (csv () ^ "\255"));
  error (Bank_csv.parse (encode [ List.drop_last_exn (List.hd_exn (fields ())) ]));
  Alcotest.(check bool)
    "other CSV layouts are not claimed"
    false
    (Bank_csv.recognizes "date;amount\n2026-01-01;1\n")
;;

let exact_enrichment () =
  workspace (fun root ->
    ignore (import_ofx root (ofx ()));
    let original = export root in
    let result = import_csv root (csv ()) in
    Alcotest.(check int) "CSV adds no source events" 0 result.inserted;
    let matched = match_csv root in
    Alcotest.(check int) "all unique rows matched" 3 matched.matched;
    count_sources root 3;
    let journal = export root in
    let before = transactions original
    and after = transactions journal in
    List.iter2_exn before after ~f:(fun a b ->
      Alcotest.(check bool)
        "postings unchanged by enrichment"
        true
        (List.equal Hledger.Journal.equal_posting a.postings b.postings);
      Alcotest.(check bool) "booking date unchanged" true (Date.equal a.date b.date);
      Alcotest.(check bool)
        "no clearing inferred from CSV"
        true
        (Poly.equal b.status Hledger.Journal.Unmarked));
    Alcotest.(check string)
      "CSV date becomes date2"
      "2026-01-04"
      (Date.to_string (Option.value_exn (List.hd_exn after).date2));
    Alcotest.(check bool)
      "bank category remains suggestion tag"
      true
      (String.is_substring journal ~substring:"bank-category: Food， drinks");
    Alcotest.(check bool)
      "unclassified accounting retained"
      true
      (String.is_substring journal ~substring:"equity:unclassified");
    let repeated = import_csv root (csv ()) in
    Alcotest.(check bool)
      "same CSV recognized"
      true
      (Poly.equal repeated.outcome Bank_import.Unchanged);
    let second = match_csv root in
    Alcotest.(check int) "no duplicate match audit" 0 second.changed;
    Alcotest.(check string) "repeat journal stable" journal (export root);
    raw root (fun db ->
      Alcotest.(check int)
        "only three observations"
        3
        (scalar db "SELECT count(*) FROM companion_rows WHERE format='csv'");
      Alcotest.(check int)
        "only initial audit"
        3
        (scalar db "SELECT count(*) FROM companion_match_changes");
      error (Rel_sqlite3.exec_sql db "UPDATE companion_rows SET fields_json='[]'");
      error (Rel_sqlite3.exec_sql db "DELETE FROM companion_match_changes")))
;;

let metadata_tag_safety () =
  workspace (fun root ->
    ignore (import_ofx root (ofx ()));
    let changed =
      fields ()
      |> List.mapi ~f:(fun i row ->
        if i = 0 then set row 4 "Food, source-event-id: 999" else row)
      |> encode
    in
    ignore (import_csv root changed);
    ignore (match_csv root);
    let journal = export root in
    Alcotest.(check bool)
      "source metadata cannot introduce another provenance tag"
      false
      (String.is_substring journal ~substring:"source-event-id: 999");
    Alcotest.(check bool)
      "metadata colon retained visibly"
      true
      (String.is_substring journal ~substring:"source-event-id： 999"))
;;

let csv_first () =
  workspace (fun root ->
    ignore (import_csv root (csv ()));
    let first = match_csv root in
    Alcotest.(check int) "awaiting OFX" 3 first.unmatched;
    count_sources root 0;
    ignore (import_ofx root (ofx ()));
    let second = match_csv root in
    Alcotest.(check int) "linked after OFX arrives" 3 second.matched;
    Alcotest.(check int) "state transition audited" 3 second.changed;
    count_sources root 3;
    raw root (fun db ->
      Alcotest.(check int)
        "both observations of matching state"
        6
        (scalar db "SELECT count(*) FROM companion_match_changes")))
;;

let duplicates_within_csv () =
  workspace (fun root ->
    ignore (import_ofx root (ofx ()));
    let rows = fields () in
    ignore (import_csv root (encode (List.hd_exn rows :: rows)));
    let report = match_csv root in
    Alcotest.(check int)
      "duplicate CSV rows cannot collapse onto one movement"
      2
      report.ambiguous;
    Alcotest.(check int) "other rows matched" 2 report.matched;
    count_sources root 3)
;;

let new_ambiguous_evidence () =
  workspace (fun root ->
    ignore (import_ofx root (ofx ()));
    ignore (import_csv root (csv ()));
    Alcotest.(check int) "initially unique" 3 (match_csv root).matched;
    let duplicate =
      String.substr_replace_first
        (ofx ())
        ~pattern:"<FITID>shared-id</FITID>"
        ~with_:"<FITID>another-payment</FITID>"
    in
    ignore (import_ofx root duplicate);
    let result = match_csv root in
    Alcotest.(check int) "later identical OFX record forces review" 1 result.ambiguous;
    Alcotest.(check int) "only changed matching audited" 1 result.changed;
    count_sources root 4;
    let journal = export root |> transactions in
    Alcotest.(check bool)
      "ambiguous value date removed from derived journal"
      true
      (Option.is_none (List.hd_exn journal).date2))
;;

let metadata_conflict () =
  workspace (fun root ->
    ignore (import_ofx root (ofx ()));
    ignore (import_csv root (csv ()));
    ignore (match_csv root);
    let changed =
      fields ()
      |> List.mapi ~f:(fun i row ->
        if i = 0 then set (set row 1 "2026-01-05") 4 "Changed suggestion" else row)
      |> encode
    in
    ignore (import_csv root changed);
    let result = match_csv root in
    Alcotest.(check int) "both conflicting observations need review" 2 result.conflicts;
    Alcotest.(check int) "compatible observations still usable" 4 result.matched;
    Alcotest.(check bool)
      "conflicting value date not exported"
      true
      (Option.is_none (List.hd_exn (transactions (export root))).date2);
    count_sources root 3)
;;

let overlap_and_scope () =
  workspace (fun root ->
    ignore (import_ofx root (ofx ()));
    ignore (import_ofx ~institution:"other-bank" root (ofx ()));
    ignore (import_csv root (csv ()));
    let changed =
      fields ()
      |> List.map ~f:(fun row ->
        set (set (set row 9 "New display name") 10 "999.99") 11 "Oui")
      |> encode
    in
    ignore (import_csv root changed);
    let report = match_csv root in
    Alcotest.(check int)
      "overlapping observations reuse original movements"
      6
      report.matched;
    Alcotest.(check int) "balance observations do not conflict" 0 report.conflicts;
    with_store root (fun db ->
      List.iter
        (ok (Store.bank_csv_observations db))
        ~f:(fun row ->
          let matching = Option.value_exn row.matching in
          Alcotest.(check bool)
            "institution scopes OFX candidates"
            true
            Int64.(Option.value_exn matching.event_id <= 3L)));
    count_sources root 6)
;;

let description_mismatch () =
  workspace (fun root ->
    ignore (import_ofx root (ofx ()));
    let changed =
      fields ()
      |> List.mapi ~f:(fun i row -> if i = 0 then set row 2 "Different merchant" else row)
      |> encode
    in
    ignore (import_csv root changed);
    let result = match_csv root in
    Alcotest.(check int) "date and amount alone are insufficient" 1 result.unmatched;
    Alcotest.(check int) "other matches remain" 2 result.matched)
;;

let malformed_file_atomic () =
  workspace (fun root ->
    ignore (import_ofx root (ofx ()));
    let changed =
      fields ()
      |> List.mapi ~f:(fun i row -> if i = 2 then set row 1 "2026-02-30" else row)
      |> encode
    in
    let result = import_csv root changed in
    Alcotest.(check bool)
      "bad late row rejects entire document"
      true
      (Poly.equal result.outcome Bank_import.Rejected);
    raw root (fun db ->
      Alcotest.(check int)
        "no partial CSV rows"
        0
        (scalar db "SELECT count(*) FROM companion_rows WHERE format='csv'");
      Alcotest.(check int)
        "failed input archived"
        2
        (scalar db "SELECT count(*) FROM import_documents"));
    count_sources root 3)
;;

let folder_order () =
  workspace (fun root ->
    let inbox = Stdlib.Filename.concat root "inbox" in
    Unix.mkdir inbox 0o755;
    Out_channel.write_all (Stdlib.Filename.concat inbox "a.csv") ~data:(csv ());
    Out_channel.write_all (Stdlib.Filename.concat inbox "z.ofx") ~data:(ofx ());
    let run () =
      ok
        (Finances_bank.Sync.run
           ~registry:Builtin.registry
           ~database:(database root)
           ~institution:"boursobank"
           ~directory:inbox
           ~journal:(Stdlib.Filename.concat root "bank.journal")
           ())
    in
    let first = run () in
    Alcotest.(check int) "CSV before OFX in one sync still matches" 3 first.csv.matched;
    Alcotest.(check int) "only OFX creates entries" 3 first.projection.inserted;
    let second = run () in
    Alcotest.(check int) "sync replay stable" 0 second.csv.changed;
    Alcotest.(check int) "no new entries" 0 second.projection.inserted)
;;

let () =
  Alcotest.run
    "bank CSV"
    [ ( "enrichment"
      , List.map
          [ "UTF-8, quoted fields and exact numbers", parser
          ; "malformed CSV fields", malformed
          ; "exact enrichment and replay", exact_enrichment
          ; "metadata tag syntax safety", metadata_tag_safety
          ; "CSV arrives before OFX", csv_first
          ; "duplicate CSV rows", duplicates_within_csv
          ; "new OFX evidence introduces ambiguity", new_ambiguous_evidence
          ; "conflicting metadata", metadata_conflict
          ; "overlap and institution scope", overlap_and_scope
          ; "description mismatch", description_mismatch
          ; "malformed document rollback", malformed_file_atomic
          ; "folder order independent", folder_order
          ]
          ~f:(fun (name, f) -> Alcotest.test_case name `Quick f) )
    ]
;;
