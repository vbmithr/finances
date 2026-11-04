open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let status database json_output =
  let module Status = Finances_import.Bank_status in
  let%bind report = with_database database Status.read in
  let banks =
    List.map report.accounts ~f:(fun a -> a.Transfers.mapping.institution)
    |> List.dedup_and_sort ~compare:String.compare
    |> List.length
  in
  if json_output
  then Report_json.print Report_json.status (database, banks, report)
  else (
    printf "Database: %s\n\n" (Terminal.text database);
    printf
      "%d banks, %d accounts, %d imported movements\n"
      banks
      (List.length report.accounts)
      report.movements;
    if List.is_empty report.accounts && List.is_empty report.operations
    then print_endline "\nNo records yet. See fin import --help.";
    if not (List.is_empty report.accounts)
    then (
      print_endline "";
      Terminal.table
        [ "Bank"; "Account"; "Currency"; "Movements"; "First observed"; "Last observed" ]
        (List.map report.accounts ~f:(fun a ->
           [ a.Transfers.mapping.institution
           ; a.mapping.ledger_account
           ; Iso4217.to_alpha_code a.currency
           ; Int.to_string a.movements
           ; Date.to_string a.first_on
           ; Date.to_string a.last_on
           ])));
    if not (List.is_empty report.operations)
    then (
      print_endline "\nOperations without a bank account";
      Terminal.table
        [ "Institution"; "Operations"; "Unresolved"; "First observed"; "Last observed" ]
        (List.map
           report.operations
           ~f:(fun (institution, (s : Explain.operation_summary)) ->
             [ institution
             ; Int.to_string s.operations
             ; Int.to_string s.unresolved
             ; Option.value s.first_on ~default:"-"
             ; Option.value s.last_on ~default:"-"
             ])));
    printf
      "\nReview: %d unclassified movements; %d unreviewed transfer candidate pairs\n"
      report.unclassified_movements
      report.transfer_candidate_pairs;
    (match report.latest_import with
     | None -> print_endline "Latest inbox import: none recorded."
     | Some a ->
       printf
         "Latest inbox import: %s  %s  %s\n  %s\n"
         (Timestamp.to_rfc3339 a.attempted_at)
         (Terminal.text a.institution)
         (Bank_import.outcome_name a.outcome)
         (Terminal.text a.path);
       Option.iter a.message ~f:(fun message -> printf "  %s\n" (Terminal.text message)));
    print_endline
      "\n\
       Date ranges show observed records, not complete statement coverage. Financial \
       reports: fin journal export, then query with hledger.";
    print_endline
      "Next: fin operation review; fin transfer candidates; fin institution history.");
  Ok ()
;;

let cmd =
  command
    "status"
    "Show observed account coverage, import history and review progress; no balances are \
     inferred."
    Term.(const status $ database $ json_output)
;;
