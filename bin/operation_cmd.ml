open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let refresh_transaction_evidence database institution yes =
  with_database database (fun db ->
    let%bind changes = Ledger.preview_source_evidence ?institution db in
    let describe (e : Source_evidence.t) =
      Printf.sprintf
        "card=%s reversal=%s"
        (Option.value_map e.card_reference ~default:"-" ~f:Terminal.text)
        (if e.withdrawal_reversal then "yes" else "no")
    in
    if List.is_empty changes
    then (
      print_endline "No source evidence changes.";
      Ok ())
    else (
      printf "Proposed source evidence changes: %d\n" (List.length changes);
      List.iter changes ~f:(fun (change : Ledger.source_evidence_change) ->
        printf
          "  #%Ld  %s  %s -> %s  %s\n"
          change.event_id
          (Terminal.text change.institution)
          (describe change.before)
          (describe change.after)
          (Terminal.text change.description));
      let%bind confirmed =
        confirm ~yes ~destructive:false "Apply these source evidence changes?"
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%map count =
          Ledger.refresh_source_evidence ?institution db ~expected:changes
        in
        printf "source_evidence_changed=%d\n" count)))
;;

let refresh_transaction_evidence_cmd =
  command
    "refresh-evidence"
    "Preview and refresh imported settlement and reversal evidence."
    Term.(
      const refresh_transaction_evidence
      $ database
      $ Statement_cmd.institution_filter
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Apply the preview without prompting."))
;;

let print_fields indent fields =
  let width =
    List.fold fields ~init:0 ~f:(fun acc (name, _) -> Int.max acc (String.length name))
  in
  List.iter fields ~f:(fun (name, value) ->
    printf "%s%-*s  %s\n" indent width (Terminal.text name) (Terminal.text value))
;;

let print_document indent (d : Transaction_detail.document) =
  printf
    "%sdocument #%Ld, sha256 %s, imported %s\n"
    indent
    d.id
    (String.prefix d.sha256 12)
    d.imported_at;
  List.iter d.paths ~f:(fun path -> printf "%s  file: %s\n" indent (Terminal.text path))
;;

let print_transaction_detail (t : Transaction_detail.t) =
  let e = t.explanation in
  printf
    "Transaction #%Ld  %s  %s  %s %s\n"
    e.event_id
    (Terminal.text e.institution)
    e.booked_on
    e.amount
    e.currency;
  List.iter e.descriptions ~f:(fun d -> printf "  Description: %s\n" (Terminal.text d));
  printf
    "  Type: %s (%s)\n"
    (Rule_test_cmd.type_name e.effective)
    (match e.source with
     | Manual_override -> "manual override"
     | Rules -> "type rules"
     | Untyped -> "nothing decides it");
  printf
    "  Category: %s\n"
    (match e.category with
     | Some (account, reason) ->
       Printf.sprintf "%s (%s)" (Terminal.text account) (Terminal.text reason)
     | None -> "none recorded");
  Option.iter t.transfer ~f:(fun (first, second, reason) ->
    printf
      "  Transfer: confirmed with #%Ld (%s)\n"
      (if Int64.equal first e.event_id then second else first)
      (Terminal.text reason));
  print_endline "\nSource";
  print_fields
    "  "
    [ "institution", t.venue
    ; "account", t.account
    ; "account type", e.account_type
    ; "kind", t.kind
    ; "source id", t.source_id
    ; "occurred", t.occurred
    ; "retrieved", t.retrieved_at
    ];
  print_endline "\nImported from";
  if List.is_empty t.documents
  then print_endline "  no stored document"
  else List.iter t.documents ~f:(print_document "  ");
  List.iter t.statements ~f:(fun s ->
    printf
      "  statement %s: account %s %s, %s, balance %s to %s\n"
      (Option.value_map s.reference ~default:"(no reference)" ~f:Terminal.text)
      (Terminal.text s.source_account)
      s.currency
      s.period
      s.opening
      s.closing);
  print_endline "\nSource fields (payload)";
  String.split_lines t.payload
  |> List.iter ~f:(fun line -> printf "  %s\n" (Terminal.text line));
  if not (List.is_empty t.fields)
  then (
    print_endline "\nSource columns (stored, matchable by rules)";
    print_fields "  " t.fields);
  (match t.companions with
   | [] -> print_endline "\nCompanion files: no matched rows"
   | companions ->
     printf "\nCompanion files: %d matched\n" (List.length companions);
     List.iter companions ~f:(fun c ->
       printf
         "  %s row %d, %s (%s: %s)\n"
         (Terminal.text c.format)
         c.row_number
         c.state
         (Terminal.text c.policy)
         (Terminal.text c.reason);
       print_document "    " c.document;
       print_fields "    " c.fields;
       Option.iter c.display_name ~f:(fun name ->
         printf "    display name: %s\n" (Terminal.text name));
       List.iter c.tags ~f:(fun (tag, value) ->
         printf "    tag %s: %s\n" (Terminal.text tag) (Terminal.text value))));
  print_endline "\nUse fin rule explain OPERATION-ID to inspect its type rules."
;;

let show_operation_links db event_id =
  let%map operations = Review.operation_ids_for_events db [ event_id ] in
  List.iter operations ~f:(fun (_, operation_id) ->
    printf "Operation: #%Ld (fin db event operations %Ld)\n" operation_id event_id)
;;

(* A source event without a bank movement, such as a CDC row, has no
   review detail. Show the operation it supports instead of failing. *)
let show_operation_only db event_id =
  let%bind operations = Operations.operations_for_event db event_id in
  match operations with
  | [] -> Or_error.error_string "unknown source event ID"
  | operations ->
    print_endline "This source event has no bank movement; showing its operations.";
    List.iter operations ~f:(fun (id, operation) ->
      printf "Operation #%Ld  %s\n" id (Temporal.to_string operation.Operation.occurred);
      let sources =
        List.filter_map operation.evidence ~f:(function
          | Operation.Stored_event id -> Some (Printf.sprintf "#%Ld" id)
          | File_row _ -> None)
      in
      printf "  Sources: %s\n" (String.concat ~sep:", " sources);
      List.iter operation.movements ~f:(fun movement ->
        let role =
          Option.value_map movement.Operation.identity ~default:"unknown" ~f:(fun id ->
            id.role)
        in
        let account =
          Option.value movement.account ~default:"account not assigned in operation"
        in
        printf
          "  %s: %s %s (%s)\n"
          (Terminal.text role)
          (Quantity.to_string movement.quantity)
          (Terminal.text (Commodity.display movement.commodity))
          (Terminal.text account)));
    Ok ()
;;

let transaction_show database operation_id =
  with_database database (fun db ->
    let%bind operation = Operations.get_operation db operation_id in
    let sources =
      List.filter_map operation.Operation.evidence ~f:(function
        | Stored_event id -> Some (Int64.to_string id)
        | File_row _ -> None)
    in
    printf "Source event IDs: %s\n" (String.concat ~sep:", " sources);
    (* The first movement is the group's main one, so a fee never heads the detail. *)
    let event_id =
      List.find_map
        (List.concat_map operation.Operation.movements ~f:(fun m -> m.evidence)
         @ operation.evidence)
        ~f:(function
          | Stored_event id -> Some id
          | File_row _ -> None)
    in
    match event_id with
    | None -> Or_error.error_string "operation has no stored source event"
    | Some event_id ->
      (match Explain.show_movement db event_id with
       | Ok detail ->
         print_transaction_detail detail;
         let%map () = show_operation_links db event_id in
         if List.length operation.movements > 1
         then (
           print_endline "\nOperation movements";
           List.iter operation.movements ~f:(fun movement ->
             printf
               "  %s %s (%s)\n"
               (Quantity.to_string movement.Operation.quantity)
               (Terminal.text (Commodity.display movement.commodity))
               (Terminal.text
                  (Option.value movement.account ~default:"account not assigned"))))
       | Error _ -> show_operation_only db event_id))
;;

let transaction_show_cmd =
  command
    "show"
    "Show everything stored about one operation: its source, files, fields and decisions."
    Term.(
      const transaction_show
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"OPERATION-ID" ~doc:"Operation ID from fin operation list."))
;;

let operation_resolve database operation_id role account commodity_json symbol =
  let%bind commodity = Json.decode Commodity.codec commodity_json in
  let%bind symbol =
    match symbol, commodity with
    | Some symbol, _ -> Ok symbol
    | None, Commodity.Fiat currency -> Ok (Iso4217.to_alpha_code currency)
    | None, _ -> Or_error.error_string "choose a journal symbol with --symbol"
  in
  with_database database (fun db ->
    let%map changed =
      Review.resolve_operation_movement db ~operation_id ~role ~account ~commodity ~symbol
    in
    printf "movement_resolution_changes=%d\n" (if changed then 1 else 0))
;;

let operation_resolve_cmd =
  command
    "resolve"
    "Assign an account and canonical commodity to a proposed movement."
    Term.(
      const operation_resolve
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"OPERATION-ID")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"ROLE")
      $ Arg.(required & pos 2 (some string) None & info [] ~docv:"ACCOUNT")
      $ Arg.(required & pos 3 (some string) None & info [] ~docv:"COMMODITY-JSON")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "symbol" ]
              ~docv:"SYMBOL"
              ~doc:
                "Explicit hledger symbol (required for crypto; fiat uses its ISO code)."))
;;

let operation_exchange_review database operation_id undo yes =
  with_database database (fun db ->
    let%bind preview =
      if undo
      then Ok None
      else (
        let%map source, entry = Operations.operation_exchange_preview db operation_id in
        printf
          "Proposed exchange: %s  %s\n"
          (Date.to_string entry.date)
          (Terminal.text entry.description);
        List.iter entry.postings ~f:(fun (p : Accounting.posting) ->
          printf
            "  %-30s %s %s\n"
            (Terminal.text p.account)
            (Quantity.to_string p.quantity)
            (Terminal.text p.commodity));
        Some (source, entry))
    in
    let%bind confirmed =
      confirm
        ~yes
        ~destructive:false
        (if undo then "Undo exchange?" else "Confirm exchange?")
    in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map changed =
        Operations.set_operation_exchange
          db
          ~operation_id
          ~confirm:(not undo)
          ~expected:preview
      in
      printf "exchange_changes=%d\n" (if changed then 1 else 0)))
;;

let operation_exchange_cmd ~undo =
  command
    (if undo then "undo" else "confirm")
    (if undo
     then "Undo a reviewed exchange."
     else "Review and confirm a resolved exchange.")
    Term.(
      const operation_exchange_review
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"OPERATION-ID")
      $ const undo
      $ Arg.(
          value & flag & info [ "yes" ] ~doc:"Apply without prompting after the preview."))
;;

let transaction_status database json_output =
  with_database database (fun db ->
    let%bind report = Review.transaction_status db in
    if json_output
    then Report_json.print Report_json.transaction_status report
    else (
      let percent n =
        match Report_json.percentage n report.total with
        | None -> "n/a"
        | Some value -> Printf.sprintf "%.1f%%" value
      in
      printf "Transactions: %d\n" report.total;
      printf
        "Classified: %d (%s)\n"
        (report.total - report.unclassified)
        (percent (report.total - report.unclassified));
      printf "  Categorized: %d (%s)\n" report.categorized (percent report.categorized);
      printf
        "  Transfers: %d\n  Settlements: %d\n  Reversals: %d\n"
        report.transfers
        report.settlements
        report.reversals;
      printf "Unclassified: %d\n" report.unclassified;
      let show_groups title groups =
        printf "\n%s:\n" title;
        if List.is_empty groups
        then print_endline "  (none)"
        else
          List.iter groups ~f:(fun (name, count) ->
            printf "  %s: %d\n" (Terminal.text name) count)
      in
      show_groups "By category" report.by_category;
      show_groups "By movement type" report.by_type);
    Ok ())
;;

let transaction_status_cmd =
  command
    "status"
    "Summarize accounting decisions and classification progress."
    Term.(const transaction_status $ database $ json_output)
;;

let bank_review database all json_output =
  with_database database (fun db ->
    let%bind rows = Review.bank_review db in
    let rows =
      List.filter rows ~f:(fun r ->
        all || String.equal r.Review.counter_account "equity:unclassified")
    in
    if json_output
    then Report_json.print (Jsont.list Report_json.review) rows
    else Terminal.review rows;
    Ok ())
;;

let bank_review_cmd =
  command
    "review"
    "List unclassified bank movements for review; use --json for scripts."
    Term.(
      const bank_review
      $ database
      $ Arg.(
          value & flag & info [ "all" ] ~doc:"Include movements with reviewed categories.")
      $ json_output)
;;

let cmd =
  group
    "operation"
    "Inspect and classify imported operations."
    [ Operation_list_cmd.transaction_list_cmd
    ; transaction_status_cmd
    ; transaction_show_cmd
    ; operation_resolve_cmd
    ; group
        "exchange"
        "Review commodity exchanges."
        [ operation_exchange_cmd ~undo:false; operation_exchange_cmd ~undo:true ]
    ; refresh_transaction_evidence_cmd
    ; group
        "type"
        "Inspect and override movement types."
        [ Operation_type_cmd.transaction_type_list_cmd
        ; Operation_type_cmd.transaction_type_show_cmd
        ; Operation_type_cmd.transaction_type_set_cmd
        ; Operation_type_cmd.transaction_type_reset_cmd
        ; Operation_type_cmd.transaction_type_history_cmd
        ]
    ; bank_review_cmd
    ; group
        "category"
        "Manage reviewed categories."
        [ Category_review_cmd.categorize_bank_cmd
        ; Category_review_cmd.category_set_cmd
        ; Category_review_cmd.category_history_cmd
        ; Category_review_cmd.category_similar_cmd
        ; Category_review_cmd.category_review_cmd
        ; Category_review_cmd.category_queue_cmd
        ]
    ]
;;
