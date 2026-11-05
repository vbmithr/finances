open Base
open Stdio
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let transaction_type_list database =
  with_database database (fun db ->
    let%map provider_types = Restore.observed_provider_movement_types db in
    print_endline "Common movement types:";
    List.iter Movement_type.common ~f:(fun (kind, description) ->
      printf "  %-18s %s\n" (Movement_type.to_string kind) description);
    print_endline "\nProvider-specific types in imported data:";
    if List.is_empty provider_types
    then print_endline "  None."
    else List.iter provider_types ~f:(fun name -> printf "  %s\n" (Terminal.text name));
    print_endline
      "\nUse --type unknown with fin operation list to find movements without a type.")
;;

let transaction_type_list_cmd =
  command
    "list"
    "List common and observed provider-specific movement types."
    Term.(const transaction_type_list $ database)
;;

let source_event_selector =
  Arg.(
    value
    & opt (some int64) None
    & info
        [ "source-event" ]
        ~docv:"EVENT-ID"
        ~doc:"Select this source event when the operation has multiple movements.")
;;

let transaction_type_set database value operation_ids source_event_id =
  let%bind kind =
    if String.is_empty (String.strip value) || String.equal value "unknown"
    then
      Or_error.error_string
        "choose a type from fin operation type list; use fin operation type reset to \
         remove an override"
    else Ok (Movement_type.of_string value)
  in
  with_database database (fun db ->
    let%bind event_ids =
      Category_review_cmd.bank_event_ids_for_operations ?source_event_id db operation_ids
    in
    let%map changed =
      Restore.set_movement_type_overrides db ~event_ids ~movement_type:(Some kind)
    in
    printf "type_changes=%d\n" changed)
;;

let transaction_type_set_cmd =
  command
    "set"
    "Set a manual movement type on one or more transactions atomically."
    Term.(
      const transaction_type_set
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"TYPE" ~doc:"Type from fin operation type list.")
      $ Arg.(
          non_empty
          & pos_right 0 int64 []
          & info [] ~docv:"OPERATION-ID" ~doc:"One or more operation IDs.")
      $ source_event_selector)
;;

let transaction_type_reset database operation_id source_event_id =
  with_database database (fun db ->
    let%bind event_id =
      Category_review_cmd.bank_event_ids_for_operations
        ?source_event_id
        db
        [ operation_id ]
      |> Result.map ~f:List.hd_exn
    in
    let%map changed =
      Restore.set_movement_type_override db ~event_id ~movement_type:None
    in
    print_endline
      (if changed then "Movement type override removed." else "No movement type override."))
;;

let transaction_type_reset_cmd =
  command
    "reset"
    "Remove a manual type override and use the imported type."
    Term.(
      const transaction_type_reset
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"OPERATION-ID" ~doc:"Operation ID.")
      $ source_event_selector)
;;

let transaction_type_show database operation_id source_event_id =
  with_database database (fun db ->
    let%bind event_id =
      Category_review_cmd.bank_event_ids_for_operations
        ?source_event_id
        db
        [ operation_id ]
      |> Result.map ~f:List.hd_exn
    in
    let%bind rows = Review.reviews_of_events db [ event_id ] in
    let%map row =
      List.find rows ~f:(fun r -> Int64.equal r.Review.event_id event_id)
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
    in
    Terminal.review [ row ];
    if Option.is_none row.movement_type_override
    then print_endline "Type source: imported evidence (no manual override).")
;;

let transaction_type_show_cmd =
  command
    "show"
    "Show a transaction's movement type and its source."
    Term.(
      const transaction_type_show
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"OPERATION-ID" ~doc:"Operation ID.")
      $ source_event_selector)
;;

let transaction_type_history database json_output =
  with_database database (fun db ->
    let%map rows = Restore.movement_type_history db in
    if json_output
    then Report_json.print (Jsont.list Report_json.movement_type_change) rows
    else if List.is_empty rows
    then print_endline "No movement type changes."
    else
      List.iter rows ~f:(fun (r : Restore.movement_type_history_entry) ->
        let name = Option.value_map ~default:"no override" ~f:Movement_type.to_string in
        printf
          "#%Ld  event #%Ld  %s -> %s  %s  %s\n"
          r.id
          r.event_id
          (name r.previous_type)
          (name r.movement_type)
          (Timestamp.to_rfc3339 r.changed_at)
          (Terminal.text r.reason)))
;;

let transaction_type_history_cmd =
  command
    "history"
    "Show audited manual movement type changes."
    Term.(const transaction_type_history $ database $ json_output)
;;
