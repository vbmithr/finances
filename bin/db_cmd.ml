open Base
open Stdio
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let ingest database venue account kind source_id at path =
  let%bind payload = read_file path in
  let%bind occurred_at =
    Or_error.try_with (fun () -> Timestamp.of_unix_ms_exn (Int64.of_string at))
  in
  let%bind event =
    Source_event.create ~venue ~account ~kind ~source_id ~occurred_at ~payload ()
  in
  with_database database (fun db ->
    let%bind result = Sources.ingest db event in
    let status, id =
      match result with
      | Handle.Inserted id -> "inserted", id
      | Existing id -> "existing", id
    in
    Stdio.printf "%s %Ld\n" status id;
    Ok ())
;;

let list_events database =
  with_database database (fun db ->
    let%bind events = Ledger.events db in
    Report_json.print (Jsont.list Report_json.event) events;
    Ok ())
;;

let events =
  command
    "events"
    "List imported source event metadata as JSON."
    Term.(const list_events $ database)
;;

let ingest_cmd =
  command
    "ingest"
    "Store an immutable source payload; identical retries are safe."
    Term.(
      const ingest
      $ database
      $ required "venue" "Source venue, for example paradex."
      $ required "account" "Source account identifier."
      $ required "kind" "Source record kind, for example fill."
      $ required "id" "Stable source record identifier."
      $ required
          "at-ms"
          "Event timestamp in Unix milliseconds, converted exactly to Ptime. Event \
           output uses RFC 3339 occurred_at."
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"PAYLOAD" ~doc:"File containing the unmodified source payload."))
;;

let transaction_operations database event_id json_output =
  with_database database (fun db ->
    let%bind sources = Ledger.events db in
    let%bind () =
      if List.exists sources ~f:(fun (id, _) -> Int64.equal id event_id)
      then Ok ()
      else Or_error.error_string "unknown source event ID"
    in
    let%map operations = Operations.operations_for_event db event_id in
    if json_output
    then Report_json.print (Jsont.list Report_json.operation) operations
    else if List.is_empty operations
    then print_endline "No operation is linked to this transaction."
    else
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
            (Terminal.text account))))
;;

let list_sources database institution json_output =
  with_database database (fun db ->
    let%map sources = Ledger.events db in
    let rows =
      List.filter sources ~f:(fun (_, event) ->
        Option.value_map
          institution
          ~default:true
          ~f:(String.equal event.Source_event.venue))
    in
    if json_output
    then Report_json.print (Jsont.list Report_json.source_event) rows
    else
      List.iter rows ~f:(fun (id, event) ->
        let field name =
          Option.value
            (List.Assoc.find event.Source_event.fields name ~equal:String.equal)
            ~default:""
        in
        let kind =
          match String.strip (field "Transaction Kind") with
          | "" -> event.kind
          | value -> value
        in
        printf
          "#%Ld  %s  %s  %s  %s %s  %s\n"
          id
          (Temporal.to_string event.occurred)
          (Terminal.text event.venue)
          (Terminal.text kind)
          (Terminal.text (field "Amount"))
          (Terminal.text (field "Currency"))
          (Terminal.text (field "Transaction Description"))))
;;

let transaction_sources_cmd =
  command
    "sources"
    "List imported source records, optionally for one institution."
    Term.(const list_sources $ database $ Statement_cmd.institution_filter $ json_output)
;;

let transaction_operations_cmd =
  command
    "operations"
    "Show operations linked to a source event."
    Term.(
      const transaction_operations
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"EVENT-ID" ~doc:"Source event ID.")
      $ json_output)
;;

let cmd =
  group
    "db"
    "Initialize and inspect local source evidence."
    [ Setup_cmd.init
    ; events
    ; ingest_cmd
    ; transaction_sources_cmd
    ; group "event" "Inspect one source event." [ transaction_operations_cmd ]
    ]
;;
