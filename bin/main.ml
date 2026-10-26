open Core
open Finances
open Cmdliner

let ( let* ) x f = Result.bind x ~f
let read_file path = Or_error.try_with (fun () -> In_channel.read_all path)

let ingest database venue account kind source_id at path =
  let* payload = read_file path in
  let* occurred_at_ms = Or_error.try_with (fun () -> Int64.of_string at) in
  let* event =
    Source_event.create ~venue ~account ~kind ~source_id ~occurred_at_ms ~payload
  in
  Store.with_open database (fun db ->
    let* result = Store.ingest db event in
    let status, id =
      match result with
      | Store.Inserted id -> "inserted", id
      | Existing id -> "existing", id
    in
    Printf.printf "%s %Ld\n" status id;
    Ok ())
;;

let list_events database =
  Store.with_open database (fun db ->
    let* events = Store.events db in
    let json =
      `List
        (List.map events ~f:(fun (id, (e : Source_event.t)) ->
           `Assoc
             [ "id", `Intlit (Int64.to_string id)
             ; "venue", `String e.venue
             ; "account", `String e.account
             ; "kind", `String e.kind
             ; "source_id", `String e.source_id
             ; ( "occurred_at_ms"
               , match e.occurred with
                 | At_ms ms -> `Intlit (Int64.to_string ms)
                 | On_date _ -> `Null )
             ; ( "occurred_on"
               , match e.occurred with
                 | On_date date -> `String (Date.to_string date)
                 | At_ms _ -> `Null )
             ]))
    in
    Out_channel.print_endline (Yojson.Safe.pretty_to_string json);
    Ok ())
;;

let database =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"DATABASE" ~doc:"SQLite database path.")
;;

let required name doc = Arg.(required & opt (some string) None & info [ name ] ~doc)

let command name doc term =
  Cmd.v
    (Cmd.info name ~doc)
    Term.(
      const (fun result ->
        Result.map_error result ~f:(fun e -> `Msg (Error.to_string_hum e)))
      $ term
      |> term_result)
;;

let init =
  command
    "init"
    "Create or migrate a database."
    Term.(const (fun path -> Store.with_open path (fun _ -> Ok ())) $ database)
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
      $ required "at-ms" "Event timestamp in Unix milliseconds."
      $ Arg.(
          required
          & pos 1 (some string) None
          & info [] ~docv:"PAYLOAD" ~doc:"File containing the unmodified source payload."))
;;

let import_ofx database institution path =
  let* contents = read_file path in
  let* document = Bank_ofx.parse contents in
  Store.with_open database (fun db ->
    let* report = Store.import_ofx db ~institution document in
    printf
      "statements=%d inserted=%d existing=%d document=%Ld\n"
      report.statements
      report.inserted
      report.existing
      report.document_id;
    Ok ())
;;

let import_ofx_cmd =
  command
    "import-ofx"
    "Import all bank statements from an OFX 2.x XML export."
    Term.(
      const import_ofx
      $ database
      $ Arg.(
          value
          & opt string "boursobank"
          & info
              [ "institution" ]
              ~doc:"Stable institution identifier used to scope source identities.")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"OFX_FILE"))
;;

let project_bank database =
  Store.with_open database (fun db ->
    let* report = Store.project_bank db in
    printf "inserted=%d existing=%d\n" report.inserted report.existing;
    Ok ())
;;

let project_bank_cmd =
  command
    "project-bank"
    "Create balanced bank movement entries from stored events."
    Term.(const project_bank $ database)
;;

let export_hledger database =
  Store.with_open database (fun db ->
    let* journal = Finances_hledger.export db in
    Out_channel.output_string stdout journal;
    Ok ())
;;

let export_hledger_cmd =
  command
    "export-hledger"
    "Write a reproducible hledger journal to standard output."
    Term.(const export_hledger $ database)
;;

let () =
  exit
    (Cmd.eval
       (Cmd.group
          (Cmd.info "finances" ~doc:"Personal finance accounting and position tracking.")
          [ init
          ; ingest_cmd
          ; events
          ; import_ofx_cmd
          ; project_bank_cmd
          ; export_hledger_cmd
          ]))
;;
