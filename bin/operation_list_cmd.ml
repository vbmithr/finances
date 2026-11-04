open Base
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let parse_movement_type_filter = function
  | "unknown" -> Ok Review.Untyped
  | value when String.is_empty (String.strip value) ->
    Or_error.error_string "--type must not be empty"
  | value -> Ok (Review.Typed (Movement_type.of_string value))
;;

let matches_movement_type_filter filter movement_type =
  match filter with
  | Review.Untyped -> Option.is_none movement_type
  | Typed kind -> Option.value_map movement_type ~default:false ~f:(Poly.equal kind)
;;

let transaction_sort_columns =
  [ "date", Review.By_date
  ; "booked_on", By_date
  ; "amount", By_amount
  ; "currency", By_currency
  ; "institution", By_institution
  ; "account", By_account
  ; "account-id", By_account
  ; "account_id", By_account
  ; "type", By_type
  ; "movement_type", By_type
  ; "counter-account", By_counter_account
  ; "counter_account", By_counter_account
  ; "description", By_description
  ; "event", By_event
  ; "event-id", By_event
  ; "event_id", By_event
  ]
;;

let parse_transaction_sort = function
  | None -> Ok [ Review.By_occurrence ]
  | Some value ->
    let%bind columns =
      String.split value ~on:','
      |> List.map ~f:(fun name ->
        let name = String.strip name in
        List.Assoc.find transaction_sort_columns name ~equal:String.equal
        |> Or_error.of_option ~error:(Error.createf "unknown --order-by column: %s" name))
      |> Or_error.combine_errors
    in
    if List.contains_dup columns ~compare:Poly.compare
    then Or_error.error_string "--order-by columns must not be repeated"
    else Ok columns
;;

let parse_list_date label = function
  | None -> Ok None
  | Some text ->
    Or_error.try_with (fun () ->
      let date = Date.of_string text in
      if not (String.equal text (Date.to_string date)) then failwith "expected YYYY-MM-DD";
      Some date)
    |> Or_error.tag ~tag:label
;;

let list_transactions
      database
      account_id
      institution
      from_date
      to_date
      search
      movement_type
      classification
      counter_account
      limit
      order_by
      order
      json_output
  =
  let%bind sort_columns = parse_transaction_sort order_by in
  let%bind from_date = parse_list_date "--from" from_date in
  let%bind to_date = parse_list_date "--to" to_date in
  let%bind movement_type =
    match movement_type with
    | None -> Ok None
    | Some value -> Result.map (parse_movement_type_filter value) ~f:Option.some
  in
  let%bind () =
    match from_date, to_date, limit with
    | Some first, Some last, _ when Date.compare first last > 0 ->
      Or_error.error_string "--from must not be after --to"
    | _, _, Some n when n <= 0 -> Or_error.error_string "--limit must be positive"
    | _ -> Ok ()
  in
  with_database database (fun db ->
    let%bind accounts = Transfers.bank_account_ids db in
    let%bind only_account =
      match account_id with
      | None -> Ok None
      | Some id ->
        (match
           List.filter accounts ~f:(fun (_, _, candidate) -> String.equal candidate id)
         with
         | [ (institution, source, _) ] -> Ok (Some (institution, source))
         | [] -> Or_error.error_string "unknown account ID; see fin account list"
         | _ -> Or_error.error_string "account ID collision; filtering is refused")
    in
    let ids = Hashtbl.Poly.create () in
    List.iter accounts ~f:(fun (institution, source, id) ->
      Hashtbl.set ids ~key:(institution, source) ~data:id);
    let%bind operations =
      Review.operations
        db
        ~filter:
          { only_account
          ; only_institution = institution
          ; only_type = movement_type
          ; only_decision = classification
          ; only_counter_account = counter_account
          ; from_date
          ; to_date
          ; search
          }
        ~accounts
        ~order:sort_columns
        ~ascending:order
        ?limit
    in
    let account_of (review : Review.bank_review) =
      let account = review.transaction.account in
      let source =
        Json.encode
          (Jsont.list Jsont.string)
          [ account.bank_id; account.account_id; account.account_type ]
      in
      Hashtbl.find ids (review.institution, source)
    in
    let%bind rows =
      List.map operations ~f:(fun (operation : Review.operation_review) ->
        let%map account_id =
          match operation.review with
          | None -> Ok None
          | Some review ->
            (match account_of review with
             | None -> Or_error.error_string "transaction references an unknown account"
             | Some account_id -> Ok (Some account_id))
        in
        let movement_rows =
          List.filter_map operation.movements ~f:(fun m ->
            Option.map m.Review.review ~f:(fun review ->
              { Report_json.account_id = Option.value (account_of review) ~default:""
              ; review
              }))
        in
        { Report_json.account_id; operation; movement_rows })
      |> Or_error.combine_errors
    in
    let print_rows ?out () =
      if json_output
      then Report_json.print (Jsont.list Report_json.operation_row) rows
      else Terminal.operations ?out rows
    in
    if json_output
    then (
      print_rows ();
      Ok ())
    else if Unix.isatty Unix.stdout && not (List.is_empty rows)
    then (
      let pager = Option.value (Sys.getenv "PAGER") ~default:"less -FRX" in
      if String.is_empty (String.strip pager)
      then (
        print_rows ();
        Ok ())
      else (
        let out = Unix.open_process_out pager in
        Exn.protect
          ~f:(fun () ->
            try print_rows ~out () with
            | Sys_error _ -> ())
          ~finally:(fun () ->
            try ignore (Unix.close_process_out out) with
            | Sys_error _ -> ());
        Ok ()))
    else (
      print_rows ();
      Ok ()))
;;

let transaction_list_cmd =
  command
    "list"
    "List imported operations, newest first. Terminal tables use PAGER (default: less \
     -FRX); redirected and JSON output is not paged."
    Term.(
      const list_transactions
      $ database
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account" ] ~docv:"ID" ~doc:"Only this ID from fin account list.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "institution" ] ~docv:"NAME" ~doc:"Only operations from this provider.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "from" ]
              ~docv:"DATE"
              ~doc:"Include booking dates on or after YYYY-MM-DD.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "to" ]
              ~docv:"DATE"
              ~doc:"Include booking dates on or before YYYY-MM-DD.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "search" ]
              ~docv:"TEXT"
              ~doc:"ASCII case-insensitive operation description substring.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "type" ]
              ~docv:"TYPE"
              ~doc:
                "Movement type as displayed, or unknown for movements without a type. \
                 Provider-specific types are accepted.")
      $ Arg.(
          value
          & opt
              (some
                 (enum
                    [ "unclassified", Review.Unclassified
                    ; "category", Category
                    ; "transfer", Transfer
                    ; "settlement", Settlement
                    ; "reversal", Reversal
                    ]))
              None
          & info
              [ "decision" ]
              ~docv:"DECISION"
              ~doc:
                "Accounting decision kind: unclassified, category, transfer, settlement \
                 or reversal.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "category"; "counter-account" ]
              ~docv:"ACCOUNT"
              ~doc:
                "Only operations with a movement carrying this exact counter-account, \
                 including categories.")
      $ Arg.(
          value
          & opt (some int) None
          & info [ "limit" ] ~docv:"COUNT" ~doc:"Maximum number of operations.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "order-by" ]
              ~docv:"COLUMNS"
              ~doc:
                "Comma-separated sort columns: date, amount, currency, institution, \
                 account, type, counter-account, description, event. Default: source \
                 event date/time.")
      $ Arg.(
          value
          & vflag
              false
              [ true, info [ "asc" ] ~doc:"Sort in ascending order."
              ; false, info [ "desc" ] ~doc:"Sort in descending order (default)."
              ])
      $ json_output)
;;
