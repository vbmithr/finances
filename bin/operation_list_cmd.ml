open Base
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

type movement_type_filter =
  | Unknown_type
  | Known_type of Movement_type.t

let parse_movement_type_filter = function
  | "unknown" -> Ok Unknown_type
  | value when String.is_empty (String.strip value) ->
    Or_error.error_string "--type must not be empty"
  | value -> Ok (Known_type (Movement_type.of_string value))
;;

let matches_movement_type_filter filter movement_type =
  match filter with
  | Unknown_type -> Option.is_none movement_type
  | Known_type kind -> Option.value_map movement_type ~default:false ~f:(Poly.equal kind)
;;

type transaction_sort_column =
  | Sort_occurrence
  | Sort_date
  | Sort_amount
  | Sort_currency
  | Sort_institution
  | Sort_account
  | Sort_type
  | Sort_counter_account
  | Sort_description
  | Sort_event

let transaction_sort_columns =
  [ "date", Sort_date
  ; "booked_on", Sort_date
  ; "amount", Sort_amount
  ; "currency", Sort_currency
  ; "institution", Sort_institution
  ; "account", Sort_account
  ; "account-id", Sort_account
  ; "account_id", Sort_account
  ; "type", Sort_type
  ; "movement_type", Sort_type
  ; "counter-account", Sort_counter_account
  ; "counter_account", Sort_counter_account
  ; "description", Sort_description
  ; "event", Sort_event
  ; "event-id", Sort_event
  ; "event_id", Sort_event
  ]
;;

let parse_transaction_sort = function
  | None -> Ok [ Sort_occurrence ]
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

let operation_matches
      ~account_id
      ~institution
      ~movement_type
      ~classification
      ~counter_account
      ~from_date
      ~to_date
      ~search
      (row : Report_json.operation_row)
  =
  let any_movement f =
    List.exists
      row.operation.Review.movements
      ~f:(fun (m : Review.operation_movement_review) ->
        Option.value_map m.review ~default:false ~f)
  in
  let occurred_date =
    Date.of_string (String.prefix (Temporal.to_string row.operation.occurred) 10)
  in
  Option.value_map account_id ~default:true ~f:(fun id ->
    List.exists row.movement_rows ~f:(fun m -> String.equal m.account_id id))
  && Option.value_map institution ~default:true ~f:(fun name ->
    String.equal name row.operation.institution
    || any_movement (fun (r : Review.bank_review) -> String.equal name r.institution))
  && Option.value_map movement_type ~default:true ~f:(fun filter ->
    any_movement (fun (r : Review.bank_review) ->
      matches_movement_type_filter filter r.movement_type))
  && Option.value_map classification ~default:true ~f:(fun wanted ->
    any_movement (fun (r : Review.bank_review) -> Poly.equal r.classification wanted))
  && Option.value_map counter_account ~default:true ~f:(fun account ->
    any_movement (fun (r : Review.bank_review) -> String.equal r.counter_account account))
  && Option.value_map from_date ~default:true ~f:(fun date ->
    Date.compare occurred_date date >= 0)
  && Option.value_map to_date ~default:true ~f:(fun date ->
    Date.compare occurred_date date <= 0)
  && Option.value_map search ~default:true ~f:(fun text ->
    String.Caseless.is_substring row.operation.description ~substring:text)
;;

let compare_operation_column
      column
      (a : Report_json.operation_row)
      (b : Report_json.operation_row)
  =
  let compare_reviews f =
    match a.operation.Review.review, b.operation.Review.review with
    | Some ar, Some br -> f ar br
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> 0
  in
  match column with
  | Sort_occurrence ->
    String.compare
      (Temporal.to_string a.operation.Review.occurred)
      (Temporal.to_string b.operation.Review.occurred)
  | Sort_date ->
    compare_reviews (fun ar br ->
      Date.compare ar.Review.transaction.booked_on br.Review.transaction.booked_on)
  | Sort_amount ->
    compare_reviews (fun ar br ->
      Quantity.compare ar.Review.transaction.amount br.Review.transaction.amount)
  | Sort_currency ->
    compare_reviews (fun ar br ->
      String.compare
        (Iso4217.to_alpha_code ar.Review.transaction.account.currency)
        (Iso4217.to_alpha_code br.Review.transaction.account.currency))
  | Sort_institution -> String.compare a.operation.institution b.operation.institution
  | Sort_account ->
    String.compare
      (Option.value a.account_id ~default:"")
      (Option.value b.account_id ~default:"")
  | Sort_type ->
    let name (row : Report_json.operation_row) =
      Option.bind row.operation.Review.review ~f:(fun r ->
        Option.map r.Review.movement_type ~f:Movement_type.to_string)
      |> Option.value ~default:"-"
    in
    String.compare (name a) (name b)
  | Sort_counter_account ->
    let name (row : Report_json.operation_row) =
      Option.bind row.operation.Review.review ~f:(fun r ->
        if String.equal r.Review.counter_account "equity:unclassified"
        then None
        else Some r.Review.counter_account)
      |> Option.value ~default:"-"
    in
    String.compare (name a) (name b)
  | Sort_description ->
    compare_reviews (fun ar br ->
      String.compare ar.Review.description br.Review.description)
  | Sort_event ->
    let event (row : Report_json.operation_row) =
      Option.map row.operation.Review.review ~f:(fun r -> r.Review.event_id)
      |> Option.value ~default:Int64.max_value
    in
    Int64.compare (event a) (event b)
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
    let%bind accounts = Transfers.bank_account_info db in
    let%bind () =
      match account_id with
      | None -> Ok ()
      | Some id ->
        (match
           List.count accounts ~f:(fun a ->
             String.equal (Bank_account_mapping.id a.Transfers.mapping) id)
         with
         | 1 -> Ok ()
         | 0 -> Or_error.error_string "unknown account ID; see fin account list"
         | _ -> Or_error.error_string "account ID collision; filtering is refused")
    in
    let ids = Hashtbl.Poly.create () in
    List.iter accounts ~f:(fun a ->
      Hashtbl.set
        ids
        ~key:
          (a.Transfers.mapping.institution, Bank_account_mapping.source_account a.mapping)
        ~data:(Bank_account_mapping.id a.mapping));
    let%bind operations = Review.operation_review db in
    let%bind rows =
      List.map operations ~f:(fun (operation : Review.operation_review) ->
        let%map account_id =
          match operation.review with
          | None -> Ok None
          | Some review ->
            let account = review.transaction.account in
            let source =
              Json.encode
                (Jsont.list Jsont.string)
                [ account.bank_id; account.account_id; account.account_type ]
            in
            (match Hashtbl.find ids (review.institution, source) with
             | None -> Or_error.error_string "transaction references an unknown account"
             | Some account_id -> Ok (Some account_id))
        in
        let movement_rows =
          List.filter_map operation.movements ~f:(fun m ->
            Option.map m.Review.review ~f:(fun review ->
              let account = review.Review.transaction.account in
              let source =
                Json.encode
                  (Jsont.list Jsont.string)
                  [ account.bank_id; account.account_id; account.account_type ]
              in
              let account_id =
                Option.value (Hashtbl.find ids (review.institution, source)) ~default:""
              in
              { Report_json.account_id; review }))
        in
        { Report_json.account_id; operation; movement_rows })
      |> Or_error.combine_errors
    in
    let rows =
      List.filter
        rows
        ~f:
          (operation_matches
             ~account_id
             ~institution
             ~movement_type
             ~classification
             ~counter_account
             ~from_date
             ~to_date
             ~search)
      |> fun filtered ->
      List.sort filtered ~compare:(fun a b ->
        let rec compare = function
          | [] ->
            Int64.compare
              a.Report_json.operation.operation_id
              b.Report_json.operation.operation_id
          | column :: rest ->
            (match compare_operation_column column a b with
             | 0 -> compare rest
             | result -> result)
        in
        if order then compare sort_columns else -compare sort_columns)
    in
    let rows = Option.value_map limit ~default:rows ~f:(List.take rows) in
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
