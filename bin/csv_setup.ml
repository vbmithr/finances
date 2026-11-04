open Base
open Stdio
open Finances
open Finances_store
open Or_error.Let_syntax
open Cli

(* Prompts live at the CLI boundary; library callers and redirected stdin never
   need a terminal. EOF has the same meaning as skipping an optional IBAN. *)
let import_account_label registry institution account_id =
  let validate = Validation.token "account label" in
  match Registry.find registry institution with
  | Some (module A : Adapter.S) when A.takes_account_label ->
    let message =
      Printf.sprintf
        "%s files do not say which account they come from: name it with --account-id \
         LABEL, for example main, and use the same label every time."
        institution
    in
    (match account_id with
     | Some label -> Result.map (validate label) ~f:Option.some
     | None when Unix.isatty Unix.stdin ->
       eprintf "%s files do not say which account they come from.\n" institution;
       let rec read () =
         match
           prompt "Label for this account (e.g. main; use the same one every time): "
         with
         | None -> Or_error.error_string message
         | Some label ->
           (match validate label with
            | Ok label -> Ok (Some label)
            | Error error ->
              eprintf "%s\n" (Error.to_string_hum error);
              read ())
       in
       read ()
     | None -> Or_error.error_string message)
  | _ -> Ok account_id
;;

let configure_new_accounts db fresh =
  let same_group (a : Bank_account_mapping.t) (b : Bank_account_mapping.t) =
    String.equal a.institution b.institution
    && String.equal a.bank_id b.bank_id
    && String.equal a.account_id b.account_id
  in
  let rec groups remaining =
    match remaining with
    | [] -> Ok 0
    | (first : Transfers.bank_account_info) :: rest ->
      let siblings, rest =
        List.partition_tf rest ~f:(fun info ->
          same_group first.mapping info.Transfers.mapping)
      in
      let group = first :: siblings in
      let missing =
        List.filter group ~f:(fun info ->
          String.equal info.Transfers.iban_source "missing")
      in
      let%bind count =
        match missing with
        | [] -> Ok 0
        | first :: _ ->
          eprintf
            "New account: %s %s (%s), %d movements, %s to %s.\n"
            first.mapping.institution
            first.mapping.account_id
            (String.concat
               ~sep:", "
               (List.map missing ~f:(fun i -> i.Transfers.mapping.account_type)))
            (List.sum (module Int) missing ~f:(fun i -> i.Transfers.movements))
            (Date.to_string
               (List.reduce_exn
                  (List.map missing ~f:(fun i -> i.Transfers.first_on))
                  ~f:(fun a b -> if Date.compare a b <= 0 then a else b)))
            (Date.to_string
               (List.reduce_exn
                  (List.map missing ~f:(fun i -> i.Transfers.last_on))
                  ~f:(fun a b -> if Date.compare a b >= 0 then a else b)));
          let rec read () =
            match
              prompt "Its statements do not include an IBAN. IBAN (Enter to skip): "
            with
            | None | Some "" -> Ok None
            | Some value ->
              (match Iban.of_string value with
               | Ok iban -> Ok (Some iban)
               | Error error ->
                 eprintf "%s\n" (Error.to_string_hum error);
                 read ())
          in
          let%bind iban = read () in
          (match iban with
           | None -> Ok 0
           | Some iban ->
             let%bind current = Accounts.export_bank_accounts db in
             let updates =
               List.filter_map current ~f:(fun row ->
                 if
                   List.exists missing ~f:(fun info ->
                     String.equal
                       row.Bank_account_config.reference.institution
                       info.Transfers.mapping.institution
                     && String.equal
                          (Bank_account_config.source_account row.reference)
                          (Bank_account_mapping.source_account info.mapping))
                 then Some { row with iban = Set iban }
                 else None)
             in
             let%map result = Accounts.apply_bank_accounts db updates in
             result.identity_changes)
      in
      let%map remaining = groups rest in
      count + remaining
  in
  if Unix.isatty Unix.stdin then groups fresh else Ok 0
;;

let csv_import_profile ?(setup_only = false) database path max_mib ~institution =
  let%bind contents =
    Or_error.try_with (fun () ->
      let stat = Unix.stat path in
      if not (Poly.equal stat.st_kind Unix.S_REG)
      then failwith "CSV setup needs a regular file";
      if stat.st_size > max_mib * 1024 * 1024
      then failwith "CSV file exceeds --max-file-mib";
      In_channel.read_all path)
  in
  let%bind contexts =
    with_database database (fun db -> Import.document_contexts db ~institution)
  in
  let%bind profiles =
    List.map contexts ~f:(fun context ->
      let%bind fields = Json.decode Json.string_map context in
      match List.Assoc.find fields "csv_profile" ~equal:String.equal with
      | None -> Ok None
      | Some value ->
        let%map p = Json.decode Csv_profile.codec value in
        Some p)
    |> Or_error.combine_errors
  in
  let%bind configured =
    with_database database (fun db -> Setup.institution_profile db institution)
  in
  let%bind configured =
    match configured with
    | None -> Ok []
    | Some encoded ->
      let%map profile = Json.decode Csv_profile.codec encoded in
      [ profile ]
  in
  let matching =
    configured @ List.filter_opt profiles
    |> List.filter ~f:(fun (p : Csv_profile.t) ->
      Csv_reader.recognizes ~separator:p.separator ~trim:false ~header:p.header contents)
    |> List.dedup_and_sort ~compare:Poly.compare
  in
  let read message =
    match prompt message with
    | Some s when not (String.is_empty (String.strip s)) -> String.strip s
    | _ -> failwith "CSV setup cancelled"
  in
  let choose question options =
    eprintf "%s\n" question;
    List.iteri options ~f:(fun i (label, _, _) -> eprintf "  %d. %s\n" (i + 1) label);
    let rec loop () =
      match prompt "Choice: " with
      | None -> failwith "CSV setup cancelled"
      | Some input ->
        let input = String.strip input in
        let by_number =
          Option.try_with (fun () -> Int.of_string input)
          |> Option.bind ~f:(fun n -> List.nth options (n - 1))
        in
        let selected =
          match by_number with
          | Some choice -> Some choice
          | None ->
            List.find options ~f:(fun (label, aliases, _) ->
              String.Caseless.equal input label
              || List.mem aliases input ~equal:String.Caseless.equal)
        in
        (match selected with
         | Some (_, _, value) when not (String.is_empty input) -> value
         | _ ->
           eprintf "Choose one of the numbered options.\n";
           loop ())
    in
    loop ()
  in
  let yes_no question =
    choose question [ "Yes", [ "y"; "yes" ], true; "No", [ "n"; "no" ], false ]
  in
  let%bind profile =
    match matching with
    | [ p ] -> Ok p
    | _ :: _ ->
      Or_error.error_string "multiple saved CSV layouts match; choose a distinct header"
    | [] when not (Unix.isatty Unix.stdin) ->
      Or_error.error_string
        "unknown CSV layout; run this command in a terminal to set it up"
    | [] ->
      Or_error.try_with (fun () ->
        eprintf "Unknown CSV layout. Set up the columns below.\n";
        let separator =
          match Csv_reader.detect_separator contents with
          | Some separator ->
            eprintf
              "Detected separator: %s.\n"
              (if Char.equal separator ',' then "comma" else "semicolon");
            separator
          | None ->
            choose "Separator:" [ "Comma", [ "," ], ','; "Semicolon", [ ";" ], ';' ]
        in
        let header = Csv.next (Csv_reader.channel ~separator contents) in
        eprintf "Columns: %s\n" (String.concat ~sep:", " header);
        let find names =
          List.find header ~f:(fun h ->
            List.mem names (String.lowercase (String.strip h)) ~equal:String.equal)
        in
        let date_guess = find [ "date"; "transaction date"; "booking date" ] in
        let description_guess = find [ "merchant"; "description"; "payee" ] in
        let currencies =
          List.filter_map header ~f:(fun h ->
            Option.map (Iso4217.of_alpha_code h) ~f:(fun currency -> h, currency))
        in
        let amount_guess =
          match currencies with
          | [ pair ] -> Some pair
          | _ -> None
        in
        let proposed =
          match date_guess, description_guess, amount_guess with
          | Some date, Some description, Some (amount, currency) ->
            eprintf
              "\n\
               Suggested mapping:\n\
              \  Transaction date: %s\n\
              \  Description: %s\n\
              \  Account amount: %s (%s)\n"
              date
              description
              amount
              (Iso4217.to_alpha_code currency);
            let other =
              List.filter header ~f:(fun h ->
                not (List.mem [ date; description; amount ] h ~equal:String.equal))
            in
            if not (List.is_empty other)
            then eprintf "  Other source details: %s\n" (String.concat ~sep:", " other);
            eprintf
              "These columns describe the transaction date, description, and amount in \
               the account currency.\n";
            if yes_no "Use these columns?"
            then Some (date, description, amount, currency)
            else None
          | _ ->
            eprintf "I could not identify all transaction columns; choose them below.\n";
            None
        in
        let date_column, description_column, amount_column, currency =
          match proposed with
          | Some mapping -> mapping
          | None ->
            let column question =
              choose question (List.map header ~f:(fun h -> h, [], h))
            in
            let date = column "Column containing the transaction date:" in
            let description = column "Column containing the merchant or description:" in
            let amount = column "Column containing the amount charged to the account:" in
            let currency =
              read "Account currency (e.g. USD): "
              |> Iso4217.of_alpha_code
              |> Option.value_exn
            in
            date, description, amount, currency
        in
        let sample_rows = Csv_reader.channel ~separator contents in
        ignore (Csv.next sample_rows : string list);
        let sample_rows = Csv.input_all sample_rows in
        let values column =
          match List.findi header ~f:(fun _ value -> String.equal value column) with
          | None -> []
          | Some (index, _) ->
            List.filter_map sample_rows ~f:(fun row ->
              Option.bind (List.nth row index) ~f:(fun value ->
                let value = String.strip value in
                Option.some_if (not (String.is_empty value)) value))
        in
        let date_values = values date_column in
        let iso_date = Re.Perl.compile_pat "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" in
        let iso_datetime =
          Re.Perl.compile_pat "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$"
        in
        let date_format =
          if
            (not (List.is_empty date_values))
            && List.for_all date_values ~f:(Re.execp iso_date)
          then Csv_profile.Iso
          else if
            (not (List.is_empty date_values))
            && List.for_all date_values ~f:(Re.execp iso_datetime)
          then (
            eprintf
              "Detected date and time; the calendar date will be used for booking.\n";
            Csv_profile.Iso_datetime)
          else (
            eprintf
              "Dates must be interpreted consistently; examples: %s\n"
              (String.concat ~sep:", " (List.take date_values 3));
            choose
              "Date format:"
              [ "YYYY-MM-DD", [], Csv_profile.Iso
              ; "DD/MM/YYYY or DD-MM-YYYY", [ "day-first" ], Csv_profile.Day_first
              ; "MM/DD/YYYY or MM-DD-YYYY", [ "month-first" ], Csv_profile.Month_first
              ; ( "YYYY-MM-DD HH:MM:SS (date only)"
                , [ "iso-datetime" ]
                , Csv_profile.Iso_datetime )
              ])
        in
        let amount_values = values amount_column in
        let amount_format =
          if List.exists amount_values ~f:(fun value -> String.contains value ',')
          then (
            eprintf
              "Check the decimal mark in amounts; examples: %s\n"
              (String.concat ~sep:", " (List.take amount_values 3));
            choose
              "Amount decimal mark:"
              [ "Dot (e.g. 1234.56)", [ "dot" ], Csv_profile.Dot_decimal
              ; "Comma (e.g. 1234,56)", [ "comma" ], Csv_profile.Comma_decimal
              ])
          else Csv_profile.Dot_decimal
        in
        eprintf "A prepaid card holds money you add; a credit card is paid later.\n";
        let account_type =
          choose
            "Account type:"
            [ "Prepaid card", [ "prepaid-card" ], Csv_profile.Prepaid_card
            ; "Credit card", [ "credit-card" ], Csv_profile.Credit_card
            ; "Current account", [ "current"; "checking" ], Csv_profile.Current
            ; "Savings account", [ "savings" ], Csv_profile.Savings
            ]
        in
        let invert_amount =
          match account_type with
          | Csv_profile.Prepaid_card | Credit_card ->
            eprintf
              "A purchase should reduce the card balance. Check how your file writes \
               purchases.\n";
            yes_no "Are purchases positive amounts in the file?"
          | Current | Savings ->
            eprintf
              "Money leaving the account should be negative. Check how your file writes \
               withdrawals or payments.\n";
            yes_no "Are outgoing amounts positive in the file?"
        in
        let raw = Csv_reader.channel ~separator contents in
        ignore (Csv.next raw : string list);
        let records = Csv.input_all raw in
        let summary =
          match List.findi header ~f:(fun _ h -> String.equal h date_column) with
          | None -> None
          | Some (date_pos, _) ->
            List.filter records ~f:(fun row ->
              List.length row = List.length header
              && String.is_empty (String.strip (List.nth_exn row date_pos)))
            |> List.filter_map ~f:(fun row ->
              List.find_mapi row ~f:(fun i value ->
                Option.some_if
                  (String.Caseless.equal (String.strip value) "total")
                  (List.nth_exn header i, String.strip value)))
            |> List.dedup_and_sort ~compare:[%compare: string * string]
            |> (function
             | [ result ] -> Some result
             | _ -> None)
        in
        let skip_when =
          match summary with
          | Some (column, value) ->
            eprintf "Found a summary row with an empty date: %s = %s.\n" column value;
            if yes_no "Skip this summary row?" then Some (column, value) else None
          | None ->
            if yes_no "Skip a summary row with an empty date?"
            then (
              let column =
                choose "Summary label column:" (List.map header ~f:(fun h -> h, [], h))
              in
              let value = read "Summary label value: " in
              Some (column, value))
            else None
        in
        Csv_profile.
          { separator
          ; header
          ; date_column
          ; date_format
          ; amount_format
          ; description_column
          ; amount_column
          ; currency
          ; account_type
          ; invert_amount
          ; skip_when
          })
  in
  let%bind rows = Csv_profile.parse profile contents in
  if Unix.isatty Unix.stdin
  then (
    eprintf
      "Preview: %d transactions; %s account.\n"
      (List.length rows)
      (Csv_profile.account_type_name profile.account_type);
    List.take rows 5
    |> List.iter ~f:(fun (r : Csv_profile.row) ->
      eprintf "  %s  %s\n" (Date.to_string r.date) r.description;
      let currency = Iso4217.to_alpha_code r.currency in
      let source_amount =
        List.zip_exn profile.header r.fields
        |> fun fields ->
        List.Assoc.find fields profile.amount_column ~equal:String.equal
        |> Option.value_exn
      in
      eprintf
        "    CSV amount: %s %s; account movement: %s %s\n"
        source_amount
        currency
        (Quantity.to_string r.amount)
        currency;
      let other =
        List.zip_exn profile.header r.fields
        |> List.filter ~f:(fun (name, value) ->
          (not
             (List.mem
                [ profile.date_column; profile.description_column; profile.amount_column ]
                name
                ~equal:String.equal))
          && not (String.is_empty (String.strip value)))
        |> List.map ~f:(fun (name, value) -> name ^ ": " ^ value)
      in
      if not (List.is_empty other)
      then eprintf "    CSV details: %s\n" (String.concat ~sep:"; " other)));
  let%bind () =
    if not (Unix.isatty Unix.stdin)
    then Ok ()
    else (
      let%bind confirmed =
        Or_error.try_with (fun () ->
          yes_no
            (if setup_only then "Use this CSV layout?" else "Import these transactions?"))
      in
      if confirmed then Ok () else Or_error.error_string "CSV import cancelled")
  in
  Ok profile
;;

let builtin_institution_name registry name =
  List.exists (Registry.institutions registry) ~f:(fun institution ->
    List.mem (Registry.aliases registry institution) name ~equal:String.equal)
;;

let ensure_generic_source database institution expected_kind =
  with_database database (fun db ->
    let%bind companion = Setup.institution_companion_profile db institution in
    let%bind () =
      if String.equal expected_kind "csv-movement" && Option.is_some companion
      then
        Or_error.error_string
          "this institution uses a CSV companion; import it with fin import INSTITUTION \
           PATH"
      else Ok ()
    in
    let%bind events = Ledger.events db in
    if
      List.exists events ~f:(fun (_, event) ->
        String.equal event.Source_event.venue institution
        && List.mem [ "csv-movement"; "bank-transaction" ] event.kind ~equal:String.equal
        && not (String.equal event.kind expected_kind))
    then
      Or_error.error_string
        "this institution already has imports in another generic format; CSV and OFX are \
         separate movement sources and may duplicate transactions"
    else Ok ())
;;
