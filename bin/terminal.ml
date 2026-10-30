open Base
open Stdio
open Finances

(* Bank descriptions may contain newlines or terminal control characters. Keep
   them on one line without changing the source evidence or JSON representation. *)
let text s =
  String.map s ~f:(fun c -> if Char.to_int c < 32 || Char.to_int c = 127 then ' ' else c)
;;

let table ?(out = stdout) headers rows =
  let rows = List.map (headers :: rows) ~f:(List.map ~f:text) in
  let widths =
    List.fold
      (List.tl_exn rows)
      ~init:(List.map headers ~f:String.length)
      ~f:(fun widths row ->
        List.map2_exn widths row ~f:(fun width cell -> Int.max width (String.length cell)))
  in
  List.iteri rows ~f:(fun i row ->
    if i = 1
    then
      Out_channel.output_string
        out
        (String.concat ~sep:"  " (List.map widths ~f:(fun n -> String.make n '-')));
    if i = 1 then Out_channel.output_char out '\n';
    Out_channel.output_string
      out
      (String.concat
         ~sep:"  "
         (List.map2_exn widths row ~f:(fun width cell -> String.pad_right cell ~len:width)));
    Out_channel.output_char out '\n')
;;

let classification = function
  | Store.Unclassified -> "unclassified"
  | Category -> "category"
  | Transfer -> "transfer"
  | Settlement -> "settlement"
  | Reversal -> "reversal"
;;

let account (a : Store.bank_account_info) =
  Stdio.printf "%s  %s\n" (text a.mapping.institution) (text a.mapping.ledger_account);
  Stdio.printf "  ID: %s\n" (Bank_account_mapping.id a.mapping);
  Stdio.printf
    "  Source: %s / %s (%s)\n"
    (text a.mapping.bank_id)
    (text a.mapping.account_id)
    (text a.mapping.account_type);
  Stdio.printf
    "  IBAN: %s (%s)\n"
    (text (Option.value_map a.iban ~default:"none" ~f:Iban.to_string))
    a.iban_source;
  Stdio.printf
    "  Records: %d movements in %s; %s to %s\n"
    a.movements
    (text (Iso4217.to_alpha_code a.currency))
    (Date.to_string a.first_on)
    (Date.to_string a.last_on);
  Option.iter a.settlement_account ~f:(fun parent ->
    Stdio.printf
      "  Settles to: %s / %s (%s)\n"
      (text parent.institution)
      (text parent.account_id)
      (text parent.account_type));
  print_endline ""
;;

let import_history attempts =
  if List.is_empty attempts
  then print_endline "No import attempts yet. See fin import --help to import statements."
  else (
    print_endline "Import attempts, newest first:\n";
    List.iter attempts ~f:(fun (a : Bank_import.file_report) ->
      Stdio.printf
        "#%Ld  %s  %s  %s\n"
        a.attempt_id
        (text (Timestamp.to_rfc3339 a.attempted_at))
        (text a.institution)
        (Bank_import.outcome_name a.outcome);
      Stdio.printf "  File: %s\n" (text a.path);
      Stdio.printf
        "  Format: %s; new movements: %d; existing movements: %d\n"
        a.format
        a.inserted
        a.existing;
      Option.iter a.message ~f:(fun message ->
        Stdio.printf "  Detail: %s\n" (text message));
      print_endline ""))
;;

let review rows =
  if List.is_empty rows
  then print_endline "No movements to review."
  else
    List.iter rows ~f:(fun (r : Store.bank_review) ->
      let tx = r.transaction in
      Stdio.printf
        "#%Ld  %s  %s / %s\n"
        r.event_id
        (Date.to_string tx.booked_on)
        (text r.institution)
        (text tx.account.account_id);
      Stdio.printf
        "  Amount: %s %s; fee: %s %s\n"
        (Quantity.to_string tx.amount)
        (text (Iso4217.to_alpha_code tx.account.currency))
        (Quantity.to_string tx.fee)
        (text (Iso4217.to_alpha_code tx.account.currency));
      let description = String.concat ~sep:" | " tx.descriptions in
      Option.iter r.display_name ~f:(fun name -> Stdio.printf "  Name: %s\n" (text name));
      Option.iter r.movement_type ~f:(fun kind ->
        Stdio.printf "  Type: %s\n" (text (Movement_type.to_string kind)));
      Option.iter r.movement_type_override ~f:(fun _ ->
        Stdio.printf
          "  Type source: manual override; imported type: %s\n"
          (text
             (Option.value_map
                r.imported_movement_type
                ~default:"unknown"
                ~f:Movement_type.to_string)));
      if not (String.is_empty description) then Stdio.printf "  %s\n" (text description);
      Stdio.printf "  Classification: %s\n" (text r.counter_account);
      Option.iter r.reason ~f:(fun reason -> Stdio.printf "  Reason: %s\n" (text reason));
      print_endline "")
;;

let transactions ?(out = stdout) rows =
  if List.is_empty rows
  then Out_channel.output_string out "No transactions found.\n"
  else
    table
      ~out
      [ "DATE"
      ; "AMOUNT"
      ; "CURRENCY"
      ; "INSTITUTION"
      ; "ACCOUNT ID"
      ; "TYPE"
      ; "CLASSIFICATION"
      ; "DESCRIPTION"
      ; "EVENT"
      ]
      (List.map rows ~f:(fun (row : Report_json.transaction) ->
         let r = row.review in
         [ Date.to_string r.transaction.booked_on
         ; Quantity.to_string r.transaction.amount
         ; Iso4217.to_alpha_code r.transaction.account.currency
         ; r.institution
         ; row.account_id
         ; Option.value_map r.movement_type ~default:"-" ~f:Movement_type.to_string
         ; classification r.classification
         ; Option.value
             r.display_name
             ~default:(String.concat ~sep:" | " r.transaction.descriptions)
         ; Int64.to_string r.event_id
         ]))
;;

let transfer (c : Store.transfer_candidate) =
  Stdio.printf
    "%Ld <-> %Ld  principal %s %s  (%d days apart)\n"
    c.first_event_id
    c.second_event_id
    (Quantity.to_string (Movement.principal c.first_transaction))
    (text (Iso4217.to_alpha_code c.first_transaction.account.currency))
    c.days_apart;
  Stdio.printf
    "  %s / %s (%s) <-> %s / %s (%s)\n"
    (text c.first.venue)
    (text c.first_transaction.account.account_id)
    (Date.to_string c.first_transaction.booked_on)
    (text c.second.venue)
    (text c.second_transaction.account.account_id)
    (Date.to_string c.second_transaction.booked_on);
  Stdio.printf
    "  Fees: %s / %s %s\n"
    (Quantity.to_string c.first_transaction.fee)
    (Quantity.to_string c.second_transaction.fee)
    (text (Iso4217.to_alpha_code c.first_transaction.account.currency));
  List.iter [ c.first_transaction; c.second_transaction ] ~f:(fun tx ->
    let description = String.concat ~sep:" | " tx.descriptions in
    if not (String.is_empty description) then Stdio.printf "  %s\n" (text description));
  List.iter c.iban_evidence ~f:(fun evidence -> Stdio.printf "  %s\n" (text evidence))
;;
