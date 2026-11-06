open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let list_bank_accounts database json_output =
  with_database database (fun db ->
    let%bind accounts = Transfers.bank_account_info db in
    if json_output
    then Report_json.print (Jsont.list Report_json.account) accounts
    else if List.is_empty accounts
    then
      print_endline
        "No bank accounts yet. Import statements with fin import; see fin import --help."
    else List.iter accounts ~f:Terminal.account;
    Ok ())
;;

let decision_counts db (account : Transfers.bank_account_info) =
  Handle.collect
    db
    (Rel_sql.Stmt.(
       func
         "SELECT decision,count(*) FROM reviews WHERE institution=? AND source=? GROUP \
          BY decision ORDER BY decision"
         (text @-> text @-> ret Rel.Row.(t2 (text "decision") (int "count"))))
       account.mapping.institution
       (Bank_account_mapping.source_account account.mapping))
;;

let decision_json =
  Jsont.Object.enc_only ()
  |> Jsont.Object.mem "decision" Jsont.string ~enc:fst
  |> Jsont.Object.mem "count" Jsont.int ~enc:snd
  |> Jsont.Object.finish
;;

let show_account database account_id json_output =
  with_database database (fun db ->
    let%bind accounts = Transfers.bank_account_info db in
    let%bind account =
      match
        List.filter accounts ~f:(fun (a : Transfers.bank_account_info) ->
          String.equal (Bank_account_mapping.id a.mapping) account_id)
      with
      | [ account ] -> Ok account
      | [] -> Or_error.error_string "no matching account ID; see fin account list"
      | _ -> Or_error.error_string "account ID collision; use fin account list"
    in
    let source = Bank_account_mapping.source_account account.mapping in
    let%bind cards = Cards.list db in
    let cards =
      List.filter cards ~f:(fun (c : Cards.t) ->
        String.equal c.institution account.mapping.institution
        && Option.equal String.equal c.card_source_account (Some source))
    in
    (* Card accounts this one pays. *)
    let paid =
      List.filter accounts ~f:(fun (a : Transfers.bank_account_info) ->
        Option.exists a.settlement_account ~f:(fun parent ->
          String.equal parent.institution account.mapping.institution
          && String.equal (Bank_account_config.source_account parent) source))
    in
    let%map decisions = decision_counts db account in
    if json_output
    then
      Report_json.print
        (Jsont.Object.enc_only ()
         |> Jsont.Object.mem "account" Report_json.account ~enc:(fun () -> account)
         |> Jsont.Object.mem "cards" (Jsont.list Card_cmd.card_json) ~enc:(fun () ->
           cards)
         |> Jsont.Object.mem "pays" (Jsont.list Jsont.string) ~enc:(fun () ->
           List.map paid ~f:(fun a -> Bank_account_mapping.id a.mapping))
         |> Jsont.Object.mem "decisions" (Jsont.list decision_json) ~enc:(fun () ->
           decisions)
         |> Jsont.Object.finish)
        ()
    else (
      Terminal.account ~separate:false account;
      if not (List.is_empty cards)
      then (
        print_endline "  Cards:";
        List.iter cards ~f:(fun (c : Cards.t) ->
          let period =
            match c.first_seen, c.last_seen with
            | Some first, Some last ->
              Printf.sprintf " from %s to %s" (Date.to_string first) (Date.to_string last)
            | _ -> ""
          in
          printf
            "    %s  %s; %d bank rows%s, %d settled, %d undecided\n"
            (Terminal.text c.card_reference)
            (Option.value c.known_by ~default:"-")
            c.bank_rows
            period
            c.settled
            c.undecided));
      if not (List.is_empty paid)
      then (
        print_endline "  Pays:";
        List.iter paid ~f:(fun (a : Transfers.bank_account_info) ->
          printf
            "    %s  %s\n"
            (Bank_account_mapping.id a.mapping)
            (Terminal.text a.mapping.ledger_account)));
      print_endline "  Decisions:";
      List.iter decisions ~f:(fun (decision, count) ->
        printf "    %-13s %d\n" decision count)))
;;

let editor () =
  List.find_map [ "VISUAL"; "EDITOR" ] ~f:(fun name ->
    Option.filter (Sys.getenv name) ~f:(Fn.non String.is_empty))
  |> Option.value ~default:"vi"
;;

let ask question =
  Out_channel.output_string stdout question;
  Out_channel.flush stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> false
  | Some answer ->
    (match String.lowercase (String.strip answer) with
     | "" | "y" | "yes" -> true
     | _ -> false)
;;

(* The edit draft is recoverable, but SQLite is the sole persistent settings store. *)
let accounts_edit database =
  with_database database (fun db ->
    let%bind current = Accounts.export_bank_accounts db in
    let original = Bank_account_config.to_json current ^ "\n" in
    let draft = database ^ ".edit" in
    let%bind () =
      if Stdlib.Sys.file_exists draft
      then (
        printf "Continuing the draft that was not applied: %s\n" draft;
        Ok ())
      else write_file draft original
    in
    let discard () = ignore (Stdlib.Sys.remove draft : unit) in
    let rec edit () =
      match Stdlib.Sys.command (editor () ^ " " ^ Stdlib.Filename.quote draft) with
      | status when status <> 0 ->
        Or_error.errorf
          "the editor exited with status %d; your draft is in %s"
          status
          draft
      | _ ->
        let%bind contents = read_file draft in
        if String.equal contents original
        then (
          discard ();
          print_endline "No changes.";
          Ok ())
        else (
          match
            Result.bind (Bank_account_config.of_json contents) ~f:(fun edited ->
              Accounts.apply_bank_accounts db edited)
          with
          | Ok result ->
            discard ();
            printf
              "Applied %d change%s.\n"
              (List.length result.changes)
              (if List.length result.changes = 1 then "" else "s");
            List.iter result.changes ~f:(fun c ->
              printf
                "  %s %s %s: %s -> %s\n"
                c.institution
                c.source_account
                c.kind
                (Option.value c.previous_value ~default:"none")
                (Option.value c.value ~default:"none"));
            Ok ()
          | Error error ->
            eprintf "fin: %s\n" (Error.to_string_hum error);
            Out_channel.flush stderr;
            if Unix.isatty Unix.stdin && ask "Nothing was applied. Edit again? [Y/n] "
            then edit ()
            else Or_error.errorf "nothing was applied; your draft is in %s" draft)
    in
    edit ())
;;

let accounts_history database =
  with_database database (fun db ->
    let%bind rows = Accounts.bank_account_history db in
    Report_json.print (Jsont.list Report_json.account_change) rows;
    Ok ())
;;

let accounts_delete database account_id dry_run yes =
  with_database database (fun db ->
    let%bind accounts = Transfers.bank_account_info db in
    let matches =
      List.filter accounts ~f:(fun (a : Transfers.bank_account_info) ->
        String.equal (Bank_account_mapping.id a.mapping) account_id)
    in
    let%bind account =
      match matches with
      | [ account ] -> Ok account
      | [] -> Or_error.error_string "no matching account ID; see fin account list"
      | _ -> Or_error.error_string "account ID collision; deletion is refused"
    in
    let%bind preview = Accounts.delete_account ~dry_run:true db account.mapping in
    printf
      "Account: %s / %s (%s), ID %s\nTransactions: %d\nSource documents: %d\n"
      account.mapping.institution
      account.mapping.account_id
      account.mapping.account_type
      account_id
      preview.transactions
      preview.documents;
    if dry_run
    then (
      print_endline "Dry run: nothing deleted.";
      Ok ())
    else (
      let%bind confirmed =
        confirm ~yes ~destructive:true "Delete this account and its records?"
      in
      if not confirmed
      then (
        print_endline "Nothing deleted.";
        Ok ())
      else (
        let%bind _ = Accounts.delete_account db account.mapping in
        let journal =
          Stdlib.Filename.concat (Stdlib.Filename.dirname database) "bank.journal"
        in
        let%bind () =
          if Stdlib.Sys.file_exists journal
          then
            (let%bind contents = Finances_hledger.export db in
             write_file journal contents)
            |> Or_error.tag
                 ~tag:"account deleted; failed to refresh the generated journal"
          else Ok ()
        in
        print_endline "Account and its records deleted.";
        Ok ())))
;;

let accounts_link_card database card_id bank_id yes =
  with_database database (fun db ->
    let%bind accounts = Transfers.bank_account_info db in
    let find id =
      match
        List.filter accounts ~f:(fun a ->
          String.equal (Bank_account_mapping.id a.Transfers.mapping) id)
      with
      | [ account ] -> Ok account
      | [] -> Or_error.errorf "unknown account ID: %s" id
      | _ -> Or_error.errorf "account ID collision: %s" id
    in
    let%bind card = find card_id in
    let%bind bank = find bank_id in
    if
      not
        (List.mem
           [ "CREDITCRD"; "credit-card" ]
           card.mapping.account_type
           ~equal:String.equal)
    then Or_error.error_string "the first account must be a credit card"
    else if String.equal bank.iban_source "not_applicable" || String.equal card_id bank_id
    then
      Or_error.error_string "the settlement account must be a separate non-card account"
    else if not (Iso4217.equal card.currency bank.currency)
    then
      Or_error.error_string "the card and settlement account must use the same currency"
    else (
      let%bind applied = Settlement_cmd.setup_card db ~card ~bank ~yes ~allow_yes:true in
      if applied
      then print_endline "Card setup complete."
      else print_endline "Nothing changed.";
      Ok ()))
;;

let cmd =
  Cmd.group
    (Cmd.info
       "account"
       ~doc:"Manage bank account settings stored in SQLite; export and import snapshots.")
    [ command
        "list"
        "List account details, IBAN provenance, coverage and card settlement links."
        Term.(const list_bank_accounts $ database $ json_output)
    ; command
        "show"
        "Show one account: its details, cards, the card accounts it pays and how its \
         movements are decided."
        Term.(
          const show_account
          $ database
          $ Arg.(
              required
              & pos 0 (some string) None
              & info [] ~docv:"ID" ~doc:"Account ID shown by fin account list.")
          $ json_output)
    ; command
        "link-card"
        "Link a credit card to the account that pays it."
        Term.(
          const accounts_link_card
          $ database
          $ Arg.(
              required
              & pos 0 (some string) None
              & info [] ~docv:"CARD-ID" ~doc:"Credit card ID from fin account list.")
          $ Arg.(
              required
              & pos 1 (some string) None
              & info
                  []
                  ~docv:"ACCOUNT-ID"
                  ~doc:"Settlement account ID from fin account list.")
          $ Arg.(
              value
              & flag
              & info [ "yes" ] ~doc:"Apply the proposed setup without prompting."))
    ; command
        "edit"
        "Edit account names, IBANs and card settlement links in your editor, and apply \
         them on save."
        Term.(const accounts_edit $ database)
    ; command
        "delete"
        "Delete one account and its transactions after showing what will be removed."
        Term.(
          const accounts_delete
          $ database
          $ Arg.(
              required
              & pos 0 (some string) None
              & info [] ~docv:"ID" ~doc:"Account ID shown by fin account list.")
          $ Arg.(
              value
              & flag
              & info
                  [ "dry-run" ]
                  ~doc:"Show what would be deleted without changing data.")
          $ Arg.(
              value
              & flag
              & info [ "yes" ] ~doc:"Delete without interactive confirmation."))
    ; command
        "history"
        "Inspect audited ledger name, IBAN and settlement relationship changes."
        Term.(const accounts_history $ database)
    ]
;;
