open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

type settlement_suggestion =
  { transaction : Review.bank_review
  ; card : Transfers.bank_account_info
  ; bank : Transfers.bank_account_info
  }

let account_reference (mapping : Bank_account_mapping.t) =
  { Bank_account_config.institution = mapping.institution
  ; bank_id = mapping.bank_id
  ; account_id = mapping.account_id
  ; account_type = mapping.account_type
  }
;;

let settlement_suggestions db =
  let%bind rows = Review.bank_review db in
  let%bind accounts = Transfers.bank_account_info db in
  let suggestions =
    List.filter_map rows ~f:(fun (row : Review.bank_review) ->
      let movement = row.transaction in
      let hint = row.source_evidence.card_reference in
      Option.bind hint ~f:(fun hint ->
        if
          (not (String.equal row.counter_account "equity:unclassified"))
          || Option.is_some row.reason
          || Quantity.(movement.amount >= zero)
          || not (Quantity.is_zero movement.fee)
        then None
        else (
          let banks =
            List.filter accounts ~f:(fun a ->
              String.equal a.Transfers.mapping.institution row.institution
              && String.equal a.mapping.bank_id movement.account.bank_id
              && String.equal a.mapping.account_id movement.account.account_id
              && String.equal a.mapping.account_type movement.account.account_type)
          in
          match banks with
          | [ bank ] when not (String.equal bank.iban_source "not_applicable") ->
            let parent = account_reference bank.mapping in
            let cards =
              List.filter accounts ~f:(fun card ->
                String.equal card.Transfers.mapping.institution row.institution
                && String.equal card.mapping.bank_id movement.account.bank_id
                && String.equal card.mapping.account_id hint
                && String.equal card.mapping.account_type "CREDITCRD"
                && Iso4217.equal card.currency movement.account.currency
                && Option.for_all card.settlement_account ~f:(fun linked ->
                  String.equal linked.institution parent.institution
                  && String.equal
                       (Bank_account_config.source_account linked)
                       (Bank_account_config.source_account parent)))
            in
            (match cards with
             | [ card ] ->
               let card_has_payment =
                 List.exists rows ~f:(fun other ->
                   let tx = other.Review.transaction in
                   String.equal other.institution card.mapping.institution
                   && String.equal tx.account.bank_id card.mapping.bank_id
                   && String.equal tx.account.account_id card.mapping.account_id
                   && String.equal tx.account.account_type card.mapping.account_type
                   && Iso4217.equal tx.account.currency movement.account.currency
                   && Quantity.equal tx.amount (Quantity.neg movement.amount)
                   && Int.abs (Date.diff tx.booked_on movement.booked_on) <= 7)
               in
               Option.some_if (not card_has_payment) { transaction = row; card; bank }
             | _ -> None)
          | _ -> None)))
  in
  Ok suggestions
;;

let print_settlement_suggestions suggestions =
  if List.is_empty suggestions
  then print_endline "No unambiguous card settlements found."
  else
    List.iter suggestions ~f:(fun s ->
      printf
        "#%Ld  %s  %s %s  %s / %s\n    %s\n"
        s.transaction.event_id
        (Date.to_string s.transaction.transaction.booked_on)
        (Quantity.to_string s.transaction.transaction.amount)
        (Iso4217.to_alpha_code s.transaction.transaction.account.currency)
        (Terminal.text s.card.mapping.institution)
        (Terminal.text s.card.mapping.account_id)
        (Terminal.text (String.concat ~sep:" | " s.transaction.transaction.descriptions)))
;;

let settlement_candidates_cmd =
  command
    "candidates"
    "Find unclassified card settlements with a unique card match."
    Term.(
      const (fun database ->
        with_database database (fun db ->
          let%map suggestions = settlement_suggestions db in
          print_settlement_suggestions suggestions))
      $ database)
;;

let card_setup_plan
      ~(card : Transfers.bank_account_info)
      ~(bank : Transfers.bank_account_info)
  =
  let proposed =
    if String.is_prefix card.mapping.ledger_account ~prefix:"liabilities:"
    then card.mapping.ledger_account
    else
      "liabilities:cards:"
      ^ card.mapping.institution
      ^ ":"
      ^ Bank_account_mapping.id card.mapping
  in
  let parent = account_reference bank.mapping in
  let needs_link =
    not
      (Option.value_map card.settlement_account ~default:false ~f:(fun linked ->
         String.equal linked.institution parent.institution
         && String.equal
              (Bank_account_config.source_account linked)
              (Bank_account_config.source_account parent)))
  in
  let needs_mapping = not (String.equal proposed card.mapping.ledger_account) in
  let update =
    Bank_account_config.
      { reference = account_reference card.mapping
      ; ledger_account = (if needs_mapping then Some proposed else None)
      ; iban = Keep
      ; settlement_account = (if needs_link then Set parent else Keep)
      }
  in
  proposed, needs_link, needs_mapping, update
;;

let setup_card
      db
      ~(card : Transfers.bank_account_info)
      ~(bank : Transfers.bank_account_info)
      ~yes
      ~allow_yes
  =
  let proposed, needs_link, needs_mapping, update = card_setup_plan ~card ~bank in
  if (not needs_link) && not needs_mapping
  then Ok true
  else if yes && not allow_yes
  then Or_error.error_string "card setup requires interactive confirmation"
  else if (not yes) && not (Unix.isatty Unix.stdin)
  then Or_error.error_string "card setup requires a terminal"
  else (
    printf
      "Card setup proposal: %s / %s\n"
      (Terminal.text card.mapping.institution)
      (Terminal.text card.mapping.account_id);
    if needs_mapping
    then
      printf
        "  Card account: %s -> %s\n"
        (Terminal.text card.mapping.ledger_account)
        (Terminal.text proposed);
    if needs_link
    then
      printf
        "  Pay from: %s / %s\n"
        (Terminal.text bank.mapping.institution)
        (Terminal.text bank.mapping.account_id);
    let%bind confirmed = confirm ~yes ~destructive:false "Apply this card setup?" in
    if not confirmed
    then Ok false
    else (
      let%map _ = Accounts.apply_bank_accounts db [ update ] in
      true))
;;

let settlement_tui db suggestions =
  if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout)
  then Or_error.error_string "the full-screen review requires a terminal"
  else if List.is_empty suggestions
  then (
    print_endline "No unambiguous card settlements found.";
    Ok ())
  else (
    let choices =
      List.map suggestions ~f:(fun s ->
        let proposed, needs_link, needs_mapping, _ =
          card_setup_plan ~card:s.card ~bank:s.bank
        in
        let movement = s.transaction.transaction in
        let currency = Iso4217.to_alpha_code movement.account.currency in
        let title =
          Printf.sprintf
            "#%Ld  %s  %s %s"
            s.transaction.event_id
            (Date.to_string movement.booked_on)
            (Quantity.to_string movement.amount)
            currency
        in
        let details =
          [ "Description: " ^ String.concat ~sep:" | " movement.descriptions
          ; "Checking: " ^ s.bank.mapping.institution ^ " / " ^ s.bank.mapping.account_id
          ; "Card: " ^ s.card.mapping.institution ^ " / " ^ s.card.mapping.account_id
          ; "Card repayment: "
            ^ Quantity.to_string (Quantity.neg movement.amount)
            ^ " "
            ^ currency
          ; "Card account: " ^ proposed
          ]
          @ (if needs_mapping
             then [ "Change card account from: " ^ s.card.mapping.ledger_account ]
             else [])
          @ (if needs_link
             then [ "Link card to the checking account shown above." ]
             else [])
          @ [ "Only this settlement will be recorded." ]
        in
        Settlement_tui.{ title; details })
    in
    match Settlement_tui.run choices with
    | None ->
      print_endline "Nothing changed.";
      Ok ()
    | Some index ->
      let s = List.nth_exn suggestions index in
      let _, needs_link, needs_mapping, setup =
        card_setup_plan ~card:s.card ~bank:s.bank
      in
      let%bind () =
        if needs_link || needs_mapping
        then
          Accounts.confirm_card_settlement_with_setup
            db
            ~event_id:s.transaction.event_id
            ~card_id:(Bank_account_mapping.id s.card.mapping)
            ~expected_ledger:s.card.mapping.ledger_account
            ~expected_link:s.card.settlement_account
            ~setup
        else
          Transfers.confirm_card_settlement
            db
            s.transaction.event_id
            ~card_id:(Bank_account_mapping.id s.card.mapping)
            ~expected_ledger:s.card.mapping.ledger_account
      in
      print_endline
        "Card settlement recorded. Use fin journal export for an updated journal.";
      Ok ())
;;

(* Every suggestion at one confirmation, card by card. Setting up a card happens with its
   first settlement, and stays if a later one fails: rerunning settles what is left. *)
let settle_all db suggestions ~yes =
  let cards =
    List.sort_and_group suggestions ~compare:(fun a b ->
      String.compare
        (Bank_account_mapping.id a.card.mapping)
        (Bank_account_mapping.id b.card.mapping))
  in
  if List.is_empty cards
  then (
    print_endline "No unambiguous card settlements found.";
    Ok ())
  else (
    let plans =
      List.map cards ~f:(fun group ->
        let first = List.hd_exn group in
        group, first, card_setup_plan ~card:first.card ~bank:first.bank)
    in
    let needs_setup =
      List.exists plans ~f:(fun (_, _, (_, needs_link, needs_mapping, _)) ->
        needs_link || needs_mapping)
    in
    if yes && needs_setup
    then Or_error.error_string "card setup requires interactive confirmation"
    else (
      List.iter plans ~f:(fun (group, first, (proposed, needs_link, needs_mapping, _)) ->
        let total =
          List.fold group ~init:Quantity.zero ~f:(fun sum s ->
            Quantity.(sum - s.transaction.transaction.amount))
        in
        printf
          "%s / %s: %d settlements, %s %s\n"
          (Terminal.text first.card.mapping.institution)
          (Terminal.text first.card.mapping.account_id)
          (List.length group)
          (Quantity.to_string total)
          (Iso4217.to_alpha_code first.transaction.transaction.account.currency);
        if needs_mapping
        then
          printf
            "  Card account: %s -> %s\n"
            (Terminal.text first.card.mapping.ledger_account)
            (Terminal.text proposed);
        if needs_link
        then
          printf
            "  Pay from: %s / %s\n"
            (Terminal.text first.bank.mapping.institution)
            (Terminal.text first.bank.mapping.account_id));
      print_endline
        "If a card statement already includes a payment, review it as a transfer instead.";
      let count = List.length suggestions in
      let%bind confirmed =
        confirm
          ~yes
          ~destructive:false
          (Printf.sprintf
             "%s %d card settlements?"
             (if needs_setup then "Apply this setup and record" else "Record")
             count)
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%bind () =
          List.map
            plans
            ~f:(fun (group, first, (proposed, needs_link, needs_mapping, setup)) ->
              let card_id = Bank_account_mapping.id first.card.mapping in
              let settle (s : settlement_suggestion) ~expected_ledger =
                Transfers.confirm_card_settlement
                  ~card_id
                  ~expected_ledger
                  db
                  s.transaction.event_id
              in
              if needs_link || needs_mapping
              then (
                let%bind () =
                  Accounts.confirm_card_settlement_with_setup
                    db
                    ~event_id:first.transaction.event_id
                    ~card_id
                    ~expected_ledger:first.card.mapping.ledger_account
                    ~expected_link:first.card.settlement_account
                    ~setup
                in
                List.map (List.tl_exn group) ~f:(settle ~expected_ledger:proposed)
                |> Or_error.all_unit)
              else
                List.map
                  group
                  ~f:(settle ~expected_ledger:first.card.mapping.ledger_account)
                |> Or_error.all_unit)
          |> Or_error.all_unit
        in
        printf
          "%d card settlements recorded. Use fin journal export for an updated journal.\n"
          count;
        Ok ())))
;;

let settlement_review database requested_event card_id all yes tui =
  with_database database (fun db ->
    let%bind suggestions = settlement_suggestions db in
    let suggestions =
      List.filter suggestions ~f:(fun s ->
        Option.value_map
          card_id
          ~default:true
          ~f:(String.equal (Bank_account_mapping.id s.card.mapping)))
    in
    if all
    then
      if Option.is_some requested_event || tui
      then Or_error.error_string "--all cannot be combined with EVENT-ID or --tui"
      else settle_all db suggestions ~yes
    else if tui
    then
      if yes
      then Or_error.error_string "--tui cannot be combined with --yes"
      else (
        let suggestions =
          List.filter suggestions ~f:(fun s ->
            Option.value_map
              requested_event
              ~default:true
              ~f:(Int64.equal s.transaction.event_id))
        in
        settlement_tui db suggestions)
    else (
      let%bind event_id =
        match requested_event with
        | Some id -> Ok (Some id)
        | None when yes -> Or_error.error_string "provide EVENT-ID when using --yes"
        | None when not (Unix.isatty Unix.stdin) ->
          Or_error.error_string "select a settlement in a terminal or provide EVENT-ID"
        | None ->
          print_settlement_suggestions suggestions;
          if List.is_empty suggestions
          then Ok None
          else (
            let rec choose () =
              match prompt "Choose an event ID (or q): " with
              | None -> Ok None
              | Some answer when String.Caseless.equal (String.strip answer) "q" ->
                Ok None
              | Some answer ->
                (match
                   Option.try_with (fun () -> Int64.of_string (String.strip answer))
                   |> Option.bind ~f:(fun id ->
                     List.find suggestions ~f:(fun s ->
                       Int64.equal s.transaction.event_id id))
                 with
                 | Some selected -> Ok (Some selected.transaction.event_id)
                 | None ->
                   eprintf "Choose a listed event ID or q.\n";
                   choose ())
            in
            choose ())
      in
      match event_id with
      | None ->
        print_endline "Nothing changed.";
        Ok ()
      | Some event_id ->
        let suggestion =
          List.find suggestions ~f:(fun s ->
            Int64.equal s.transaction.event_id event_id
            && Option.value_map
                 card_id
                 ~default:true
                 ~f:(String.equal (Bank_account_mapping.id s.card.mapping)))
        in
        let%bind row, card, planned =
          match suggestion with
          | None ->
            let%map row, card = Transfers.card_settlement_preview ?card_id db event_id in
            row, card, None
          | Some s ->
            let proposed, needs_link, needs_mapping, update =
              card_setup_plan ~card:s.card ~bank:s.bank
            in
            if (not needs_link) && not needs_mapping
            then (
              let%map row, card =
                Transfers.card_settlement_preview ?card_id db event_id
              in
              row, card, None)
            else if yes
            then Or_error.error_string "card setup requires interactive confirmation"
            else
              Ok
                ( s.transaction
                , s.card
                , Some (s, proposed, needs_link, needs_mapping, update) )
        in
        print_endline "Bank transaction:";
        Terminal.review [ row ];
        printf
          "Card: %s / %s (ID %s)\nCard account: %s\n"
          (Terminal.text card.mapping.institution)
          (Terminal.text card.mapping.account_id)
          (Bank_account_mapping.id card.mapping)
          (Terminal.text
             (Option.value_map
                planned
                ~default:card.mapping.ledger_account
                ~f:(fun (_, proposed, _, _, _) -> proposed)));
        Option.iter planned ~f:(fun (s, proposed, needs_link, needs_mapping, _) ->
          print_endline "Proposed card setup (saved only if you confirm):";
          if needs_mapping
          then
            printf
              "  Card account: %s -> %s\n"
              (Terminal.text card.mapping.ledger_account)
              (Terminal.text proposed);
          if needs_link
          then
            printf
              "  Pay from: %s / %s\n"
              (Terminal.text s.bank.mapping.institution)
              (Terminal.text s.bank.mapping.account_id));
        printf
          "Card repayment: %s %s\n"
          (Quantity.to_string (Quantity.neg row.transaction.amount))
          (Iso4217.to_alpha_code row.transaction.account.currency);
        print_endline
          "If the card statement already includes this payment, review it as a transfer \
           instead.";
        let%bind confirmed =
          confirm
            ~yes
            ~destructive:false
            (if Option.is_some planned
             then "Apply this setup and record this one settlement?"
             else "Record this card settlement?")
        in
        if not confirmed
        then (
          print_endline "Nothing changed.";
          Ok ())
        else (
          let%bind () =
            match planned with
            | None ->
              Transfers.confirm_card_settlement
                ~card_id:(Bank_account_mapping.id card.mapping)
                ~expected_ledger:card.mapping.ledger_account
                db
                event_id
            | Some (s, _, _, _, setup) ->
              Accounts.confirm_card_settlement_with_setup
                db
                ~event_id
                ~card_id:(Bank_account_mapping.id s.card.mapping)
                ~expected_ledger:s.card.mapping.ledger_account
                ~expected_link:s.card.settlement_account
                ~setup
          in
          print_endline
            "Card settlement recorded. Use fin journal export for an updated journal.";
          Ok ())))
;;

let settlement_review_cmd =
  command
    "review"
    "Review and confirm a bank debit as a linked card settlement."
    Term.(
      const settlement_review
      $ database
      $ Arg.(
          value
          & pos 0 (some int64) None
          & info
              []
              ~docv:"EVENT-ID"
              ~doc:
                "Bank transaction ID; omit to choose a suggested settlement in a \
                 terminal.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "card" ]
              ~docv:"ID"
              ~doc:"Card account ID, needed if several cards are linked.")
      $ Arg.(
          value
          & flag
          & info
              [ "all" ]
              ~doc:
                "Record every settlement fin settlement candidates lists, or those of \
                 --card, at one confirmation.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Confirm without prompting.")
      $ Arg.(
          value & flag & info [ "tui" ] ~doc:"Use the experimental full-screen review."))
;;

let settlement_undo database event_id yes =
  with_database database (fun db ->
    let%bind confirmed = confirm ~yes ~destructive:false "Undo this card settlement?" in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%bind () = Transfers.undo_card_settlement db event_id in
      print_endline
        "Card settlement undone. Use fin journal export for an updated journal.";
      Ok ()))
;;

let settlement_undo_cmd =
  command
    "undo"
    "Undo a confirmed card settlement."
    Term.(
      const settlement_undo
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"EVENT-ID" ~doc:"Confirmed settlement event ID.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Undo without prompting."))
;;

let settlement_history_cmd =
  command
    "history"
    "List card settlement decisions as JSON."
    Term.(
      const (fun database ->
        with_database database (fun db ->
          let%map rows = Transfers.card_settlement_history db in
          Report_json.print (Jsont.list Report_json.card_settlement_change) rows))
      $ database)
;;

let cmd =
  group
    "settlement"
    "Review credit-card settlements."
    [ settlement_candidates_cmd
    ; settlement_review_cmd
    ; settlement_undo_cmd
    ; settlement_history_cmd
    ]
;;
