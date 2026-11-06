open Base
open Stdio
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let date = Option.value_map ~default:"-" ~f:Date.to_string

let card_json =
  let date =
    Jsont.option (Jsont.map Jsont.string ~dec:Date.of_string ~enc:Date.to_string)
  in
  Jsont.Object.enc_only ()
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun (c : Cards.t) -> c.institution)
  |> Jsont.Object.mem "card" Jsont.string ~enc:(fun (c : Cards.t) -> c.card_reference)
  |> Jsont.Object.mem "known_by" (Jsont.option Jsont.string) ~enc:(fun (c : Cards.t) ->
    c.known_by)
  |> Jsont.Object.mem "account_id" (Jsont.option Jsont.string) ~enc:(fun (c : Cards.t) ->
    c.account_id)
  |> Jsont.Object.mem
       "ledger_account"
       (Jsont.option Jsont.string)
       ~enc:(fun (c : Cards.t) -> c.ledger_account)
  |> Jsont.Object.mem "bank_rows" Jsont.int ~enc:(fun (c : Cards.t) -> c.bank_rows)
  |> Jsont.Object.mem "first_seen" date ~enc:(fun (c : Cards.t) -> c.first_seen)
  |> Jsont.Object.mem "last_seen" date ~enc:(fun (c : Cards.t) -> c.last_seen)
  |> Jsont.Object.mem "settled" Jsont.int ~enc:(fun (c : Cards.t) -> c.settled)
  |> Jsont.Object.mem "undecided" Jsont.int ~enc:(fun (c : Cards.t) -> c.undecided)
  |> Jsont.Object.mem "purchases_from" date ~enc:(fun (c : Cards.t) -> c.purchases_from)
  |> Jsont.Object.finish
;;

let card_list database json_output =
  with_database database (fun db ->
    let%map cards = Cards.list db in
    if json_output
    then Report_json.print (Jsont.list card_json) cards
    else if List.is_empty cards
    then print_endline "No cards yet: no statement names a card."
    else (
      Terminal.table
        [ "Institution"
        ; "Card"
        ; "Account ID"
        ; "Account"
        ; "Known by"
        ; "Bank rows"
        ; "From"
        ; "To"
        ; "Settled"
        ; "Undecided"
        ; "Purchases from"
        ]
        (List.map cards ~f:(fun (c : Cards.t) ->
           [ c.institution
           ; c.card_reference
           ; Option.value c.account_id ~default:"-"
           ; Option.value c.ledger_account ~default:"unassigned"
           ; Option.value c.known_by ~default:"-"
           ; Int.to_string c.bank_rows
           ; date c.first_seen
           ; date c.last_seen
           ; Int.to_string c.settled
           ; Int.to_string c.undecided
           ; date c.purchases_from
           ]));
      if List.exists cards ~f:(fun c -> Option.is_none c.known_by && c.undecided > 0)
      then
        print_endline
          "\n\
           An unassigned card's debits cannot settle. If it belonged to a card account, \
           run fin card assign CARD ACCOUNT-ID.";
      if
        List.exists cards ~f:(fun c ->
          match c.first_seen, c.purchases_from with
          | Some first, Some from -> Date.( < ) first from
          | _ -> false)
      then
        print_endline
          "Some cards were paid before their account's purchases begin: settling those \
           payments leaves the account without the purchases behind them."))
;;

let card_list_cmd =
  command
    "list"
    "List the cards your statements name and the card accounts they belong to."
    Term.(
      const card_list $ database $ Arg.(value & flag & info [ "json" ] ~doc:"Print JSON."))
;;

let card_assign database card account_id yes =
  with_database database (fun db ->
    let%bind plan = Cards.preview_assign db ~card ~account_id in
    match plan with
    | None ->
      printf "Card %s already belongs to account %s.\n" card account_id;
      Ok ()
    | Some (a : Cards.assignment) ->
      printf
        "Card %s (%s) belongs to %s (%s).\n\
         %d bank rows name it, from %s to %s; %d are undecided.\n"
        a.card.card_reference
        a.card.institution
        a.account_id
        a.ledger_account
        a.card.bank_rows
        (date a.card.first_seen)
        (date a.card.last_seen)
        a.card.undecided;
      let%bind confirmed = confirm ~yes ~destructive:false "Record this card?" in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%bind () = Cards.assign db a in
        let%map candidates = Settlement_cmd.settlement_suggestions db in
        let offered =
          List.count candidates ~f:(fun (s : Settlement_cmd.settlement_suggestion) ->
            Option.equal
              String.equal
              s.transaction.source_evidence.card_reference
              (Some a.card.card_reference))
        in
        printf
          "Recorded. fin settlement candidates now offers %d of its debits.\n"
          offered))
;;

let card_assign_cmd =
  command
    "assign"
    "Record that a card number belongs to a card account, such as a replaced card's."
    ~man:
      [ `S Manpage.s_description
      ; `P
          "A card account's own number is one of its cards. A bank may replace a card, \
           or add one, without changing the account; its settlement debits then name a \
           number fin cannot match. Assigning the number lets those debits settle. See \
           $(b,fin card list)."
      ]
    Term.(
      const card_assign
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"CARD"
              ~doc:"The card as fin card list prints it, or a unique ending of it.")
      $ Arg.(
          required
          & pos 1 (some string) None
          & info [] ~docv:"ACCOUNT-ID" ~doc:"A card account from fin account list.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Confirm without prompting."))
;;

let card_unassign database card yes =
  with_database database (fun db ->
    let%bind card = Cards.preview_unassign db ~card in
    printf
      "Card %s will no longer belong to %s.\n"
      card.card_reference
      (Option.value card.ledger_account ~default:"-");
    let%bind confirmed = confirm ~yes ~destructive:false "Remove this card?" in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () = Cards.unassign db card in
      print_endline "Removed."))
;;

let card_unassign_cmd =
  command
    "unassign"
    "Remove a card assignment that no recorded settlement uses."
    Term.(
      const card_unassign
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"CARD"
              ~doc:"The card as fin card list prints it, or a unique ending of it.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Confirm without prompting."))
;;

let cmd =
  group
    "card"
    "List cards and the card accounts they belong to."
    [ card_list_cmd; card_assign_cmd; card_unassign_cmd ]
;;
