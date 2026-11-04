open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let find_rule db name =
  let%bind rules = Import.classification_rules db in
  List.find rules ~f:(fun r -> String.equal r.Classification_rule.name name)
  |> Or_error.of_option ~error:(Error.of_string ("unknown rule: " ^ name))
;;

let rule_add
      database
      name
      positional_pattern
      positional_target
      institution
      case_sensitive
      settlement
      match_type
      set_type
      category
      description
      automatic
      account_type
      priority
      field
      replace
  =
  let%bind pattern =
    match positional_pattern, description with
    | Some _, Some _ ->
      Or_error.error_string
        "provide the description either positionally or with --description"
    | Some p, None | None, Some p -> Ok (Some p)
    | None, None -> Ok None
  in
  let%bind target =
    match positional_target, category with
    | Some _, Some _ ->
      Or_error.error_string "provide the category either positionally or with --category"
    | Some c, None | None, Some c -> Ok (Some c)
    | None, None -> Ok None
  in
  let parse_type = function
    | None -> Ok None
    | Some value when String.is_empty (String.strip value) || String.equal value "unknown"
      -> Or_error.error_string "choose a movement type from fin operation type list"
    | Some value -> Ok (Some (Movement_type.of_string value))
  in
  let%bind match_type =
    match match_type with
    | Some "unknown" -> Ok (Some Classification_rule.Unknown)
    | other ->
      let%map kind = parse_type other in
      Option.map kind ~f:(fun kind -> Classification_rule.Is kind)
  in
  let%bind set_type = parse_type set_type in
  let%bind action =
    match settlement, target, set_type with
    | true, Some card, None -> Ok (Classification_rule.Settlement card)
    | true, _, _ ->
      Or_error.error_string
        "settlement rules require a card ID and do not set a movement type"
    | false, Some account, Some kind ->
      Ok (Classification_rule.Type_and_category (kind, account))
    | false, Some account, None -> Ok (Classification_rule.Category account)
    | false, None, Some kind -> Ok (Classification_rule.Type kind)
    | false, None, None -> Or_error.error_string "specify --set-type, --category, or both"
  in
  let%bind () =
    match automatic, action with
    | true, (Classification_rule.Type _ | Category _ | Type_and_category _) -> Ok ()
    | true, Settlement _ -> Or_error.error_string "settlement rules require review"
    | true, (Card_reference | Withdrawal_reversal_hint) -> Ok ()
    | false, _ -> Ok ()
  in
  let%bind rule =
    Classification_rule.create
      ~mode:(if automatic then Automatic else Review)
      ~account_type
      ~priority
      ~name
      ~pattern
      ~institution
      ~match_type
      ~case_sensitive
      ~action
      ?field
      ()
  in
  with_database database (fun db ->
    let%bind observed = Restore.observed_provider_movement_types db in
    let%bind () =
      List.filter_opt
        [ Option.bind match_type ~f:(function
            | Is kind -> Some kind
            | Unknown -> None)
        ; set_type
        ]
      |> List.map ~f:(function
        | Movement_type.Other name when not (List.mem observed name ~equal:String.equal)
          -> Or_error.error_string "unknown movement type; see fin operation type list"
        | _ -> Ok ())
      |> Or_error.combine_errors_unit
    in
    let%bind () =
      match action with
      | Settlement id ->
        let%bind accounts = Transfers.bank_account_info db in
        (match
           List.filter accounts ~f:(fun a ->
             String.equal (Bank_account_mapping.id a.Transfers.mapping) id)
         with
         | [ card ]
           when List.mem
                  [ "CREDITCRD"; "credit-card" ]
                  card.mapping.account_type
                  ~equal:String.equal -> Ok ()
         | [ _ ] -> Or_error.error_string "settlement target must be a credit card"
         | [] -> Or_error.error_string "unknown card ID; see fin account list"
         | _ -> Or_error.error_string "card ID collision; rule is refused")
      | _ -> Ok ()
    in
    let%bind () =
      if replace
      then Ledger.replace_classification_rule db rule
      else Ledger.add_classification_rule db rule
    in
    printf
      "Saved rule %s (%s). Run fin rule preview %s to review existing matches.\n"
      (Terminal.text name)
      (if automatic then "automatic on import" else "review")
      (Terminal.text name);
    Ok ())
;;

let field_doc =
  "Match this field instead of the main description: text (any description), or a field \
   the source carries such as name, memo, reference or type. Automatic type rules only."
;;

let field_arg ~settlement =
  if settlement
  then Term.const None
  else
    Arg.(value & opt (some string) None & info [ "field" ] ~docv:"FIELD" ~doc:field_doc)
;;

let rule_add_cmd ~settlement =
  let pattern_arg =
    if settlement
    then
      Term.(
        const Option.some
        $ Arg.(
            required
            & pos 1 (some string) None
            & info [] ~docv:"REGEX" ~doc:"Description expression."))
    else
      Arg.(
        value
        & pos 1 (some string) None
        & info
            []
            ~docv:"REGEX"
            ~doc:"Optional description expression; alternatively use --description.")
  in
  let target_arg =
    if settlement
    then
      Term.(
        const Option.some
        $ Arg.(
            required
            & pos 2 (some string) None
            & info [] ~docv:"CARD-ID" ~doc:"Credit card ID from fin account list."))
    else
      Arg.(
        value
        & pos 2 (some string) None
        & info [] ~docv:"CATEGORY" ~doc:"Optional category; alternatively use --category.")
  in
  let set_type_arg =
    if settlement
    then Term.const None
    else
      Arg.(
        value
        & opt (some string) None
        & info
            [ "set-type" ]
            ~docv:"TYPE"
            ~doc:"Propose this movement type; see fin operation type list.")
  in
  let category_arg =
    if settlement
    then Term.const None
    else
      Arg.(
        value
        & opt (some string) None
        & info
            [ "category" ]
            ~docv:"ACCOUNT"
            ~doc:"Propose this expense or income account.")
  in
  let description_arg =
    if settlement
    then Term.const None
    else
      Arg.(
        value
        & opt (some string) None
        & info
            [ "description" ]
            ~docv:"REGEX"
            ~doc:
              "Perl-style expression: automatic rules match the first source \
               description; reviewed rules also match display names.")
  in
  command
    (if settlement then "add-settlement" else "add")
    (if settlement
     then "Save a rule for a linked card's settlements."
     else "Save a rule to assign a type, category, or both.")
    Term.(
      const rule_add
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Unique rule name.")
      $ pattern_arg
      $ target_arg
      $ Statement_cmd.institution_filter
      $ Arg.(
          value
          & flag
          & info [ "case-sensitive" ] ~doc:"Match descriptions case sensitively.")
      $ const settlement
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "match-type" ]
              ~docv:"TYPE"
              ~doc:
                "Match a movement type or unknown; automatic type actions match the \
                 parser type, while category-only rules match the imported type.")
      $ set_type_arg
      $ category_arg
      $ description_arg
      $ Arg.(
          value
          & flag
          & info
              [ "automatic" ]
              ~doc:"Apply this rule on import (settlements require review).")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account-type" ] ~docv:"TYPE" ~doc:"Match this source account type.")
      $ Arg.(
          value
          & opt int 1000
          & info [ "priority" ] ~docv:"NUMBER" ~doc:"Rule order; lower numbers run first.")
      $ field_arg ~settlement
      $ Arg.(
          value
          & flag
          & info
              [ "replace" ]
              ~doc:"Replace the named rule; existing transactions are unchanged."))
;;

let rule_add_evidence
      database
      name
      action
      pattern
      institution
      case_sensitive
      account_type
      match_type
      priority
      replace
  =
  let%bind rule =
    Classification_rule.create
      ~mode:Automatic
      ~account_type
      ~priority
      ~name
      ~pattern:(Some pattern)
      ~institution
      ~match_type:(Option.map match_type ~f:Classification_rule.condition_of_string)
      ~case_sensitive
      ~action
      ()
  in
  with_database database (fun db ->
    let%bind () =
      if replace
      then Ledger.replace_classification_rule db rule
      else Ledger.add_classification_rule db rule
    in
    printf
      "Saved evidence rule %s. It applies to future imports; run fin operation \
       refresh-evidence to preview existing changes.\n"
      (Terminal.text name);
    Ok ())
;;

let rule_add_evidence_cmd =
  command
    "add-evidence"
    "Add an automatic settlement-reference or withdrawal-reversal evidence rule."
    Term.(
      const rule_add_evidence
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Unique rule name.")
      $ Arg.(
          required
          & pos
              1
              (some
                 (enum
                    [ "card-reference", Classification_rule.Card_reference
                    ; "withdrawal-reversal", Classification_rule.Withdrawal_reversal_hint
                    ]))
              None
          & info
              []
              ~docv:"KIND"
              ~doc:"Evidence kind: card-reference or withdrawal-reversal.")
      $ Arg.(
          required
          & pos 2 (some string) None
          & info
              []
              ~docv:"REGEX"
              ~doc:
                "Match the first source description; card-reference requires a single \
                 description and uses capture group 1.")
      $ Statement_cmd.institution_filter
      $ Arg.(value & flag & info [ "case-sensitive" ] ~doc:"Match case sensitively.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account-type" ] ~docv:"TYPE" ~doc:"Match this source account type.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "match-type" ]
              ~docv:"TYPE"
              ~doc:"Match the stored imported type, or unknown.")
      $ Arg.(
          value
          & opt int 1000
          & info [ "priority" ] ~docv:"NUMBER" ~doc:"Lower numbers run first.")
      $ Arg.(value & flag & info [ "replace" ] ~doc:"Replace an existing rule."))
;;
