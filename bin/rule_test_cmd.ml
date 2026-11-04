open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let print_type_rule_test
      (rule : Classification_rule.t)
      (result : Explain.type_rule_test)
      limit
  =
  let count f = List.count result.matches ~f:(fun r -> f r.Explain.outcome) in
  printf
    "Draft rule %s (automatic on import): %s -> %s, %s.\n"
    (Terminal.text rule.name)
    (Terminal.text (Option.value rule.pattern ~default:"any description"))
    (Terminal.text (Classification_rule.action_target rule.action))
    (Option.value_map rule.institution ~default:"all institutions" ~f:Terminal.text);
  printf "Scanned %d movements; %d match.\n" result.scanned (List.length result.matches);
  printf
    "  newly typed: %d\n\
    \  type changed: %d\n\
    \  already this type: %d\n\
    \  decided by an earlier rule: %d\n\
    \  manual type override: %d\n"
    (count (function
       | Newly_typed -> true
       | _ -> false))
    (count (function
       | Changed -> true
       | _ -> false))
    (count (function
       | Unchanged -> true
       | _ -> false))
    (count (function
       | Shadowed _ -> true
       | _ -> false))
    (count (function
       | Overridden -> true
       | _ -> false));
  printf
    "Movements without a type: %d -> %d.\n"
    result.untyped_before
    result.untyped_after;
  let name = Option.value_map ~default:"none" ~f:Movement_type.to_string in
  let section title select =
    let rows = List.filter result.matches ~f:(fun r -> select r.Explain.outcome) in
    if not (List.is_empty rows)
    then (
      printf "\n%s:\n" title;
      List.iter (List.take rows limit) ~f:(fun r ->
        printf
          "  #%Ld  %s  %s -> %s  %s%s\n"
          r.Explain.event_id
          (Terminal.text r.institution)
          (name r.before)
          (name r.after)
          (Terminal.text r.description)
          (match r.outcome with
           | Shadowed rule_name -> "  [" ^ Terminal.text rule_name ^ "]"
           | _ -> ""));
      let hidden = List.length rows - limit in
      if hidden > 0 then printf "  ... %d more (use --limit)\n" hidden)
  in
  section "Type changes" (function
    | Explain.Changed -> true
    | _ -> false);
  section "Decided by an earlier rule; the draft changes nothing here" (function
    | Explain.Shadowed _ -> true
    | _ -> false);
  section "Newly typed" (function
    | Explain.Newly_typed -> true
    | _ -> false);
  section "Manual type override; unchanged" (function
    | Explain.Overridden -> true
    | _ -> false)
;;

let rule_test
      database
      name
      positional_pattern
      institution
      case_sensitive
      match_type
      set_type
      description
      account_type
      priority
      field
      limit
      json_output
  =
  let%bind pattern =
    match positional_pattern, description with
    | Some _, Some _ ->
      Or_error.error_string
        "provide the description either positionally or with --description"
    | Some p, None | None, Some p -> Ok (Some p)
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
  let%bind kind =
    let%bind kind = parse_type set_type in
    Or_error.of_option
      kind
      ~error:(Error.of_string "specify --set-type: only type rules can be tested")
  in
  let%bind rule =
    Classification_rule.create
      ~mode:Automatic
      ~account_type
      ~priority
      ~name
      ~pattern
      ~institution
      ~match_type
      ~case_sensitive
      ~action:(Classification_rule.Type kind)
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
        ; Some kind
        ]
      |> List.map ~f:(function
        | Movement_type.Other name when not (List.mem observed name ~equal:String.equal)
          -> Or_error.error_string "unknown movement type; see fin operation type list"
        | _ -> Ok ())
      |> Or_error.combine_errors_unit
    in
    let%map result = Explain.test_type_rule db rule in
    if json_output
    then Report_json.print Report_json.type_rule_test result
    else print_type_rule_test rule result limit)
;;

let rule_test_cmd =
  command
    "test"
    "Test a draft type rule against stored movements without saving it."
    Term.(
      const rule_test
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"NAME"
              ~doc:"Draft rule name; a saved rule with this name is replaced in the test.")
      $ Arg.(
          value
          & pos 1 (some string) None
          & info
              []
              ~docv:"REGEX"
              ~doc:"Optional description expression; alternatively use --description.")
      $ Statement_cmd.institution_filter
      $ Arg.(
          value
          & flag
          & info [ "case-sensitive" ] ~doc:"Match descriptions case sensitively.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "match-type" ]
              ~docv:"TYPE"
              ~doc:"Match the type the parser reported, or unknown.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "set-type" ]
              ~docv:"TYPE"
              ~doc:"The movement type the rule assigns; see fin operation type list.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "description" ]
              ~docv:"REGEX"
              ~doc:"Perl-style expression matched against the first source description.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account-type" ] ~docv:"TYPE" ~doc:"Match this source account type.")
      $ Arg.(
          value
          & opt int 1000
          & info [ "priority" ] ~docv:"NUMBER" ~doc:"Rule order; lower numbers run first.")
      $ Rule_add_cmd.field_arg ~settlement:false
      $ Arg.(
          value
          & opt int 10
          & info [ "limit" ] ~docv:"COUNT" ~doc:"Examples shown per outcome.")
      $ json_output)
;;

let type_name = Option.value_map ~default:"no type" ~f:Movement_type.to_string

let print_explain_checks ~all (checks : Explanation.check list) =
  let hidden = ref 0 in
  List.iter checks ~f:(fun c ->
    match c.verdict with
    | Decides kind ->
      printf
        "       decides  %s  /%s/ -> %s   [%s]\n"
        (Terminal.text c.name)
        (Terminal.text c.pattern)
        (match kind with
         | Some kind -> Terminal.text (Movement_type.to_string kind)
         | None -> "leaves it untyped")
        (Terminal.text c.input)
    | Also_matches ->
      printf
        "       matches too, but an earlier rule decides  %s  /%s/   [%s]\n"
        (Terminal.text c.name)
        (Terminal.text c.pattern)
        (Terminal.text c.input)
    | Does_not_match reason ->
      if all
      then
        printf
          "       no match  %s  /%s/: %s\n"
          (Terminal.text c.name)
          (Terminal.text c.pattern)
          reason
      else Int.incr hidden);
  if !hidden > 0 then printf "       %d rules did not match (use --all)\n" !hidden;
  if List.is_empty checks then print_endline "       no rules apply"
;;

let print_explanation ~all (e : Explanation.t) =
  printf
    "Movement #%Ld  %s  %s  %s %s  (account type %s)\n"
    e.event_id
    (Terminal.text e.institution)
    e.booked_on
    e.amount
    e.currency
    (Terminal.text e.account_type);
  List.iter e.descriptions ~f:(fun d -> printf "  Description: %s\n" (Terminal.text d));
  printf
    "\nType: %s  (%s)\n"
    (type_name e.effective)
    (match e.source with
     | Manual_override -> "manual override"
     | Rules -> "type rules"
     | Untyped -> "nothing decides it");
  printf
    "  1. Manual override: %s\n"
    (Option.value_map e.override ~default:"none" ~f:Movement_type.to_string);
  printf
    "  2. Type rules, each on the field it names (default: description). Companion \
     columns count as fields:\n";
  print_explain_checks ~all e.checks;
  printf "     the rules give: %s\n\n" (type_name e.rules_type);
  match e.category with
  | Some (account, reason) ->
    printf "Category: %s (%s)\n" (Terminal.text account) (Terminal.text reason)
  | None ->
    print_endline "Category: none recorded. Category rules: see fin rule preview NAME."
;;

let rule_explain database operation_id all =
  with_database database (fun db ->
    let%bind event_id =
      Category_review_cmd.bank_event_ids_for_operations db [ operation_id ]
      |> Result.map ~f:List.hd_exn
    in
    let%map explanation = Explain.explain_movement db event_id in
    print_explanation ~all explanation)
;;

let rule_explain_cmd =
  command
    "explain"
    "Explain how a movement got its type, rule by rule."
    Term.(
      const rule_explain
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"OPERATION-ID" ~doc:"Operation ID from fin operation list.")
      $ Arg.(
          value
          & flag
          & info [ "all" ] ~doc:"Also list the rules that did not match, with the reason."))
;;

let print_description_gaps gaps =
  if List.is_empty gaps
  then print_endline "Every movement has a type."
  else
    List.iter gaps ~f:(fun (g : Explain.description_gap) ->
      printf
        "%4d  %s  %s  %s..%s  debits %d, credits %d\n"
        g.count
        (Terminal.text g.institution)
        (if String.is_empty g.key
         then "(blank, or starts with a digit)"
         else Terminal.text ("\"" ^ g.key ^ "\""))
        (Date.to_string g.first_on)
        (Date.to_string g.last_on)
        g.debits
        g.credits;
      printf "      example: %s\n" (Terminal.text g.example);
      if not (String.is_empty g.pattern)
      then printf "      pattern: %s\n" (Terminal.text g.pattern))
;;

let rule_gaps database institution limit json_output =
  with_database database (fun db ->
    let%map gaps = Explain.description_gaps ?institution db in
    let gaps = List.take gaps limit in
    if json_output
    then Report_json.print (Jsont.list Report_json.description_gap) gaps
    else print_description_gaps gaps)
;;

let rule_gaps_cmd =
  command
    "gaps"
    "List movements without a type, grouped by how their descriptions start."
    Term.(
      const rule_gaps
      $ database
      $ Statement_cmd.institution_filter
      $ Arg.(
          value
          & opt int 20
          & info [ "limit" ] ~docv:"COUNT" ~doc:"Maximum number of groups.")
      $ json_output)
;;
