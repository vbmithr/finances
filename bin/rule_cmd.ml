open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let rule_list database json_output =
  with_database database (fun db ->
    let%map rules = Import.classification_rules db in
    if json_output
    then Report_json.print (Jsont.list Report_json.classification_rule) rules
    else if List.is_empty rules
    then print_endline "No rules configured."
    else
      List.iter rules ~f:(fun r ->
        printf
          "%s  mode=%s  priority=%d  %s -> %s  /%s/  %s  account-type=%s  match-type=%s  \
           set-type=%s  field=%s\n"
          (Terminal.text r.name)
          (match r.mode with
           | Automatic -> "automatic"
           | Review -> "review"
           | Disabled -> "disabled")
          r.priority
          (Classification_rule.action_name r.action)
          (Terminal.text (Classification_rule.action_target r.action))
          (Terminal.text (Option.value r.pattern ~default:"any description"))
          (Terminal.text (Option.value r.institution ~default:"all institutions"))
          (Terminal.text (Option.value r.account_type ~default:"any"))
          (Option.value_map
             r.match_type
             ~default:"any"
             ~f:Classification_rule.condition_name)
          (Option.value_map
             (Classification_rule.action_type r.action)
             ~default:"none"
             ~f:Movement_type.to_string)
          (Terminal.text (Option.value r.field ~default:"code"))))
;;

let rule_list_cmd =
  command
    "list"
    "List saved transaction rules."
    Term.(const rule_list $ database $ json_output)
;;

let rule_mode database name value =
  let%bind mode =
    match value with
    | "automatic" -> Ok Classification_rule.Automatic
    | "review" -> Ok Classification_rule.Review
    | "disabled" -> Ok Classification_rule.Disabled
    | _ -> Or_error.error_string "mode must be automatic, review, or disabled"
  in
  with_database database (fun db ->
    let%map () = Ledger.set_classification_rule_mode db name mode in
    printf
      "Rule %s is now %s. Existing transactions are unchanged.\n"
      (Terminal.text name)
      value)
;;

let rule_mode_cmd =
  command
    "mode"
    "Set a rule to automatic, review, or disabled."
    Term.(
      const rule_mode
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"MODE"))
;;

let print_rule_preview (rule : Classification_rule.t) matches =
  let eligible, skipped = List.partition_tf matches ~f:(fun m -> m.Accounts.eligible) in
  printf
    "Rule %s: %s -> %s; %d eligible, %d already reviewed or ineligible.\n"
    (Terminal.text rule.name)
    (Classification_rule.action_name rule.action)
    (Terminal.text (Classification_rule.action_target rule.action))
    (List.length eligible)
    (List.length skipped);
  let show_current m =
    Terminal.review [ m.Accounts.transaction ];
    if Option.is_none m.transaction.movement_type_override
    then print_endline "  Type source: imported evidence (no manual override)."
  in
  if not (List.is_empty eligible)
  then (
    print_endline "Eligible matches:";
    List.iter eligible ~f:(fun m ->
      show_current m;
      if m.apply_type
      then
        Option.iter (Classification_rule.action_type rule.action) ~f:(fun kind ->
          printf "  Proposed type: %s\n" (Movement_type.to_string kind));
      if m.apply_account
      then
        printf
          "  Proposed %s: %s\n"
          (match rule.action with
           | Settlement _ -> "settlement"
           | _ -> "category")
          (Terminal.text (Classification_rule.action_target rule.action));
      Option.iter m.settlement_ledger ~f:(fun ledger ->
        if m.apply_account then printf "  Settlement account: %s\n" (Terminal.text ledger));
      Option.iter m.explanation ~f:(fun reason ->
        printf "  Skipped action: %s\n" (Terminal.text reason));
      print_endline ""));
  if not (List.is_empty skipped)
  then (
    print_endline "Skipped matches:";
    List.iter skipped ~f:(fun m ->
      show_current m;
      printf
        "  Skipped: %s\n\n"
        (Terminal.text (Option.value m.explanation ~default:"ineligible"))))
;;

let rule_from_category database operation_id name contains institution yes =
  with_database database (fun db ->
    let%bind event_id =
      Category_review_cmd.bank_event_ids_for_operations db [ operation_id ]
      |> Result.map ~f:List.hd_exn
    in
    let%bind rows = Review.bank_review db in
    let%bind seed =
      List.find rows ~f:(fun (r : Review.bank_review) -> Int64.equal r.event_id event_id)
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
    in
    let%bind () =
      if Poly.equal seed.classification Review.Category
      then Ok ()
      else Or_error.error_string "choose a categorized transaction"
    in
    let text = Option.value contains ~default:seed.description |> String.strip in
    let%bind () =
      if
        String.is_empty text
        || not (String.Caseless.is_substring seed.description ~substring:text)
      then
        Or_error.error_string
          "--contains must be nonempty text in the transaction description"
      else Ok ()
    in
    let pattern =
      String.to_list text
      |> List.map ~f:(fun c ->
        let s = String.of_char c in
        if
          List.mem
            [ '.'; '^'; '$'; '*'; '+'; '?'; '('; ')'; '['; ']'; '{'; '}'; '|'; '\\' ]
            c
            ~equal:Char.equal
        then "\\" ^ s
        else s)
      |> String.concat
    in
    let%bind rule =
      Classification_rule.create
        ~mode:Review
        ~account_type:None
        ~priority:1000
        ~name
        ~pattern:(Some pattern)
        ~institution
        ~match_type:None
        ~case_sensitive:false
        ~action:(Category seed.counter_account)
        ()
    in
    let%bind matches = Accounts.preview_classification_rule db rule in
    print_rule_preview rule matches;
    let%bind confirmed = confirm ~yes ~destructive:false "Save this review-mode rule?" in
    if not confirmed
    then (
      print_endline "Rule not saved.";
      Ok ())
    else (
      let%bind () = Ledger.add_classification_rule db rule in
      printf "Saved review rule %s.\n" (Terminal.text name);
      Ok ()))
;;

let rule_from_category_cmd =
  command
    "from-category"
    "Draft a literal description rule from a categorized transaction, show its matches \
     and ask before saving it."
    Term.(
      const rule_from_category
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"OPERATION-ID")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"NAME")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "contains" ]
              ~docv:"TEXT"
              ~doc:
                "Literal text from the example description; defaults to its full \
                 description.")
      $ Statement_cmd.institution_filter
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Save without asking, for scripts."))
;;

let rule_preview database name force json_output =
  with_database database (fun db ->
    let%bind rule = Rule_add_cmd.find_rule db name in
    let%bind () =
      match rule.action with
      | Card_reference | Withdrawal_reversal_hint ->
        Or_error.error_string
          "use fin operation refresh-evidence to preview automatic evidence changes"
      | Type _ when Option.is_some rule.field ->
        Or_error.error_string
          "this rule names a field and applies automatically; use fin rule test to \
           preview it"
      | _ -> Ok ()
    in
    let%map matches = Accounts.preview_classification_rule ~force db rule in
    if json_output
    then Report_json.print (Jsont.list Report_json.classification_rule_match) matches
    else print_rule_preview rule matches)
;;

let rule_preview_cmd =
  command
    "preview"
    "Preview a saved rule without changing transactions."
    Term.(
      const rule_preview
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Rule name from fin rule list.")
      $ Arg.(
          value
          & flag
          & info [ "force" ] ~doc:"Include existing categories that would change.")
      $ json_output)
;;

let rule_apply database name force yes =
  with_database database (fun db ->
    let%bind rule = Rule_add_cmd.find_rule db name in
    let%bind () =
      match rule.action with
      | Card_reference | Withdrawal_reversal_hint ->
        Or_error.error_string
          "use fin operation refresh-evidence to preview and apply evidence changes"
      | Type _ when Option.is_some rule.field ->
        Or_error.error_string
          "this rule names a field and applies automatically; use fin rule test to \
           preview it"
      | _ -> Ok ()
    in
    let%bind matches = Accounts.preview_classification_rule ~force db rule in
    print_rule_preview rule matches;
    let selected = List.filter matches ~f:(fun m -> m.Accounts.eligible) in
    if List.is_empty selected
    then (
      print_endline "Nothing to apply.";
      Ok ())
    else (
      let%bind confirmed =
        confirm ~yes ~destructive:false "Apply to these transactions?"
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%bind changed =
          Accounts.apply_classification_rule ~force db ~expected:rule ~selected
        in
        printf "rule_changes=%d\n" changed;
        Ok ())))
;;

let rule_apply_cmd =
  command
    "apply"
    "Preview and explicitly apply a saved rule."
    Term.(
      const rule_apply
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Rule name from fin rule list.")
      $ Arg.(
          value
          & flag
          & info [ "force" ] ~doc:"Replace existing categories on matching transactions.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Apply without prompting."))
;;

let rule_delete database name yes =
  with_database database (fun db ->
    let%bind _ = Rule_add_cmd.find_rule db name in
    let%bind confirmed = confirm ~yes ~destructive:true "Delete this rule?" in
    if not confirmed
    then (
      print_endline "Nothing deleted.";
      Ok ())
    else (
      let%bind () = Ledger.delete_classification_rule db name in
      print_endline "Rule deleted.";
      Ok ()))
;;

let rule_delete_cmd =
  command
    "delete"
    "Delete a saved rule; existing decisions are retained."
    Term.(
      const rule_delete
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"Rule name from fin rule list.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Delete without prompting."))
;;

let rule_rename database old_name new_name =
  with_database database (fun db ->
    let%map () = Ledger.rename_classification_rule db ~old_name ~new_name in
    printf "Renamed rule %s to %s.\n" (Terminal.text old_name) (Terminal.text new_name))
;;

let rule_rename_cmd =
  command
    "rename"
    "Rename a saved rule; existing decision history is retained."
    Term.(
      const rule_rename
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"OLD-NAME" ~doc:"Existing rule name.")
      $ Arg.(
          required
          & pos 1 (some string) None
          & info [] ~docv:"NEW-NAME" ~doc:"New, unused rule name."))
;;

let cmd =
  group
    "rule"
    "Manage automatic and reviewed transaction rules."
    [ rule_list_cmd
    ; rule_from_category_cmd
    ; rule_mode_cmd
    ; Rule_add_cmd.rule_add_cmd ~settlement:false
    ; Rule_add_cmd.rule_add_cmd ~settlement:true
    ; Rule_add_cmd.rule_add_evidence_cmd
    ; rule_preview_cmd
    ; rule_apply_cmd
    ; rule_delete_cmd
    ; rule_rename_cmd
    ; Rule_test_cmd.rule_test_cmd
    ; Rule_test_cmd.rule_explain_cmd
    ; Rule_test_cmd.rule_gaps_cmd
    ]
;;
