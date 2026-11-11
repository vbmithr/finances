open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let definition_rules (definition : Institution_definition.t) =
  let name = Institution_definition.institution definition in
  let%bind () =
    if
      List.contains_dup
        (List.map definition.rules ~f:(fun r -> r.name))
        ~compare:String.compare
    then Or_error.error_string "definition has duplicate rule names"
    else Ok ()
  in
  List.map definition.rules ~f:(fun (r : Institution_definition.rule) ->
    let kind =
      match r.action with
      | Type -> "type"
      | Card_reference -> "card-reference"
      | Withdrawal_reversal -> "withdrawal-reversal"
    in
    let%bind action = Classification_rule.action_of_fields kind r.target None in
    Classification_rule.create
      ~mode:Automatic
      ~account_type:r.account_type
      ~priority:r.priority
      ~name:r.name
      ~pattern:(Some r.pattern)
      ~institution:(Some name)
      ~match_type:(Option.map r.match_type ~f:Classification_rule.condition_of_string)
      ~case_sensitive:r.case_sensitive
      ~action
      ?field:r.field
      ())
  |> Or_error.combine_errors
;;

let read_institution_definition path =
  let%bind text = read_file path in
  let%bind definition =
    if String.is_suffix path ~suffix:".toml"
    then
      Institution_toml.decode text ~load:(fun relative ->
        read_file (Stdlib.Filename.concat (Stdlib.Filename.dirname path) relative))
    else Json.decode Institution_definition.codec text
  in
  let%map _ = definition_rules definition in
  definition
;;

let check_institution_definition path sample account_id =
  let%bind definition = read_institution_definition path in
  let module A = (val Institution_definition.adapter definition : Adapter.S) in
  printf
    "Definition: %s\nFormats: %s\n"
    (Terminal.text A.institution)
    (String.concat ~sep:", " A.formats);
  match sample with
  | None ->
    print_endline "Definition is valid.";
    Ok ()
  | Some sample ->
    let%bind contents = read_file sample in
    let%bind format =
      A.probe ~filename:sample contents
      |> Or_error.of_option
           ~error:(Error.of_string "sample is not recognized by this definition")
    in
    let%bind document =
      A.parse
        ~context:
          { account_label = Some (Option.value account_id ~default:"preview")
          ; path = None
          }
        ~format
        contents
    in
    let events =
      document.events
      @ List.concat_map document.statements ~f:(fun s -> s.Document.events)
    in
    let operation_events, movement_events =
      List.partition_tf events ~f:(fun event ->
        List.mem A.operation_kinds event.Source_event.kind ~equal:String.equal)
    in
    let%bind _ =
      List.map operation_events ~f:A.operation_rows |> Or_error.combine_errors
    in
    let%bind movements =
      List.map movement_events ~f:A.decode |> Or_error.combine_errors
    in
    let%bind rules = definition_rules definition in
    let rules =
      List.sort rules ~compare:(fun a b ->
        match Int.compare a.Classification_rule.priority b.priority with
        | 0 -> String.compare a.name b.name
        | n -> n)
    in
    let%bind typing =
      rules
      |> List.filter_map ~f:(fun (rule : Classification_rule.t) ->
        match rule.action with
        | Type kind ->
          Some
            (let%map r =
               Source_typing.make_rule
                 ?field:rule.field
                 (Option.value_exn rule.pattern)
                 rule.account_type
                 rule.case_sensitive
                 (Movement_type.to_string kind)
             in
             Source_typing.with_match_type
               r
               (Option.map rule.match_type ~f:Classification_rule.condition_name))
        | _ -> None)
      |> Or_error.combine_errors
    in
    let typing = Source_typing.create ~institution:A.institution typing in
    let movements = List.map movements ~f:(Source_typing.apply typing) in
    printf
      "Sample: %s; %d statements, %d transactions, %d companion rows.\n"
      (Terminal.text format)
      (List.length document.statements)
      (List.length movements)
      (List.length document.companion_rows);
    if not (List.is_empty operation_events)
    then printf "  %d operations.\n" (List.length operation_events);
    List.take movements 5
    |> List.iter ~f:(fun m ->
      printf
        "  %s  %s %s  %s  %s\n"
        (Date.to_string m.Movement.booked_on)
        (Quantity.to_string m.amount)
        (Iso4217.to_alpha_code m.account.currency)
        (Option.value_map m.movement_type ~default:"unknown" ~f:Movement_type.to_string)
        (Terminal.text (String.concat ~sep:" | " m.descriptions)));
    (match
       List.find A.companions ~f:(fun (c : Companion.t) -> String.equal c.format format)
     with
     | None -> ()
     | Some companion ->
       List.take document.companion_rows 5
       |> List.iter ~f:(fun (number, fields) ->
         let row : Companion.row =
           { row_id = Int64.of_int number
           ; document_id = 0L
           ; institution = A.institution
           ; format
           ; row_number = number
           ; fields
           ; context = document.context
           }
         in
         match companion.describe row with
         | Error e -> eprintf "  Row %d: %s\n" number (Error.to_string_hum e)
         | Ok details ->
           printf
             "  Row %d: %s\n"
             number
             (String.concat
                ~sep:"; "
                (List.filter_map details ~f:(fun (key, value) ->
                   Option.map value ~f:(fun v ->
                     Terminal.text key ^ "=" ^ Terminal.text v))))));
    print_endline "Preview only; no data was imported.";
    Ok ()
;;

let institution_definition_check_cmd =
  command
    "check"
    "Validate a TOML or JSON institution definition and preview a sample statement. \
     Referenced paths in TOML are relative to its directory."
    Term.(
      const check_institution_definition
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"DEFINITION.toml")
      $ Arg.(value & pos 1 (some string) None & info [] ~docv:"SAMPLE")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "account-id" ]
              ~docv:"LABEL"
              ~doc:"Account label for a primary CSV sample (default: preview)."))
;;

let show_institution_definition database name =
  let builtin = Builtin.definition name in
  let%bind () = require_database database in
  let%bind definitions = institution_definitions database in
  let%bind definition =
    match
      List.find definitions ~f:(fun d ->
        String.equal (Institution_definition.institution d) name)
    with
    | Some d -> Ok d
    | None ->
      Or_error.of_option builtin ~error:(Error.of_string "unknown institution definition")
  in
  print_endline (Json.encode ~format:Jsont.Indent Institution_definition.codec definition);
  Ok ()
;;

let institution_definition_show_cmd =
  command
    "show"
    "Show a bundled or installed institution definition as JSON."
    Term.(
      const show_institution_definition
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION"))
;;

let set_institution_definition database name path yes =
  let%bind () = require_database database in
  let%bind definition = read_institution_definition path in
  let module A = (val Institution_definition.adapter definition : Adapter.S) in
  if not (String.equal A.institution name)
  then
    Or_error.error_string "definition institution does not match the selected institution"
  else (
    let%bind registry = registry_for database in
    let%bind generic = with_database database Setup.institutions in
    if
      not
        (Registry.is_registered registry name || List.mem generic name ~equal:String.equal)
    then
      Or_error.errorf "unknown institution: %s; create it with fin institution add" name
    else (
      let registered =
        Registry.institutions registry
        |> List.filter ~f:(fun owner -> not (String.equal owner name))
        |> List.concat_map ~f:(Registry.aliases registry)
      in
      if
        List.contains_dup (name :: A.aliases) ~compare:String.compare
        || List.exists A.aliases ~f:(fun alias ->
          List.mem (registered @ generic) alias ~equal:String.equal)
      then Or_error.error_string "institution alias is already in use"
      else (
        let%bind _ = Registry.create [ (module A : Adapter.S) ] in
        let%bind rules = definition_rules definition in
        let json = Json.encode Institution_definition.codec definition in
        let%bind expected, expected_rules =
          with_database database (fun db ->
            let%bind definitions = Setup.institution_definitions db in
            let%map rules = Import.classification_rules db in
            List.Assoc.find definitions name ~equal:String.equal, rules)
        in
        let%bind source_unchanged =
          match expected with
          | None -> Ok false
          | Some text ->
            let%map old = Json.decode Institution_definition.codec text in
            Institution_definition.same_sources old definition
        in
        let%bind _ =
          with_database database (fun db ->
            Ledger.set_institution_definition
              ~dry_run:true
              db
              ~name
              ~definition:json
              ~expected
              ~expected_rules
              ~rules
              ~source_unchanged)
        in
        let added, retained =
          List.partition_tf rules ~f:(fun r ->
            not (List.exists expected_rules ~f:(fun old -> String.equal old.name r.name)))
        in
        printf
          "Definition for %s: %s; %d new rules, %d existing rules retained.\n"
          (Terminal.text name)
          (String.concat ~sep:", " A.formats)
          (List.length added)
          (List.length retained);
        List.iter added ~f:(fun r ->
          printf
            "  add %s: %s -> %s\n"
            (Terminal.text r.name)
            (Classification_rule.action_name r.action)
            (Terminal.text (Classification_rule.action_target r.action)));
        List.iter retained ~f:(fun r ->
          printf "  retain existing %s\n" (Terminal.text r.name));
        let%bind confirmed = confirm ~yes ~destructive:false "Attach this definition?" in
        if not confirmed
        then (
          print_endline "Nothing changed.";
          Ok ())
        else
          with_database database (fun db ->
            let%map count =
              Ledger.set_institution_definition
                db
                ~name
                ~definition:json
                ~expected
                ~expected_rules
                ~rules
                ~source_unchanged
            in
            printf
              "Attached definition for %s; added %d rules. Existing transactions and \
               rules are unchanged.\n"
              (Terminal.text name)
              count))))
;;

let institution_definition_set_cmd =
  command
    "set"
    "Attach a TOML or JSON import definition to an existing institution. Referenced \
     paths in TOML are relative to its directory."
    Term.(
      const set_institution_definition
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"DEFINITION.toml")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Accept the preview without prompting."))
;;

let institution_companion_show database name =
  with_database database (fun db ->
    let%bind profile = Setup.institution_companion_profile db name in
    match profile with
    | None -> Or_error.error_string "no companion profile configured for this institution"
    | Some json ->
      let%map profile = Json.decode Csv_companion_layout.codec json in
      print_endline (Json.encode ~format:Jsont.Indent Csv_companion_layout.codec profile))
;;

let institution_companion_example () =
  print_string Bundled_definitions.companion_example;
  Ok ()
;;

let institution_companion_example_cmd =
  command
    "example"
    "Print an example layout to adapt for an institution's CSV companion."
    Term.(const institution_companion_example $ const ())
;;

let institution_companion_show_cmd =
  command
    "show"
    "Show the institution's CSV companion layout as JSON."
    Term.(
      const institution_companion_show
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION"))
;;

let institution_companion_set database name path yes =
  let%bind contents = read_file path in
  let%bind profile = Json.decode Csv_companion_layout.codec contents in
  let%bind () =
    if Option.is_none profile.currency
    then Or_error.error_string "a generic companion profile must specify its ISO currency"
    else Ok ()
  in
  let json = Json.encode Csv_companion_layout.codec profile in
  with_database database (fun db ->
    let%bind current = Setup.institution_companion_profile db name in
    let%bind () =
      Setup.set_institution_companion_profile ~dry_run:true db name ~expected:current json
    in
    printf
      "CSV companion profile for %s:\n%s\n"
      (Terminal.text name)
      (Json.encode ~format:Jsont.Indent Csv_companion_layout.codec profile);
    print_endline
      "CSV rows will enrich matching OFX movements. Previously imported rows keep their \
       original layout.";
    let%bind confirmed = confirm ~yes ~destructive:false "Save this companion profile?" in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () =
        Setup.set_institution_companion_profile db name ~expected:current json
      in
      printf "Saved companion profile for %s.\n" (Terminal.text name)))
;;

let institution_companion_set_cmd =
  command
    "set"
    "Configure an institution's OFX companion CSV layout."
    Term.(
      const institution_companion_set
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"FILE")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Save without prompting."))
;;

let institution_companion_clear database name yes =
  with_database database (fun db ->
    let%bind expected =
      Setup.institution_companion_profile db name
      |> Or_error.bind ~f:(function
        | Some profile -> Ok profile
        | None -> Or_error.error_string "no companion profile configured")
    in
    let%bind () =
      Setup.clear_institution_companion_profile ~dry_run:true db name ~expected
    in
    printf "Remove the CSV companion profile for %s?\n" (Terminal.text name);
    let%bind confirmed =
      confirm ~yes ~destructive:true "Remove this companion profile?"
    in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () = Setup.clear_institution_companion_profile db name ~expected in
      printf "Removed companion profile for %s.\n" (Terminal.text name)))
;;

let institution_companion_clear_cmd =
  command
    "clear"
    "Remove a companion profile before importing companion rows."
    Term.(
      const institution_companion_clear
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTITUTION")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Remove without prompting."))
;;
