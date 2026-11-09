open Base
open Stdio
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let import_history database limit json_output =
  with_database database (fun db ->
    let%bind attempts = Import.bank_import_history ~limit db in
    if json_output
    then Report_json.print (Jsont.list Report_json.import) attempts
    else Terminal.import_history attempts;
    Ok ())
;;

let import_history_cmd =
  command
    "history"
    "List recent file import attempts, newest first; use --json for scripts."
    Term.(
      const import_history
      $ database
      $ Arg.(value & opt int 100 & info [ "limit" ] ~doc:"Maximum attempts (1-10000).")
      $ json_output)
;;

type institution_origin =
  | Built_in of string list
  | Definition
  | Generic

(* Every institution fin knows: bundled, installed from a definition, or added
   without one. A missing database only lacks the last two. *)
let institution_entries database =
  let present = Stdlib.Sys.file_exists database in
  let%bind definitions = if present then institution_definitions database else Ok [] in
  let built_in =
    List.filter_map (Registry.institutions Builtin.registry) ~f:(fun institution ->
      if String.equal institution "csv"
      then None
      else
        Some
          ( institution
          , Option.value
              (Builtin.institution_type institution)
              ~default:Institution_type.Bank
          , Built_in (List.tl_exn (Registry.aliases Builtin.registry institution)) ))
  in
  let defined =
    List.filter_map definitions ~f:(fun definition ->
      let name = Institution_definition.institution definition in
      Option.some_if
        (not (Registry.is_registered Builtin.registry name))
        (name, Institution_definition.typ definition, Definition))
  in
  let%map generic =
    if not present
    then Ok []
    else
      with_database database (fun db ->
        let%map names = Setup.institutions db in
        List.filter_map names ~f:(fun name ->
          Option.some_if
            (not
               (List.exists definitions ~f:(fun definition ->
                  String.equal name (Institution_definition.institution definition))))
            (name, Institution_type.Bank, Generic)))
  in
  built_in @ defined @ generic
;;

let institution_list database =
  let%bind () = require_database database in
  let%map entries = institution_entries database in
  List.iter entries ~f:(fun (name, institution_type, origin) ->
    let kind = Institution_type.label institution_type in
    match origin with
    | Built_in [] -> printf "%s (%s, built-in)\n" name kind
    | Built_in aliases ->
      printf
        "%s (%s, built-in; aliases: %s)\n"
        name
        kind
        (String.concat ~sep:", " aliases)
    | Definition -> printf "%s (%s, definition)\n" (Terminal.text name) kind
    | Generic -> printf "%s (%s, generic)\n" (Terminal.text name) kind)
;;

let institution_list_cmd =
  command
    "list"
    "List built-in and user-added institutions."
    Term.(const institution_list $ database)
;;

let print_institution_description
      ~institution_type
      (d : Institution_definition.description)
  =
  printf "%s\n" (Terminal.text d.institution);
  printf "  Type: %s\n" (Institution_type.label institution_type);
  printf "  Kind: %s\n" d.kind;
  printf
    "  Aliases: %s\n"
    (if List.is_empty d.aliases
     then "none"
     else String.concat ~sep:", " (List.map d.aliases ~f:Terminal.text));
  print_endline "\n  What fin reads, by role:";
  List.iter d.fields ~f:(fun (role, name) ->
    printf "    %-24s %s\n" role (Terminal.text name));
  if not (List.is_empty d.unread)
  then (
    print_endline "\n  Columns fin does not read:";
    printf "    %s\n" (String.concat ~sep:", " (List.map d.unread ~f:Terminal.text)));
  print_endline "\n  Rules can match:";
  List.iter d.rule_inputs ~f:(fun input -> printf "    %s\n" input)
;;

let print_institution_rules (definition : Institution_definition.t) =
  printf "\n  Default rules (%d):\n" (List.length definition.rules);
  List.iter definition.rules ~f:(fun r ->
    let action =
      match r.action with
      | Type -> "type"
      | Card_reference -> "card-reference"
      | Withdrawal_reversal -> "withdrawal-reversal"
    in
    printf
      "    %3d  %-18s %-40s /%s/%s -> %s\n"
      r.priority
      action
      (Terminal.text r.name)
      (Terminal.text r.pattern)
      (Option.value_map r.field ~default:"" ~f:(fun column ->
         Printf.sprintf " on column %s" (Terminal.text column)))
      (Terminal.text r.target))
;;

let print_institution_data db institution =
  let%bind summary = Explain.institution_summary db institution in
  let%map operations = Explain.operation_summary db institution in
  (match summary.first_on, summary.last_on with
   | Some first, Some last ->
     printf
       "\n  Imported data: %d movements, %s to %s.\n  By type:\n"
       summary.movements
       first
       last;
     List.iter summary.by_type ~f:(fun (kind, n) ->
       printf
         "    %6d  %s\n"
         n
         (Option.value_map kind ~default:"(no type)" ~f:(fun kind ->
            Terminal.text (Movement_type.to_string kind))))
   | _ -> if operations.operations = 0 then print_endline "\n  Imported data: none.");
  match operations.first_on, operations.last_on with
  | Some first, Some last ->
    printf
      "\n  Imported data: %d operations, %s to %s; %d unresolved.\n"
      operations.operations
      first
      last
      operations.unresolved
  | _ -> ()
;;

let institution_overview database =
  let%bind entries = institution_entries database in
  let%map rows =
    if Stdlib.Sys.file_exists database
    then
      with_database database (fun db ->
        List.map entries ~f:(fun (name, institution_type, origin) ->
          let%bind movements = Explain.institution_summary db name in
          let%map operations = Explain.operation_summary db name in
          let dates =
            List.filter_opt
              [ movements.first_on
              ; operations.first_on
              ; movements.last_on
              ; operations.last_on
              ]
          in
          let count n = if n = 0 then "-" else Int.to_string n in
          [ name
          ; Institution_type.label institution_type
          ; (match origin with
             | Built_in _ -> "built-in"
             | Definition -> "definition"
             | Generic -> "generic")
          ; count movements.movements
          ; count operations.operations
          ; count operations.unresolved
          ; Option.value (List.min_elt dates ~compare:String.compare) ~default:"-"
          ; Option.value (List.max_elt dates ~compare:String.compare) ~default:"-"
          ])
        |> Or_error.combine_errors)
    else
      Ok
        (List.map entries ~f:(fun (name, institution_type, _) ->
           [ name
           ; Institution_type.label institution_type
           ; "built-in"
           ; "-"
           ; "-"
           ; "-"
           ; "-"
           ; "-"
           ]))
  in
  Terminal.table
    [ "Institution"
    ; "Type"
    ; "Origin"
    ; "Movements"
    ; "Operations"
    ; "Unresolved"
    ; "First"
    ; "Last"
    ]
    rows;
  print_endline "\nDetails and rules: fin institution show INSTITUTION."
;;

let institution_show database name =
  match name with
  | None -> institution_overview database
  | Some name ->
    let%bind installed =
      if Stdlib.Sys.file_exists database then institution_definitions database else Ok []
    in
    let%bind () =
      match
        List.find installed ~f:(fun d ->
          List.mem (Institution_definition.names d) name ~equal:String.equal)
      with
      | Some d ->
        print_institution_description
          ~institution_type:(Institution_definition.typ d)
          (Institution_definition.describe d);
        print_institution_rules d;
        Ok ()
      | None ->
        (match Builtin.definition name with
         | Some d ->
           print_institution_description
             ~institution_type:(Institution_definition.typ d)
             (Institution_definition.describe d);
           print_institution_rules d;
           Ok ()
         | None ->
           Or_error.error_string
             "unknown institution, or one without a definition; see fin institution list")
    in
    let canonical =
      match Builtin.definition name with
      | Some d -> Institution_definition.institution d
      | None ->
        (match
           List.find installed ~f:(fun d ->
             List.mem (Institution_definition.names d) name ~equal:String.equal)
         with
         | Some d -> Institution_definition.institution d
         | None -> name)
    in
    if Stdlib.Sys.file_exists database
    then with_database database (fun db -> print_institution_data db canonical)
    else (
      print_endline "\n  Imported data: no database yet.";
      Ok ())
;;

let institution_show_cmd =
  command
    "show"
    "Without an argument, give an overview of every institution and what is imported; \
     with one, show what fin reads for it, its default rules and what is imported."
    Term.(
      const institution_show
      $ database
      $ Arg.(value & pos 0 (some string) None & info [] ~docv:"INSTITUTION"))
;;

let institution_add database name =
  let%bind name =
    match name with
    | Some name -> Ok name
    | None when Unix.isatty Unix.stdin ->
      (match prompt "Institution name (required): " with
       | Some name -> Ok (String.strip name)
       | None -> Or_error.error_string "institution name is required")
    | None -> Or_error.error_string "institution name is required"
  in
  let%bind name = Validation.token "institution name" name in
  let%bind () = require_database database in
  let%bind runtime_registry = registry_for database in
  let reserved = Csv_setup.builtin_institution_name runtime_registry name in
  if reserved
  then Or_error.error_string "institution name is reserved for a built-in importer"
  else (
    let%bind existing = with_database database Setup.institutions in
    if List.mem existing name ~equal:String.equal
    then Or_error.errorf "institution already exists: %s" name
    else
      with_database database (fun db ->
        let%bind () = Setup.add_institution db name in
        printf "Added %s. Attach a definition for CSV imports.\n" (Terminal.text name);
        Ok ()))
;;

let institution_add_cmd =
  command
    "add"
    "Add an institution for OFX statements or an attached definition."
    Term.(
      const institution_add
      $ database
      $ Arg.(
          value
          & pos 0 (some string) None
          & info
              []
              ~docv:"NAME"
              ~doc:"Institution name; prompted on a terminal if omitted."))
;;

let institution_delete database name yes =
  with_database database (fun db ->
    let%bind () = Setup.delete_institution ~dry_run:true db name in
    printf "Delete institution %s and its saved CSV layout?\n" (Terminal.text name);
    let%bind confirmed = confirm ~yes ~destructive:true "Delete this institution?" in
    if not confirmed
    then (
      print_endline "Nothing deleted.";
      Ok ())
    else (
      let%bind () = Setup.delete_institution db name in
      print_endline "Institution deleted.";
      Ok ()))
;;

let institution_delete_cmd =
  command
    "delete"
    "Delete an unused user-added institution and its saved CSV layout."
    Term.(
      const institution_delete
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"NAME" ~doc:"User-added institution name.")
      $ Arg.(
          value & flag & info [ "yes" ] ~doc:"Delete without interactive confirmation."))
;;

let cmd =
  group
    "institution"
    "Manage institutions and their importers."
    [ institution_list_cmd
    ; institution_show_cmd
    ; institution_add_cmd
    ; institution_delete_cmd
    ; group
        "definition"
        "Validate and install institution definitions."
        [ Institution_definition_cmd.institution_definition_show_cmd
        ; Institution_definition_cmd.institution_definition_check_cmd
        ; Institution_definition_cmd.institution_definition_set_cmd
        ]
    ; import_history_cmd
    ; group
        "companion"
        "Configure CSV enrichment layouts."
        [ Institution_definition_cmd.institution_companion_example_cmd
        ; Institution_definition_cmd.institution_companion_show_cmd
        ; Institution_definition_cmd.institution_companion_set_cmd
        ; Institution_definition_cmd.institution_companion_clear_cmd
        ]
    ]
;;
