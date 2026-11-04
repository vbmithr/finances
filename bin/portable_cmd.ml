open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

type portable_scope =
  | Catalog
  | Category_decisions
  | Rules

let portable_codec = function
  | Catalog -> Category_portable.categories_codec
  | Category_decisions -> Category_portable.decisions_codec
  | Rules -> Category_portable.rules_codec
;;

let select_portable scope (snapshot : Category_portable.t) =
  match scope with
  | Catalog -> { snapshot with rules = []; decisions = []; resolutions = [] }
  | Category_decisions -> { snapshot with categories = []; rules = [] }
  | Rules -> { snapshot with categories = []; decisions = []; resolutions = [] }
;;

let portable_export scope database =
  with_database database (fun db ->
    let%bind snapshot =
      match scope with
      | Catalog ->
        let%map categories = Setup.categories db in
        Category_portable.{ categories; rules = []; decisions = []; resolutions = [] }
      | Rules ->
        let%map rules = Import.classification_rules db in
        Category_portable.{ categories = []; rules; decisions = []; resolutions = [] }
      | Category_decisions ->
        let%map snapshot = Restore.category_snapshot db in
        select_portable scope snapshot
    in
    Report_json.print (portable_codec scope) snapshot;
    Ok ())
;;

let portable_export_cmd ?man scope doc =
  command ?man "export" doc Term.(const (portable_export scope) $ database)
;;

let category_export_cmd =
  portable_export_cmd Catalog "Export the category catalog as portable JSON."
;;

let category_decisions_export_cmd =
  portable_export_cmd
    Category_decisions
    "Export your decisions on operations (categories, manual types, resolved accounts) \
     as JSON."
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Writes the decisions you made on imported operations: transaction categories \
           (including explicit resets), manual movement-type overrides and the accounts \
           and commodities you resolved for movements. The operations themselves are not \
           exported; they come back from $(b,fin import). Use $(b,fin operation restore) \
           to re-apply the file to a rebuilt database."
      ; `P
          "Transfers, exchanges, rules and the category catalog have their own export \
           commands: $(b,fin transfer export), $(b,fin operation exchange export), \
           $(b,fin rule export) and $(b,fin category export)."
      ]
;;

let rules_export_cmd = portable_export_cmd Rules "Export stored rules as portable JSON."

let portable_restore scope database path yes institution =
  let%bind contents = read_file path in
  let%bind snapshot =
    match Json.decode (portable_codec scope) contents with
    | Ok snapshot -> Ok snapshot
    | Error scoped_error ->
      (match scope, Json.decode Category_portable.version4_decisions_codec contents with
       | Category_decisions, Ok legacy -> Ok legacy
       | _ ->
         (match
            scope, Json.decode Category_portable.previous_decisions_codec contents
          with
          | Category_decisions, Ok legacy -> Ok legacy
          | _ ->
            (match
               scope, Json.decode Category_portable.legacy_decisions_codec contents
             with
             | Category_decisions, Ok legacy -> Ok legacy
             | _ ->
               (match Json.decode Category_portable.codec contents with
                | Ok legacy -> Ok (select_portable scope legacy)
                | Error _ -> Error scoped_error))))
  in
  let%bind snapshot, skipped =
    match institution with
    | None -> Ok (snapshot, 0)
    | Some name ->
      let decisions =
        List.filter snapshot.decisions ~f:(fun (d : Category_portable.decision) ->
          String.equal d.institution name)
      in
      let resolutions =
        List.filter snapshot.resolutions ~f:(fun (r : Category_portable.resolution) ->
          String.equal r.source.institution name)
      in
      if List.is_empty decisions && List.is_empty resolutions
      then Or_error.errorf "snapshot has no decisions for institution %s" name
      else
        Ok
          ( { snapshot with decisions; resolutions }
          , List.length snapshot.decisions
            - List.length decisions
            + List.length snapshot.resolutions
            - List.length resolutions )
  in
  with_database database (fun db ->
    let%bind preview = Restore.preview_category_restore db snapshot in
    Option.iter institution ~f:(fun name ->
      printf "Institution: %s; decisions skipped: %d\n" (Terminal.text name) skipped);
    printf "Categories to add: %d\n" (List.length preview.categories_to_add);
    List.iter preview.categories_to_add ~f:(fun c -> printf "  %s\n" (Terminal.text c));
    (match scope with
     | Rules ->
       printf "Rules to add: %d\n" (List.length preview.rules_to_add);
       List.iter preview.rules_to_add ~f:(fun r ->
         printf "  %s\n" (Terminal.text r.Classification_rule.name))
     | Category_decisions ->
       printf
         "Decisions to restore: %d; already matching: %d\n"
         (List.length preview.decisions_to_apply)
         preview.unchanged;
       List.iter preview.decisions_to_apply ~f:(fun (d : Bank_category.t) ->
         printf "  #%Ld -> %s\n" d.event_id (Terminal.text d.account));
       printf "Type overrides to restore: %d\n" (List.length preview.types_to_apply);
       List.iter preview.types_to_apply ~f:(fun (id, kind) ->
         printf "  #%Ld -> %s\n" id (Terminal.text (Movement_type.to_string kind)));
       printf
         "Movement resolutions to restore: %d\n"
         (List.length preview.resolutions_to_apply)
     | Catalog -> ());
    if not (List.is_empty preview.problems)
    then (
      print_endline "Problems (restore is blocked):";
      List.iter preview.problems ~f:(fun p -> printf "  %s\n" (Terminal.text p)));
    if not (List.is_empty preview.problems)
    then Or_error.error_string "resolve restore problems before applying"
    else if
      List.is_empty preview.categories_to_add
      && List.is_empty preview.rules_to_add
      && List.is_empty preview.decisions_to_apply
      && List.is_empty preview.types_to_apply
      && List.is_empty preview.resolutions_to_apply
    then (
      print_endline "categories_added=0 rules_added=0 category_changes=0 type_changes=0";
      Ok ())
    else (
      let%bind confirmed = confirm ~yes ~destructive:false "Restore this snapshot?" in
      if not confirmed
      then (
        print_endline "Nothing restored.";
        Ok ())
      else (
        let%bind changed = Restore.restore_categories db ~expected:preview snapshot in
        printf
          "categories_added=%d rules_added=%d category_changes=%d type_changes=%d\n"
          (List.length preview.categories_to_add)
          (List.length preview.rules_to_add)
          changed
          (List.length preview.types_to_apply);
        Ok ())))
;;

let portable_restore_cmd ?man scope doc =
  let institution =
    match scope with
    | Category_decisions ->
      Arg.(
        value
        & opt (some string) None
        & info
            [ "institution" ]
            ~docv:"NAME"
            ~doc:
              "Restore only decisions for this institution; other decisions are skipped.")
    | Catalog | Rules -> Term.const None
  in
  command
    ?man
    "restore"
    doc
    Term.(
      const (portable_restore scope)
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"FILE" ~doc:"Portable JSON export.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Restore without asking, for scripts.")
      $ institution)
;;

let category_restore_cmd =
  portable_restore_cmd
    Catalog
    "Restore a category catalog export; shows the changes and asks first."
;;

let category_decisions_restore_cmd =
  portable_restore_cmd
    Category_decisions
    "Re-apply saved decisions (categories, manual types, resolved accounts) to imported \
     operations; shows the changes and asks first."
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Reads a file written by $(b,fin operation export) and re-applies the \
           decisions it holds to operations that are already imported: transaction \
           categories (including explicit resets), manual movement-type overrides and \
           the accounts and commodities you resolved for movements."
      ; `P
          "It never imports statements or creates operations; those come only from \
           $(b,fin import), so run that first. Each decision is matched to its operation \
           by source identity. A decision that matches nothing is reported as a problem \
           and blocks the restore; it is never created. fin shows what it would change \
           and asks first; answer $(b,n) to leave everything as it was, or pass \
           $(b,--yes) in scripts."
      ; `P
          "Transfers, exchanges, rules and the category catalog have their own restore \
           commands: $(b,fin transfer restore), $(b,fin operation exchange restore), \
           $(b,fin rule restore) and $(b,fin category restore)."
      ]
;;

let rules_restore_cmd =
  portable_restore_cmd Rules "Restore stored rules; shows the changes and asks first."
;;
