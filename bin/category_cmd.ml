open Base
open Stdio
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let category_list database json_output =
  with_database database (fun db ->
    let%bind categories = Setup.categories db in
    if json_output
    then Report_json.print (Jsont.list Jsont.string) categories
    else List.iter categories ~f:(fun category -> printf "%s\n" (Terminal.text category));
    Ok ())
;;

let category_list_cmd =
  command
    "list"
    "List the personal income and expense category catalog."
    Term.(const category_list $ database $ json_output)
;;

let category_add database name =
  with_database database (fun db ->
    let%bind () = Setup.add_categories db [ name ] in
    printf "Category available: %s\n" (Terminal.text name);
    Ok ())
;;

let category_add_cmd =
  command
    "add"
    "Add an income or expense category to the catalog."
    Term.(
      const category_add
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"ACCOUNT"))
;;

let category_delete database name =
  with_database database (fun db ->
    let%bind () = Setup.delete_category db name in
    printf "Category deleted: %s\n" (Terminal.text name);
    Ok ())
;;

let category_delete_cmd =
  command
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Remove one unused catalog entry. Change transaction assignments and rules \
           that use the category first. Historical decisions and subcategories are \
           retained."
      ]
    "delete"
    "Delete an unused category from the catalog."
    Term.(
      const category_delete
      $ database
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"ACCOUNT"))
;;

let starter_categories =
  [ "expenses:home"
  ; "expenses:utilities"
  ; "expenses:food:groceries"
  ; "expenses:food:restaurants"
  ; "expenses:transport"
  ; "expenses:health"
  ; "expenses:shopping"
  ; "expenses:entertainment"
  ; "expenses:travel"
  ; "expenses:taxes"
  ; "expenses:other"
  ; "income:salary"
  ; "income:interest"
  ; "income:other"
  ]
;;

let category_init database =
  if not (Unix.isatty Unix.stdin)
  then Or_error.error_string "category initialization requires a terminal"
  else
    with_database database (fun db ->
      let%bind existing = Setup.categories db in
      let available =
        List.filter starter_categories ~f:(fun name ->
          not (List.mem existing name ~equal:String.equal))
      in
      if List.is_empty available
      then (
        print_endline "All starter categories already exist.";
        Ok ())
      else (
        print_endline "Suggested starter categories:";
        List.iteri available ~f:(fun i name -> printf "  %d. %s\n" (i + 1) name);
        let rec select () =
          match
            prompt "Select all, none, or numbers separated by spaces (q to cancel): "
          with
          | None -> Ok None
          | Some input ->
            let input = String.strip input in
            if String.Caseless.equal input "q" || String.Caseless.equal input "none"
            then Ok None
            else if String.Caseless.equal input "all"
            then Ok (Some available)
            else (
              let nums =
                String.split_on_chars input ~on:[ ' '; ','; '\t' ]
                |> List.filter ~f:(Fn.non String.is_empty)
              in
              let indexes =
                List.map nums ~f:(fun n -> Option.try_with (fun () -> Int.of_string n))
              in
              if
                List.is_empty indexes
                || List.exists indexes ~f:(function
                  | None -> true
                  | Some n -> n < 1 || n > List.length available)
              then (
                eprintf "Enter all, none, or valid numbers.\n";
                select ())
              else
                Ok
                  (Some
                     (List.filteri available ~f:(fun i _ ->
                        List.mem indexes (Some (i + 1)) ~equal:(Option.equal Int.equal)))))
        in
        let%bind selected = select () in
        match selected with
        | None ->
          print_endline "No categories added.";
          Ok ()
        | Some selected ->
          print_endline "Categories to add:";
          List.iter selected ~f:(fun name -> printf "  %s\n" name);
          let%bind confirmed =
            confirm ~yes:false ~destructive:false "Add these categories?"
          in
          if confirmed
          then (
            let%bind () = Setup.add_categories db selected in
            printf "categories_added=%d\n" (List.length selected);
            Ok ())
          else (
            print_endline "No categories added.";
            Ok ())))
;;

let category_init_cmd =
  command
    "init"
    "Choose suggested starter categories for your personal catalog."
    Term.(const category_init $ database)
;;

let cmd =
  group
    "category"
    "Manage the personal income and expense category catalog."
    [ category_list_cmd; category_add_cmd; category_delete_cmd; category_init_cmd ]
;;
