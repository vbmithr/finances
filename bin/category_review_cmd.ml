open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

(* A typo must not create a category, so assigning names only what the catalog holds. Only
   equity:unclassified, which returns a movement to review, needs no entry. *)
let require_categories db accounts =
  let%bind known = Setup.categories db in
  match
    List.dedup_and_sort accounts ~compare:String.compare
    |> List.filter ~f:(fun account ->
      not
        (String.equal account "equity:unclassified"
         || List.mem known account ~equal:String.equal))
  with
  | [] -> Ok ()
  | [ account ] ->
    Or_error.errorf
      "no category %s; see fin category list, or add it with fin category add %s"
      account
      account
  | unknown ->
    Or_error.errorf
      "no categories %s; see fin category list, or add them with fin category add"
      (String.concat ~sep:", " unknown)
;;

let categorize_bank database path =
  let%bind contents = read_file path in
  let%bind decisions = Bank_category.of_json contents in
  with_database database (fun db ->
    let%bind () =
      require_categories
        db
        (List.map decisions ~f:(fun (d : Bank_category.t) -> d.account))
    in
    let%bind changed = Review.set_bank_categories db decisions in
    printf "category_changes=%d\n" changed;
    Ok ())
;;

let bank_event_ids_for_operations ?source_event_id db operation_ids =
  let%bind operations = Review.operation_review db in
  let%bind () =
    if Option.is_some source_event_id && List.length operation_ids <> 1
    then Or_error.error_string "--source-event requires exactly one operation ID"
    else Ok ()
  in
  List.map operation_ids ~f:(fun operation_id ->
    match
      List.find operations ~f:(fun (o : Review.operation_review) ->
        Int64.equal o.operation_id operation_id)
    with
    | None -> Or_error.errorf "unknown operation ID: %Ld" operation_id
    | Some operation ->
      (match source_event_id, operation.movements with
       | Some id, movements ->
         (match
            List.find movements ~f:(fun m -> Int64.equal m.Review.anchor_event_id id)
          with
          | Some m when Option.is_some m.review -> Ok id
          | _ ->
            Or_error.errorf
              "source event %Ld is not a bank movement of operation %Ld"
              id
              operation_id)
       | None, [ movement ] when Option.is_some movement.review ->
         Ok movement.anchor_event_id
       | None, [ _ ] ->
         Or_error.errorf "operation %Ld has no reviewable bank movement" operation_id
       | None, [] ->
         Or_error.errorf "operation %Ld has no movement to review" operation_id
       | None, _ ->
         Or_error.errorf
           "operation %Ld has multiple movements; select a side with --source-event \
            where supported"
           operation_id))
  |> Or_error.combine_errors
;;

(* What a piped selection would change, shown before asking. *)
let category_set_preview db account event_ids =
  let%map rows = Review.bank_review db in
  let selected = Set.of_list (module Int64) event_ids in
  let changing =
    List.filter rows ~f:(fun (r : Review.bank_review) ->
      Set.mem selected r.event_id && not (String.equal r.counter_account account))
  in
  if not (List.is_empty changing)
  then (
    eprintf
      "This sets %d transaction(s) to %s.\nNow:\n"
      (List.length changing)
      (Terminal.text account);
    List.map changing ~f:(fun r -> r.counter_account)
    |> List.sort_and_group ~compare:String.compare
    |> List.map ~f:(fun group -> List.length group, List.hd_exn group)
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare b a)
    |> List.iter ~f:(fun (count, name) ->
      eprintf "  %6d  %s\n" count (Terminal.text name));
    eprintf "For example:\n";
    List.iter (List.take changing 3) ~f:(fun r ->
      eprintf "  %s\n" (Terminal.text r.description)));
  List.length changing
;;

let category_set database account yes inputs =
  let stdin_count = List.count inputs ~f:(String.equal "-") in
  if stdin_count > 1
  then Or_error.error_string "standard input can only be specified once"
  else (
    let transaction_id =
      Jsont.Object.map Fn.id
      |> Jsont.Object.mem "operation_id" Json.int64_codec
      |> Jsont.Object.finish
    in
    let%bind id_lists =
      List.map inputs ~f:(fun input ->
        if String.equal input "-"
        then
          if Unix.isatty Unix.stdin
          then
            Or_error.error_string
              "pipe fin operation list --json output into standard input"
          else (
            let%bind contents =
              Or_error.try_with (fun () -> In_channel.input_all In_channel.stdin)
            in
            Json.decode (Jsont.list transaction_id) contents
            |> Or_error.tag ~tag:"invalid transaction list on standard input")
        else
          Or_error.try_with (fun () -> [ Int64.of_string input ])
          |> Or_error.tag ~tag:("invalid operation ID: " ^ input))
      |> Or_error.combine_errors
    in
    let operation_ids = List.concat id_lists in
    let%bind _ =
      Bank_category.create
        ~event_id:1L
        ~account
        ~reason:"Set with fin operation category set"
    in
    with_database database (fun db ->
      let%bind () = require_categories db [ account ] in
      let%bind event_ids = bank_event_ids_for_operations db operation_ids in
      let%bind decisions =
        List.map event_ids ~f:(fun event_id ->
          Bank_category.create
            ~event_id
            ~account
            ~reason:"Set with fin operation category set")
        |> Or_error.combine_errors
      in
      let%bind confirmed =
        if stdin_count = 0
        then Ok true
        else (
          match%bind category_set_preview db account event_ids with
          | 0 -> Ok true
          | count ->
            confirm_on_terminal
              ~yes
              (Printf.sprintf "Set %d transaction(s) to %s?" count account))
      in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%bind changed = Review.set_bank_categories db decisions in
        printf "category_changes=%d\n" changed;
        Ok ())))
;;

let category_undo database change_id yes =
  with_database database (fun db ->
    let%bind batch = Review.category_batch ?change_id db in
    match List.hd batch with
    | None ->
      print_endline "No category changes to undo.";
      Ok ()
    | Some first ->
      let%bind rows = Review.bank_review db in
      let description =
        Hashtbl.of_alist_multi
          (module Int64)
          (List.map rows ~f:(fun (r : Review.bank_review) -> r.event_id, r.description))
      in
      printf
        "Last changed %s: %s\nThis puts back %d transaction(s):\n"
        (Timestamp.to_rfc3339 first.changed_at)
        (Terminal.text first.reason)
        (List.length batch);
      List.map batch ~f:(fun (c : Review.category_change) ->
        Option.value c.previous_account ~default:"(no decision)", c.account)
      |> List.sort_and_group ~compare:[%compare: string * string]
      |> List.iter ~f:(fun group ->
        let previous, account = List.hd_exn group in
        printf
          "  %6d  %s <- %s\n"
          (List.length group)
          (Terminal.text previous)
          (Terminal.text account));
      print_endline "For example:";
      List.iter (List.take batch 3) ~f:(fun (c : Review.category_change) ->
        printf
          "  %s\n"
          (Terminal.text
             (Option.value
                (Option.bind (Hashtbl.find description c.event_id) ~f:List.hd)
                ~default:"")));
      let%bind confirmed = confirm ~yes ~destructive:true "Undo this batch?" in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%bind changed = Review.undo_category_batch db batch in
        printf "category_changes=%d\n" changed;
        Ok ()))
;;

let category_undo_cmd =
  command
    "undo"
    "Undo the last batch of category changes; shows it and asks first."
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Puts back what one command changed: each transaction returns to its earlier \
           category and reason, or to having no decision. Changes within a second of \
           each other count as one batch, so check what it shows. Undoing an undo redoes \
           it. $(b,fin operation category history) lists change IDs; an older batch can \
           be undone only if nothing in it changed since."
      ]
    Term.(
      const category_undo
      $ database
      $ Arg.(
          value
          & pos 0 (some int64) None
          & info
              []
              ~docv:"CHANGE-ID"
              ~doc:
                "Undo the batch holding this change from the history, not the last one.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Undo without asking, for scripts."))
;;

let category_set_cmd =
  command
    "set"
    "Assign or change the category of one or more transactions atomically."
    Term.(
      const category_set
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"ACCOUNT"
              ~doc:"A category from fin category list, or equity:unclassified to reset.")
      $ Arg.(
          value
          & flag
          & info [ "yes" ] ~doc:"With -, confirm without asking, for scripts.")
      $ Arg.(
          non_empty
          & pos_right 0 string []
          & info
              []
              ~docv:"OPERATION-ID|-"
              ~doc:
                "Operation IDs, or - to read fin operation list --json from standard \
                 input, which shows what would change and asks first."))
;;

let categorize_bank_cmd =
  command
    "apply"
    "Apply a JSON batch of reviewed bank counter-accounts atomically."
    Term.(
      const categorize_bank
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"DECISIONS"
              ~doc:
                "JSON decisions: event_id, account, reason. Large IDs use decimal \
                 strings."))
;;

let category_similar database seed json_output =
  with_database database (fun db ->
    let%bind rows = Review.bank_review db in
    let%bind preview = Category_similarity.find ~seed rows in
    if json_output
    then Report_json.print Report_json.category_similar preview
    else (
      printf
        "Example and proposed category: %s\n"
        (Terminal.text preview.seed.counter_account);
      Terminal.review [ preview.seed ];
      print_endline
        "Matching unclassified transactions (across institutions, matching normalized \
         effective descriptions):";
      Terminal.review preview.candidates;
      if not (List.is_empty preview.conflicts)
      then (
        print_endline "Matching transactions classified differently:";
        Terminal.review preview.conflicts));
    Ok ())
;;

let category_similar_cmd =
  command
    "similar"
    "Preview unclassified transactions similar to a reviewed example."
    Term.(
      const category_similar
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info
              []
              ~docv:"EVENT-ID"
              ~doc:"Reviewed source event ID from fin operation show.")
      $ json_output)
;;

let category_review database operation_id =
  if not (Unix.isatty Unix.stdin)
  then Or_error.error_string "interactive category review requires a terminal"
  else
    with_database database (fun db ->
      let%bind event_id =
        bank_event_ids_for_operations db [ operation_id ] |> Result.map ~f:List.hd_exn
      in
      let%bind rows = Review.bank_review db in
      let%bind seed =
        List.find rows ~f:(fun (r : Review.bank_review) ->
          Int64.equal r.event_id event_id)
        |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
      in
      if
        not
          (Poly.equal seed.classification Review.Unclassified
           || Poly.equal seed.classification Review.Category)
      then
        Or_error.error_string
          "transaction belongs to a confirmed transfer, settlement or reversal"
      else (
        print_endline "Transaction to classify:";
        Terminal.review [ seed ];
        let%bind categories = Setup.categories db in
        if not (List.is_empty categories)
        then (
          print_endline "Available categories:";
          List.iteri categories ~f:(fun i category ->
            printf "  %d. %s\n" (i + 1) (Terminal.text category)));
        let rec pick_category () =
          match prompt "Category name or number (q to cancel): " with
          | None -> Ok None
          | Some input ->
            let input = String.strip input in
            if String.Caseless.equal input "q"
            then Ok None
            else (
              let name =
                match Option.try_with (fun () -> Int.of_string input) with
                | Some n -> Option.value (List.nth categories (n - 1)) ~default:""
                | None -> input
              in
              match
                Bank_category.create
                  ~event_id
                  ~account:name
                  ~reason:"Reviewed interactively"
              with
              | Ok _ when not (String.equal name "equity:unclassified") -> Ok (Some name)
              | Ok _ ->
                eprintf "Choose an income or expense category.\n";
                pick_category ()
              | Error e ->
                eprintf "%s\n" (Error.to_string_hum e);
                pick_category ())
        in
        let%bind category = pick_category () in
        match category with
        | None ->
          print_endline "Nothing classified.";
          Ok ()
        | Some category ->
          let example =
            { seed with
              counter_account = category
            ; reason = Some "Reviewed interactively"
            }
          in
          let%bind matches =
            if String.is_empty (String.strip seed.description)
            then
              Ok Category_similarity.{ seed = example; candidates = []; conflicts = [] }
            else
              Category_similarity.find
                ~seed:event_id
                (example
                 :: List.filter rows ~f:(fun r ->
                   not (Int64.equal r.Review.event_id event_id)))
          in
          let needs_category (r : Review.bank_review) =
            not
              (Poly.equal r.classification Review.Category
               && String.equal r.counter_account category)
          in
          let fuzzy =
            Category_similarity.fuzzy ~seed rows
            |> List.filter ~f:(fun ((r : Review.bank_review), _) ->
              Poly.equal r.classification Review.Unclassified)
          in
          let initial = matches.candidates @ List.map fuzzy ~f:fst in
          let eligible (r : Review.bank_review) =
            Poly.equal r.classification Review.Unclassified
            || Poly.equal r.classification Review.Category
          in
          let show_rows
                (selected : Review.bank_review list)
                (candidates : Review.bank_review list)
                show_fuzzy
            =
            print_endline "Matches (selected rows marked x):";
            if List.is_empty candidates then print_endline "  No matches.";
            List.iteri candidates ~f:(fun i (r : Review.bank_review) ->
              let chosen =
                List.exists selected ~f:(fun x ->
                  Int64.equal x.Review.event_id r.event_id)
              in
              let label =
                if show_fuzzy
                then
                  Option.value_map
                    (List.find fuzzy ~f:(fun (row, _) ->
                       Int64.equal row.Review.event_id r.event_id))
                    ~default:"exact"
                    ~f:(fun (_, score) -> Printf.sprintf "fuzzy %.2f" score)
                else "search"
              in
              printf
                "  %d. [%s] %s  #%Ld  %s  %s  %s %s  %s  (%s)\n"
                (i + 1)
                (if chosen then "x" else " ")
                label
                r.event_id
                (Terminal.text r.institution)
                (Date.to_string r.transaction.booked_on)
                (Quantity.to_string r.transaction.amount)
                (Iso4217.to_alpha_code r.transaction.account.currency)
                (Terminal.text r.description)
                (Terminal.text r.counter_account))
          in
          let rec select
                    (candidates : Review.bank_review list)
                    (selected : Review.bank_review list)
                    show_fuzzy
            =
            show_rows selected candidates show_fuzzy;
            printf
              "Selected: %d. all selects all selectable displayed matches and proceeds.\n"
              (List.length selected);
            print_endline
              "Enter numbers or ranges to select or deselect, e.g. 1-5,7,8-19.";
            print_endline "Commands: all, none, search, fuzzy, done, q";
            match prompt "Select matches: " with
            | None -> Ok None
            | Some input ->
              let input = String.strip input in
              if String.Caseless.equal input "q"
              then Ok None
              else if String.Caseless.equal input "done"
              then Ok (Some selected)
              else if String.Caseless.equal input "none"
              then Ok (Some [])
              else if String.Caseless.equal input "all"
              then
                Ok
                  (Some
                     (List.dedup_and_sort
                        ~compare:(fun (a : Review.bank_review) (b : Review.bank_review) ->
                          Int64.compare a.event_id b.event_id)
                        (selected @ List.filter candidates ~f:eligible)))
              else if
                String.Caseless.equal input "fuzzy" || String.Caseless.equal input "f"
              then select initial selected true
              else if
                String.Caseless.equal input "search" || String.Caseless.equal input "s"
              then (
                match prompt "Search description (empty to return): " with
                | None -> Ok None
                | Some query ->
                  let query = String.strip query in
                  if String.is_empty query
                  then select candidates selected show_fuzzy
                  else (
                    let results =
                      List.filter rows ~f:(fun (r : Review.bank_review) ->
                        (not (Int64.equal r.event_id event_id))
                        && needs_category r
                        && String.Caseless.is_substring r.description ~substring:query)
                    in
                    select results selected false))
              else (
                let number text =
                  Option.try_with (fun () -> Int.of_string (String.strip text))
                in
                let parse piece =
                  let range = String.split (String.strip piece) ~on:'-' in
                  let bounds =
                    match range with
                    | [ one ] -> Option.map (number one) ~f:(fun n -> n, n)
                    | [ first; last ] -> Option.both (number first) (number last)
                    | _ -> None
                  in
                  Option.bind bounds ~f:(fun (first, last) ->
                    if first < 1 || last < first || last > List.length candidates
                    then None
                    else
                      Some
                        (List.init
                           (last - first + 1)
                           ~f:(fun i -> List.nth_exn candidates (first + i - 1))))
                in
                let parsed = List.map (String.split input ~on:',') ~f:parse in
                if List.exists parsed ~f:Option.is_none
                then (
                  eprintf "Enter valid comma-separated numbers or ascending ranges.\n";
                  select candidates selected show_fuzzy)
                else (
                  let choices = List.concat (List.filter_opt parsed) in
                  if List.exists choices ~f:(Fn.non eligible)
                  then (
                    eprintf
                      "Only unclassified or categorized transactions can be selected.\n";
                    select candidates selected show_fuzzy)
                  else if
                    List.contains_dup
                      (List.map choices ~f:(fun r -> r.Review.event_id))
                      ~compare:Int64.compare
                  then (
                    eprintf "Select each match only once.\n";
                    select candidates selected show_fuzzy)
                  else (
                    let selected =
                      List.fold choices ~init:selected ~f:(fun selected r ->
                        if
                          List.exists selected ~f:(fun x ->
                            Int64.equal x.Review.event_id r.Review.event_id)
                        then
                          List.filter selected ~f:(fun x ->
                            not (Int64.equal x.Review.event_id r.Review.event_id))
                        else selected @ [ r ])
                    in
                    select candidates selected show_fuzzy)))
          in
          let%bind selected = select initial [] true in
          (match selected with
           | None ->
             print_endline "Nothing classified.";
             Ok ()
           | Some selected ->
             let chosen = if needs_category seed then seed :: selected else selected in
             if List.is_empty chosen
             then (
               print_endline "No category changes.";
               Ok ())
             else (
               printf "Proposed category: %s\n" (Terminal.text category);
               Terminal.review
                 (List.map chosen ~f:(fun r ->
                    { r with Review.counter_account = category; reason = None }));
               let%bind confirmed =
                 confirm
                   ~yes:false
                   ~destructive:false
                   (Printf.sprintf "Apply to %d transaction(s)?" (List.length chosen))
               in
               if not confirmed
               then (
                 print_endline "Nothing classified.";
                 Ok ())
               else (
                 let%bind decisions =
                   List.map chosen ~f:(fun (r : Review.bank_review) ->
                     Bank_category.create
                       ~event_id:r.event_id
                       ~account:category
                       ~reason:
                         (if Int64.equal r.event_id event_id
                          then "Reviewed interactively"
                          else
                            Printf.sprintf
                              "Selected during category review of event %Ld"
                              event_id))
                   |> Or_error.combine_errors
                 in
                 let expected =
                   List.map chosen ~f:(fun (r : Review.bank_review) ->
                     ( r.event_id
                     , Option.map r.reason ~f:(fun reason -> r.counter_account, reason) ))
                 in
                 let%bind changed =
                   Review.review_bank_categories db ~expected decisions
                 in
                 printf "category_changes=%d\n" changed;
                 Ok ())))))
;;

let category_queue database shortcuts types institution =
  if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout)
  then Or_error.error_string "the category queue requires a terminal"
  else (
    let%bind types =
      List.map types ~f:Operation_list_cmd.parse_movement_type_filter
      |> Or_error.combine_errors
    in
    with_database database (fun db ->
      let%bind institution =
        match institution with
        | None -> Ok None
        | Some name ->
          (match
             List.find (Registry.institutions db.registry) ~f:(fun known ->
               String.equal known name
               || List.mem (Registry.aliases db.registry known) name ~equal:String.equal)
           with
           | Some known -> Ok (Some known)
           | None -> Or_error.errorf "unknown institution: %s" name)
      in
      let%bind rows = Review.bank_review db in
      let rows =
        List.filter rows ~f:(fun (row : Review.bank_review) ->
          Poly.equal row.classification Review.Unclassified
          && Option.for_all institution ~f:(String.equal row.institution)
          && (List.is_empty types
              || List.exists types ~f:(fun filter ->
                Operation_list_cmd.matches_movement_type_filter filter row.movement_type)
             ))
      in
      let%bind categories = Setup.categories db in
      Category_tui.run db rows categories shortcuts))
;;

let category_queue_cmd =
  command
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Review unclassified transactions one at a time. Assign with a displayed \
           letter, skip with s, search with /, browse with b, add a category with a, \
           undo with u, and quit with q. Assignments include other unclassified exact \
           descriptions in the queue; x toggles this. Undo reverses the whole batch. q \
           saves all staged changes atomically; Ctrl-C discards them."
      ; `P
          "Shortcuts are stable for the session; use --key LETTER=ACCOUNT to choose a \
           letter for an existing category. The letters a, b, q, s, u and x are \
           reserved."
      ]
    "queue"
    "Classify unreviewed transactions in a full-screen keyboard queue."
    Term.(
      const category_queue
      $ database
      $ Arg.(
          value
          & opt_all string []
          & info
              [ "key" ]
              ~docv:"LETTER=ACCOUNT"
              ~doc:"Assign a category shortcut; repeat for additional shortcuts.")
      $ Arg.(
          value
          & opt_all string []
          & info
              [ "type" ]
              ~docv:"TYPE"
              ~doc:
                "Only movements of this effective type, or unknown for no type; repeat \
                 to include multiple types.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "institution" ] ~docv:"NAME" ~doc:"Only movements from this provider."))
;;

let category_review_cmd =
  command
    "review"
    "Review or change a category and selected similar movements."
    Term.(
      const category_review
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info
              []
              ~docv:"OPERATION-ID"
              ~doc:
                "Transaction ID from fin operation list; existing categories may be \
                 changed."))
;;

let category_history database =
  with_database database (fun db ->
    let%bind rows = Review.bank_category_history db in
    Report_json.print (Jsont.list Report_json.category_change) rows;
    Ok ())
;;

let category_history_cmd =
  command
    "history"
    "List the append-only history of reviewed bank categories."
    Term.(const category_history $ database)
;;
