open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let inspect_movement_csv (profile : Movement_csv.profile) files summary_only json_output =
  let%bind rows =
    List.map files ~f:(fun file ->
      let%bind contents = read_file file in
      Movement_csv.parse profile ~file contents)
    |> Or_error.combine_errors
    |> Result.map ~f:List.concat
  in
  if summary_only
  then (
    let summary = Movement_csv.summarize rows in
    if json_output
    then Report_json.print Report_json.movement_csv_summary summary
    else (
      printf
        "%s: %d rows; proposed effects are not reconciled.\n"
        profile.institution
        summary.total;
      List.iter summary.by_kind ~f:(fun (kind : Movement_csv.kind_summary) ->
        printf
          "  %s: %d (single %d, exchange %d, review %d)\n"
          (Terminal.text
             (if String.is_empty kind.kind then "(empty kind)" else kind.kind))
          kind.rows
          kind.single
          kind.exchange
          kind.review);
      printf
        "Identical-row groups (review, not automatically deduplicated): %d\n"
        (List.length summary.identical_rows);
      List.iter summary.identical_rows ~f:(fun refs ->
        List.iter refs ~f:(fun (r : Movement_csv.row_reference) ->
          printf "  %s:%d\n" (Terminal.text r.file) r.row_number))))
  else if json_output
  then Report_json.print (Jsont.list Report_json.movement_csv_row) rows
  else (
    let review =
      List.count rows ~f:(fun (row : Movement_csv.row) ->
        match row.interpretation with
        | Review _ -> true
        | _ -> false)
    in
    printf
      "%s: %d rows; %d need review. Proposed legs are not posted.\n"
      profile.institution
      (List.length rows)
      review;
    List.iter rows ~f:(fun (row : Movement_csv.row) ->
      let kind = if String.is_empty row.kind then "(empty kind)" else row.kind in
      let legs =
        List.map row.proposed_legs ~f:(fun (leg : Movement_csv.leg) ->
          Quantity.to_string leg.amount ^ " " ^ Commodity.display leg.commodity)
        |> String.concat ~sep:", "
      in
      let result =
        match row.interpretation with
        | Review reason -> "review: " ^ reason
        | Single | Exchange -> legs
      in
      printf
        "%s:%d  %s  %s\n"
        (Terminal.text row.file)
        row.row_number
        (Terminal.text kind)
        (Terminal.text result)));
  Ok ()
;;

let import_bank database institution paths journal max_mib account_id provider =
  if max_mib < 1 || max_mib > 1024
  then Or_error.error_string "max-file-mib must be between 1 and 1024"
  else (
    let%bind () = require_database database in
    let%bind runtime_registry = registry_for database in
    let%bind definitions = institution_definitions database in
    let%bind account_id =
      Csv_setup.import_account_label runtime_registry institution account_id
    in
    let journal =
      Option.value
        journal
        ~default:
          (Stdlib.Filename.concat (Stdlib.Filename.dirname database) "bank.journal")
    in
    let selected_profile = ref None in
    let%bind companion_profiles = companion_profiles database in
    let%bind institution, registry =
      if String.equal institution "csv"
      then (
        let%bind provider =
          match provider with
          | Some name -> Validation.token "CSV provider" name
          | None when Unix.isatty Unix.stdin ->
            let rec read_provider () =
              match prompt "Provider name (required): " with
              | None -> Or_error.error_string "CSV provider is required"
              | Some name when String.is_empty (String.strip name) ->
                eprintf "A provider name is required.\n";
                read_provider ()
              | Some name ->
                (match Validation.token "CSV provider" (String.strip name) with
                 | Ok name
                   when not (Csv_setup.builtin_institution_name runtime_registry name) ->
                   Ok name
                 | Ok _ ->
                   eprintf "Choose a provider name that is not a built-in institution.\n";
                   read_provider ()
                 | Error error ->
                   eprintf "%s\n" (Error.to_string_hum error);
                   read_provider ())
            in
            read_provider ()
          | None -> Or_error.error_string "generic CSV imports require --provider NAME"
        in
        if Csv_setup.builtin_institution_name runtime_registry provider
        then Or_error.error_string "CSV provider must not be a built-in institution name"
        else (
          let%bind () =
            Csv_setup.ensure_generic_source database provider "csv-movement"
          in
          match paths with
          | [ path ] ->
            let%map profile =
              Csv_setup.csv_import_profile database path max_mib ~institution:provider
            in
            selected_profile := Some (Json.encode Csv_profile.codec profile);
            ( provider
            , Builtin.registry_with_csv_profile
                ~definitions
                ~companions:companion_profiles
                ~institution:provider
                profile )
          | _ ->
            Or_error.error_string
              "generic CSV import currently accepts one file at a time"))
      else if Option.is_some provider
      then Or_error.error_string "--provider applies only to generic CSV imports"
      else if Registry.is_registered runtime_registry institution
      then Ok (institution, runtime_registry)
      else
        Ok
          ( institution
          , Builtin.registry_with_ofx
              ~definitions
              ~companions:companion_profiles
              ~institution
              () )
    in
    let%bind report =
      Finances_import.Workflow.run
        ~registry
        ~configure_new_accounts:Csv_setup.configure_new_accounts
        ?account_id
        ~max_file_bytes:Int64.(of_int max_mib * 1048576L)
        ~database
        ~institution
        ~paths
        ~journal
        ()
    in
    let%bind () =
      if
        Registry.is_registered runtime_registry institution
        || not
             (List.exists report.inputs.files ~f:(fun row ->
                Poly.equal row.outcome Imported || Poly.equal row.outcome Unchanged))
      then Ok ()
      else
        with_database database (fun db ->
          let%bind existing = Setup.institutions db in
          if List.mem existing institution ~equal:String.equal
          then (
            match !selected_profile with
            | None -> Ok ()
            | Some profile ->
              Setup.set_institution_profile_if_missing db institution profile)
          else Setup.add_institution ?profile:!selected_profile db institution)
    in
    List.iter report.inputs.files ~f:(fun row ->
      printf
        "%s\t%s\tinserted=%d existing=%d%s\n"
        (Bank_import.outcome_name row.outcome)
        row.path
        row.inserted
        row.existing
        (Option.value_map row.message ~default:"" ~f:(fun message -> "\t" ^ message)));
    List.iter report.inputs.ignored ~f:(fun row ->
      printf "ignored\t%s\t%s\n" row.path row.reason);
    let count outcome =
      List.count report.inputs.files ~f:(fun row -> Poly.equal row.outcome outcome)
    in
    printf
      "imported=%d unchanged=%d rejected=%d unsupported=%d unreadable=%d ignored=%d\n"
      (count Imported)
      (count Unchanged)
      (count Rejected)
      (count Unsupported)
      (count Unreadable)
      (List.length report.inputs.ignored);
    printf
      "entries_inserted=%d entries_existing=%d\njournal=%s\n"
      report.projection.inserted
      report.projection.existing
      report.journal;
    printf "identity_changes=%d\n" report.identity_changes;
    printf
      "companions_matched=%d companions_unmatched=%d companions_ambiguous=%d \
       companions_conflicts=%d companions_changes=%d\n"
      report.companions.matched
      report.companions.unmatched
      report.companions.ambiguous
      report.companions.conflicts
      report.companions.changed;
    (* A file named on the command line that no adapter recognizes was not
       imported, unlike an unrelated file found inside a directory. *)
    let unsupported_explicit =
      List.count report.inputs.files ~f:(fun row ->
        Poly.equal row.outcome Unsupported
        && List.mem paths row.path ~equal:String.equal
        && Stdlib.Sys.file_exists row.path
        && not (Stdlib.Sys.is_directory row.path))
    in
    if count Rejected + count Unreadable + unsupported_explicit > 0
    then
      Or_error.error_string
        "some inputs need review; successful inputs were imported and the journal was \
         generated"
    else Ok ())
;;

let statement_files ?(extensions = [ ".csv"; ".ofx"; ".zip" ]) paths =
  let rec walk path =
    Or_error.try_with (fun () -> Unix.lstat path)
    |> Result.bind ~f:(fun stat ->
      match stat.Unix.st_kind with
      | Unix.S_REG ->
        if
          List.mem
            [ ".csv"; ".ofx"; ".zip" ]
            (String.lowercase (Stdlib.Filename.extension path))
            ~equal:String.equal
        then Ok [ path ]
        else Or_error.errorf "unsupported statement file: %s" path
      | Unix.S_DIR ->
        let%bind children =
          Bos.OS.Dir.contents (Fpath.v path)
          |> Result.map_error ~f:(fun (`Msg m) -> Error.of_string m)
        in
        let children =
          List.filter children ~f:(fun child ->
            not (String.is_prefix (Fpath.basename child) ~prefix:"."))
          |> List.sort ~compare:Fpath.compare
        in
        List.map children ~f:(fun child ->
          let child = Fpath.to_string child in
          match Unix.lstat child with
          | { st_kind = S_DIR; _ } -> walk child
          | { st_kind = S_REG; _ }
            when List.mem
                   extensions
                   (String.lowercase (Stdlib.Filename.extension child))
                   ~equal:String.equal -> Ok [ child ]
          | _ -> Ok [])
        |> Or_error.combine_errors
        |> Result.map ~f:List.concat
      | _ -> Or_error.errorf "not a regular statement file or directory: %s" path)
  in
  let%bind files =
    List.map paths ~f:walk |> Or_error.combine_errors |> Result.map ~f:List.concat
  in
  let files = List.dedup_and_sort files ~compare:String.compare in
  if List.is_empty files
  then
    Or_error.errorf
      "no %s files found"
      (String.concat
         ~sep:", "
         (List.map extensions ~f:(fun e -> String.uppercase (String.drop_prefix e 1))))
  else Ok files
;;

let generic_statement_files paths = statement_files paths

let cmd =
  command
    ~man:
      [ `S Manpage.s_synopsis
      ; `P "$(b,fin import) [$(i,OPTION)]… $(i,INSTITUTION) $(i,PATH)…"
      ; `P "$(b,fin import) [$(i,OPTION)]… $(i,CSV-FILE)"
      ; `S Manpage.s_description
      ; `P
          "Import an institution's files or directories by naming it, for example \
           $(b,fin import wise ~/statements/wise). Run $(b,fin institution list) to see \
           institutions. User-added institutions support OFX 2.x or generic CSV; these \
           are separate primary sources and cannot be mixed. To configure a generic CSV, \
           pass its filename directly."
      ; `P
          "Movement CSV institutions (bundled: cdc) use the same command and the same \
           import pipeline as every other venue: $(b,fin import cdc FILE-OR-DIRECTORY) \
           stores source evidence and proposed operations, and $(b,--inspect) reads a \
           CSV without a database. Review wallets, commodities and kinds afterwards with \
           $(b,fin db sources) and $(b,fin operation list)."
      ]
    "import"
    "Import a bank's statement files, apply mappings, and publish a bank journal."
    Term.(
      const
        (fun
            database
             first
             paths
             journal
             max_mib
             account_id
             provider
             inspect
             summary
             json
           ->
           let movement_profile = Builtin.movement_profile (String.lowercase first) in
           match movement_profile with
           | Some profile when inspect ->
             (match paths with
              | [] -> Or_error.error_string "provide at least one CSV file or directory"
              | _ ->
                let%bind files = statement_files ~extensions:[ ".csv" ] paths in
                inspect_movement_csv profile files summary json)
           | _ ->
             if inspect || summary || json
             then
               Or_error.error_string
                 (match movement_profile with
                  | Some _ -> "--summary and --json require --inspect"
                  | None ->
                    "--inspect, --summary and --json apply only to movement CSV \
                     institutions (for example cdc)")
             else (
               let%bind () = require_database database in
               let%bind runtime_registry = registry_for database in
               let institutions =
                 List.filter
                   (Registry.institutions runtime_registry)
                   ~f:(fun institution ->
                     (not (String.equal institution "csv"))
                     && List.mem
                          (Registry.aliases runtime_registry institution)
                          first
                          ~equal:String.equal)
               in
               let%bind user_institutions =
                 if
                   List.is_empty institutions
                   && ((not (Stdlib.Sys.file_exists first)) || not (List.is_empty paths))
                 then with_database database Setup.institutions
                 else Ok []
               in
               match institutions with
               | [ institution ] ->
                 if List.is_empty paths
                 then Or_error.error_string "provide at least one statement path"
                 else
                   import_bank
                     database
                     institution
                     paths
                     journal
                     max_mib
                     account_id
                     provider
               | _ :: _ :: _ -> Or_error.errorf "ambiguous institution alias: %s" first
               | [] when List.mem user_institutions first ~equal:String.equal ->
                 if List.is_empty paths
                 then
                   Or_error.error_string
                     "provide at least one statement file or directory"
                 else if Option.is_some provider
                 then
                   Or_error.error_string
                     "--provider is unnecessary when the institution is named"
                 else (
                   let%bind files = generic_statement_files paths in
                   let%bind companion =
                     with_database database (fun db ->
                       Setup.institution_companion_profile db first)
                   in
                   if Option.is_some companion
                   then import_bank database first files journal max_mib account_id None
                   else (
                     let csv, ofx =
                       List.partition_tf files ~f:(fun path ->
                         String.Caseless.equal (Stdlib.Filename.extension path) ".csv")
                     in
                     if (not (List.is_empty csv)) && not (List.is_empty ofx)
                     then
                       Or_error.error_string
                         "generic CSV and OFX are separate movement sources; import one \
                          format to avoid duplicate transactions"
                     else if not (List.is_empty ofx)
                     then (
                       let%bind () =
                         Csv_setup.ensure_generic_source database first "bank-transaction"
                       in
                       import_bank database first ofx journal max_mib account_id None)
                     else (
                       let%bind () =
                         Csv_setup.ensure_generic_source database first "csv-movement"
                       in
                       List.fold_result csv ~init:() ~f:(fun () path ->
                         printf "CSV file: %s\n" path;
                         Out_channel.flush stdout;
                         import_bank
                           database
                           "csv"
                           [ path ]
                           journal
                           max_mib
                           account_id
                           (Some first)))))
               | [] when Stdlib.Sys.file_exists first && Stdlib.Sys.is_directory first ->
                 Or_error.error_string
                   "name the bank before a directory, for example: fin import wise \
                    DIRECTORY"
               | [] when Stdlib.Sys.file_exists first ->
                 import_bank
                   database
                   "csv"
                   (first :: paths)
                   journal
                   max_mib
                   account_id
                   provider
               | [] -> Or_error.errorf "unknown institution or file: %s" first))
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"SOURCE"
              ~doc:
                ("Institution name or generic CSV filename. Built-in institutions: "
                 ^ String.concat
                     ~sep:", "
                     (List.filter (Registry.institutions Builtin.registry) ~f:(fun name ->
                        not (String.equal name "csv")))
                 ^ ". Registered aliases are accepted (for example bourso and boursorama \
                    for boursobank). Unknown CSV layouts are configured interactively."))
      $ Arg.(
          value
          & pos_right 0 string []
          & info
              []
              ~docv:"PATH"
              ~doc:
                "Statement files or directories for the named institution. Directories \
                 are searched recursively; hidden files are skipped. Generic \
                 institutions accept CSV or OFX files and OFX ZIPs, without mixing CSV \
                 and OFX. ZIPs are read in memory; nested ZIPs and encrypted archives \
                 are unsupported. Files are only read, never modified; repeated imports \
                 add only new records. Revolut and Wise CSV clocks are assumed UTC; Wise \
                 matching retains CAMT source-local clocks.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "journal" ]
              ~doc:"Output path (default bank.journal beside the database).")
      $ Arg.(
          value
          & opt int 64
          & info
              [ "max-file-mib" ]
              ~doc:
                "Per-file read limit in MiB (1-1024, default 64). For ZIPs, also limits \
                 the total expanded size; at most 1024 members per archive.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "account-id" ]
              ~doc:
                "Stable account label for account-free exports (Revolut and generic \
                 CSV). Asked on a terminal if omitted; required in scripts. New accounts \
                 missing an IBAN prompt once on a terminal; Enter skips, and account \
                 edit can add it later.")
      $ Arg.(
          value
          & opt (some string) None
          & info
              [ "provider" ]
              ~doc:"Institution name for a generic CSV; prompted on a terminal.")
      $ Arg.(
          value
          & flag
          & info
              [ "inspect" ]
              ~doc:
                "Movement CSV institutions such as cdc: read-only interpretation of the \
                 CSV rows; needs no database.")
      $ Arg.(
          value
          & flag
          & info
              [ "summary" ]
              ~doc:"With --inspect, summarize kinds and identical row candidates.")
      $ json_output)
;;
