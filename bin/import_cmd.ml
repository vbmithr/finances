open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let import_bank database institution paths journal max_mib account_id =
  if max_mib < 1 || max_mib > 1024
  then Or_error.error_string "max-file-mib must be between 1 and 1024"
  else (
    let%bind () = require_database database in
    let%bind runtime_registry = registry_for database in
    let%bind definitions = institution_definitions database in
    let journal =
      Option.value
        journal
        ~default:
          (Stdlib.Filename.concat (Stdlib.Filename.dirname database) "bank.journal")
    in
    let%bind companion_profiles = companion_profiles database in
    let%bind institution, registry =
      if Registry.is_registered runtime_registry institution
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
          then Ok ()
          else Setup.add_institution db institution)
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

(* One source, named as on the command line: an institution or alias with its paths, a
   or a user-added institution. *)
let import_source database first paths journal max_mib account_id =
  let%bind () = require_database database in
  let%bind runtime_registry = registry_for database in
  let institutions =
    List.filter (Registry.institutions runtime_registry) ~f:(fun institution ->
      List.mem (Registry.aliases runtime_registry institution) first ~equal:String.equal)
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
    else import_bank database institution paths journal max_mib account_id
  | _ :: _ :: _ -> Or_error.errorf "ambiguous institution alias: %s" first
  | [] when List.mem user_institutions first ~equal:String.equal ->
    if List.is_empty paths
    then Or_error.error_string "provide at least one statement file or directory"
    else (
      let%bind files = generic_statement_files paths in
      let%bind companion =
        with_database database (fun db -> Setup.institution_companion_profile db first)
      in
      if Option.is_some companion
      then import_bank database first files journal max_mib account_id
      else (
        let%bind () = Csv_setup.ensure_generic_source database first "bank-transaction" in
        import_bank database first files journal max_mib account_id))
  | [] when Stdlib.Sys.file_exists first && Stdlib.Sys.is_directory first ->
    Or_error.error_string
      "name the bank before a directory, for example: fin import wise DIRECTORY"
  | [] when Stdlib.Sys.file_exists first ->
    Or_error.error_string
      "name an institution with a records converter before importing a CSV"
  | [] -> Or_error.errorf "unknown institution or file: %s" first
;;

(* Each folder of [directory] named after an institution or an alias, imported as if by
   [fin import NAME FOLDER], in name order. Every name is checked before anything is
   imported. *)
let import_all database directory journal max_mib =
  let%bind () = require_database database in
  let%bind runtime_registry = registry_for database in
  let%bind user_institutions = with_database database Setup.institutions in
  let%bind entries =
    Or_error.try_with (fun () -> Stdlib.Sys.readdir directory |> Array.to_list)
  in
  let visible =
    List.filter entries ~f:(fun name -> not (String.is_prefix name ~prefix:"."))
    |> List.sort ~compare:String.compare
  in
  let folders, files =
    List.partition_tf visible ~f:(fun name ->
      Stdlib.Sys.is_directory (Stdlib.Filename.concat directory name))
  in
  let known name =
    List.mem user_institutions name ~equal:String.equal
    || List.exists (Registry.institutions runtime_registry) ~f:(fun institution ->
      List.mem (Registry.aliases runtime_registry institution) name ~equal:String.equal)
  in
  let unknown = List.filter folders ~f:(Fn.non known) in
  if List.is_empty folders
  then Or_error.errorf "%s has no institution folders" directory
  else if not (List.is_empty unknown)
  then
    Or_error.errorf
      "these folders are not institutions or aliases (see fin institution list): %s. \
       Rename them, or import them separately. Nothing was imported."
      (String.concat ~sep:", " unknown)
  else if not (List.is_empty files)
  then
    Or_error.errorf
      "files directly in %s belong to no institution: %s. Move them into an \
       institution's folder. Nothing was imported."
      directory
      (String.concat ~sep:", " files)
  else (
    let results =
      List.map folders ~f:(fun name ->
        printf "== %s\n" name;
        Out_channel.flush stdout;
        let result =
          import_source
            database
            name
            [ Stdlib.Filename.concat directory name ]
            journal
            max_mib
            None
        in
        (match result with
         | Ok () -> ()
         | Error error -> printf "%s: %s\n" name (Error.to_string_hum error));
        name, result)
    in
    match List.filter results ~f:(fun (_, r) -> Result.is_error r) with
    | [] -> Ok ()
    | failed ->
      Or_error.errorf
        "%d of %d institutions need review: %s; the others were imported"
        (List.length failed)
        (List.length results)
        (String.concat ~sep:", " (List.map failed ~f:fst)))
;;

let cmd =
  command
    ~man:
      [ `S Manpage.s_synopsis
      ; `P "$(b,fin import) [$(i,OPTION)]… $(i,INSTITUTION) $(i,PATH)…"
      ; `P "$(b,fin import) --all [$(i,OPTION)]… $(i,DIRECTORY)"
      ; `S Manpage.s_description
      ; `P
          "Import an institution's files or directories by naming it, for example \
           $(b,fin import wise ~/statements/wise). Run $(b,fin institution list) to see \
           institutions. User-added institutions support OFX 2.x; attach a records \
           definition to import their CSV files."
      ; `P
          "Movement CSV institutions (bundled: cdc) use the same command and the same \
           import pipeline as every other venue: $(b,fin import cdc FILE-OR-DIRECTORY) \
           stores source evidence and proposed operations, and $(b,--inspect) reads a \
           CSV without a database. Review wallets, commodities and kinds afterwards with \
           $(b,fin db sources) and $(b,fin operation list)."
      ; `P
          "With $(b,--all), import a directory holding one folder per institution, each \
           named after the institution or an alias, for example $(b,fin import --all \
           ~/statements) with $(i,~/statements/wise) and $(i,~/statements/bourso)."
      ]
    "import"
    "Import a bank's statement files, apply mappings, and publish a bank journal."
    Term.(
      const (fun database first paths journal max_mib account_id all ->
        if all
        then
          if not (List.is_empty paths)
          then Or_error.error_string "--all takes one directory"
          else if Option.is_some account_id
          then Or_error.error_string "--account-id names one import, not --all"
          else import_all database first journal max_mib
        else import_source database first paths journal max_mib account_id)
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info
              []
              ~docv:"SOURCE"
              ~doc:
                ("Institution name. Built-in institutions: "
                 ^ String.concat ~sep:", " (Registry.institutions Builtin.registry)
                 ^ ". Registered aliases are accepted (for example bourso and boursorama \
                    for boursobank)."))
      $ Arg.(
          value
          & pos_right 0 string []
          & info
              []
              ~docv:"PATH"
              ~doc:
                "Statement files or directories for the named institution. Directories \
                 are searched recursively; hidden files are skipped. ZIPs are read in \
                 memory; nested ZIPs and encrypted archives are unsupported. Files are \
                 only read, never modified; repeated imports add only new records.")
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
          & info [ "account-id" ] ~doc:"Account label for an adapter that requires one.")
      $ Arg.(
          value
          & flag
          & info
              [ "all" ]
              ~doc:
                "Import every folder of the directory given as SOURCE, each named after \
                 an institution or an alias, as if by fin import NAME FOLDER. Any other \
                 folder, or a file directly in the directory, stops the import before it \
                 starts."))
;;
