open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let decisions_json db =
  let%map snapshot = Decisions.export db in
  Json.encode ~format:Jsont.Indent Decisions_portable.codec snapshot ^ "\n"
;;

let export database path =
  with_database database (fun db ->
    let%bind contents = decisions_json db in
    let%map () = write_file path contents in
    printf "Decisions saved to %s. It holds personal details; keep it private.\n" path)
;;

let export_cmd =
  command
    "export"
    "Save everything you decided to one file."
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Saves account settings, card assignments, the category catalog, rules, \
           category assignments, manual types, resolved movements, card settlements, \
           reversals, exchanges and transfers. Statements are not in the file: they come \
           back from $(b,fin import). $(b,fin backup) also writes this file."
      ]
    Term.(
      const export
      $ database
      $ Arg.(
          required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc:"File to write."))
;;

let print_changes (c : Decisions.changes) =
  printf
    "accounts=%d assets=%d cards=%d categories=%d rules=%d assignments=%d types=%d \
     resolutions=%d settlements=%d reversals=%d exchanges=%d transfers=%d\n"
    c.accounts
    c.assets
    c.cards
    c.categories
    c.rules
    c.assignments
    c.types
    c.resolutions
    c.settlements
    c.reversals
    c.exchanges
    c.transfers
;;

let restore database path yes =
  let%bind contents = read_file path in
  let%bind snapshot = Json.decode Decisions_portable.codec contents in
  with_database database (fun db ->
    let%bind outcome =
      Decisions.restore db snapshot ~confirm:(fun changes ->
        print_changes changes;
        confirm ~yes ~destructive:false "Restore these decisions?")
    in
    match outcome with
    | Declined _ ->
      print_endline "Nothing restored.";
      Ok ()
    | Applied changes ->
      print_endline "Restored:";
      print_changes changes;
      Ok ())
;;

let restore_cmd =
  command
    "restore"
    "Re-apply a decisions file to imported statements; shows the changes and asks first."
    ~man:
      [ `S Manpage.s_description
      ; `P
          "Run $(b,fin import) first: restoring never imports statements or creates \
           operations. Each decision finds its movement by source identity. A decision \
           that matches nothing, or whose source changed, is a problem. The file applies \
           as one transaction, so a problem or answering $(b,n) changes nothing. A file \
           may hold only some sections."
      ]
    Term.(
      const restore
      $ database
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"FILE" ~doc:"Decisions file.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Restore without asking, for scripts."))
;;

let decisions_cmd =
  Cmd.group
    (Cmd.info "decisions" ~doc:"Save and re-apply everything you decided.")
    [ export_cmd; restore_cmd ]
;;
