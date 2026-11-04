open Base
open Stdio
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let institution_filter =
  Arg.(
    value
    & opt (some string) None
    & info
        [ "institution" ]
        ~docv:"INSTITUTION"
        ~doc:"Only this institution; all registered institutions by default.")
;;

let statements database institution =
  with_database database (fun db ->
    let%bind statements = Review.statement_reconciliations ?institution db in
    Report_json.print Report_json.statements statements;
    Ok ())
;;

let statements_cmd =
  command
    "list"
    "Show bank-stated balances against the movements each statement lists, as JSON."
    Term.(const statements $ database $ institution_filter)
;;

let companions database institution review =
  with_database database (fun db ->
    let%bind observations = Ledger.companion_observations ?institution db in
    let observations =
      if not review
      then observations
      else
        List.filter observations ~f:(fun o ->
          not
            (Option.exists o.Ledger.matching ~f:(fun m ->
               Poly.equal m.state Bank_match.Matched)))
    in
    let describers = Registry.companions (Accounts.registry db) in
    let%map rows =
      List.map observations ~f:(fun (o : Ledger.companion_observation) ->
        let row = o.row in
        let%bind (companion : Companion.t) =
          Or_error.of_option
            (List.find_map describers ~f:(fun (owner, c) ->
               Option.some_if
                 (String.equal owner row.institution
                  && String.equal c.Companion.format row.format)
                 c))
            ~error:(Error.of_string "companion row of an unregistered format")
        in
        let%map fields = companion.describe row in
        o, fields)
      |> Or_error.combine_errors
    in
    Report_json.print (Jsont.list Report_json.companion) rows)
;;

let companions_cmd =
  command
    "companions"
    "List companion observations (CSV enrichment exports), their fields and matches, as \
     JSON."
    Term.(
      const companions
      $ database
      $ institution_filter
      $ Arg.(
          value
          & flag
          & info
              [ "review" ]
              ~doc:"Only unmatched, ambiguous, conflicting or pending observations."))
;;

let match_companions database institution =
  with_database database (fun db ->
    let%bind result = Ledger.match_companions ?institution db in
    printf
      "matched=%d unmatched=%d ambiguous=%d conflicts=%d changed=%d\n"
      result.matched
      result.unmatched
      result.ambiguous
      result.conflicts
      result.changed;
    Ok ())
;;

let match_companions_cmd =
  command
    "match"
    "Recompute and audit companion matches against stored movements."
    Term.(const match_companions $ database $ institution_filter)
;;

let cmd =
  group
    "statement"
    "Inspect and reconcile imported statements and companion files."
    [ statements_cmd; companions_cmd; match_companions_cmd ]
;;
