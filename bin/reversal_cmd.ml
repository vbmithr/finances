open Base
open Stdio
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let reversal_candidates database =
  with_database database (fun db ->
    let%map candidates = Transfers.reversal_candidates db in
    if List.is_empty candidates
    then print_endline "No unreviewed reversal candidates."
    else
      List.iter candidates ~f:(fun (c : Transfers.reversal_candidate) ->
        printf "Original #%Ld; reversal #%Ld\n" c.original.event_id c.reversal.event_id;
        Terminal.review [ c.original; c.reversal ]))
;;

let reversal_candidates_cmd =
  command
    "candidates"
    "Show possible full withdrawal reversals."
    Term.(const reversal_candidates $ database)
;;

let reversal_review database original reversal undo yes =
  with_database database (fun db ->
    let%bind rows = Review.bank_review db in
    let find id =
      List.find rows ~f:(fun r -> Int64.equal r.Review.event_id id)
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
    in
    let%bind first = find original in
    let%bind second = find reversal in
    if not undo
    then (
      print_endline "Confirm this original withdrawal and its reversal?";
      Terminal.review [ first; second ])
    else (
      print_endline "Undo this confirmed reversal?";
      Terminal.review [ first; second ]);
    let%bind confirmed = confirm ~yes ~destructive:false "Apply this decision?" in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () = Transfers.reversal_change db ~original ~reversal ~undo in
      print_endline (if undo then "Reversal undone." else "Reversal confirmed.")))
;;

let reversal_review_cmd ~undo =
  command
    (if undo then "undo" else "review")
    (if undo
     then "Undo a confirmed reversal pair."
     else "Confirm a full withdrawal reversal pair.")
    Term.(
      const reversal_review
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"ORIGINAL-ID" ~doc:"Original withdrawal event ID.")
      $ Arg.(
          required
          & pos 1 (some int64) None
          & info [] ~docv:"REVERSAL-ID" ~doc:"Reversal event ID.")
      $ const undo
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Confirm without prompting."))
;;

let reversal_history_cmd =
  command
    "history"
    "Show the append-only reversal decision history."
    Term.(
      const (fun database ->
        with_database database (fun db ->
          let%map changes = Transfers.reversal_history db in
          List.iter changes ~f:(fun (r : Transfers.reversal_history_entry) ->
            printf
              "#%Ld  #%Ld -> #%Ld  %s  %s\n"
              r.id
              r.original
              r.reversal
              (match r.action with
               | Reversal_confirmed -> "confirmed"
               | Reversal_undone -> "undone")
              (Timestamp.to_rfc3339 r.changed_at))))
      $ database)
;;

let cmd =
  group
    "reversal"
    "Review and undo transaction reversals."
    [ reversal_candidates_cmd
    ; reversal_review_cmd ~undo:false
    ; reversal_review_cmd ~undo:true
    ; reversal_history_cmd
    ]
;;
