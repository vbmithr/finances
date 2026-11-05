open Base
open Stdio
open Finances
open Finances_store
open Cmdliner
open Or_error.Let_syntax
open Cli

let transfer_candidates database all json_output =
  with_database database (fun db ->
    let%bind rows = Transfers.transfer_candidates db in
    let%bind ranked = Transfer_ranking.analyze (Accounts.registry db) rows in
    let%bind extra =
      if all then Transfers.transfer_candidates ~include_reviewed:true db else Ok []
    in
    if json_output
    then Report_json.print Report_json.preview (ranked, extra)
    else (
      print_endline
        "Transfer candidates: evidence scores are not probabilities; nothing is \
         confirmed automatically.\n";
      if List.is_empty ranked
      then print_endline "No compatible unreviewed transfer candidates.";
      List.iter ranked ~f:(fun r ->
        Terminal.transfer r.Transfer_ranking.candidate;
        printf
          "  Score: %d; confidence: %s; assignment: %s\n"
          r.score
          r.confidence
          r.assignment.status;
        List.iter r.evidence ~f:(fun e ->
          printf "  %s (%+d)\n" (Terminal.text e.Transfer_ranking.detail) e.points);
        print_endline "");
      if all
      then
        List.iter extra ~f:(fun r ->
          if
            not
              (List.exists ranked ~f:(fun ranked ->
                 Int64.equal
                   ranked.Transfer_ranking.candidate.first_event_id
                   r.first_event_id
                 && Int64.equal ranked.candidate.second_event_id r.second_event_id))
          then (
            Terminal.transfer r;
            printf
              "  State: %s%s\n"
              (Option.value_map
                 r.state
                 ~default:"unreviewed; unavailable"
                 ~f:Bank_transfer.state_name)
              (if r.iban_compatible then "" else " (excluded: conflicting IBAN evidence)");
            List.iter r.iban_evidence ~f:(fun evidence ->
              printf "  %s\n" (Terminal.text evidence));
            Option.iter r.reason ~f:(fun reason ->
              printf "  Reason: %s\n" (Terminal.text reason));
            print_endline "")));
    Ok ())
;;

let transfer_candidates_cmd =
  command
    "candidates"
    "Show possible transfers between unclassified movements, highest score first."
    Term.(
      const transfer_candidates
      $ database
      $ Arg.(
          value
          & flag
          & info
              [ "all" ]
              ~doc:
                "Include reviewed pairs and movements already assigned to transfers; \
                 unreviewed categorized movements remain excluded.")
      $ json_output)
;;

let review_transfers database path =
  with_database database (fun db ->
    let%bind decisions =
      match path with
      | Some path ->
        let%bind contents = read_file path in
        Bank_transfer.of_json contents
      | None ->
        if not (Unix.isatty Unix.stdin)
        then
          Or_error.error_string
            "interactive review requires a terminal or a decisions file"
        else (
          let%bind candidates = Transfers.transfer_candidates db in
          let%bind ranked = Transfer_ranking.analyze (Accounts.registry db) candidates in
          print_endline
            "Review one candidate pair at a time. Choices are staged until you confirm \
             the whole batch.";
          print_endline
            "Confirm only pairs representing transfers between your own accounts.";
          print_endline
            "For the current pair: c = confirm, r = reject, s = skip.\n\
             For the batch: a = select supported pairs and finish, d = finish, q = cancel.\n\
             Other candidates remain for later review. Nothing is saved until the final \
             confirmation.";
          let make state (item : Transfer_ranking.ranked) =
            let c = item.candidate in
            Bank_transfer.create
              ~first_event_id:c.first_event_id
              ~second_event_id:c.second_event_id
              ~state
              ~reason:
                (if Poly.equal state Bank_transfer.Confirmed
                 then "Confirmed after interactive review"
                 else "Rejected after interactive review")
          in
          let overlaps (a : Bank_transfer.decision) (b : Bank_transfer.decision) =
            Poly.equal a.state Bank_transfer.Confirmed
            && Poly.equal b.state Bank_transfer.Confirmed
            && (Int64.equal a.first_event_id b.first_event_id
                || Int64.equal a.first_event_id b.second_event_id
                || Int64.equal a.second_event_id b.first_event_id
                || Int64.equal a.second_event_id b.second_event_id)
          in
          let stage pending decision =
            if List.exists pending ~f:(overlaps decision)
            then (
              print_endline "This conflicts with a pending confirmation; skipped.";
              pending)
            else decision :: pending
          in
          let rec loop pending = function
            | [] -> Ok (Some (List.rev pending))
            | (item : Transfer_ranking.ranked) :: rest ->
              let c = item.candidate in
              if
                List.exists pending ~f:(fun (d : Bank_transfer.decision) ->
                  Int64.equal
                    d.first_event_id
                    (Int64.min c.first_event_id c.second_event_id)
                  && Int64.equal
                       d.second_event_id
                       (Int64.max c.first_event_id c.second_event_id))
              then loop pending rest
              else (
                printf
                  "\nCurrent pair: #%Ld and #%Ld\n"
                  c.first_event_id
                  c.second_event_id;
                Terminal.transfer c;
                printf
                  "  Score: %d; confidence: %s; assignment: %s\n"
                  item.score
                  item.confidence
                  item.assignment.status;
                List.iter item.evidence ~f:(fun e ->
                  printf "  %s (%+d)\n" (Terminal.text e.Transfer_ranking.detail) e.points);
                match
                  prompt "Current pair or batch choice [c/r/s/a/d/q]: "
                  |> Option.map ~f:(fun s -> String.lowercase (String.strip s))
                with
                | None | Some "q" -> Ok None
                | Some "d" -> Ok (Some (List.rev pending))
                | Some "s" -> loop pending rest
                | (Some "c" | Some "r") as choice ->
                  let state =
                    if Poly.equal choice (Some "c")
                    then Bank_transfer.Confirmed
                    else Rejected
                  in
                  let%bind decision = make state item in
                  loop (stage pending decision) rest
                | Some "a" ->
                  let%bind supported =
                    List.filter ranked ~f:(fun r ->
                      String.equal r.Transfer_ranking.assignment.status "supported")
                    |> List.filter ~f:(fun r ->
                      let c = r.Transfer_ranking.candidate in
                      not
                        (List.exists pending ~f:(fun (d : Bank_transfer.decision) ->
                           Int64.equal
                             d.first_event_id
                             (Int64.min c.first_event_id c.second_event_id)
                           && Int64.equal
                                d.second_event_id
                                (Int64.max c.first_event_id c.second_event_id))))
                    |> List.map ~f:(make Bank_transfer.Confirmed)
                    |> Or_error.combine_errors
                  in
                  let updated = List.fold supported ~init:pending ~f:stage in
                  printf
                    "Staged %d supported pairs.\n"
                    (List.length updated - List.length pending);
                  print_endline "Remaining candidates are left for later review.";
                  Ok (Some (List.rev updated))
                | Some _ ->
                  print_endline "Choose c, r, s, a, d or q.";
                  loop pending (item :: rest))
          in
          let%bind pending = loop [] ranked in
          match pending with
          | None -> Ok []
          | Some [] -> Ok []
          | Some decisions ->
            printf "Pending decisions: %d\n" (List.length decisions);
            List.iter decisions ~f:(fun (d : Bank_transfer.decision) ->
              Option.iter
                (List.find ranked ~f:(fun r ->
                   let c = r.Transfer_ranking.candidate in
                   Int64.equal
                     d.first_event_id
                     (Int64.min c.first_event_id c.second_event_id)
                   && Int64.equal
                        d.second_event_id
                        (Int64.max c.first_event_id c.second_event_id)))
                ~f:(fun r -> Terminal.transfer r.candidate);
              printf
                "  #%Ld <-> #%Ld: %s\n"
                d.first_event_id
                d.second_event_id
                (Bank_transfer.state_name d.state));
            let%bind apply =
              confirm ~yes:false ~destructive:false "Apply these decisions?"
            in
            if apply then Ok decisions else Ok [])
    in
    if List.is_empty decisions
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map changed =
        Transfers.set_bank_transfers
          ~require_unreviewed:(Option.is_none path)
          db
          decisions
      in
      printf "transfer_changes=%d\n" changed))
;;

let review_transfers_cmd =
  command
    "review"
    "Review candidates by descending score, or apply JSON confirm/reject decisions."
    Term.(
      const review_transfers
      $ database
      $ Arg.(
          value
          & pos 0 (some string) None
          & info
              []
              ~docv:"DECISIONS"
              ~doc:"Optional JSON transfer review decisions; omit for interactive review."))
;;

let transfer_link database first_operation second_operation first_role second_role yes =
  with_database database (fun db ->
    let%bind preview =
      Operations.preview_general_transfer
        db
        ~first_operation
        ~first_role
        ~second_operation
        ~second_role
    in
    printf "Proposed transfer:\n";
    List.iter
      [ preview.first; preview.second ]
      ~f:(fun (leg : Operations.general_transfer_leg) ->
        printf
          "  operation #%Ld (%s): %s %s  %s\n"
          leg.operation_id
          (Terminal.text leg.role)
          (Quantity.to_string leg.quantity)
          (Terminal.text leg.symbol)
          (Terminal.text leg.account));
    let%bind confirmed = confirm ~yes ~destructive:false "Confirm transfer?" in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () = Operations.confirm_general_transfer db ~expected:preview in
      print_endline "transfer_changes=1"))
;;

let transfer_link_cmd =
  command
    "link"
    "Review and link two resolved single-movement operations."
    Term.(
      const transfer_link
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"FIRST-OPERATION-ID")
      $ Arg.(required & pos 1 (some int64) None & info [] ~docv:"SECOND-OPERATION-ID")
      $ Arg.(
          value
          & opt string "primary"
          & info
              [ "first-role" ]
              ~docv:"ROLE"
              ~doc:"First movement role (default: primary).")
      $ Arg.(
          value
          & opt string "primary"
          & info
              [ "second-role" ]
              ~docv:"ROLE"
              ~doc:"Second movement role (default: primary).")
      $ Arg.(
          value
          & flag
          & info [ "yes" ] ~doc:"Confirm without prompting after the preview."))
;;

let transfer_unlink database operation_id yes =
  with_database database (fun db ->
    let%bind links = Review.operation_transfer_links db in
    let%bind link =
      List.find links ~f:(fun l ->
        Int64.equal l.first_operation_id operation_id
        || Int64.equal l.second_operation_id operation_id)
      |> Or_error.of_option
           ~error:(Error.of_string "operation is not a confirmed transfer")
    in
    printf
      "Transfer: operation #%Ld <-> #%Ld\n"
      link.first_operation_id
      link.second_operation_id;
    let%bind confirmed = confirm ~yes ~destructive:false "Undo transfer?" in
    if not confirmed
    then (
      print_endline "Nothing changed.";
      Ok ())
    else (
      let%map () = Operations.undo_general_transfer db operation_id in
      print_endline "transfer_changes=1"))
;;

let transfer_unlink_cmd =
  command
    "unlink"
    "Undo a reviewed non-bank transfer link."
    Term.(
      const transfer_unlink
      $ database
      $ Arg.(required & pos 0 (some int64) None & info [] ~docv:"OPERATION-ID")
      $ Arg.(
          value & flag & info [ "yes" ] ~doc:"Undo without prompting after the preview."))
;;

let transfer_undo database first second yes =
  with_database database (fun db ->
    let%bind decisions = Restore.bank_transfer_decisions db in
    let first, second = Int64.min first second, Int64.max first second in
    let%bind previous =
      List.find decisions ~f:(fun (d : Bank_transfer.decision) ->
        Int64.equal d.first_event_id first
        && Int64.equal d.second_event_id second
        && Poly.equal d.state Bank_transfer.Confirmed)
      |> Or_error.of_option
           ~error:(Error.of_string "no confirmed transfer for these event IDs")
    in
    let%bind rows = Review.bank_review db in
    let pair =
      List.filter rows ~f:(fun r ->
        Int64.equal r.Review.event_id first || Int64.equal r.event_id second)
    in
    if List.length pair <> 2
    then Or_error.error_string "transfer movements are missing"
    else (
      print_endline
        "Undo this confirmed transfer? Both movements will return to unclassified.";
      Terminal.review pair;
      let%bind confirmed = confirm ~yes ~destructive:false "Undo this transfer?" in
      if not confirmed
      then (
        print_endline "Nothing changed.";
        Ok ())
      else (
        let%bind decision =
          Bank_transfer.create
            ~first_event_id:first
            ~second_event_id:second
            ~state:Rejected
            ~reason:"Undone by user"
        in
        let%map changed =
          Transfers.set_bank_transfers ~expected_previous:previous db [ decision ]
        in
        printf "transfer_changes=%d\n" changed)))
;;

let transfer_undo_cmd =
  command
    "undo"
    "Undo a confirmed transfer and return both movements to review."
    Term.(
      const transfer_undo
      $ database
      $ Arg.(
          required
          & pos 0 (some int64) None
          & info [] ~docv:"FIRST-EVENT-ID" ~doc:"First source event ID.")
      $ Arg.(
          required
          & pos 1 (some int64) None
          & info [] ~docv:"SECOND-EVENT-ID" ~doc:"Second source event ID.")
      $ Arg.(value & flag & info [ "yes" ] ~doc:"Undo without prompting."))
;;

let transfer_history database =
  with_database database (fun db ->
    let%bind rows = Transfers.bank_transfer_history db in
    Report_json.print (Jsont.list Report_json.transfer_change) rows;
    Ok ())
;;

let transfer_history_cmd =
  command
    "history"
    "Inspect the append-only transfer review history."
    Term.(const transfer_history $ database)
;;

let cmd =
  group
    "transfer"
    "Review transfer candidates and decisions."
    [ transfer_candidates_cmd
    ; review_transfers_cmd
    ; transfer_link_cmd
    ; transfer_unlink_cmd
    ; transfer_undo_cmd
    ; transfer_history_cmd
    ]
;;
