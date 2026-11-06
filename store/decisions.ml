open Base
open Rel
open Handle
open Ledger
open Review
open Restore

let movement_reference (e : Source_event.t) : Category_portable.decision =
  { institution = e.venue
  ; source_account = e.account
  ; kind = e.kind
  ; source_id = e.source_id
  ; occurred = Temporal.to_string e.occurred
  ; payload_sha256 = payload_hash e
  ; fields_sha256 = category_fields_hash e
  ; movement_role = "primary"
  ; account = None
  ; reason = None
  ; type_override = None
  }
;;

let export db =
  transaction db (fun db ->
    let* accounts = Accounts.export_bank_accounts db in
    let* categories = Setup.categories db in
    let* rules = Import.classification_rules db in
    let* assignments = category_snapshot db in
    let* exchanges = Operations.exchange_snapshot db in
    let* transfers = Operations.transfer_snapshot db in
    let* source = events db in
    let by_id = Hashtbl.of_alist_exn (module Int64) source in
    let reference id =
      Hashtbl.find by_id id
      |> Or_error.of_option ~error:(Error.createf "decision on unknown event %Ld" id)
      |> Or_error.map ~f:movement_reference
    in
    let* settled =
      collect
        db
        (statement
           "SELECT event_id,card_institution,card_source_account,ledger_account,reason \
            FROM card_settlement_decisions ORDER BY event_id"
           Row.(
             t5
               (int64 "event_id")
               (text "card_institution")
               (text "card_source_account")
               (text "ledger_account")
               (text "reason")))
    in
    let* settlements =
      traverse
        (fun (id, card_institution, card_source_account, ledger_account, reason) ->
           let* source = reference id in
           Ok
             ({ source; card_institution; card_source_account; ledger_account; reason }
              : Decisions_portable.settlement))
        settled
    in
    let* pairs =
      collect
        db
        (statement
           "SELECT original,reversal,reason FROM reversal_pairs ORDER BY original"
           Db_schema.Reversal_pairs.row)
    in
    let* reversals =
      traverse
        (fun (r : Db_schema.Reversal_pairs.t) ->
           let* original = reference r.original in
           let* reversal = reference r.reversal in
           Ok ({ original; reversal } : Decisions_portable.reversal))
        pairs
    in
    let* cards = Cards.assignments db in
    Ok
      ({ accounts = Some accounts
       ; cards =
           Some
             (List.map cards ~f:(fun (institution, card_reference, card_source_account) ->
                ({ institution; card_reference; card_source_account }
                 : Decisions_portable.card)))
       ; categories = Some categories
       ; rules = Some rules
       ; assignments = Some { assignments with categories = []; rules = [] }
       ; settlements = Some settlements
       ; reversals = Some reversals
       ; exchanges = Some exchanges
       ; transfers = Some transfers
       }
       : Decisions_portable.t))
;;

type changes =
  { accounts : int
  ; cards : int
  ; categories : int
  ; rules : int
  ; assignments : int
  ; types : int
  ; resolutions : int
  ; settlements : int
  ; reversals : int
  ; exchanges : int
  ; transfers : int
  }

type outcome =
  | Applied of changes
  | Declined of changes

let nothing_changed =
  { accounts = 0
  ; cards = 0
  ; categories = 0
  ; rules = 0
  ; assignments = 0
  ; types = 0
  ; resolutions = 0
  ; settlements = 0
  ; reversals = 0
  ; exchanges = 0
  ; transfers = 0
  }
;;

let blocked problems =
  Or_error.error_string
    ("Problems (restore is blocked):\n  " ^ String.concat ~sep:"\n  " problems)
;;

(* Finds the rows saved decisions name, by source identity. *)
let locate db =
  let* source = events db in
  let by_identity = Hashtbl.Poly.create () in
  List.iter source ~f:(fun (id, (e : Source_event.t)) ->
    Hashtbl.add_multi
      by_identity
      ~key:(e.venue, e.account, e.kind, e.source_id)
      ~data:(id, e));
  Ok
    (fun (r : Category_portable.decision) ->
      let label = r.institution ^ "/" ^ r.source_id in
      match
        Hashtbl.find by_identity (r.institution, r.source_account, r.kind, r.source_id)
      with
      | None | Some [] -> Error ("no imported movement for " ^ label)
      | Some [ (id, e) ] ->
        if String.equal (payload_hash e) r.payload_sha256
        then Ok (id, e)
        else Error ("source evidence changed for " ^ label)
      | Some _ -> Error ("several imported movements for " ^ label))
;;

let restore_settlements db settlements =
  if List.is_empty settlements
  then Ok 0
  else
    let* find = locate db in
    let* reviews = bank_review db in
    let* cards = Transfers.bank_account_info db in
    let* existing =
      collect
        db
        (statement
           "SELECT event_id FROM card_settlement_decisions"
           Row.(t1 (int64 "event_id")))
    in
    let existing = Set.of_list (module Int64) existing in
    let review_of =
      Hashtbl.of_alist_exn (module Int64) (List.map reviews ~f:(fun r -> r.event_id, r))
    in
    let planned =
      List.filter_map settlements ~f:(fun (s : Decisions_portable.settlement) ->
        match find s.source with
        | Error problem -> Some (Error problem)
        | Ok (id, _) when Set.mem existing id -> None
        | Ok (id, event) ->
          let label = s.source.institution ^ "/" ^ s.source.source_id in
          let card =
            List.find cards ~f:(fun (c : Transfers.bank_account_info) ->
              String.equal c.mapping.institution s.card_institution
              && String.equal
                   (Bank_account_mapping.source_account c.mapping)
                   s.card_source_account)
          in
          (match card, Hashtbl.find review_of id with
           | None, _ -> Some (Error ("no such card for settlement " ^ label))
           | _, None -> Some (Error ("settlement is not a bank movement: " ^ label))
           | Some card, Some row ->
             if not (String.equal card.mapping.ledger_account s.ledger_account)
             then Some (Error ("card account differs for settlement " ^ label))
             else if
               (not (String.equal row.counter_account "equity:unclassified"))
               || Option.is_some row.reason
             then Some (Error ("movement already reviewed for settlement " ^ label))
             else Some (Ok (s, id, event, card))))
    in
    let problems, planned =
      List.partition_map planned ~f:(function
        | Error problem -> First problem
        | Ok plan -> Second plan)
    in
    if not (List.is_empty problems)
    then blocked problems
    else
      let* _ =
        traverse
          (fun ((s : Decisions_portable.settlement), id, event, card) ->
             Transfers.confirm_card_settlement_in_transaction
               ~reason:s.reason
               ~validated_card:card
               ~source_event:event
               db
               id)
          planned
      in
      Ok (List.length planned)
;;

let restore_reversals db reversals =
  if List.is_empty reversals
  then Ok 0
  else
    let* find = locate db in
    let located =
      List.map reversals ~f:(fun (r : Decisions_portable.reversal) ->
        match find r.original, find r.reversal with
        | Ok (original, _), Ok (reversal, _) -> Ok (original, reversal)
        | Error problem, _ | _, Error problem -> Error problem)
    in
    let problems, pairs =
      List.partition_map located ~f:(function
        | Error problem -> First problem
        | Ok pair -> Second pair)
    in
    if not (List.is_empty problems)
    then blocked problems
    else
      let* changed =
        traverse
          (fun (original, reversal) ->
             let* existing = linked_reversal db original in
             match existing with
             | Some (x, y, _) when Int64.equal x original && Int64.equal y reversal ->
               Ok 0
             | _ ->
               let* () = Transfers.reversal_change db ~original ~reversal ~undo:false in
               Ok 1)
          pairs
      in
      Ok (List.sum (module Int) changed ~f:Fn.id)
;;

let restore db (snapshot : Decisions_portable.t) ~confirm =
  let declined = ref None in
  let result =
    transaction db (fun db ->
      let* accounts =
        match snapshot.accounts with
        | None -> Ok 0
        | Some config ->
          let* report = Accounts.apply_bank_accounts db config in
          Ok (report.mapping_changes + report.identity_changes + report.settlement_changes)
      in
      (* Before settlements: a settlement of a replaced card matches through its card. *)
      let* cards =
        let* restored =
          traverse
            (fun (c : Decisions_portable.card) ->
               Cards.restore_assignment
                 db
                 ~institution:c.institution
                 ~card_reference:c.card_reference
                 ~card_source_account:c.card_source_account)
            (Option.value snapshot.cards ~default:[])
        in
        Ok (List.count restored ~f:Fn.id)
      in
      let assignments =
        Option.value
          snapshot.assignments
          ~default:{ categories = []; rules = []; decisions = []; resolutions = [] }
      in
      let portable =
        { assignments with
          categories = Option.value snapshot.categories ~default:[]
        ; rules = Option.value snapshot.rules ~default:[]
        }
      in
      let* preview = preview_category_restore db portable in
      let* () =
        if List.is_empty preview.problems then Ok () else blocked preview.problems
      in
      let* assigned =
        if
          List.is_empty preview.categories_to_add
          && List.is_empty preview.rules_to_add
          && List.is_empty preview.decisions_to_apply
          && List.is_empty preview.types_to_apply
          && List.is_empty preview.resolutions_to_apply
        then Ok 0
        else restore_categories db ~expected:preview portable
      in
      let* settlements =
        restore_settlements db (Option.value snapshot.settlements ~default:[])
      in
      let* reversals =
        restore_reversals db (Option.value snapshot.reversals ~default:[])
      in
      let* exchanges =
        match snapshot.exchanges with
        | None -> Ok 0
        | Some exchanges ->
          let* preview = Operations.preview_exchange_restore db exchanges in
          if not (List.is_empty preview.exchange_problems)
          then blocked preview.exchange_problems
          else if List.is_empty preview.exchange_decisions
          then Ok 0
          else Operations.restore_exchanges db ~expected:preview exchanges
      in
      let* transfers =
        match snapshot.transfers with
        | None -> Ok 0
        | Some transfers ->
          let* preview = Operations.preview_transfer_restore db transfers in
          if not (List.is_empty preview.problems)
          then blocked preview.problems
          else if
            List.is_empty preview.decisions && List.is_empty preview.general_decisions
          then Ok 0
          else Operations.restore_transfers db ~expected:preview transfers
      in
      let changes =
        { accounts
        ; cards
        ; categories = List.length preview.categories_to_add
        ; rules = List.length preview.rules_to_add
        ; assignments = assigned
        ; types = List.length preview.types_to_apply
        ; resolutions = List.length preview.resolutions_to_apply
        ; settlements
        ; reversals
        ; exchanges
        ; transfers
        }
      in
      if Poly.equal changes nothing_changed
      then Ok changes
      else
        let* confirmed = confirm changes in
        if confirmed
        then Ok changes
        else (
          declined := Some changes;
          Or_error.error_string "declined"))
  in
  match result, !declined with
  | _, Some changes -> Ok (Declined changes)
  | Ok changes, None -> Ok (Applied changes)
  | (Error _ as error), None -> error
;;
