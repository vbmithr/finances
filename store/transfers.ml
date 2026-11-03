open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Setup
open Sources
open Ledger
open Review
open Restore

type reversal_candidate =
  { original : bank_review
  ; reversal : bank_review
  }

let reversal_candidates db =
  let* rows = bank_review db in
  let eligible r = Poly.equal r.classification Unclassified && Option.is_none r.reason in
  let originals =
    List.filter rows ~f:(fun r ->
      eligible r && Poly.equal r.movement_type (Some Movement_type.Withdrawal))
  in
  let reversals =
    List.filter rows ~f:(fun r ->
      eligible r
      && Poly.equal r.movement_type (Some Movement_type.Withdrawal)
      && Quantity.(r.transaction.amount > zero)
      && r.source_evidence.withdrawal_reversal)
  in
  Ok
    (List.concat_map reversals ~f:(fun reversal ->
       List.filter_map originals ~f:(fun original ->
         let a = original.transaction
         and b = reversal.transaction in
         Option.some_if
           (String.equal original.institution reversal.institution
            && Poly.equal a.account b.account
            && Quantity.(a.amount < zero && b.amount > zero && a.amount + b.amount = zero)
            && Quantity.is_zero a.fee
            && Quantity.is_zero b.fee
            && Date.compare a.booked_on b.booked_on <= 0)
           { original; reversal })))
;;

let reversal_change db ~original ~reversal ~undo =
  let original_id = original
  and reversal_id = reversal in
  transaction db (fun db ->
    let* source = events db in
    let get id =
      List.Assoc.find source id ~equal:Int64.equal
      |> Or_error.of_option ~error:(Error.of_string "unknown event ID")
    in
    let* a = get original in
    let* b = get reversal in
    let* entries =
      traverse
        (fun (id, e) ->
           let* entry = verify_bank_entry db id e in
           Ok (id, entry))
        [ original, a; reversal, b ]
    in
    let* () =
      if undo
      then
        let* existing = linked_reversal db original in
        if
          not
            (Option.exists existing ~f:(fun (x, y, _) ->
               Int64.equal x original && Int64.equal y reversal))
        then Or_error.error_string "no confirmed reversal for those event IDs"
        else (
          let delete =
            Rel_sql.Stmt.(
              func
                "DELETE FROM reversal_pairs WHERE original=? AND reversal=?"
                (int64 @-> int64 @-> unit))
          in
          db_result (Db.exec db.conn (delete original reversal)))
      else
        let* candidates = reversal_candidates db in
        if
          not
            (List.exists candidates ~f:(fun c ->
               Int64.equal c.original.event_id original
               && Int64.equal c.reversal.event_id reversal))
        then Or_error.error_string "pair is not an eligible unreviewed reversal candidate"
        else
          let open Db_schema.Reversal_pairs in
          let insert = Rel_sql.insert_into_cols Db.dialect table in
          db_result
            (Db.exec
               db.conn
               (insert
                  Col.
                    [ Value (Db_schema.Reversal_pairs.original, original_id)
                    ; Value (Db_schema.Reversal_pairs.reversal, reversal_id)
                    ; Value (reason, "Confirmed withdrawal reversal")
                    ]))
    in
    let* _ =
      traverse
        (fun (id, entry) ->
           match entry with
           | None -> Ok ()
           | Some entry_id ->
             let* account = counter_account db id in
             db_result
               (Db.exec
                  db.conn
                  (update_posting
                     (Option.value account ~default:"equity:unclassified")
                     entry_id
                     1)))
        entries
    in
    let open Db_schema.Reversal_changes in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    db_result
      (Db.exec
         db.conn
         (insert
            Col.
              [ Value (Db_schema.Reversal_changes.original, original_id)
              ; Value (Db_schema.Reversal_changes.reversal, reversal_id)
              ; Value (action, if undo then "undone" else "confirmed")
              ])))
;;

type reversal_action =
  | Reversal_confirmed
  | Reversal_undone

type reversal_history_entry =
  { id : int64
  ; original : int64
  ; reversal : int64
  ; action : reversal_action
  ; changed_at : Ptime.t
  }

let reversal_history db =
  let* rows =
    collect
      db
      (statement
         "SELECT id,original,reversal,action,changed_at FROM reversal_changes ORDER BY id"
         Db_schema.Reversal_changes.row)
  in
  traverse
    (fun (row : Db_schema.Reversal_changes.t) ->
       let* action =
         match row.action with
         | "confirmed" -> Ok Reversal_confirmed
         | "undone" -> Ok Reversal_undone
         | _ -> Or_error.error_string "unknown reversal history action"
       in
       Ok
         ({ id = row.id
          ; original = row.original
          ; reversal = row.reversal
          ; action
          ; changed_at = row.changed_at
          }
          : reversal_history_entry))
    rows
;;

let sync_bank_transfer_link db ~first ~second ~confirming ~was_confirmed ~reason =
  if not (confirming || was_confirmed)
  then Ok ()
  else
    let* first_id = bank_primary_movement_id db first in
    let* second_id = bank_primary_movement_id db second in
    let upsert =
      Rel_sql.Stmt.(
        func
          "INSERT INTO \
           operation_transfer_links(first_movement_id,second_movement_id,reason) VALUES \
           (?,?,?) ON CONFLICT(first_movement_id,second_movement_id) DO UPDATE SET \
           reason=excluded.reason"
          (int64 @-> int64 @-> text @-> unit))
    in
    let member =
      Rel_sql.Stmt.(
        func
          "INSERT INTO \
           operation_transfer_memberships(movement_id,first_movement_id,second_movement_id) \
           VALUES (?,?,?) ON CONFLICT(movement_id) DO NOTHING"
          (int64 @-> int64 @-> int64 @-> unit))
    in
    let check_member =
      Rel_sql.Stmt.(
        func
          "SELECT EXISTS(SELECT 1 FROM operation_transfer_memberships WHERE \
           movement_id=? AND first_movement_id=? AND second_movement_id=?)"
          (int64 @-> int64 @-> int64 @-> ret Row.(t1 (bool "present"))))
    in
    let remove_members =
      Rel_sql.Stmt.(
        func
          "DELETE FROM operation_transfer_memberships WHERE first_movement_id=? AND \
           second_movement_id=?"
          (int64 @-> int64 @-> unit))
    in
    let remove_link =
      Rel_sql.Stmt.(
        func
          "DELETE FROM operation_transfer_links WHERE first_movement_id=? AND \
           second_movement_id=?"
          (int64 @-> int64 @-> unit))
    in
    let audit =
      Rel_sql.Stmt.(
        func
          "INSERT INTO \
           operation_transfer_changes(first_movement_id,second_movement_id,action) \
           VALUES (?,?,?)"
          (int64 @-> int64 @-> text @-> unit))
    in
    if confirming
    then
      let* () = db_result (Db.exec db.conn (upsert first_id second_id reason)) in
      let* () = db_result (Db.exec db.conn (member first_id first_id second_id)) in
      let* () = db_result (Db.exec db.conn (member second_id first_id second_id)) in
      let* first_ok =
        db_result (Db.first db.conn (check_member first_id first_id second_id))
      in
      let* second_ok =
        db_result (Db.first db.conn (check_member second_id first_id second_id))
      in
      if Option.value first_ok ~default:false && Option.value second_ok ~default:false
      then db_result (Db.exec db.conn (audit first_id second_id "confirmed"))
      else
        Or_error.error_string "a movement already belongs to another operation transfer"
    else
      let* () = db_result (Db.exec db.conn (remove_members first_id second_id)) in
      let* () = db_result (Db.exec db.conn (remove_link first_id second_id)) in
      db_result (Db.exec db.conn (audit first_id second_id "undone"))
;;

let set_bank_transfers_internal
      ?(require_unreviewed = false)
      ?expected_previous
      ?(in_transaction = false)
      db
      decisions
  =
  let* decisions =
    traverse
      (fun (d : Bank_transfer.decision) ->
         Bank_transfer.create
           ~first_event_id:d.first_event_id
           ~second_event_id:d.second_event_id
           ~state:d.state
           ~reason:d.reason)
      decisions
  in
  if
    List.contains_dup
      (List.map decisions ~f:(fun d -> d.Bank_transfer.first_event_id, d.second_event_id))
      ~compare:[%compare: int64 * int64]
  then Or_error.error_string "duplicate transfer pair in batch"
  else
    (if in_transaction then fun f -> f db else transaction db) (fun db ->
      let* source = events db in
      let get id =
        Or_error.of_option
          (List.Assoc.find source id ~equal:Int64.equal)
          ~error:(Error.of_string "unknown transfer event_id")
      in
      let upsert =
        Rel_sql.Stmt.(
          func
            "INSERT INTO \
             bank_transfer_decisions(first_event_id,second_event_id,state,reason) VALUES \
             (?,?,?,?) ON CONFLICT(first_event_id,second_event_id) DO UPDATE SET \
             state=excluded.state,reason=excluded.reason"
            (int64 @-> int64 @-> text @-> text @-> unit))
      in
      let add =
        Rel_sql.Stmt.(
          func
            "INSERT INTO \
             bank_transfer_memberships(event_id,first_event_id,second_event_id) VALUES \
             (?,?,?) ON CONFLICT(event_id) DO NOTHING"
            (int64 @-> int64 @-> int64 @-> unit))
      in
      let remove =
        let open Db_schema.Bank_transfer_memberships in
        fun v_first_event_id v_second_event_id ->
          Rel_sql.delete_from
            Db.dialect
            table
            ~where:
              Col.
                [ Value (first_event_id, v_first_event_id)
                ; Value (second_event_id, v_second_event_id)
                ]
      in
      let update = fun account entry_id -> update_posting account entry_id 1 in
      let audit =
        let open Db_schema.Bank_transfer_changes in
        let insert = Rel_sql.insert_into_cols Db.dialect table in
        fun v_first_event_id v_second_event_id v_previous_state v_state v_reason ->
          insert
            Col.
              [ Value (first_event_id, v_first_event_id)
              ; Value (second_event_id, v_second_event_id)
              ; Value (previous_state, v_previous_state)
              ; Value (state, v_state)
              ; Value (reason, v_reason)
              ]
      in
      let rec apply changed = function
        | [] -> Ok changed
        | (d : Bank_transfer.decision) :: rest ->
          let first = d.first_event_id
          and second = d.second_event_id in
          let* a = get first in
          let* b = get second in
          let* ta = movement db first in
          let* tb = movement db second in
          let* previous =
            db_result (Db.first db.conn (find_transfer_decision first second))
          in
          let* () =
            match expected_previous with
            | None -> Ok ()
            | Some (expected : Bank_transfer.decision) ->
              if
                Int64.equal first expected.first_event_id
                && Int64.equal second expected.second_event_id
                && Option.exists previous ~f:(fun (_, _, state, reason) ->
                  String.equal state (Bank_transfer.state_name expected.state)
                  && String.equal reason expected.reason)
              then Ok ()
              else
                Or_error.error_string
                  "transfer decision changed since preview; review it again"
          in
          let* () =
            if require_unreviewed && Option.is_some previous
            then
              Or_error.error_string
                "transfer decision changed since review; review the batch again"
            else Ok ()
          in
          let state = Bank_transfer.state_name d.state in
          if
            Option.exists previous ~f:(fun (_, _, old, reason) ->
              String.equal old state && String.equal reason d.reason)
          then apply changed rest
          else (
            let confirming = Poly.equal d.state Bank_transfer.Confirmed in
            let was_confirmed =
              Option.exists previous ~f:(fun (_, _, state, _) ->
                String.equal state "confirmed")
            in
            let* iban_compatible, _, _ = transfer_iban_evidence db source a ta b tb in
            let* () =
              if confirming && not iban_compatible
              then
                Or_error.error_string
                  "counterparty IBAN contradicts this transfer pairing"
              else Ok ()
            in
            let* () =
              if confirming && not (Bank_transfer.eligible a ta b tb)
              then
                Or_error.error_string
                  "transfer requires opposite principal amounts, same currency, distinct \
                   accounts and dates within seven days"
              else Ok ()
            in
            let* _ =
              traverse
                (fun id ->
                   let* existing = linked_transfer db id in
                   let* category = category_account db id in
                   let* settlement = card_settlement db id in
                   let* reversal = linked_reversal db id in
                   if
                     confirming
                     && Option.exists existing ~f:(fun (x, y, _) ->
                       not (Int64.equal x first && Int64.equal y second))
                   then
                     Or_error.error_string
                       "a movement already belongs to another confirmed transfer"
                   else if confirming && Option.is_some settlement
                   then
                     Or_error.error_string
                       "a movement is already a confirmed card settlement"
                   else if confirming && Option.is_some reversal
                   then
                     Or_error.error_string
                       "a movement is already part of a confirmed reversal"
                   else if
                     confirming
                     && Option.exists category ~f:(fun name ->
                       not (String.equal name "equity:unclassified"))
                   then
                     Or_error.error_string
                       "return reviewed categories to equity:unclassified before \
                        confirming a transfer"
                   else Ok ())
                [ first; second ]
            in
            let* entries =
              if confirming || was_confirmed
              then
                traverse
                  (fun (id, event) ->
                     let* entry = verify_bank_entry db id event in
                     Ok (id, entry))
                  [ first, a; second, b ]
              else Ok []
            in
            let* () = db_result (Db.exec db.conn (upsert first second state d.reason)) in
            let* () =
              sync_bank_transfer_link
                db
                ~first
                ~second
                ~confirming
                ~was_confirmed
                ~reason:d.reason
            in
            let* () =
              if confirming
              then
                let* _ =
                  traverse
                    (fun id -> db_result (Db.exec db.conn (add id first second)))
                    [ first; second ]
                in
                Ok ()
              else db_result (Db.exec db.conn (remove first second))
            in
            let* _ =
              traverse
                (fun (id, entry) ->
                   match entry with
                   | None -> Ok ()
                   | Some entry_id ->
                     let* account = counter_account db id in
                     db_result
                       (Db.exec
                          db.conn
                          (update
                             (Option.value account ~default:"equity:unclassified")
                             entry_id)))
                entries
            in
            let* () =
              db_result
                (Db.exec
                   db.conn
                   (audit
                      first
                      second
                      (Option.map previous ~f:(fun (_, _, state, _) -> state))
                      state
                      d.reason))
            in
            apply (changed + 1) rest)
      in
      apply 0 decisions)
;;

let set_bank_transfers ?require_unreviewed ?expected_previous db decisions =
  set_bank_transfers_internal ?require_unreviewed ?expected_previous db decisions
;;

type transfer_change =
  { id : int64
  ; first_event_id : int64
  ; second_event_id : int64
  ; previous_state : string option
  ; state : Bank_transfer.state
  ; reason : string
  ; changed_at : Ptime.t
  }

let bank_transfer_history db =
  let row =
    Row.(
      unit (fun id first second previous state reason at ->
        id, first, second, previous, state, reason, at)
      * read_column Db_schema.Bank_transfer_changes.id
      * read_column Db_schema.Bank_transfer_changes.first_event_id
      * read_column Db_schema.Bank_transfer_changes.second_event_id
      * read_column Db_schema.Bank_transfer_changes.previous_state
      * read_column Db_schema.Bank_transfer_changes.state
      * read_column Db_schema.Bank_transfer_changes.reason
      * read_column Db_schema.Bank_transfer_changes.changed_at)
  in
  let* rows =
    collect
      db
      (statement
         "SELECT \
          id,first_event_id,second_event_id,previous_state,state,reason,changed_at FROM \
          bank_transfer_changes ORDER BY id"
         row)
  in
  traverse
    (fun (id, first_event_id, second_event_id, previous_state, state, reason, changed_at) ->
       let* state = Bank_transfer.state_of_string state in
       Ok
         { id
         ; first_event_id
         ; second_event_id
         ; previous_state
         ; state
         ; reason
         ; changed_at
         })
    rows
;;

type transfer_candidate =
  { first_event_id : int64
  ; second_event_id : int64
  ; first : Source_event.t
  ; second : Source_event.t
  ; first_transaction : Movement.t
  ; second_transaction : Movement.t
  ; days_apart : int
  ; state : Bank_transfer.state option
  ; reason : string option
  ; unique : bool
  ; iban_compatible : bool
  ; iban_match : bool
  ; iban_evidence : string list
  }

(* Movement pairs that may be one transfer between owned accounts: exactly opposite
   principals in one currency, on different accounts, booked at most seven days
   apart. Bank_transfer.eligible states the same rule for confirmations. Pairs come
   in the order of a scan over events in their listing order: later first
   movements first, then their counterparts in order. *)
let transfer_pairs =
  statement
    "WITH ordered AS (SELECT id, row_number() OVER (ORDER BY coalesce(occurred_on, \
     substr(occurred_at, 1, 10)), occurred_at, id) AS position FROM source_events) \
     SELECT a.event_id, b.event_id FROM movements a JOIN movements b ON b.currency = \
     a.currency AND b.principal_magnitude = a.principal_magnitude AND b.principal_sign \
     <> a.principal_sign JOIN ordered oa ON oa.id = a.event_id JOIN ordered ob ON ob.id \
     = b.event_id WHERE oa.position < ob.position AND a.principal_magnitude <> '0' AND \
     NOT EXISTS (SELECT 1 FROM card_settlement_decisions s WHERE s.event_id IN \
     (a.event_id,b.event_id)) AND NOT (a.venue = b.venue AND a.account = b.account) AND \
     NOT EXISTS (SELECT 1 FROM reversal_pairs r WHERE r.original IN \
     (a.event_id,b.event_id) OR r.reversal IN (a.event_id,b.event_id)) AND \
     abs(julianday(a.booked_on) - julianday(b.booked_on)) <= 7 ORDER BY oa.position \
     DESC, ob.position"
    Row.(
      t2
        (Col.make "first" Type.int64 Col.no_proj)
        (Col.make "second" Type.int64 Col.no_proj))
;;

let transfer_candidates_from
      ?pairs
      ?(include_reviewed = false)
      db
      source
      table
      categories
      transfers
  =
  let* known = known_account_ibans db source ~movement:(stored table) in
  let* items =
    traverse
      (fun (id, e) ->
         let* tx = stored table id in
         Ok (id, (id, e, tx, Hashtbl.mem transfers id)))
      (List.filter source ~f:(fun (id, _) -> Hashtbl.mem table id))
  in
  let items = Hashtbl.of_alist_exn (module Int64) items in
  let* decisions = bank_transfer_decisions db in
  let indexed = Hashtbl.Poly.create () in
  List.iter decisions ~f:(fun d ->
    Hashtbl.set indexed ~key:(d.Bank_transfer.first_event_id, d.second_event_id) ~data:d);
  let candidate (id, a, ta, la) (other, b, tb, lb) =
    let id, a, ta, other, b, tb =
      if Int64.(id < other) then id, a, ta, other, b, tb else other, b, tb, id, a, ta
    in
    let decision = Hashtbl.find indexed (id, other) in
    let categorized event_id =
      Option.exists (Hashtbl.find categories event_id) ~f:(fun (account, _) ->
        not (String.equal account "equity:unclassified"))
    in
    let unavailable = categorized id || categorized other in
    if unavailable && ((not include_reviewed) || Option.is_none decision)
    then Ok None
    else
      let* a_iban, _ = account_iban db a ta in
      let* b_iban, _ = account_iban db b tb in
      let* iban_compatible, iban_match, iban_evidence =
        transfer_iban_evidence_with known ~a_iban ~b_iban a ta b tb
      in
      if
        (not include_reviewed)
        && (la || lb || Option.is_some decision || not iban_compatible)
      then Ok None
      else
        Ok
          (Some
             { first_event_id = id
             ; second_event_id = other
             ; first = a
             ; second = b
             ; first_transaction = ta
             ; second_transaction = tb
             ; days_apart = Int.abs (Date.diff ta.booked_on tb.booked_on)
             ; state = Option.map decision ~f:(fun d -> d.state)
             ; reason = Option.map decision ~f:(fun d -> d.reason)
             ; unique = false
             ; iban_compatible
             ; iban_match
             ; iban_evidence
             })
  in
  let* pairs =
    match pairs with
    | Some pairs -> Ok pairs
    | None -> collect db transfer_pairs
  in
  let* found =
    traverse
      (fun (first, second) ->
         let* x =
           Or_error.of_option
             (Hashtbl.find items first)
             ~error:(Error.of_string "missing transfer movement")
         in
         let* y =
           Or_error.of_option
             (Hashtbl.find items second)
             ~error:(Error.of_string "missing transfer movement")
         in
         candidate x y)
      pairs
  in
  let candidates = List.filter_opt found in
  let claims = Hashtbl.create (module Int64) in
  List.iter candidates ~f:(fun c ->
    Hashtbl.incr claims c.first_event_id;
    Hashtbl.incr claims c.second_event_id);
  Ok
    (List.map candidates ~f:(fun c ->
       { c with
         unique =
           Hashtbl.find_exn claims c.first_event_id = 1
           && Hashtbl.find_exn claims c.second_event_id = 1
       })
     |> List.sort ~compare:(fun a b ->
       [%compare: int64 * int64]
         (a.first_event_id, a.second_event_id)
         (b.first_event_id, b.second_event_id)))
;;

let transfer_candidates ?include_reviewed db =
  let* source = movement_source db in
  let* table = movement_table db in
  let* categories, transfers, _, _ = review_metadata db in
  transfer_candidates_from ?include_reviewed db source table categories transfers
;;

let find_settlement =
  let open Db_schema.Bank_account_settlements in
  select_by_pair table institution source_account settlement_account
;;

type bank_account_info =
  { mapping : Bank_account_mapping.t
  ; iban : Iban.t option
  ; iban_source : string
  ; currency : Iso4217.t
  ; movements : int
  ; first_on : Date.t
  ; last_on : Date.t
  ; settlement_account : Bank_account_config.reference option
  }

let bank_account_info_from ~summaries db source table =
  let* mappings = bank_accounts_from db source ~movement:(stored table) in
  traverse
    (fun mapping ->
       let source_account = Bank_account_mapping.source_account mapping in
       let members =
         List.filter source ~f:(fun (_, e) ->
           Registry.is_movement db.registry e
           && String.equal e.Source_event.venue mapping.Bank_account_mapping.institution
           && String.equal e.account source_account)
       in
       let event_id, event = List.hd_exn members in
       let* tx = stored table event_id in
       let* iban, iban_source = account_iban db event tx in
       let* settlement =
         db_result (Db.first db.conn (find_settlement mapping.institution source_account))
       in
       let* settlement_account =
         match settlement with
         | None -> Ok None
         | Some text ->
           let* reference = Json.decode Bank_account_config.reference_jsont text in
           Ok (Some reference)
       in
       let* count, first_on, last_on =
         Or_error.of_option
           (Hashtbl.Poly.find summaries (mapping.institution, source_account))
           ~error:(Error.of_string "missing account summary")
       in
       Ok
         { mapping
         ; iban
         ; iban_source
         ; currency = tx.account.currency
         ; movements = count
         ; first_on
         ; last_on
         ; settlement_account
         })
    mappings
;;

(* Aggregate separately by event kind so an unregistered adapter's rows do not
   enter status counts. The registry check below uses a representative event. *)
let status_summaries db source =
  let recognized = Hash_set.create (module Int64) in
  List.iter source ~f:(fun (id, _) -> Hash_set.add recognized id);
  let* rows =
    collect
      db
      (statement
         "SELECT \
          min(m.event_id),m.venue,m.account,count(*),min(m.booked_on),max(m.booked_on),sum(CASE \
          WHEN t.event_id IS NULL AND s.event_id IS NULL AND r.original IS NULL AND \
          coalesce(c.account,'equity:unclassified')='equity:unclassified' THEN 1 ELSE 0 \
          END) FROM movements m JOIN source_events e ON e.id=m.event_id LEFT JOIN \
          bank_categories c ON c.event_id=m.event_id LEFT JOIN (SELECT \
          om.anchor_event_id AS event_id FROM operation_movements om JOIN \
          operation_transfer_memberships ot ON ot.movement_id=om.id) t ON \
          t.event_id=m.event_id LEFT JOIN card_settlement_decisions s ON \
          s.event_id=m.event_id LEFT JOIN reversal_pairs r ON r.original=m.event_id OR \
          r.reversal=m.event_id GROUP BY m.venue,m.account,e.kind"
         Row.(
           unit (fun id venue account count first last unclassified ->
             id, venue, account, count, first, last, unclassified)
           * int64 "event_id"
           * text "venue"
           * text "account"
           * int "count"
           * text "first"
           * text "last"
           * int "unclassified"))
  in
  let summaries = Hashtbl.Poly.create () in
  let* counts =
    traverse
      (fun (id, venue, account, count, first, last, unclassified) ->
         if not (Hash_set.mem recognized id)
         then Ok (0, 0)
         else
           let* first = Or_error.try_with (fun () -> Date.of_string first) in
           let* last = Or_error.try_with (fun () -> Date.of_string last) in
           Hashtbl.update summaries (venue, account) ~f:(function
             | None -> count, first, last
             | Some (n, a, b) ->
               ( n + count
               , (if Date.compare a first < 0 then a else first)
               , if Date.compare b last > 0 then b else last ));
           Ok (count, unclassified))
      rows
  in
  Ok (summaries, List.sum (module Int) counts ~f:fst, List.sum (module Int) counts ~f:snd)
;;

(* Include both the chronological first row used for account inspection and
   the representative chosen by account discovery and IBAN evidence. *)
let account_representatives source =
  let needed = Hash_set.create (module Int64) in
  let accounts = Hashtbl.Poly.create () in
  List.iter source ~f:(fun (id, e) ->
    Hashtbl.add accounts ~key:(e.Source_event.venue, e.account) ~data:id |> ignore);
  Hashtbl.iter accounts ~f:(Hash_set.add needed);
  List.dedup_and_sort source ~compare:(fun (_, a) (_, b) ->
    [%compare: string * string]
      (a.Source_event.venue, a.account)
      (b.Source_event.venue, b.account))
  |> List.iter ~f:(fun (id, _) -> Hash_set.add needed id);
  needed
;;

let selected_movements db needed =
  (* Only database-generated int64 IDs are rendered here, never input text. *)
  let ids =
    Hash_set.to_list needed |> List.map ~f:Int64.to_string |> String.concat ~sep:","
  in
  let* rows = movement_rows db (" WHERE event_id IN (" ^ ids ^ ")") in
  Ok (Hashtbl.of_alist_exn (module Int64) rows)
;;

let bank_account_info db =
  let* source = movement_source db in
  let* summaries, _, _ = status_summaries db source in
  let* table = selected_movements db (account_representatives source) in
  bank_account_info_from ~summaries db source table
;;

let card_settlement_preview ?card_id db event_id =
  let* reviews = bank_review db in
  let* row =
    List.find reviews ~f:(fun r -> Int64.equal r.event_id event_id)
    |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
  in
  let* () =
    if
      (not (String.equal row.counter_account "equity:unclassified"))
      || Option.is_some row.reason
    then Or_error.error_string "transaction is already reviewed"
    else if
      Quantity.(row.transaction.amount >= zero)
      || not (Quantity.is_zero row.transaction.fee)
    then Or_error.error_string "settlement requires a fee-free outgoing bank movement"
    else Ok ()
  in
  let* accounts = bank_account_info db in
  let account = row.transaction.account in
  let* reference =
    Bank_account_config.reference
      ~institution:row.institution
      ~bank_id:account.bank_id
      ~account_id:account.account_id
      ~account_type:account.account_type
  in
  let candidates =
    List.filter accounts ~f:(fun card ->
      List.mem
        [ "CREDITCRD"; "credit-card" ]
        card.mapping.account_type
        ~equal:String.equal
      && Option.value_map card.settlement_account ~default:false ~f:(fun r ->
        String.equal r.institution reference.institution
        && String.equal
             (Bank_account_config.source_account r)
             (Bank_account_config.source_account reference))
      && Iso4217.equal card.currency account.currency
      && Option.value_map card_id ~default:true ~f:(fun id ->
        String.equal id (Bank_account_mapping.id card.mapping)))
  in
  let* card =
    match candidates with
    | [ card ] -> Ok card
    | [] ->
      Or_error.error_string
        "no linked card matches this transaction; configure the card settlement account \
         with fin account edit"
    | _ -> Or_error.error_string "multiple linked cards; select one with --card ID"
  in
  if not (String.is_prefix card.mapping.ledger_account ~prefix:"liabilities:")
  then
    Or_error.error_string
      "map the card to a liabilities account with fin account edit first"
  else
    let* source_account =
      List.find accounts ~f:(fun a ->
        String.equal a.mapping.institution reference.institution
        && String.equal
             (Bank_account_mapping.source_account a.mapping)
             (Bank_account_config.source_account reference))
      |> Or_error.of_option ~error:(Error.of_string "missing settlement bank account")
    in
    if String.equal source_account.mapping.ledger_account card.mapping.ledger_account
    then Or_error.error_string "the bank and card must use different ledger accounts"
    else Ok (row, card)
;;

let confirm_card_settlement_in_transaction
      ?card_id
      ?expected_ledger
      ?(reason = "Confirmed card settlement")
      ?validated_card
      ?source_event
      db
      event_id
  =
  let* card =
    match validated_card with
    | Some card -> Ok card
    | None ->
      let* _, card = card_settlement_preview ?card_id db event_id in
      Ok card
  in
  let* () =
    if
      Option.value_map
        expected_ledger
        ~default:true
        ~f:(String.equal card.mapping.ledger_account)
    then Ok ()
    else Or_error.error_string "card account changed since the preview; review it again"
  in
  let* event =
    match source_event with
    | Some event -> Ok event
    | None ->
      let* source = events db in
      List.Assoc.find source event_id ~equal:Int64.equal
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
  in
  let* existing = verify_bank_entry db event_id event in
  let* reason_value = Validation.text "settlement reason" reason in
  let id_value = event_id in
  let open Db_schema.Card_settlement_decisions in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  let* () =
    db_result
      (Db.exec
         db.conn
         (insert
            Col.
              [ Value (event_id, id_value)
              ; Value (card_institution, card.mapping.institution)
              ; Value
                  (card_source_account, Bank_account_mapping.source_account card.mapping)
              ; Value (ledger_account, card.mapping.ledger_account)
              ; Value (reason, reason_value)
              ]))
  in
  let* () =
    match existing with
    | None -> Ok ()
    | Some entry_id ->
      db_result (Db.exec db.conn (update_posting card.mapping.ledger_account entry_id 1))
  in
  let open Db_schema.Card_settlement_changes in
  let audit = Rel_sql.insert_into_cols Db.dialect table in
  db_result
    (Db.exec
       db.conn
       (audit
          Col.
            [ Value (event_id, id_value)
            ; Value (card_institution, card.mapping.institution)
            ; Value (card_source_account, Bank_account_mapping.source_account card.mapping)
            ; Value (ledger_account, card.mapping.ledger_account)
            ; Value (action, "confirmed")
            ]))
;;

let confirm_card_settlement ?card_id ?expected_ledger db event_id =
  transaction db (fun db ->
    confirm_card_settlement_in_transaction ?card_id ?expected_ledger db event_id)
;;

let undo_card_settlement db event_id =
  transaction db (fun db ->
    let open Rel_sql.Stmt in
    let query =
      func
        "SELECT card_institution,card_source_account,ledger_account FROM \
         card_settlement_decisions WHERE event_id=?"
        (int64
         @-> ret
               Row.(
                 t3
                   (text "card_institution")
                   (text "card_source_account")
                   (text "ledger_account")))
    in
    let* info = db_result (Db.first db.conn (query event_id)) in
    let* institution, source_account, account =
      Or_error.of_option
        info
        ~error:(Error.of_string "no confirmed settlement for this transaction")
    in
    let* source = events db in
    let* event =
      List.Assoc.find source event_id ~equal:Int64.equal
      |> Or_error.of_option ~error:(Error.of_string "unknown transaction")
    in
    let* existing = verify_bank_entry db event_id event in
    let delete =
      func "DELETE FROM card_settlement_decisions WHERE event_id=?" (int64 @-> unit)
    in
    let* () = db_result (Db.exec db.conn (delete event_id)) in
    let* category = category_account db event_id in
    let* () =
      match existing with
      | None -> Ok ()
      | Some entry_id ->
        db_result
          (Db.exec
             db.conn
             (update_posting
                (Option.value category ~default:"equity:unclassified")
                entry_id
                1))
    in
    let id_value = event_id in
    let open Db_schema.Card_settlement_changes in
    let audit = Rel_sql.insert_into_cols Db.dialect table in
    db_result
      (Db.exec
         db.conn
         (audit
            Col.
              [ Value (event_id, id_value)
              ; Value (card_institution, institution)
              ; Value (card_source_account, source_account)
              ; Value (ledger_account, account)
              ; Value (action, "undone")
              ])))
;;

type card_settlement_action =
  | Confirmed
  | Undone

type card_settlement_change =
  { id : int64
  ; event_id : int64
  ; card_institution : string
  ; card_source_account : string
  ; ledger_account : string
  ; action : card_settlement_action
  ; changed_at : Ptime.t
  }

let card_settlement_history db =
  let open Db_schema.Card_settlement_changes in
  let row =
    Row.(
      unit
        (fun
            id
             event_id
             card_institution
             card_source_account
             ledger_account
             action
             changed_at
           ->
           ( id
           , event_id
           , card_institution
           , card_source_account
           , ledger_account
           , action
           , changed_at ))
      * read_column id
      * read_column event_id
      * read_column card_institution
      * read_column card_source_account
      * read_column ledger_account
      * read_column action
      * read_column changed_at)
  in
  let* rows =
    collect
      db
      (statement
         "SELECT \
          id,event_id,card_institution,card_source_account,ledger_account,action,changed_at \
          FROM card_settlement_changes ORDER BY id"
         row)
  in
  traverse
    (fun ( id
         , event_id
         , card_institution
         , card_source_account
         , ledger_account
         , action
         , changed_at ) ->
       let* action =
         match action with
         | "confirmed" -> Ok Confirmed
         | "undone" -> Ok Undone
         | _ -> Or_error.error_string "unknown card settlement action"
       in
       Ok
         ({ id
          ; event_id
          ; card_institution
          ; card_source_account
          ; ledger_account
          ; action
          ; changed_at
          }
          : card_settlement_change))
    rows
;;
