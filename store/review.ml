open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Setup
open Sources
open Import
open Ledger

type statement_reconciliation =
  { statement_id : int64
  ; document_id : int64
  ; institution : string
  ; source_account : string
  ; reference : string option
  ; currency : Iso4217.t
  ; period_start : Temporal.t
  ; period_end : Temporal.t
  ; opening : Quantity.t
  ; closing : Quantity.t
  ; movements : Quantity.t
  ; entries : int
  ; difference : Quantity.t
  }

let statement_reconciliations ?institution db =
  let row =
    Row.(
      unit
        (fun
            id
             document_id
             institution
             source_account
             reference
             currency
             period_start
             period_end
             opening
             closing
           ->
           ( id
           , document_id
           , institution
           , source_account
           , reference
           , currency
           , period_start
           , period_end
           , opening
           , closing ))
      * read_column Db_schema.Statements.id
      * read_column Db_schema.Statements.document_id
      * read_column Db_schema.Statements.institution
      * read_column Db_schema.Statements.source_account
      * read_column Db_schema.Statements.reference
      * read_column Db_schema.Statements.currency
      * read_column Db_schema.Statements.period_start
      * read_column Db_schema.Statements.period_end
      * read_column Db_schema.Statements.opening
      * read_column Db_schema.Statements.closing)
  in
  let* statements =
    collect
      db
      (statement
         "SELECT \
          id,document_id,institution,source_account,reference,currency,period_start,period_end,opening,closing \
          FROM statements ORDER BY id"
         row)
  in
  let statements =
    List.filter statements ~f:(fun (_, _, i, _, _, _, _, _, _, _) ->
      Option.for_all institution ~f:(String.equal i))
  in
  let* movements = movements db in
  let amounts = Hashtbl.create (module Int64) in
  List.iter movements ~f:(fun (id, _, (m : Movement.t)) ->
    Hashtbl.set amounts ~key:id ~data:m.amount);
  traverse
    (fun ( statement_id
         , document_id
         , institution
         , source_account
         , reference
         , currency
         , period_start
         , period_end
         , opening
         , closing ) ->
       let find =
         let open Db_schema.Statement_events in
         select_by_column table statement_id event_id
       in
       let* ids = collect db (find statement_id) in
       let* amounts =
         traverse
           (fun id ->
              Or_error.of_option
                (Hashtbl.find amounts id)
                ~error:(Error.of_string "missing statement movement"))
           ids
       in
       let* opening, closing =
         Or_error.try_with (fun () ->
           Quantity.of_string opening, Quantity.of_string closing)
       in
       let* currency = Validation.currency "stored statement currency" currency in
       let movements = List.fold amounts ~init:Quantity.zero ~f:Quantity.( + ) in
       Ok
         { statement_id
         ; document_id
         ; institution
         ; source_account
         ; reference
         ; currency
         ; period_start
         ; period_end
         ; opening
         ; closing
         ; movements
         ; entries = List.length ids
         ; difference = Quantity.(opening + movements - closing)
         })
    statements
;;

let set_bank_categories_in_transaction ?(only_unclassified = false) db decisions =
  let* decisions =
    traverse
      (fun (d : Bank_category.t) ->
         Bank_category.create ~event_id:d.event_id ~account:d.account ~reason:d.reason)
      decisions
  in
  if
    List.contains_dup
      (List.map decisions ~f:(fun d -> d.Bank_category.event_id))
      ~compare:Int64.compare
  then Or_error.error_string "duplicate event_id in category batch"
  else (
    let apply_batch db =
      let* source = events db in
      let rec apply changed = function
        | [] -> Ok changed
        | (d : Bank_category.t) :: rest ->
          let* event =
            Or_error.of_option
              (List.Assoc.find source d.event_id ~equal:Int64.equal)
              ~error:(Error.of_string "unknown category event_id")
          in
          let* transfer = linked_transfer db d.event_id in
          let linked_query =
            Rel_sql.Stmt.(
              func
                "SELECT EXISTS(SELECT 1 FROM operation_movements m JOIN \
                 operation_transfer_memberships t ON t.movement_id=m.id WHERE \
                 m.anchor_event_id=?)"
                (int64 @-> ret Row.(t1 (bool "linked"))))
          in
          let* operation_linked =
            db_result (Db.first db.conn (linked_query d.event_id))
          in
          let* settlement = card_settlement db d.event_id in
          let* reversal = linked_reversal db d.event_id in
          if Option.is_some reversal
          then
            Or_error.error_string
              "undo the confirmed reversal before changing its category"
          else if Option.is_some settlement
          then Or_error.error_string "transaction is a confirmed card settlement"
          else if Option.is_some transfer || Option.value operation_linked ~default:false
          then
            Or_error.error_string
              "reject the confirmed transfer before changing its category"
          else if not (Registry.is_movement db.registry event)
          then Or_error.error_string "category decision requires a bank event"
          else
            let* previous = db_result (Db.first db.conn (find_category d.event_id)) in
            if
              only_unclassified
              && Option.exists previous ~f:(fun (account, _) ->
                not (String.equal account "equity:unclassified"))
            then
              Or_error.error_string
                "a selected transaction is no longer unclassified; review it again"
            else if
              Option.equal [%equal: string * string] previous (Some (d.account, d.reason))
            then apply changed rest
            else
              let* movement_id = bank_primary_movement_id db d.event_id in
              let* existing =
                db_result (Db.first db.conn (find_entry d.event_id Bank_projection.key))
              in
              let* () =
                match existing with
                | None -> Ok ()
                | Some (id, date, description) ->
                  let* stored = read_entry db id date description in
                  let* expected = mapped_bank_entry db d.event_id event in
                  if not (same_entry stored expected)
                  then
                    Or_error.error_string
                      "category change conflicts with existing bank projection"
                  else (
                    let update =
                      fun account entry_id -> update_posting account entry_id 1
                    in
                    db_result (Db.exec db.conn (update d.account id)))
              in
              let upsert =
                Rel_sql.Stmt.(
                  func
                    "INSERT INTO bank_categories(movement_id,event_id,account,reason) \
                     VALUES (?,?,?,?) ON CONFLICT(event_id) DO UPDATE SET \
                     account=excluded.account,reason=excluded.reason"
                    (int64 @-> int64 @-> text @-> text @-> unit))
              in
              let audit =
                let open Db_schema.Bank_category_changes in
                let insert = Rel_sql.insert_into_cols Db.dialect table in
                fun v_movement_id v_event_id v_previous_account v_account v_reason ->
                  insert
                    Col.
                      [ Value (movement_id, v_movement_id)
                      ; Value (event_id, v_event_id)
                      ; Value (previous_account, v_previous_account)
                      ; Value (account, v_account)
                      ; Value (reason, v_reason)
                      ]
              in
              let catalog =
                Rel_sql.Stmt.(
                  func
                    "INSERT OR IGNORE INTO category_catalog(account) VALUES (?)"
                    (text @-> unit))
              in
              let* () =
                if String.equal d.account "equity:unclassified"
                then Ok ()
                else db_result (Db.exec db.conn (catalog d.account))
              in
              let* () =
                db_result
                  (Db.exec db.conn (upsert movement_id d.event_id d.account d.reason))
              in
              let* () =
                db_result
                  (Db.exec
                     db.conn
                     (audit
                        movement_id
                        d.event_id
                        (Option.map previous ~f:fst)
                        d.account
                        d.reason))
              in
              apply (changed + 1) rest
      in
      apply 0 decisions
    in
    apply_batch db)
;;

let set_bank_categories ?(only_unclassified = false) db decisions =
  transaction db (fun db ->
    set_bank_categories_in_transaction ~only_unclassified db decisions)
;;

let review_bank_categories ?(categories = []) db ~expected decisions =
  transaction db (fun db ->
    let* () =
      List.fold_result expected ~init:() ~f:(fun () (event_id, before) ->
        let* current = db_result (Db.first db.conn (find_category event_id)) in
        if Option.equal [%equal: string * string] current before
        then Ok ()
        else
          Or_error.error_string
            "a selected transaction changed since review; review the batch again")
    in
    if
      List.length expected <> List.length decisions
      || List.contains_dup (List.map expected ~f:fst) ~compare:Int64.compare
      || not
           (List.for_all decisions ~f:(fun (d : Bank_category.t) ->
              List.Assoc.mem expected d.event_id ~equal:Int64.equal))
    then Or_error.error_string "category review expectations do not match decisions"
    else (
      let insert =
        Rel_sql.Stmt.(
          func "INSERT OR IGNORE INTO category_catalog(account) VALUES (?)" (text @-> unit))
      in
      let* () =
        List.fold_result categories ~init:() ~f:(fun () name ->
          let* category =
            Bank_category.create ~event_id:1L ~account:name ~reason:"category queue"
          in
          if String.equal category.account "equity:unclassified"
          then Or_error.error_string "unclassified is not a category"
          else db_result (Db.exec db.conn (insert category.account)))
      in
      set_bank_categories_in_transaction db decisions))
;;

type category_change =
  { id : int64
  ; event_id : int64
  ; previous_account : string option
  ; account : string
  ; reason : string
  ; changed_at : Ptime.t
  }

let bank_category_history db =
  let row =
    Row.(
      unit (fun id event_id previous_account account reason changed_at ->
        { id; event_id; previous_account; account; reason; changed_at })
      * read_column Db_schema.Bank_category_changes.id
      * read_column Db_schema.Bank_category_changes.event_id
      * read_column Db_schema.Bank_category_changes.previous_account
      * read_column Db_schema.Bank_category_changes.account
      * read_column Db_schema.Bank_category_changes.reason
      * read_column Db_schema.Bank_category_changes.changed_at)
  in
  collect
    db
    (statement
       "SELECT id,event_id,previous_account,account,reason,changed_at FROM \
        bank_category_changes ORDER BY id"
       row)
;;

type review_classification =
  | Unclassified
  | Category
  | Transfer
  | Settlement
  | Reversal

type transaction_status =
  { total : int
  ; categorized : int
  ; transfers : int
  ; settlements : int
  ; reversals : int
  ; unclassified : int
  ; by_category : (string * int) list
  ; by_type : (string * int) list
  }

let transaction_status db =
  let sql =
    "SELECT e.kind, CASE WHEN EXISTS (SELECT 1 FROM reversal_pairs r WHERE \
     r.original=m.event_id OR r.reversal=m.event_id) THEN 'reversal' WHEN EXISTS (SELECT \
     1 FROM operation_movements om JOIN operation_transfer_memberships t ON \
     t.movement_id=om.id WHERE om.anchor_event_id=m.event_id) THEN 'transfer' WHEN \
     EXISTS (SELECT 1 FROM card_settlement_decisions s WHERE s.event_id=m.event_id) THEN \
     'settlement' WHEN c.account IS NOT NULL AND c.account <> 'equity:unclassified' THEN \
     'category' ELSE 'unclassified' END AS decision, coalesce(c.account,''), \
     coalesce(o.movement_type,m.movement_type,'unknown'), count(*) FROM movements m JOIN \
     source_events e ON e.id=m.event_id LEFT JOIN bank_categories c ON \
     c.event_id=m.event_id LEFT JOIN movement_type_overrides o ON o.event_id=m.event_id \
     GROUP BY 1,2,3,4"
  in
  let row =
    Row.(t5 (text "kind") (text "decision") (text "category") (text "type") (int "count"))
  in
  let* groups = collect db (statement sql row) in
  let totals = Hashtbl.create (module String) in
  let categories = Hashtbl.create (module String) in
  let types = Hashtbl.create (module String) in
  let add table name count =
    Hashtbl.update table name ~f:(fun current -> Option.value current ~default:0 + count)
  in
  List.iter groups ~f:(fun (kind, decision, category, movement_type, count) ->
    if Registry.is_movement_kind db.registry kind
    then (
      add totals decision count;
      add types movement_type count;
      if String.equal decision "category" then add categories category count));
  let get name = Option.value (Hashtbl.find totals name) ~default:0 in
  let by_name table =
    Hashtbl.to_alist table |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  let categorized = get "category"
  and transfers = get "transfer"
  and settlements = get "settlement"
  and reversals = get "reversal"
  and unclassified = get "unclassified" in
  Ok
    { total = categorized + transfers + settlements + reversals + unclassified
    ; categorized
    ; transfers
    ; settlements
    ; reversals
    ; unclassified
    ; by_category = by_name categories
    ; by_type = by_name types
    }
;;

type bank_review =
  { event_id : int64
  ; institution : string
  ; occurred : Temporal.t
  ; transaction : Movement.t
  ; description : string
  ; movement_type : Movement_type.t option
  ; imported_movement_type : Movement_type.t option
  ; movement_type_override : Movement_type.t option
  ; source_evidence : Source_evidence.t
  ; detail_sources : int64 list
  ; classification : review_classification
  ; counter_account : string
  ; reason : string option
  }

(* Read review metadata once, rather than issuing two queries per movement. *)
let review_metadata db =
  let* categories =
    collect
      db
      (statement
         "SELECT event_id,account,reason FROM bank_categories"
         Row.(
           t3
             (read_column Db_schema.Bank_categories.event_id)
             (read_column Db_schema.Bank_categories.account)
             (read_column Db_schema.Bank_categories.reason)))
  in
  let* transfers =
    collect
      db
      (statement
         "SELECT m.event_id,m.first_event_id,m.second_event_id,d.reason FROM \
          bank_transfer_memberships m JOIN bank_transfer_decisions d ON \
          d.first_event_id=m.first_event_id AND d.second_event_id=m.second_event_id \
          WHERE d.state='confirmed'"
         Row.(
           t4
             (read_column Db_schema.Bank_transfer_memberships.event_id)
             (read_column Db_schema.Bank_transfer_memberships.first_event_id)
             (read_column Db_schema.Bank_transfer_memberships.second_event_id)
             (read_column Db_schema.Bank_transfer_decisions.reason)))
  in
  let* general_transfers =
    collect
      db
      (statement
         "SELECT \
          own.anchor_event_id,a.anchor_event_id,b.anchor_event_id,l.reason,r.account \
          FROM operation_transfer_links l JOIN operation_movements a ON \
          a.id=l.first_movement_id JOIN operation_movements b ON \
          b.id=l.second_movement_id JOIN operation_movements own ON own.id IN \
          (a.id,b.id) JOIN operations o ON o.id=own.operation_id AND \
          o.bank_event_id=own.anchor_event_id JOIN operation_movements other ON \
          other.id=CASE WHEN own.id=a.id THEN b.id ELSE a.id END JOIN operations oo ON \
          oo.id=other.operation_id AND oo.bank_event_id IS NULL JOIN \
          operation_movement_resolutions r ON r.movement_id=other.id"
         Row.(
           t5
             (int64 "event_id")
             (int64 "first_event_id")
             (int64 "second_event_id")
             (text "reason")
             (text "account")))
  in
  let* settlements =
    collect
      db
      (statement
         "SELECT event_id,ledger_account,reason FROM card_settlement_decisions"
         Row.(
           t3
             (read_column Db_schema.Card_settlement_decisions.event_id)
             (read_column Db_schema.Card_settlement_decisions.ledger_account)
             (read_column Db_schema.Card_settlement_decisions.reason)))
  in
  let* reversals =
    collect
      db
      (statement
         "SELECT original,reversal,reason FROM reversal_pairs"
         Db_schema.Reversal_pairs.row)
  in
  let reversal_map = Hashtbl.create (module Int64) in
  List.iter reversals ~f:(fun (r : Db_schema.Reversal_pairs.t) ->
    Hashtbl.set reversal_map ~key:r.original ~data:(r.original, r.reversal, r.reason);
    Hashtbl.set reversal_map ~key:r.reversal ~data:(r.original, r.reversal, r.reason));
  Ok
    ( Hashtbl.of_alist_exn
        (module Int64)
        (List.map categories ~f:(fun (id, account, reason) -> id, (account, reason)))
    , Hashtbl.of_alist_exn
        (module Int64)
        (List.map transfers ~f:(fun (id, first, second, reason) ->
           id, (first, second, reason, None))
         @ List.map general_transfers ~f:(fun (id, first, second, reason, account) ->
           id, (first, second, reason, Some account)))
    , Hashtbl.of_alist_exn
        (module Int64)
        (List.map settlements ~f:(fun (id, account, reason) -> id, (account, reason)))
    , reversal_map )
;;

let effective_description fallback annotations =
  let preferred =
    List.filter_map annotations ~f:(fun (a : Companion.annotation) ->
      Option.bind a.display_name ~f:(fun name ->
        Option.some_if (not (String.is_empty (String.strip name))) name))
    |> List.dedup_and_sort ~compare:String.compare
  in
  match preferred with
  | [ name ] -> name
  | _ -> fallback
;;

let bank_review_from
      db
      source
      table
      (categories, transfers, settlements, reversals)
      details
      overrides
      evidence
  =
  traverse
    (fun (event_id, (event : Source_event.t)) ->
       let* (transaction : Movement.t) = stored table event_id in
       let category = Hashtbl.find categories event_id in
       let transfer = Hashtbl.find transfers event_id in
       let settlement = Hashtbl.find settlements event_id in
       let reversal = Hashtbl.find reversals event_id in
       let observations = Option.value (Hashtbl.find details event_id) ~default:[] in
       let description =
         effective_description
           (Option.value
              (List.nth transaction.descriptions transaction.main_description)
              ~default:"")
           (List.map observations ~f:snd)
       in
       let* transfer_destination =
         match transfer with
         | None -> Ok None
         | Some (_, _, _, Some account) -> Ok (Some account)
         | Some (first, second, _, None) ->
           let other = if Int64.equal event_id first then second else first in
           let* (other_event : Source_event.t) =
             Or_error.of_option
               (List.Assoc.find source other ~equal:Int64.equal)
               ~error:(Error.of_string "confirmed transfer references missing movement")
           in
           let* other_movement = stored table other in
           let* mapped =
             db_result
               (Db.first db.conn (find_mapping other_event.venue other_event.account))
           in
           let* account =
             match mapped with
             | Some name -> Ok name
             | None ->
               Bank_projection.ledger_account
                 ~institution:other_event.venue
                 other_movement.account
           in
           Ok (Some account)
       in
       (* The stored type already follows the rules, companion columns included. *)
       let imported_movement_type = transaction.movement_type in
       let movement_type_override = Hashtbl.find overrides event_id in
       Ok
         { event_id
         ; institution = event.venue
         ; occurred = event.occurred
         ; transaction
         ; description
         ; movement_type = Option.first_some movement_type_override imported_movement_type
         ; imported_movement_type
         ; movement_type_override
         ; source_evidence =
             Option.value (Hashtbl.find evidence event_id) ~default:Source_evidence.empty
         ; detail_sources =
             List.map observations ~f:fst |> List.sort ~compare:Int64.compare
         ; classification =
             (match reversal, transfer, settlement, category with
              | Some _, _, _, _ -> Reversal
              | None, Some _, _, _ -> Transfer
              | None, None, Some _, _ -> Settlement
              | None, None, None, Some (account, _)
                when not (String.equal account "equity:unclassified") -> Category
              | _ -> Unclassified)
         ; counter_account =
             (match reversal, transfer with
              | Some (original, reversed, _), _ -> reversal_account original reversed
              | None, Some _ -> Option.value_exn transfer_destination
              | None, None ->
                (match settlement with
                 | Some (account, _) -> account
                 | None -> Option.value_map category ~default:"equity:unclassified" ~f:fst))
         ; reason =
             (match reversal, transfer with
              | Some (_, _, reason), _ | None, Some (_, _, reason, _) -> Some reason
              | None, None ->
                (match settlement with
                 | Some (_, reason) -> Some reason
                 | None -> Option.map category ~f:snd))
         })
    source
;;

let movement_source db =
  let* source = events db in
  Ok (List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e))
;;

let bank_review_with_order ?order db =
  let* source = events_with_order ?order db in
  let source = List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e) in
  let* table = movement_table db in
  let* metadata = review_metadata db in
  let* override_rows =
    collect
      db
      (statement
         "SELECT movement_id,event_id,movement_type FROM movement_type_overrides"
         Db_schema.Movement_type_overrides.row)
  in
  let overrides =
    Hashtbl.of_alist_exn
      (module Int64)
      (List.map override_rows ~f:(fun (r : Db_schema.Movement_type_overrides.t) ->
         r.event_id, Movement_type.of_string r.movement_type))
  in
  let* observations = companion_observations db in
  let companions = registered_companions db in
  let details = Hashtbl.create (module Int64) in
  let* () =
    traverse
      (fun (observation : companion_observation) ->
         match observation.matching with
         | Some { state = Matched; event_id = Some event_id; _ } ->
           let row = observation.row in
           let* companion =
             List.find companions ~f:(fun (owner, (c : Companion.t)) ->
               String.equal owner row.institution && String.equal c.format row.format)
             |> Or_error.of_option
                  ~error:(Error.of_string "unregistered companion format")
           in
           let* annotation = (snd companion).annotate row in
           Hashtbl.add_multi details ~key:event_id ~data:(row.row_id, annotation);
           Ok ()
         | _ -> Ok ())
      observations
    |> Or_error.map ~f:ignore
  in
  let* evidence_rows =
    collect
      db
      (statement
         "SELECT event_id,card_reference,withdrawal_reversal FROM movement_evidence"
         Row.(
           t3
             (int64 "event_id")
             (option Rel.Type.text "card_reference")
             (text "withdrawal_reversal")))
  in
  let evidence = Hashtbl.create (module Int64) in
  List.iter evidence_rows ~f:(fun (event_id, card_reference, reversal) ->
    Hashtbl.set
      evidence
      ~key:event_id
      ~data:
        { Source_evidence.card_reference
        ; withdrawal_reversal = String.equal reversal "yes"
        });
  bank_review_from db source table metadata details overrides evidence
;;

let bank_review db = bank_review_with_order db

let bank_review_ordered db ~columns ~ascending =
  bank_review_with_order ~order:(columns, ascending) db
;;

type operation_movement_review =
  { role : string
  ; account : string option
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  ; anchor_event_id : int64
  ; review : bank_review option
  }

type operation_review =
  { operation_id : int64
  ; occurred : Temporal.t
  ; institution : string
  ; description : string
  ; reported_amount : string option
  ; reported_currency : string option
  ; exchange_reviewed : bool
  ; review : bank_review option
  ; movements : operation_movement_review list
  }

type operation_transfer_link =
  { first_operation_id : int64
  ; second_operation_id : int64
  ; first_event_id : int64
  ; second_event_id : int64
  ; first_movement : Movement_identity.t
  ; second_movement : Movement_identity.t
  ; reason : string
  }

(* Persisted links use local movement IDs; their public identities are derived
   from the immutable anchor sources. *)
let operation_transfer_links db =
  let* events = events db in
  let by_id = Hashtbl.of_alist_exn (module Int64) events in
  let* rows =
    collect
      db
      (statement
         "SELECT \
          a.anchor_event_id,b.anchor_event_id,a.operation_id,b.operation_id,l.reason, \
          a.role,b.role FROM operation_transfer_links l JOIN operation_movements a ON \
          a.id=l.first_movement_id JOIN operation_movements b ON \
          b.id=l.second_movement_id JOIN operation_transfer_memberships x ON \
          x.movement_id=a.id AND x.first_movement_id=l.first_movement_id AND \
          x.second_movement_id=l.second_movement_id JOIN operation_transfer_memberships \
          y ON y.movement_id=b.id AND y.first_movement_id=l.first_movement_id AND \
          y.second_movement_id=l.second_movement_id ORDER BY \
          l.first_movement_id,l.second_movement_id"
         Row.(
           unit (fun a b c d e f g -> a, b, c, d, e, f, g)
           * int64 "first_event_id"
           * int64 "second_event_id"
           * int64 "first_operation_id"
           * int64 "second_operation_id"
           * text "reason"
           * text "first_role"
           * text "second_role"))
  in
  let* stored_count =
    db_result
      (Db.first
         db.conn
         (statement "SELECT count(*) FROM operation_transfer_links" integer))
  in
  let* () =
    if Option.value stored_count ~default:(-1) = List.length rows
    then Ok ()
    else Or_error.error_string "operation transfer link has missing movement membership"
  in
  traverse
    (fun ( first_event
         , second_event
         , first_operation_id
         , second_operation_id
         , reason
         , first_role
         , second_role ) ->
       let* first =
         Hashtbl.find by_id first_event
         |> Or_error.of_option
              ~error:(Error.of_string "confirmed transfer has no first source")
       in
       let* second =
         Hashtbl.find by_id second_event
         |> Or_error.of_option
              ~error:(Error.of_string "confirmed transfer has no second source")
       in
       let* first_movement = Movement_identity.of_event first ~role:first_role in
       let* second_movement = Movement_identity.of_event second ~role:second_role in
       if Int64.equal first_operation_id second_operation_id
       then
         Or_error.error_string
           "confirmed transfer movements share an underlying operation"
       else
         Ok
           { first_operation_id
           ; second_operation_id
           ; first_event_id = first_event
           ; second_event_id = second_event
           ; first_movement
           ; second_movement
           ; reason
           })
    rows
;;

let operation_groups db =
  let* links = operation_transfer_links db in
  let* bank_operations =
    collect
      db
      (statement
         "SELECT id,bank_event_id FROM operations WHERE bank_event_id IS NOT NULL ORDER \
          BY id"
         Row.(t2 (int64 "id") (int64 "bank_event_id")))
  in
  let* events = events db in
  let by_id = Hashtbl.of_alist_exn (module Int64) events in
  let stated = Hashtbl.Poly.create () in
  List.iter bank_operations ~f:(fun (operation_id, event_id) ->
    Option.iter (Hashtbl.find by_id event_id) ~f:(fun (event : Source_event.t) ->
      Option.iter
        (Registry.operation_group db.registry event)
        ~f:(fun (key, supporting) ->
          Hashtbl.add_multi stated ~key:(event.venue, key) ~data:(supporting, operation_id))));
  Operation_groups.create
    ~links:
      (List.map links ~f:(fun link -> link.first_operation_id, link.second_operation_id))
    ~stated:
      (Hashtbl.data stated
       |> List.map ~f:(fun group ->
         List.sort group ~compare:[%compare: bool * int64] |> List.map ~f:snd))
;;

let operation_ids_for_events db event_ids =
  let* rows =
    collect
      db
      (statement
         "SELECT m.anchor_event_id,o.id FROM operation_movements m JOIN operations o ON \
          o.id=m.operation_id WHERE m.role='primary' AND m.anchor_event_id IS NOT NULL \
          ORDER BY m.anchor_event_id"
         Row.(t2 (int64 "anchor_event_id") (int64 "operation_id")))
  in
  let wanted = Set.of_list (module Int64) event_ids in
  Ok (List.filter rows ~f:(fun (event_id, _) -> Set.mem wanted event_id))
;;

(* A provider row's own description, taken from its stored columns. The field
   names are the provider's, so this is evidence, not a normalized fact. *)
let operation_fields db operation_id =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT f.name,f.value FROM source_event_fields f JOIN operation_sources s ON \
         s.event_id=f.event_id WHERE s.operation_id=? ORDER BY f.event_id,f.ordinal"
        (int64 @-> ret Row.(t2 (text "name") (text "value"))))
  in
  collect db (query operation_id)
;;

let operation_review db =
  let* reviews = bank_review db in
  let* exchange_ids =
    collect
      db
      (statement
         "SELECT operation_id FROM operation_exchange_decisions ORDER BY operation_id"
         Row.(t1 (int64 "operation_id")))
  in
  let exchange_ids = Set.of_list (module Int64) exchange_ids in
  let review_by_event =
    Hashtbl.of_alist_exn
      (module Int64)
      (List.map reviews ~f:(fun (r : bank_review) -> r.event_id, r))
  in
  let* stored =
    collect
      db
      (statement
         "SELECT id,occurred,bank_event_id,source_event_id FROM operations ORDER BY id"
         Db_schema.Operations.row)
  in
  let* movement_rows =
    collect
      db
      (statement
         "SELECT \
          m.id,m.operation_id,m.anchor_event_id,m.role,COALESCE(r.account,m.account) AS \
          account, COALESCE(r.commodity,m.commodity) AS commodity,m.quantity FROM \
          operation_movements m LEFT JOIN operation_movement_resolutions r ON \
          r.movement_id=m.id ORDER BY m.operation_id,m.id"
         Db_schema.Operation_movements.row)
  in
  let movements = Hashtbl.create (module Int64) in
  List.iter movement_rows ~f:(fun (m : Db_schema.Operation_movements.t) ->
    Hashtbl.add_multi movements ~key:m.operation_id ~data:m);
  let* source_ids =
    collect
      db
      (statement
         "SELECT operation_id,event_id FROM operation_sources ORDER BY \
          operation_id,event_id"
         Db_schema.Operation_sources.row)
  in
  let sources = Hashtbl.create (module Int64) in
  List.iter source_ids ~f:(fun (s : Db_schema.Operation_sources.t) ->
    Hashtbl.add_multi sources ~key:s.operation_id ~data:s.event_id);
  let* events =
    collect
      db
      (statement
         "SELECT id,venue FROM source_events ORDER BY id"
         Row.(t2 (int64 "id") (text "venue")))
  in
  let venue = Hashtbl.of_alist_exn (module Int64) events in
  let source_events operation_id =
    Option.value (Hashtbl.find sources operation_id) ~default:[]
    @ List.map
        (Option.value (Hashtbl.find movements operation_id) ~default:[])
        ~f:(fun (m : Db_schema.Operation_movements.t) -> m.anchor_event_id)
  in
  let institution_of operation_id =
    List.filter_map (source_events operation_id) ~f:(Hashtbl.find venue)
    |> List.dedup_and_sort ~compare:String.compare
  in
  let movement_of_row (m : Db_schema.Operation_movements.t) =
    let* commodity = Json.decode Commodity.codec m.commodity in
    let* quantity = Or_error.try_with (fun () -> Quantity.of_string m.quantity) in
    Ok
      { role = m.role
      ; account = m.account
      ; commodity
      ; quantity
      ; anchor_event_id = m.anchor_event_id
      ; review = Hashtbl.find review_by_event m.anchor_event_id
      }
  in
  let* rows =
    traverse
      (fun (o : Db_schema.Operations.t) ->
         let* occurred =
           Or_error.try_with (fun () -> Temporal.of_string_exn o.occurred)
         in
         let* movements =
           traverse
             movement_of_row
             (Option.value (Hashtbl.find movements o.id) ~default:[])
         in
         let primary =
           List.find movements ~f:(fun m -> String.equal m.role "primary")
           |> Option.first_some (List.hd movements)
         in
         let review =
           Option.bind primary ~f:(fun m ->
             Hashtbl.find review_by_event m.anchor_event_id)
         in
         let institution =
           match review with
           | Some (r : bank_review) -> r.institution
           | None ->
             (match institution_of o.id with
              | [ name ] -> name
              | _ -> "")
         in
         let* fields =
           match review with
           | Some _ -> Ok []
           | None -> operation_fields db o.id
         in
         let field name = List.Assoc.find fields name ~equal:String.equal in
         let description =
           match review with
           | Some (r : bank_review) -> r.description
           | None ->
             List.find_map
               [ "Transaction Description"; "Description"; "description" ]
               ~f:field
             |> Option.value ~default:""
         in
         let reported_amount, reported_currency =
           if List.is_empty movements && String.equal institution "cdc"
           then field "Amount", field "Currency"
           else None, None
         in
         Ok
           { operation_id = o.id
           ; occurred
           ; institution
           ; description
           ; reported_amount
           ; reported_currency
           ; exchange_reviewed = Set.mem exchange_ids o.id
           ; review
           ; movements
           })
      stored
  in
  let* groups = operation_groups db in
  let by_id = Hashtbl.create (module Int64) in
  List.iter rows ~f:(fun row -> Hashtbl.set by_id ~key:row.operation_id ~data:row);
  Ok
    (List.filter_map rows ~f:(fun row ->
       Option.some_if
         (Int64.equal
            (Operation_groups.canonical groups row.operation_id)
            row.operation_id)
         { row with
           movements =
             List.concat_map
               (Operation_groups.members groups row.operation_id)
               ~f:(fun id ->
                 Option.value_map (Hashtbl.find by_id id) ~default:[] ~f:(fun r ->
                   r.movements))
         }))
;;

let validate_commodity_symbol commodity symbol =
  let* symbol = Validation.token "commodity symbol" symbol in
  match commodity with
  | Commodity.Reported _ ->
    Or_error.error_string "choose a canonical commodity, not a provider-reported symbol"
  | Commodity.Fiat currency
    when not (String.equal symbol (Iso4217.to_alpha_code currency)) ->
    Or_error.error_string "fiat commodity symbol must equal its ISO currency code"
  | (Commodity.Onchain_native _ | Onchain_token _)
    when Option.is_some (Iso4217.of_alpha_code (String.uppercase symbol)) ->
    Or_error.error_string "crypto commodity symbol conflicts with an ISO currency code"
  | _ -> Ok symbol
;;

let resolve_operation_movement_internal
      ?(in_transaction = false)
      db
      ~operation_id
      ~role
      ~account
      ~commodity
      ~symbol
  =
  let* account = Validation.account account in
  let* _ = Validation.token "movement role" role in
  let* symbol = validate_commodity_symbol commodity symbol in
  (if in_transaction then fun f -> f db else transaction db) (fun db ->
    let query =
      Rel_sql.Stmt.(
        func
          "SELECT m.id,o.bank_event_id,(EXISTS(SELECT 1 FROM \
           operation_transfer_memberships t WHERE t.movement_id=m.id) OR EXISTS (SELECT \
           1 FROM operation_exchange_decisions x WHERE x.operation_id=o.id)) FROM \
           operation_movements m JOIN operations o ON o.id=m.operation_id WHERE o.id=? \
           AND m.role=?"
          (int64
           @-> text
           @-> ret
                 Row.(
                   t3 (int64 "id") (option Rel.Type.int64 "bank_event_id") (bool "linked"))
          ))
    in
    let* movement = db_result (Db.first db.conn (query operation_id role)) in
    let* movement_id, bank_event_id, linked =
      Or_error.of_option
        movement
        ~error:(Error.of_string "operation has no movement with that role")
    in
    let* () =
      if Option.is_some bank_event_id
      then
        Or_error.error_string
          "bank movements use their account mapping and imported currency"
      else if linked
      then
        Or_error.error_string
          "undo the confirmed operation before changing its movement resolution"
      else Ok ()
    in
    let commodity = Json.encode Commodity.codec commodity in
    let symbol_query =
      Rel_sql.Stmt.(
        func
          "SELECT commodity,symbol FROM commodity_symbols WHERE commodity=? OR symbol=?"
          (text @-> text @-> ret Row.(t2 (text "commodity") (text "symbol"))))
    in
    let* mappings = collect db (symbol_query commodity symbol) in
    let* () =
      if
        List.exists mappings ~f:(fun (existing_commodity, existing_symbol) ->
          not
            (String.equal existing_commodity commodity
             && String.equal existing_symbol symbol))
      then
        Or_error.error_string
          "commodity identity or journal symbol already has a different mapping"
      else Ok ()
    in
    let insert_symbol =
      Rel_sql.Stmt.(
        func
          "INSERT OR IGNORE INTO commodity_symbols(commodity,symbol) VALUES (?,?)"
          (text @-> text @-> unit))
    in
    let* () = db_result (Db.exec db.conn (insert_symbol commodity symbol)) in
    let previous =
      Rel_sql.Stmt.(
        func
          "SELECT account,commodity FROM operation_movement_resolutions WHERE \
           movement_id=?"
          (int64 @-> ret Row.(t2 (text "account") (text "commodity"))))
    in
    let* current = db_result (Db.first db.conn (previous movement_id)) in
    if
      Option.exists current ~f:(fun (old_account, old_commodity) ->
        String.equal old_account account && String.equal old_commodity commodity)
    then Ok false
    else (
      let update =
        Rel_sql.Stmt.(
          func
            "INSERT INTO operation_movement_resolutions(movement_id,account,commodity) \
             VALUES (?,?,?) ON CONFLICT(movement_id) DO UPDATE SET \
             account=excluded.account,commodity=excluded.commodity"
            (int64 @-> text @-> text @-> unit))
      in
      let audit =
        Rel_sql.Stmt.(
          func
            "INSERT INTO operation_resolution_changes(movement_id,account,commodity) \
             VALUES (?,?,?)"
            (int64 @-> text @-> text @-> unit))
      in
      let* () = db_result (Db.exec db.conn (update movement_id account commodity)) in
      let* () = db_result (Db.exec db.conn (audit movement_id account commodity)) in
      Ok true))
;;

let resolve_operation_movement db ~operation_id ~role ~account ~commodity ~symbol =
  resolve_operation_movement_internal db ~operation_id ~role ~account ~commodity ~symbol
;;
