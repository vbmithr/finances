open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Setup
open Sources
open Import
open Ledger
open Review

type category_restore_preview =
  { categories_to_add : string list
  ; rules_to_add : Classification_rule.t list
  ; decisions_to_apply : Bank_category.t list
  ; types_to_apply : (int64 * Movement_type.t) list
  ; resolutions_to_apply : (int64 * string * string * Commodity.t * string) list
  ; unchanged : int
  ; problems : string list
  }

let raw_fields_hash (e : Source_event.t) =
  List.map e.fields ~f:(fun (name, value) ->
    Printf.sprintf "%d:%s%d:%s" (String.length name) name (String.length value) value)
  |> String.concat
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let sha256 text = Digestif.SHA256.(to_hex (digest_string text))

let payload_hash (e : Source_event.t) =
  sha256 (Option.value (Records.row_identity e.payload) ~default:e.payload)
;;

(* A records row's fields can grow with its converter, and its identity says what it is. *)
let category_fields_hash (e : Source_event.t) =
  match Records.row_identity e.payload with
  | Some identity -> sha256 identity
  | None -> raw_fields_hash e
;;

(* What tells this row from others of its day. An institution with row IDs says so itself. Without
   them, rows with identical fields cannot be told apart between exports, so a decision on one could
   land on the other. *)
let evidence_key db (e : Source_event.t) =
  let identified =
    match Registry.find db.registry e.venue with
    | Some (module A : Adapter.S) ->
      (match A.overlap with
       | Source_ids -> true
       | Day_inventory -> false)
    | None -> false
  in
  if identified
  then "id:" ^ e.source_id
  else if List.is_empty e.fields
  then "payload:" ^ sha256 e.payload
  else "fields:" ^ raw_fields_hash e
;;

let bank_operation_anchors db =
  let* sources =
    collect
      db
      (statement
         "SELECT o.bank_event_id FROM operations o JOIN operation_sources s ON \
          s.operation_id=o.id AND s.event_id=o.bank_event_id WHERE o.bank_event_id IS \
          NOT NULL"
         Row.(t1 (int64 "event_id")))
  in
  let* rows =
    collect
      db
      (statement
         "SELECT m.anchor_event_id,m.commodity,m.quantity FROM operation_movements m \
          JOIN operations o ON o.id=m.operation_id AND o.bank_event_id=m.anchor_event_id \
          WHERE m.role='primary' AND EXISTS (SELECT 1 FROM operation_movement_sources s \
          WHERE s.movement_id=m.id AND s.operation_id=m.operation_id AND \
          s.event_id=m.anchor_event_id)"
         Row.(t3 (int64 "event_id") (text "commodity") (text "quantity")))
  in
  Ok
    ( Hashtbl.of_alist_exn
        (module Int64)
        (List.map rows ~f:(fun (id, commodity, quantity) -> id, (commodity, quantity)))
    , Set.of_list (module Int64) sources )
;;

let verify_bank_operation_anchor (anchors, sources) (row : bank_review) =
  if not (Set.mem sources row.event_id)
  then Or_error.error_string "bank primary movement or its source link is missing"
  else (
    match Hashtbl.find anchors row.event_id with
    | None -> Or_error.error_string "bank primary movement or its source link is missing"
    | Some (commodity, amount) ->
      let* commodity = Json.decode Commodity.codec commodity in
      let* quantity = Or_error.try_with (fun () -> Quantity.of_string amount) in
      if
        Commodity.equal commodity (Commodity.fiat row.transaction.account.currency)
        && Quantity.equal quantity row.transaction.amount
      then Ok ()
      else Or_error.error_string "bank primary movement differs from source movement")
;;

let category_snapshot db =
  transaction db (fun db ->
    let* categories = categories db in
    let* rules = classification_rules db in
    let rules =
      List.filter rules ~f:(fun r ->
        match r.Classification_rule.action with
        | Category _ | Type_and_category _ -> true
        | _ -> false)
    in
    let* events = events db in
    let events_by_id = Hashtbl.of_alist_exn (module Int64) events in
    let* review = bank_review db in
    let* anchors = bank_operation_anchors db in
    let* decisions =
      List.filter review ~f:(fun r ->
        Option.is_some r.movement_type_override
        || ((Poly.equal r.classification Unclassified
             || Poly.equal r.classification Category)
            && Option.is_some r.reason))
      |> traverse (fun r ->
        let* () = verify_bank_operation_anchor anchors r in
        let* event =
          Hashtbl.find events_by_id r.event_id
          |> Or_error.of_option
               ~error:(Error.of_string "category references missing event")
        in
        Ok
          Category_portable.
            { institution = event.venue
            ; source_account = event.account
            ; kind = event.kind
            ; source_id = event.source_id
            ; occurred = Temporal.to_string event.occurred
            ; payload_sha256 = payload_hash event
            ; fields_sha256 = category_fields_hash event
            ; movement_role = "primary"
            ; account =
                Option.some_if
                  ((Poly.equal r.classification Unclassified
                    || Poly.equal r.classification Category)
                   && Option.is_some r.reason)
                  r.counter_account
            ; reason =
                (if
                   Poly.equal r.classification Unclassified
                   || Poly.equal r.classification Category
                 then r.reason
                 else None)
            ; type_override =
                Option.map r.movement_type_override ~f:Movement_type.to_string
            })
    in
    let* saved =
      collect
        db
        (statement
           "SELECT m.anchor_event_id,m.role,r.account,r.commodity,m.quantity,m.commodity \
            FROM operation_movement_resolutions r JOIN operation_movements m ON \
            m.id=r.movement_id ORDER BY m.anchor_event_id,m.role"
           Row.(
             t6
               (int64 "event_id")
               (text "role")
               (text "account")
               (text "commodity")
               (text "quantity")
               (text "source_commodity")))
    in
    let* resolutions =
      traverse
        (fun (id, role, account, commodity, quantity, source_commodity) ->
           let* event =
             Hashtbl.find events_by_id id
             |> Or_error.of_option
                  ~error:(Error.of_string "resolved movement source missing")
           in
           let* commodity = Json.decode Commodity.codec commodity in
           let symbol_query =
             Rel_sql.Stmt.(
               func
                 "SELECT symbol FROM commodity_symbols WHERE commodity=?"
                 (text @-> ret Row.(t1 (text "symbol"))))
           in
           let* symbol =
             db_result
               (Db.first db.conn (symbol_query (Json.encode Commodity.codec commodity)))
           in
           let* symbol =
             Or_error.of_option
               symbol
               ~error:(Error.of_string "resolved commodity has no journal symbol")
           in
           let* source_commodity = Json.decode Commodity.codec source_commodity in
           let* source_quantity =
             Or_error.try_with (fun () -> Quantity.of_string quantity)
           in
           let source =
             Category_portable.
               { institution = event.venue
               ; source_account = event.account
               ; kind = event.kind
               ; source_id = event.source_id
               ; occurred = Temporal.to_string event.occurred
               ; payload_sha256 = payload_hash event
               ; fields_sha256 = category_fields_hash event
               ; movement_role = role
               ; account = None
               ; reason = None
               ; type_override = None
               }
           in
           Ok
             Category_portable.
               { source; source_quantity; source_commodity; account; commodity; symbol })
        saved
    in
    Ok Category_portable.{ categories; rules; decisions; resolutions })
;;

let preview_category_restore db (snapshot : Category_portable.t) =
  let* existing_categories = categories db in
  let* existing_rules = classification_rules db in
  let* source = events db in
  let* review = bank_review db in
  let* anchors = bank_operation_anchors db in
  let by_identity = Hashtbl.Poly.create () in
  let by_evidence = Hashtbl.Poly.create () in
  let review_by_id = Hashtbl.create (module Int64) in
  List.iter review ~f:(fun (r : bank_review) ->
    Hashtbl.set review_by_id ~key:r.event_id ~data:r);
  List.iter source ~f:(fun (id, (e : Source_event.t)) ->
    let digest = payload_hash e in
    Hashtbl.add_multi
      by_identity
      ~key:(e.venue, e.account, e.kind, e.source_id)
      ~data:(id, e, digest);
    Hashtbl.add_multi
      by_evidence
      ~key:(e.venue, e.account, e.kind, Temporal.to_string e.occurred, evidence_key db e)
      ~data:id);
  let problems = ref [] in
  let problem s = problems := s :: !problems in
  if List.contains_dup snapshot.categories ~compare:String.compare
  then problem "duplicate category names";
  List.iter snapshot.categories ~f:(fun account ->
    match Bank_category.create ~event_id:1L ~account ~reason:"portable category" with
    | Error e -> problem (Error.to_string_hum e)
    | Ok _ when String.equal account "equity:unclassified" ->
      problem "unclassified is not a category"
    | Ok _ -> ());
  let referenced_categories =
    List.filter_map snapshot.rules ~f:(fun (rule : Classification_rule.t) ->
      match rule.action with
      | Category account | Type_and_category (_, account) -> Some account
      | _ -> None)
    @ List.filter_map
        snapshot.decisions
        ~f:(fun (decision : Category_portable.decision) ->
          Option.filter decision.account ~f:(fun account ->
            not (String.equal account "equity:unclassified")))
  in
  List.iter referenced_categories ~f:(fun account ->
    match Bank_category.create ~event_id:1L ~account ~reason:"portable category" with
    | Error e -> problem (Error.to_string_hum e)
    | Ok _ -> ());
  let categories_to_add =
    let existing = Set.of_list (module String) existing_categories in
    List.dedup_and_sort
      (snapshot.categories @ referenced_categories)
      ~compare:String.compare
    |> List.filter ~f:(fun c -> not (Set.mem existing c))
  in
  if
    List.contains_dup
      (List.map snapshot.rules ~f:(fun r -> r.Classification_rule.name))
      ~compare:String.compare
  then problem "duplicate rule names";
  let* accounts =
    if
      List.exists snapshot.rules ~f:(fun (rule : Classification_rule.t) ->
        match rule.action with
        | Settlement _ -> true
        | _ -> false)
    then bank_accounts db
    else Ok []
  in
  List.iter snapshot.rules ~f:(fun (rule : Classification_rule.t) ->
    match rule.action with
    | Settlement card_id ->
      (match
         List.filter accounts ~f:(fun account ->
           String.equal (Bank_account_mapping.id account) card_id)
       with
       | [ card ]
         when List.mem
                [ "CREDITCRD"; "credit-card" ]
                card.account_type
                ~equal:String.equal -> ()
       | [ _ ] -> problem ("settlement rule target is not a credit card: " ^ rule.name)
       | [] -> problem ("settlement rule references a missing card: " ^ rule.name)
       | _ -> problem ("settlement rule card ID is ambiguous: " ^ rule.name))
    | _ -> ());
  let rules_to_add =
    let existing =
      Hashtbl.of_alist_multi
        (module String)
        (List.map existing_rules ~f:(fun (r : Classification_rule.t) -> r.name, r))
    in
    List.filter snapshot.rules ~f:(fun r ->
      match Option.bind (Hashtbl.find existing r.name) ~f:List.hd with
      | None -> true
      | Some current when Poly.equal current r -> false
      | Some _ ->
        problem ("rule already exists with different definition: " ^ r.name);
        false)
  in
  let ids =
    List.map snapshot.decisions ~f:(fun d ->
      ( d.Category_portable.institution
      , d.source_account
      , d.kind
      , d.source_id
      , d.movement_role ))
  in
  if List.contains_dup ids ~compare:[%compare: string * string * string * string * string]
  then problem "duplicate transaction identities";
  let unchanged = ref 0 in
  let types_to_apply = ref [] in
  let imported_types =
    Hash_set.of_list
      (module String)
      (List.filter_map review ~f:(fun r ->
         match r.imported_movement_type with
         | Some (Movement_type.Other value) -> Some value
         | _ -> None))
  in
  let decisions_to_apply =
    List.filter_map snapshot.decisions ~f:(fun d ->
      let label = d.institution ^ "/" ^ d.source_id in
      let matches =
        Option.value
          (Hashtbl.find
             by_identity
             (d.institution, d.source_account, d.kind, d.source_id))
          ~default:[]
      in
      if not (String.equal d.movement_role "primary")
      then (
        problem ("unsupported movement role: " ^ label);
        None)
      else (
        match matches with
        | [ (id, e, digest) ] ->
          if
            not
              (String.equal (Temporal.to_string e.occurred) d.occurred
               && String.equal digest d.payload_sha256
               && String.equal (category_fields_hash e) d.fields_sha256)
          then (
            problem ("source evidence differs: " ^ label);
            None)
          else if
            List.length
              (Option.value
                 (Hashtbl.find
                    by_evidence
                    ( d.institution
                    , d.source_account
                    , d.kind
                    , d.occurred
                    , evidence_key db e ))
                 ~default:[])
            > 1
          then (
            problem ("ambiguous duplicate source evidence: " ^ label);
            None)
          else (
            match Hashtbl.find review_by_id id with
            | None ->
              problem ("not a bank movement: " ^ label);
              None
            | Some row ->
              (match verify_bank_operation_anchor anchors row with
               | Error e -> problem (label ^ ": " ^ Error.to_string_hum e)
               | Ok () -> ());
              (match d.type_override with
               | None -> ()
               | Some name ->
                 let kind = Movement_type.of_string name in
                 if
                   String.is_empty (String.strip name)
                   || String.equal name "unknown"
                   || not (String.equal name (Movement_type.to_string kind))
                 then problem ("invalid type override: " ^ label)
                 else if
                   match kind with
                   | Other value -> not (Hash_set.mem imported_types value)
                   | _ -> false
                 then problem ("unknown movement type: " ^ label)
                 else (
                   match row.movement_type_override with
                   | None -> types_to_apply := (id, kind) :: !types_to_apply
                   | Some current when Poly.equal current kind -> ()
                   | Some _ -> problem ("existing type override differs: " ^ label)));
              (match d.account, d.reason with
               | None, None ->
                 if Option.is_none d.type_override
                 then problem ("empty transaction decision: " ^ label);
                 None
               | Some account, Some reason ->
                 (match Bank_category.create ~event_id:id ~account ~reason with
                  | Error e ->
                    problem (label ^ ": " ^ Error.to_string_hum e);
                    None
                  | Ok decision ->
                    if
                      not
                        (Poly.equal row.classification Unclassified
                         || Poly.equal row.classification Category)
                    then (
                      problem ("linked transfer, settlement or reversal: " ^ label);
                      None)
                    else if Option.is_some row.reason
                    then
                      if String.equal row.counter_account account
                      then (
                        Int.incr unchanged;
                        None)
                      else (
                        problem ("existing decision differs: " ^ label);
                        None)
                    else Some decision)
               | _ ->
                 problem ("category requires an account and reason: " ^ label);
                 None))
        | [] ->
          problem ("transaction not found: " ^ label);
          None
        | _ ->
          problem ("ambiguous transaction identity: " ^ label);
          None))
  in
  let resolution_ids =
    List.map snapshot.resolutions ~f:(fun (r : Category_portable.resolution) ->
      let d = r.source in
      d.institution, d.source_account, d.kind, d.source_id, d.movement_role)
  in
  if
    List.contains_dup
      resolution_ids
      ~compare:[%compare: string * string * string * string * string]
  then problem "duplicate movement resolutions";
  let* existing_symbols =
    collect
      db
      (statement
         "SELECT commodity,symbol FROM commodity_symbols"
         Row.(t2 (text "commodity") (text "symbol")))
  in
  let proposed_symbols =
    List.map snapshot.resolutions ~f:(fun (r : Category_portable.resolution) ->
      Json.encode Commodity.codec r.commodity, r.symbol)
  in
  List.iter proposed_symbols ~f:(fun (commodity, symbol) ->
    if
      List.exists
        (existing_symbols @ proposed_symbols)
        ~f:(fun (other_commodity, other_symbol) ->
          (String.equal commodity other_commodity
           && not (String.equal symbol other_symbol))
          || (String.equal symbol other_symbol
              && not (String.equal commodity other_commodity)))
    then problem ("commodity symbol mapping conflicts: " ^ symbol));
  let lookup =
    Rel_sql.Stmt.(
      func
        "SELECT \
         c.operation_id,m.quantity,m.commodity,r.account,r.commodity,c.bank_event_id, \
         c.linked FROM movement_claims c JOIN operation_movements m ON m.id=c.id LEFT \
         JOIN operation_movement_resolutions r ON r.movement_id=c.id WHERE \
         c.anchor_event_id=? AND c.role=? AND EXISTS (SELECT 1 FROM \
         operation_movement_sources s WHERE s.movement_id=c.id AND \
         s.event_id=c.anchor_event_id AND s.operation_id=c.operation_id)"
        (int64
         @-> text
         @-> ret
               Row.(
                 unit (fun a b c d e f g -> a, b, c, d, e, f, g)
                 * int64 "operation_id"
                 * text "quantity"
                 * text "source_commodity"
                 * option Rel.Type.text "account"
                 * option Rel.Type.text "commodity"
                 * option Rel.Type.int64 "bank_event_id"
                 * bool "linked")))
  in
  let* resolutions =
    traverse
      (fun (r : Category_portable.resolution) ->
         let d = r.source in
         let label = d.institution ^ "/" ^ d.source_id ^ "/" ^ d.movement_role in
         let matches =
           Option.value
             (Hashtbl.find
                by_identity
                (d.institution, d.source_account, d.kind, d.source_id))
             ~default:[]
         in
         match matches with
         | [ (id, e, digest) ]
           when String.equal (Temporal.to_string e.occurred) d.occurred
                && String.equal digest d.payload_sha256
                && String.equal (category_fields_hash e) d.fields_sha256 ->
           let duplicate_count =
             Option.value
               (Hashtbl.find
                  by_evidence
                  (d.institution, d.source_account, d.kind, d.occurred, evidence_key db e))
               ~default:[]
             |> List.length
           in
           if duplicate_count > 1
           then (
             problem ("ambiguous source evidence: " ^ label);
             Ok None)
           else
             let* found = db_result (Db.first db.conn (lookup id d.movement_role)) in
             (match found with
              | None ->
                problem ("movement not found: " ^ label);
                Ok None
              | Some
                  ( op
                  , quantity
                  , original
                  , current_account
                  , current_commodity
                  , bank
                  , linked ) ->
                let* original = Json.decode Commodity.codec original in
                let* quantity =
                  Or_error.try_with (fun () -> Quantity.of_string quantity)
                in
                if
                  Option.is_some d.account
                  || Option.is_some d.reason
                  || Option.is_some d.type_override
                  || (not (Quantity.equal quantity r.source_quantity))
                  || not (Commodity.equal original r.source_commodity)
                then (
                  problem ("movement interpretation differs: " ^ label);
                  Ok None)
                else if
                  Option.is_some bank
                  || Result.is_error (Validation.account r.account)
                  || Result.is_error (validate_commodity_symbol r.commodity r.symbol)
                then (
                  problem ("invalid movement resolution: " ^ label);
                  Ok None)
                else (
                  let encoded = Json.encode Commodity.codec r.commodity in
                  match current_account, current_commodity with
                  | Some account, Some commodity
                    when String.equal account r.account && String.equal commodity encoded
                    ->
                    if
                      List.exists existing_symbols ~f:(fun (c, s) ->
                        String.equal c encoded && String.equal s r.symbol)
                    then Ok None
                    else (
                      problem ("resolved commodity symbol missing: " ^ label);
                      Ok None)
                  | Some _, Some _ ->
                    problem ("existing movement resolution differs: " ^ label);
                    Ok None
                  | None, None when not linked ->
                    Ok (Some (op, d.movement_role, r.account, r.commodity, r.symbol))
                  | _ ->
                    problem ("movement resolution conflicts with a transfer: " ^ label);
                    Ok None))
         | [] ->
           problem ("transaction not found: " ^ label);
           Ok None
         | [ _ ] ->
           problem ("source evidence differs: " ^ label);
           Ok None
         | _ ->
           problem ("ambiguous transaction identity: " ^ label);
           Ok None)
      snapshot.resolutions
  in
  Ok
    { categories_to_add
    ; rules_to_add
    ; decisions_to_apply
    ; types_to_apply = List.rev !types_to_apply
    ; resolutions_to_apply = List.filter_opt resolutions
    ; unchanged = !unchanged
    ; problems = List.rev !problems
    }
;;

let write_movement_type_override db ~event_id ~previous ~movement_type ~reason =
  if Option.equal Poly.equal previous movement_type
  then Ok false
  else
    let* movement_id = bank_primary_movement_id db event_id in
    let* () =
      match movement_type with
      | None ->
        let query =
          Rel_sql.Stmt.(
            func "DELETE FROM movement_type_overrides WHERE event_id=?" (int64 @-> unit))
        in
        db_result (Db.exec db.conn (query event_id))
      | Some kind ->
        let query =
          Rel_sql.Stmt.(
            func
              "INSERT INTO movement_type_overrides(movement_id,event_id,movement_type) \
               VALUES (?,?,?) ON CONFLICT(event_id) DO UPDATE SET \
               movement_type=excluded.movement_type"
              (int64 @-> int64 @-> text @-> unit))
        in
        db_result
          (Db.exec db.conn (query movement_id event_id (Movement_type.to_string kind)))
    in
    let movement_value = movement_id
    and event_value = event_id
    and old_value = Option.map previous ~f:Movement_type.to_string
    and new_value = Option.map movement_type ~f:Movement_type.to_string
    and reason_value = reason in
    let open Db_schema.Movement_type_changes in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    let* () =
      db_result
        (Db.exec
           db.conn
           (insert
              Col.
                [ Value (Db_schema.Movement_type_changes.event_id, event_value)
                ; Value (Db_schema.Movement_type_changes.movement_id, movement_value)
                ; Value (previous_type, old_value)
                ; Value (Db_schema.Movement_type_changes.movement_type, new_value)
                ; Value (Db_schema.Movement_type_changes.reason, reason_value)
                ]))
    in
    Ok true
;;

let restore_categories db ~expected snapshot =
  transaction db (fun db ->
    let* current = preview_category_restore db snapshot in
    if not (Poly.equal current expected)
    then Or_error.error_string "restore preview changed; review it again"
    else if not (List.is_empty current.problems)
    then Or_error.error_string "resolve restore problems before applying"
    else (
      let insert =
        Rel_sql.Stmt.(
          func "INSERT OR IGNORE INTO category_catalog(account) VALUES (?)" (text @-> unit))
      in
      let* () =
        List.fold_result current.categories_to_add ~init:() ~f:(fun () account ->
          db_result (Db.exec db.conn (insert account)))
      in
      let* () =
        List.fold_result current.rules_to_add ~init:() ~f:(fun () rule ->
          add_classification_rule_in_transaction db rule)
      in
      let* _ =
        if List.is_empty current.rules_to_add
        then Ok 0
        else retype db ~reason:"restore category rules"
      in
      let* changed = set_bank_categories_in_transaction db current.decisions_to_apply in
      let* _ =
        traverse
          (fun (event_id, kind) ->
             write_movement_type_override
               db
               ~event_id
               ~previous:None
               ~movement_type:(Some kind)
               ~reason:"Restored transaction type override")
          current.types_to_apply
      in
      let* _ =
        traverse
          (fun (operation_id, role, account, commodity, symbol) ->
             resolve_operation_movement_internal
               ~in_transaction:true
               db
               ~operation_id
               ~role
               ~account
               ~commodity
               ~symbol)
          current.resolutions_to_apply
      in
      Ok changed))
;;

let observed_provider_movement_types db =
  let* rows = bank_review db in
  Ok
    (List.filter_map rows ~f:(fun r ->
       match r.imported_movement_type with
       | Some (Movement_type.Other name) -> Some name
       | _ -> None)
     |> List.dedup_and_sort ~compare:String.compare)
;;

type movement_type_history_entry =
  { id : int64
  ; event_id : int64
  ; previous_type : Movement_type.t option
  ; movement_type : Movement_type.t option
  ; reason : string
  ; changed_at : Ptime.t
  }

let movement_type_history db =
  let* rows =
    collect
      db
      (statement
         "SELECT id,movement_id,event_id,previous_type,movement_type,reason,changed_at \
          FROM movement_type_changes ORDER BY id"
         Db_schema.Movement_type_changes.row)
  in
  Ok
    (List.map rows ~f:(fun (r : Db_schema.Movement_type_changes.t) ->
       { id = r.id
       ; event_id = r.event_id
       ; previous_type = Option.map r.previous_type ~f:Movement_type.of_string
       ; movement_type = Option.map r.movement_type ~f:Movement_type.of_string
       ; reason = r.reason
       ; changed_at = r.changed_at
       }))
;;

let set_movement_type_overrides db ~event_ids ~movement_type =
  transaction db (fun db ->
    let* rows = bank_review db in
    let* () =
      if List.contains_dup event_ids ~compare:Int64.compare
      then Or_error.error_string "duplicate transaction event ID"
      else Ok ()
    in
    let* () =
      match movement_type with
      | Some (Movement_type.Other name) ->
        if
          List.exists rows ~f:(fun r ->
            Poly.equal r.imported_movement_type (Some (Movement_type.Other name)))
        then Ok ()
        else Or_error.error_string "unknown movement type; see fin tx type list"
      | _ -> Ok ()
    in
    let* changed =
      traverse
        (fun event_id ->
           let* row =
             List.find rows ~f:(fun r -> Int64.equal r.event_id event_id)
             |> Or_error.of_option ~error:(Error.of_string "unknown transaction event ID")
           in
           write_movement_type_override
             db
             ~event_id
             ~previous:row.movement_type_override
             ~movement_type
             ~reason:"Manual type override")
        event_ids
    in
    Ok (List.count changed ~f:Fn.id))
;;

let set_movement_type_override db ~event_id ~movement_type =
  let* changed = set_movement_type_overrides db ~event_ids:[ event_id ] ~movement_type in
  Ok (changed > 0)
;;

let find_account_iban =
  let open Db_schema.Bank_account_identities in
  select_by_pair table institution source_account iban
;;

(* IBANs are validated when written. A stored value that no longer parses (for
   example after a registry update) is reported, never silently ignored. *)
let registered_iban db institution source_account =
  let* stored =
    db_result (Db.first db.conn (find_account_iban institution source_account))
  in
  match stored with
  | None -> Ok None
  | Some text ->
    let* iban = Or_error.tag (Iban.of_string text) ~tag:"stored account IBAN" in
    Ok (Some iban)
;;

let account_iban db (event : Source_event.t) (tx : Movement.t) =
  let* registered = registered_iban db event.venue event.account in
  let imported =
    match tx.own_iban with
    | Known iban -> Some iban
    | Unknown | Not_applicable -> None
  in
  match imported, registered with
  | Some a, Some b when not (Iban.equal a b) ->
    Or_error.error_string "registered IBAN contradicts source account identifier"
  | Some a, _ -> Ok (Some a, "imported")
  | None, Some b -> Ok (Some b, "registered")
  | None, None ->
    Ok
      ( None
      , match tx.own_iban with
        | Not_applicable -> "not_applicable"
        | Known _ | Unknown -> "missing" )
;;

(* Every movement of an account reports the same own-IBAN status; the first
   one stands for the account. *)
let own_iban db source (account : Bank_account_mapping.t) =
  let identity = Bank_account_mapping.source_account account in
  match
    List.find source ~f:(fun (_, (e : Source_event.t)) ->
      Registry.is_movement db.registry e
      && String.equal e.venue account.institution
      && String.equal e.account identity)
  with
  | None -> Or_error.error_string "account has no imported movements"
  | Some (event_id, _) ->
    let* movement = movement db event_id in
    Ok movement.Movement.own_iban
;;

let known_account_ibans db source ~movement =
  traverse
    (fun (event_id, event) ->
       let* tx = movement event_id in
       let* iban, _ = account_iban db event tx in
       Ok (event, tx.Movement.account, iban))
    (List.dedup_and_sort source ~compare:(fun (_, a) (_, b) ->
       [%compare: string * string]
         (a.Source_event.venue, a.account)
         (b.Source_event.venue, b.account)))
;;

let transfer_iban_evidence_with known ~a_iban ~b_iban a ta b tb =
  let a_counterparty = ta.Movement.counterparty_iban
  and b_counterparty = tb.Movement.counterparty_iban in
  let assess counterparty target target_account target_iban =
    match counterparty, target_iban with
    | None, _ -> None
    | Some cp, Some own when Iban.equal cp own ->
      Some (true, "counterparty IBAN matches the other source account")
    | Some _, Some _ ->
      Some (false, "counterparty IBAN differs from the other source account's IBAN")
    | Some cp, None ->
      let conflicting =
        List.find_map known ~f:(fun (event, (account : Movement.account), iban) ->
          if
            String.equal event.Source_event.venue target.Source_event.venue
            && String.equal account.bank_id target_account.Movement.bank_id
            && String.equal account.account_id target_account.account_id
          then None
          else if Option.equal Iban.equal iban (Some cp)
          then Some event.venue
          else None)
      in
      Option.map conflicting ~f:(fun institution ->
        ( false
        , "counterparty IBAN belongs to a different known " ^ institution ^ " account" ))
  in
  let evidence =
    List.filter_opt
      [ assess a_counterparty b tb.account b_iban
      ; assess b_counterparty a ta.account a_iban
      ]
  in
  let conflicts =
    List.filter_map evidence ~f:(fun (matches, detail) ->
      if matches then None else Some detail)
  in
  Ok (List.is_empty conflicts, List.exists evidence ~f:fst, List.map evidence ~f:snd)
;;

(* Every account's IBAN as evidence for pairings. Read once per batch: it visits every
   movement. *)
let known_ibans db =
  let* source = events db in
  let source = List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e) in
  known_account_ibans db source ~movement:(movement db)
;;

let transfer_iban_evidence db known a ta b tb =
  let* a_iban, _ = account_iban db a ta in
  let* b_iban, _ = account_iban db b tb in
  transfer_iban_evidence_with known ~a_iban ~b_iban a ta b tb
;;

let transfer_decision_row =
  Row.(
    t4
      (read_column Db_schema.Bank_transfer_decisions.first_event_id)
      (read_column Db_schema.Bank_transfer_decisions.second_event_id)
      (read_column Db_schema.Bank_transfer_decisions.state)
      (read_column Db_schema.Bank_transfer_decisions.reason))
;;

let find_transfer_decision =
  Rel_sql.Stmt.(
    func
      "SELECT first_event_id,second_event_id,state,reason FROM bank_transfer_decisions \
       WHERE first_event_id=? AND second_event_id=?"
      (int64 @-> int64 @-> ret transfer_decision_row))
;;

let decode_transfer_decision (first_event_id, second_event_id, state, reason) =
  let* state = Bank_transfer.state_of_string state in
  Bank_transfer.create ~first_event_id ~second_event_id ~state ~reason
;;

let bank_transfer_decisions db =
  let* rows =
    collect
      db
      (statement
         "SELECT first_event_id,second_event_id,state,reason FROM \
          bank_transfer_decisions ORDER BY first_event_id,second_event_id"
         transfer_decision_row)
  in
  traverse decode_transfer_decision rows
;;

let verify_bank_entry db event_id event =
  let* existing =
    db_result (Db.first db.conn (find_entry event_id Bank_projection.key))
  in
  match existing with
  | None -> Ok None
  | Some (id, date, description) ->
    let* stored = read_entry db id date description in
    let* expected = mapped_bank_entry db event_id event in
    if same_entry stored expected
    then Ok (Some id)
    else
      Or_error.error_string "transfer decision conflicts with an existing bank projection"
;;
