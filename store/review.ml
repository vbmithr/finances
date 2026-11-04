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

(* What a query needs from outside the database, as one JSON parameter: SQL reads it with
   json_each, so that a query takes one value however many rows it describes. *)
let json_text = Json.encode Jsont.string
let json_array items = "[" ^ String.concat ~sep:"," items ^ "]"

let json_object members =
  "{"
  ^ String.concat
      ~sep:","
      (List.map members ~f:(fun (name, value) -> json_text name ^ ":" ^ value))
  ^ "}"
;;

(* Where a confirmed transfer's movement goes: the other side's ledger account, which
   OCaml names unless the user mapped it. *)
let transfer_destinations db =
  let* rows =
    collect
      db
      (statement
         "SELECT t.event_id,o.venue,o.bank_id,o.account_id,o.account_type,o.currency, \
          n.ledger_account FROM bank_transfer_memberships t JOIN bank_transfer_decisions \
          d ON d.first_event_id=t.first_event_id AND d.second_event_id=t.second_event_id \
          LEFT JOIN movements o ON o.event_id=CASE WHEN t.event_id=t.first_event_id THEN \
          t.second_event_id ELSE t.first_event_id END LEFT JOIN bank_account_mappings n \
          ON n.institution=o.venue AND n.source_account=o.account WHERE \
          d.state='confirmed'"
         Row.(
           unit (fun event venue bank account kind currency mapped ->
             event, venue, bank, account, kind, currency, mapped)
           * int64 "event_id"
           * option Rel.Type.text "venue"
           * option Rel.Type.text "bank_id"
           * option Rel.Type.text "account_id"
           * option Rel.Type.text "account_type"
           * option Rel.Type.text "currency"
           * option Rel.Type.text "ledger_account"))
  in
  traverse
    (fun (event_id, venue, bank_id, account_id, account_type, currency, mapped) ->
       let* name =
         match mapped, venue, bank_id, account_id, account_type, currency with
         | Some name, _, _, _, _, _ -> Ok name
         | ( None
           , Some venue
           , Some bank_id
           , Some account_id
           , Some account_type
           , Some currency ) ->
           let* currency = Validation.currency "currency" currency in
           Bank_projection.ledger_account
             ~institution:venue
             { bank_id; account_id; account_type; currency }
         | _ -> Or_error.error_string "confirmed transfer references missing movement"
       in
       Ok (json_array [ Int64.to_string event_id; json_text name ]))
    rows
;;

(* Names a companion's OCaml code gives its matched rows. Only a CSV companion layout
   needs it; a stored record's field is read by the query itself. *)
let computed_display_names db =
  let computed =
    List.filter (registered_companions db) ~f:(fun (_, (c : Companion.t)) ->
      Poly.equal c.display_name From_annotation)
  in
  let* lists =
    traverse
      (fun (owner, (companion : Companion.t)) ->
         let* rows = companion_rows db ~institution:owner ~format:companion.format in
         let* matches =
           companion_matches db ~institution:owner ~format:companion.format
         in
         let matching = Hashtbl.create (module Int64) in
         List.iter matches ~f:(fun (result, _) ->
           match result.Bank_match.state, result.event_id with
           | Matched, Some event_id ->
             Hashtbl.set matching ~key:result.row_id ~data:event_id
           | _ -> ());
         let* names =
           traverse
             (fun (row : Companion.row) ->
                match Hashtbl.find matching row.row_id with
                | None -> Ok None
                | Some event_id ->
                  let* annotation = companion.annotate row in
                  Ok
                    (Option.map annotation.display_name ~f:(fun name ->
                       json_array
                         [ Int64.to_string event_id; json_text (String.strip name) ])))
             rows
         in
         Ok (List.filter_opt names))
      computed
  in
  Ok (List.concat lists)
;;

let review_context db =
  let* destinations = transfer_destinations db in
  let* computed = computed_display_names db in
  let display =
    List.filter_map (registered_companions db) ~f:(fun (owner, (c : Companion.t)) ->
      match c.display_name with
      | Stored_field field ->
        Some (json_array [ json_text owner; json_text c.format; json_text field ])
      | No_display_name | From_annotation -> None)
  in
  Ok
    [ "destinations", json_array destinations
    ; "computed", json_array computed
    ; "display", json_array display
    ]
;;

(* One row per bank movement: its decision, counter-account and effective description.
   The effective description is the name its matched companion rows agree on, or the
   movement's main description. The parameter is [review_context]. *)
let reviews_sql =
  "destinations(event_id,account) AS (SELECT value->>0,value->>1 FROM \
   json_each(?1,'$.destinations')), named(event_id,name) AS (SELECT \
   m.event_id,trim(f.value->>1,' '||char(9,10,11,12,13)) FROM json_each(?1,'$.display') \
   d JOIN companion_rows c ON c.institution=d.value->>0 AND c.format=d.value->>1 JOIN \
   companion_matches m ON m.row_id=c.id AND m.state='matched' AND m.event_id IS NOT NULL \
   JOIN json_each(c.fields_json->>0,'$.record.details.fields') f ON \
   f.value->>0=d.value->>2 UNION ALL SELECT value->>0,value->>1 FROM \
   json_each(?1,'$.computed')), display(event_id,name) AS MATERIALIZED (SELECT \
   event_id,CASE WHEN count(DISTINCT name)=1 THEN min(name) END FROM (SELECT \
   event_id,name FROM named WHERE name<>'' UNION ALL SELECT event_id,NULL FROM \
   movements) GROUP BY event_id), transfers(event_id,first_id,second_id,reason,account) \
   AS MATERIALIZED (SELECT \
   m.event_id,m.first_event_id,m.second_event_id,d.reason,g.account FROM \
   bank_transfer_memberships m JOIN bank_transfer_decisions d ON \
   d.first_event_id=m.first_event_id AND d.second_event_id=m.second_event_id LEFT JOIN \
   destinations g ON g.event_id=m.event_id WHERE d.state='confirmed' UNION ALL SELECT \
   own.anchor_event_id,a.anchor_event_id,b.anchor_event_id,l.reason,r.account FROM \
   operation_transfer_links l JOIN operation_movements a ON a.id=l.first_movement_id \
   JOIN operation_movements b ON b.id=l.second_movement_id JOIN operation_movements own \
   ON own.id IN (a.id,b.id) JOIN operations o ON o.id=own.operation_id AND \
   o.bank_event_id=own.anchor_event_id JOIN operation_movements other ON other.id=CASE \
   WHEN own.id=a.id THEN b.id ELSE a.id END JOIN operations oo ON \
   oo.id=other.operation_id AND oo.bank_event_id IS NULL JOIN \
   operation_movement_resolutions r ON r.movement_id=other.id), reviews AS MATERIALIZED \
   (SELECT m.event_id,m.venue AS institution,m.account AS \
   source,m.booked_on,m.amount,m.currency,e.occurred_at, \
   e.occurred_on,coalesce(n.name,m.descriptions->>('$['||m.main_description||']'),'') AS \
   description,o.movement_type AS override,coalesce(o.movement_type,m.movement_type) AS \
   movement_type,CASE WHEN r.original IS NOT NULL THEN 'reversal' WHEN t.event_id IS NOT \
   NULL THEN 'transfer' WHEN s.event_id IS NOT NULL THEN 'settlement' WHEN \
   coalesce(c.account,'equity:unclassified')<>'equity:unclassified' THEN 'category' ELSE \
   'unclassified' END AS decision,CASE WHEN r.original IS NOT NULL THEN \
   'assets:reversals:pair-'||r.original||'-'||r.reversal WHEN t.event_id IS NOT NULL \
   THEN t.account WHEN s.event_id IS NOT NULL THEN s.ledger_account ELSE \
   coalesce(c.account,'equity:unclassified') END AS counter_account, \
   coalesce(r.reason,t.reason,s.reason,c.reason) AS reason,v.card_reference, \
   v.withdrawal_reversal FROM display n JOIN movements m ON m.event_id=n.event_id JOIN \
   source_events e ON e.id=m.event_id LEFT JOIN movement_type_overrides o ON \
   o.event_id=m.event_id LEFT JOIN bank_categories c ON c.event_id=m.event_id LEFT JOIN \
   card_settlement_decisions s ON s.event_id=m.event_id LEFT JOIN transfers t ON \
   t.event_id=m.event_id LEFT JOIN reversal_pairs r ON r.original=m.event_id OR \
   r.reversal=m.event_id LEFT JOIN movement_evidence v ON v.event_id=m.event_id)"
;;

let decision_of_string = function
  | "reversal" -> Reversal
  | "transfer" -> Transfer
  | "settlement" -> Settlement
  | "category" -> Category
  | _ -> Unclassified
;;

(* The reviews [where] selects, built from one query and the stored movements. [where]
   is a condition on the reviews row [r]; the parameter also holds what it reads. *)
let select_reviews db ~context ~where ~order ~movements =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           ("WITH "
            ^ reviews_sql
            ^ " SELECT \
               r.event_id,r.institution,r.occurred_at,r.occurred_on,r.description, \
               r.override,r.decision,r.counter_account,r.reason,r.card_reference, \
               r.withdrawal_reversal FROM reviews r"
            ^ where
            ^ " ORDER BY "
            ^ order)
           (text
            @-> ret
                  Row.(
                    unit
                      (fun
                          event_id
                           institution
                           at
                           on_
                           description
                           override
                           decision
                           counter
                           reason
                           card
                           reversal
                         ->
                         ( event_id
                         , institution
                         , at
                         , on_
                         , description
                         , override
                         , decision
                         , counter
                         , reason
                         , card
                         , reversal ))
                    * int64 "event_id"
                    * text "institution"
                    * read_column Db_schema.Source_events.occurred_at
                    * read_column Db_schema.Source_events.occurred_on
                    * text "description"
                    * option Rel.Type.text "override"
                    * text "decision"
                    * option Rel.Type.text "counter_account"
                    * option Rel.Type.text "reason"
                    * option Rel.Type.text "card_reference"
                    * option Rel.Type.text "withdrawal_reversal")))
         (json_object context))
  in
  let* matched =
    collect
      db
      (statement
         "SELECT event_id,row_id FROM companion_matches WHERE state='matched' AND \
          event_id IS NOT NULL ORDER BY row_id"
         Row.(t2 (int64 "event_id") (int64 "row_id")))
  in
  let sources = Hashtbl.create (module Int64) in
  List.iter (List.rev matched) ~f:(fun (event_id, row_id) ->
    Hashtbl.add_multi sources ~key:event_id ~data:row_id);
  traverse
    (fun ( event_id
         , institution
         , at
         , on_
         , description
         , override
         , decision
         , counter
         , reason
         , card_reference
         , withdrawal_reversal ) ->
       let* (transaction : Movement.t) = stored movements event_id in
       let* occurred =
         match at, on_ with
         | Some instant, None -> Ok (Temporal.Instant instant)
         | None, Some date ->
           Or_error.try_with (fun () -> Temporal.Date (Date.of_string date))
         | _ -> Or_error.error_string "invalid source event occurrence"
       in
       let imported_movement_type = transaction.movement_type in
       let movement_type_override = Option.map override ~f:Movement_type.of_string in
       Ok
         { event_id
         ; institution
         ; occurred
         ; transaction
         ; description
         ; movement_type = Option.first_some movement_type_override imported_movement_type
         ; imported_movement_type
         ; movement_type_override
         ; source_evidence =
             { Source_evidence.card_reference
             ; withdrawal_reversal =
                 Option.value_map
                   withdrawal_reversal
                   ~default:false
                   ~f:(String.equal "yes")
             }
         ; detail_sources = Option.value (Hashtbl.find sources event_id) ~default:[]
         ; classification = decision_of_string decision
         ; counter_account = Option.value counter ~default:"equity:unclassified"
         ; reason
         })
    rows
;;

let movement_source db =
  let* source = events db in
  Ok (List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e))
;;

let bank_review_with_order ?order db =
  let order =
    match order with
    | None ->
      "coalesce(r.occurred_on,substr(r.occurred_at,1,10)),r.occurred_at,r.event_id"
    | Some (columns, ascending) ->
      let direction = if ascending then " ASC" else " DESC" in
      let column = function
        | Review_date -> "r.booked_on"
        | Review_currency -> "r.currency"
        | Review_institution -> "r.institution"
        | Review_type -> "coalesce(r.movement_type,'-')"
        | Review_event -> "r.event_id"
      in
      List.map (columns @ [ Review_event ]) ~f:(fun col -> column col ^ direction)
      |> String.concat ~sep:","
  in
  let* context = review_context db in
  let* movements = movement_table db in
  select_reviews db ~context ~where:"" ~order ~movements
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
  let* links =
    collect
      db
      (statement
         "SELECT a.operation_id,b.operation_id FROM operation_transfer_links l JOIN \
          operation_movements a ON a.id=l.first_movement_id JOIN operation_movements b \
          ON b.id=l.second_movement_id ORDER BY l.first_movement_id,l.second_movement_id"
         Row.(t2 (int64 "first_operation_id") (int64 "second_operation_id")))
  in
  let* stated =
    collect
      db
      (statement
         "SELECT e.venue,e.payload->>'$.record.group',o.id, \
          coalesce(e.payload->>'$.record.supporting',0) FROM operations o JOIN \
          source_events e ON e.id=o.bank_event_id WHERE CASE WHEN json_valid(e.payload) \
          THEN e.payload->>'$.format'='fin-records-row' AND e.payload->>'$.record.group' \
          IS NOT NULL ELSE 0 END ORDER BY o.id"
         Row.(t4 (text "venue") (text "group") (int64 "id") (int "supporting")))
  in
  let groups = Hashtbl.Poly.create () in
  List.iter stated ~f:(fun (venue, key, operation_id, supporting) ->
    Hashtbl.add_multi groups ~key:(venue, key) ~data:(supporting <> 0, operation_id));
  Operation_groups.create
    ~links
    ~stated:
      (Hashtbl.data groups
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

type operation_type_filter =
  | Untyped
  | Typed of Movement_type.t

type operation_filter =
  { only_account : (string * string) option
  ; only_institution : string option
  ; only_type : operation_type_filter option
  ; only_decision : review_classification option
  ; only_counter_account : string option
  ; from_date : Date.t option
  ; to_date : Date.t option
  ; search : string option
  }

let no_operation_filter =
  { only_account = None
  ; only_institution = None
  ; only_type = None
  ; only_decision = None
  ; only_counter_account = None
  ; from_date = None
  ; to_date = None
  ; search = None
  }
;;

type operation_order =
  | By_id
  | By_occurrence
  | By_date
  | By_amount
  | By_currency
  | By_institution
  | By_account
  | By_type
  | By_counter_account
  | By_description
  | By_event

let classification_name = function
  | Unclassified -> "unclassified"
  | Category -> "category"
  | Transfer -> "transfer"
  | Settlement -> "settlement"
  | Reversal -> "reversal"
;;

(* The operations a query can see: one row per stored operation, with its group's
   canonical ID and its primary movement's review, or the provider's own columns when
   there is none. *)
let operations_sql =
  ", grouped(member,canonical) AS MATERIALIZED (SELECT value->>0,value->>1 FROM \
   json_each(?1,'$.groups')), accounts(institution,source,id) AS MATERIALIZED (SELECT \
   value->>0,value->>1,value->>2 FROM json_each(?1,'$.accounts')), anchors AS \
   MATERIALIZED (SELECT operation_id,anchor_event_id FROM (SELECT \
   operation_id,anchor_event_id,row_number() OVER (PARTITION BY operation_id ORDER BY \
   role<>'primary',id) AS rank FROM operation_movements) WHERE rank=1), ops AS \
   MATERIALIZED (SELECT o.id,o.occurred,coalesce(g.canonical,o.id) AS canonical, \
   a.anchor_event_id AS anchor FROM operations o LEFT JOIN grouped g ON g.member=o.id \
   LEFT JOIN anchors a ON a.operation_id=o.id), venues(operation_id,venue) AS \
   MATERIALIZED (SELECT operation_id,CASE WHEN count(DISTINCT venue)=1 THEN min(venue) \
   ELSE '' END FROM (SELECT s.operation_id,e.venue FROM operation_sources s JOIN \
   source_events e ON e.id=s.event_id UNION ALL SELECT m.operation_id,e.venue FROM \
   operation_movements m JOIN source_events e ON e.id=m.anchor_event_id) GROUP BY \
   operation_id), fields(operation_id,event_id,ordinal,name,value) AS (SELECT \
   s.operation_id,f.event_id,f.ordinal,f.name,f.value FROM operation_sources s JOIN \
   source_event_fields f ON f.event_id=s.event_id), views AS (SELECT \
   p.id,p.occurred,p.canonical,p.anchor,r.event_id,r.booked_on,r.amount,r.currency, \
   r.movement_type,r.counter_account,r.description AS review_description,a.id AS \
   account_id,CASE WHEN r.event_id IS NOT NULL THEN r.institution ELSE \
   coalesce(n.venue,'') END AS institution,CASE WHEN r.event_id IS NOT NULL THEN \
   r.description ELSE coalesce((SELECT f.value FROM fields f WHERE f.operation_id=p.id \
   AND f.name='Transaction Description' ORDER BY f.event_id,f.ordinal LIMIT 1),(SELECT \
   f.value FROM fields f WHERE f.operation_id=p.id AND f.name='Description' ORDER BY \
   f.event_id,f.ordinal LIMIT 1),(SELECT f.value FROM fields f WHERE f.operation_id=p.id \
   AND f.name='description' ORDER BY f.event_id,f.ordinal LIMIT 1),'') END AS \
   description FROM ops p LEFT JOIN reviews r ON r.event_id=p.anchor LEFT JOIN accounts \
   a ON a.institution=r.institution AND a.source=r.source LEFT JOIN venues n ON \
   n.operation_id=p.id)"
;;

(* A condition on any movement of the operation's group, on its review [x]. *)
let any_movement condition =
  "v.id IN (SELECT o.canonical FROM reviews x JOIN operation_movements m ON \
   m.anchor_event_id=x.event_id JOIN ops o ON o.id=m.operation_id WHERE "
  ^ condition
  ^ ")"
;;

let operation_where (filter : operation_filter) =
  List.filter_opt
    [ Option.map filter.only_account ~f:(fun _ ->
        any_movement
          "x.institution=?1->>'$.account_institution' AND \
           x.source=?1->>'$.account_source'")
    ; Option.map filter.only_institution ~f:(fun _ ->
        "(v.institution=?1->>'$.institution' OR "
        ^ any_movement "x.institution=?1->>'$.institution'"
        ^ ")")
    ; Option.map filter.only_type ~f:(function
        | Untyped -> any_movement "x.movement_type IS NULL"
        | Typed _ -> any_movement "x.movement_type=?1->>'$.movement_type'")
    ; Option.map filter.only_decision ~f:(fun _ ->
        any_movement "x.decision=?1->>'$.decision'")
    ; Option.map filter.only_counter_account ~f:(fun _ ->
        any_movement "x.counter_account=?1->>'$.counter_account'")
    ; Option.map filter.from_date ~f:(fun _ ->
        "substr(v.occurred,1,10)>=?1->>'$.from_date'")
    ; Option.map filter.to_date ~f:(fun _ -> "substr(v.occurred,1,10)<=?1->>'$.to_date'")
    ; Option.map filter.search ~f:(fun _ ->
        "instr(lower(v.description),lower(?1->>'$.search'))>0")
    ]
;;

let operation_filter_values (filter : operation_filter) =
  let text name value = name, json_text value in
  let date name value = text name (Date.to_string value) in
  List.filter_opt
    [ Option.map filter.only_account ~f:(fun (institution, _) ->
        text "account_institution" institution)
    ; Option.map filter.only_account ~f:(fun (_, source) -> text "account_source" source)
    ; Option.map filter.only_institution ~f:(text "institution")
    ; Option.bind filter.only_type ~f:(function
        | Untyped -> None
        | Typed kind -> Some (text "movement_type" (Movement_type.to_string kind)))
    ; Option.map filter.only_decision ~f:(fun c ->
        text "decision" (classification_name c))
    ; Option.map filter.only_counter_account ~f:(text "counter_account")
    ; Option.map filter.from_date ~f:(date "from_date")
    ; Option.map filter.to_date ~f:(date "to_date")
    ; Option.map filter.search ~f:(text "search")
    ]
;;

(* Sort terms, with whether each runs against the requested direction. Rows with no
   review sort after those with one, like any column only a review has. *)
let operation_order_terms =
  let term sql = sql, false in
  function
  | By_id -> [ term "v.id" ]
  | By_occurrence -> [ term "v.occurred" ]
  | By_date -> [ term "v.event_id IS NULL"; term "v.booked_on" ]
  | By_amount -> [ term "v.event_id IS NULL"; term "v.amount COLLATE decimal" ]
  | By_currency -> [ term "v.event_id IS NULL"; term "v.currency" ]
  | By_institution -> [ term "v.institution" ]
  | By_account -> [ term "coalesce(v.account_id,'')" ]
  | By_type -> [ term "coalesce(v.movement_type,'-')" ]
  | By_counter_account ->
    [ term
        "CASE WHEN \
         coalesce(v.counter_account,'equity:unclassified')='equity:unclassified' THEN \
         '-' ELSE v.counter_account END"
    ]
  | By_description -> [ term "v.event_id IS NULL"; term "v.review_description" ]
  | By_event -> [ term "coalesce(v.event_id,9223372036854775807)" ]
;;

let operations
      ?(filter = no_operation_filter)
      ?(accounts = [])
      ?(order = [ By_id ])
      ?(ascending = true)
      ?limit
      db
  =
  let* groups = operation_groups db in
  let* grouped =
    collect db (statement "SELECT id FROM operations ORDER BY id" Row.(t1 (int64 "id")))
  in
  let group_rows =
    List.filter_map grouped ~f:(fun id ->
      let canonical = Operation_groups.canonical groups id in
      Option.some_if
        (not (Int64.equal canonical id))
        (json_array [ Int64.to_string id; Int64.to_string canonical ]))
  in
  let* context = review_context db in
  let parameter =
    context
    @ [ "groups", json_array group_rows
      ; ( "accounts"
        , json_array
            (List.map accounts ~f:(fun (institution, source, id) ->
               json_array [ json_text institution; json_text source; json_text id ])) )
      ]
    @ operation_filter_values filter
    @ Option.value_map limit ~default:[] ~f:(fun n -> [ "limit", Int.to_string n ])
  in
  let ordering =
    List.concat_map (order @ [ By_id ]) ~f:operation_order_terms
    |> List.map ~f:(fun (term, against) ->
      term ^ if Bool.equal ascending against then " DESC" else " ASC")
    |> String.concat ~sep:","
  in
  let where = String.concat ~sep:" AND " ("v.canonical=v.id" :: operation_where filter) in
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           ("WITH "
            ^ reviews_sql
            ^ operations_sql
            ^ " SELECT \
               v.id,v.occurred,v.anchor,v.institution,v.description,x.operation_id IS \
               NOT NULL AS exchange,(SELECT f.value FROM fields f WHERE \
               f.operation_id=v.id AND f.name='Amount' ORDER BY f.event_id,f.ordinal \
               LIMIT 1) AS reported_amount,(SELECT f.value FROM fields f WHERE \
               f.operation_id=v.id AND f.name='Currency' ORDER BY f.event_id,f.ordinal \
               LIMIT 1) AS reported_currency FROM views v LEFT JOIN \
               operation_exchange_decisions x ON x.operation_id=v.id WHERE "
            ^ where
            ^ " ORDER BY "
            ^ ordering
            ^ " LIMIT coalesce(?1->>'$.limit',-1)")
           (text
            @-> ret
                  Row.(
                    unit
                      (fun
                          id
                           occurred
                           anchor
                           institution
                           description
                           exchange
                           amount
                           currency
                         ->
                         ( id
                         , occurred
                         , anchor
                         , institution
                         , description
                         , exchange
                         , amount
                         , currency ))
                    * int64 "id"
                    * text "occurred"
                    * option Rel.Type.int64 "anchor"
                    * text "institution"
                    * text "description"
                    * int "exchange"
                    * option Rel.Type.text "reported_amount"
                    * option Rel.Type.text "reported_currency")))
         (json_object parameter))
  in
  (* Only what these rows show is read from here on. *)
  let chosen = List.map rows ~f:(fun (id, _, _, _, _, _, _, _) -> id) in
  let member_ids = List.concat_map chosen ~f:(Operation_groups.members groups) in
  let ids_sql ids = String.concat ~sep:"," (List.map ids ~f:Int64.to_string) in
  let* movement_rows =
    collect
      db
      (statement
         ("SELECT m.id,m.operation_id,m.anchor_event_id,m.role, \
           COALESCE(r.account,m.account) AS account,COALESCE(r.commodity,m.commodity) AS \
           commodity,m.quantity FROM operation_movements m LEFT JOIN \
           operation_movement_resolutions r ON r.movement_id=m.id WHERE m.operation_id \
           IN ("
          ^ ids_sql member_ids
          ^ ") ORDER BY m.operation_id,m.id")
         Db_schema.Operation_movements.row)
  in
  let anchors =
    List.map movement_rows ~f:(fun (m : Db_schema.Operation_movements.t) ->
      m.anchor_event_id)
    |> List.dedup_and_sort ~compare:Int64.compare
  in
  let in_anchors = "event_id IN (" ^ ids_sql anchors ^ ")" in
  let* stored_movements = Ledger.movement_rows db (" WHERE " ^ in_anchors) in
  let table = Hashtbl.of_alist_exn (module Int64) stored_movements in
  let* reviews =
    select_reviews
      db
      ~context
      ~where:(" WHERE r." ^ in_anchors)
      ~order:"r.event_id"
      ~movements:table
  in
  let review_by_event =
    Hashtbl.of_alist_exn
      (module Int64)
      (List.map reviews ~f:(fun (r : bank_review) -> r.event_id, r))
  in
  let* movements =
    traverse
      (fun (m : Db_schema.Operation_movements.t) ->
         let* commodity = Json.decode Commodity.codec m.commodity in
         let* quantity = Or_error.try_with (fun () -> Quantity.of_string m.quantity) in
         Ok
           ( m.operation_id
           , { role = m.role
             ; account = m.account
             ; commodity
             ; quantity
             ; anchor_event_id = m.anchor_event_id
             ; review = Hashtbl.find review_by_event m.anchor_event_id
             } ))
      movement_rows
  in
  let by_operation = Hashtbl.create (module Int64) in
  List.iter movements ~f:(fun (id, m) -> Hashtbl.add_multi by_operation ~key:id ~data:m);
  let of_operation id = Option.value (Hashtbl.find by_operation id) ~default:[] in
  traverse
    (fun (id, occurred, anchor, institution, description, exchange, amount, currency) ->
       let* occurred = Or_error.try_with (fun () -> Temporal.of_string_exn occurred) in
       let movements =
         List.concat_map (Operation_groups.members groups id) ~f:of_operation
       in
       let own = of_operation id in
       Ok
         { operation_id = id
         ; occurred
         ; institution
         ; description
         ; reported_amount = (if List.is_empty own then amount else None)
         ; reported_currency = (if List.is_empty own then currency else None)
         ; exchange_reviewed = exchange <> 0
         ; review = Option.bind anchor ~f:(Hashtbl.find review_by_event)
         ; movements
         })
    rows
;;

let operation_review db = operations db

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
