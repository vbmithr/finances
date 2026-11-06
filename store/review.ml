open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Setup
open Sources
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
      let rec apply changed = function
        | [] -> Ok changed
        | (d : Bank_category.t) :: rest ->
          let* event = find_event db d.event_id in
          let* event =
            Or_error.of_option event ~error:(Error.of_string "unknown category event_id")
          in
          let* transfer = linked_transfer db d.event_id in
          let linked_query =
            Rel_sql.Stmt.(
              func
                "SELECT EXISTS(SELECT 1 FROM movement_claims WHERE anchor_event_id=? AND \
                 linked)"
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

let categorized_events db =
  let* rows =
    collect
      db
      (statement "SELECT event_id FROM bank_categories" Row.(t1 (int64 "event_id")))
  in
  Ok (Set.of_list (module Int64) rows)
;;

(* Rows one command wrote follow within moments, and their reasons can differ. History says
   no more than this, so two commands within a moment read as one batch; the preview shows
   what would be undone. *)
let category_batch ?change_id db =
  let* history = bank_category_history db in
  let same (a : category_change) (b : category_change) =
    Float.( <= )
      (Float.abs (Ptime.Span.to_float_s (Ptime.diff a.changed_at b.changed_at)))
      1.
  in
  let groups = List.group history ~break:(fun a b -> not (same a b)) in
  match change_id with
  | None -> Ok (Option.value (List.last groups) ~default:[])
  | Some id ->
    List.find
      groups
      ~f:(List.exists ~f:(fun (c : category_change) -> Int64.equal c.id id))
    |> Or_error.of_option ~error:(Error.createf "no category change %Ld" id)
;;

(* Each row goes back to what it was: its earlier category and reason, or no decision at all,
   so that it is eligible for whatever an undecided row is. *)
let undo_category_batch db batch =
  transaction db (fun db ->
    let* history = bank_category_history db in
    let first = Hashtbl.create (module Int64) in
    let last = Hashtbl.create (module Int64) in
    List.iter batch ~f:(fun (c : category_change) ->
      Hashtbl.update first c.event_id ~f:(function
        | None -> c
        | Some earlier -> earlier);
      Hashtbl.set last ~key:c.event_id ~data:c);
    let earlier_reason (c : category_change) =
      List.filter history ~f:(fun (h : category_change) ->
        Int64.equal h.event_id c.event_id && Int64.( < ) h.id c.id)
      |> List.last
      |> Option.map ~f:(fun (h : category_change) -> h.reason)
    in
    let undone (c : category_change) = Printf.sprintf "Undid change %Ld" c.id in
    let* () =
      traverse
        (fun (c : category_change) ->
           let* current = db_result (Db.first db.conn (find_category c.event_id)) in
           let account = Option.value_map current ~default:"equity:unclassified" ~f:fst in
           if String.equal account c.account
           then Ok ()
           else
             Or_error.errorf
               "transaction %Ld changed after this batch; undo the later change first"
               c.event_id)
        (Hashtbl.data last)
      |> Or_error.map ~f:ignore
    in
    let* decisions =
      traverse
        (fun (c : category_change) ->
           Bank_category.create
             ~event_id:c.event_id
             ~account:(Option.value c.previous_account ~default:"equity:unclassified")
             ~reason:
               (match c.previous_account with
                | None -> undone c
                | Some _ -> Option.value (earlier_reason c) ~default:(undone c)))
        (Hashtbl.data first)
    in
    let* changed = set_bank_categories_in_transaction db decisions in
    let clear =
      Rel_sql.Stmt.(func "DELETE FROM bank_categories WHERE event_id=?" (int64 @-> unit))
    in
    let* _ =
      traverse
        (fun (c : category_change) ->
           if Option.is_none c.previous_account
           then db_result (Db.exec db.conn (clear c.event_id))
           else Ok ())
        (Hashtbl.data first)
    in
    Ok changed)
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
  let row =
    Row.(t5 (text "kind") (text "decision") (text "category") (text "type") (int "count"))
  in
  let* groups = collect db (statement Sql.decision_counts row) in
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

(* Values a query reads as one JSON parameter, however many there are. *)
let json_text = Json.encode Jsont.string
let json_array items = "[" ^ String.concat ~sep:"," items ^ "]"
let json_ids ids = json_array (List.map ids ~f:Int64.to_string)

let json_object members =
  "{"
  ^ String.concat
      ~sep:","
      (List.map members ~f:(fun (name, value) -> json_text name ^ ":" ^ value))
  ^ "}"
;;

let decision_of_string = function
  | "reversal" -> Reversal
  | "transfer" -> Transfer
  | "settlement" -> Settlement
  | "category" -> Category
  | _ -> Unclassified
;;

(* A row of [Sql.reviews] or [Sql.event_reviews]. *)
let review_row =
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
    * option Rel.Type.text "withdrawal_reversal")
;;

(* The reviews [query] selects, completed by the stored movements. *)
let select_reviews db query ~movements =
  let* rows = collect db query in
  let* matched =
    collect db (statement Sql.matched_rows Row.(t2 (int64 "event_id") (int64 "row_id")))
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
    | None -> "r.listed_on,r.occurred_at,r.event_id"
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
  let* movements = movement_table db in
  select_reviews db (statement (Sql.reviews ^ " ORDER BY " ^ order) review_row) ~movements
;;

let bank_review db = bank_review_with_order db

let reviews_of_events db event_ids =
  let* stored = Ledger.movement_rows ~event_ids db in
  select_reviews
    db
    (Rel_sql.Stmt.(func Sql.event_reviews (text @-> ret review_row)) (json_ids event_ids))
    ~movements:(Hashtbl.of_alist_exn (module Int64) stored)
;;

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
  let* rows =
    collect
      db
      (statement
         "SELECT \
          l.first_event_id,l.second_event_id,l.first_operation_id,l.second_operation_id, \
          l.reason,l.first_role,l.second_role,a.venue,a.account,a.kind,a.source_id, \
          b.venue,b.account,b.kind,b.source_id FROM operation_links l LEFT JOIN \
          source_events a ON a.id=l.first_event_id LEFT JOIN source_events b ON \
          b.id=l.second_event_id WHERE (SELECT count(*) FROM \
          operation_transfer_memberships m WHERE m.first_movement_id=l.first_movement_id \
          AND m.second_movement_id=l.second_movement_id AND m.movement_id IN \
          (l.first_movement_id,l.second_movement_id))=2 ORDER BY \
          l.first_movement_id,l.second_movement_id"
         Row.(
           unit (fun a b c d e f g h i j k l m n o ->
             (a, b, c, d, e, f, g), (h, i, j, k), (l, m, n, o))
           * int64 "first_event_id"
           * int64 "second_event_id"
           * int64 "first_operation_id"
           * int64 "second_operation_id"
           * text "reason"
           * text "first_role"
           * text "second_role"
           * option Rel.Type.text "first_venue"
           * option Rel.Type.text "first_account"
           * option Rel.Type.text "first_kind"
           * option Rel.Type.text "first_source_id"
           * option Rel.Type.text "second_venue"
           * option Rel.Type.text "second_account"
           * option Rel.Type.text "second_kind"
           * option Rel.Type.text "second_source_id"))
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
  let identity which ~role = function
    | Some venue, Some source_account, Some source_kind, Some source_id ->
      Movement_identity.create ~venue ~source_account ~source_kind ~source_id ~role
    | _ -> Or_error.errorf "confirmed transfer has no %s source" which
  in
  traverse
    (fun ( ( first_event
           , second_event
           , first_operation_id
           , second_operation_id
           , reason
           , first_role
           , second_role )
         , first
         , second ) ->
       let* first_movement = identity "first" ~role:first_role first in
       let* second_movement = identity "second" ~role:second_role second in
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

let operation_filter_values (filter : operation_filter) =
  let text name value = name, json_text value in
  let date name value = text name (Date.to_string value) in
  List.filter_opt
    [ Option.map filter.only_account ~f:(fun (institution, _) ->
        text "account_institution" institution)
    ; Option.map filter.only_account ~f:(fun (_, source) -> text "account_source" source)
    ; Option.map filter.only_institution ~f:(text "institution")
    ; Option.map filter.only_type ~f:(function
        | Untyped -> text "movement_type" "-"
        | Typed kind -> text "movement_type" (Movement_type.to_string kind))
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
      ?(order = [ By_id ])
      ?(ascending = true)
      ?limit
      db
  =
  let parameter =
    operation_filter_values filter
    @ Option.value_map limit ~default:[] ~f:(fun n -> [ "limit", Int.to_string n ])
  in
  let ordering =
    List.concat_map (order @ [ By_id ]) ~f:operation_order_terms
    |> List.map ~f:(fun (term, against) ->
      term ^ if Bool.equal ascending against then " DESC" else " ASC")
    |> String.concat ~sep:","
  in
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           (Sql.operations
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
  let* movement_rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           Sql.group_movements
           (text
            @-> ret
                  Row.(
                    unit
                      (fun
                          canonical operation_id anchor role account commodity quantity ->
                         ( canonical
                         , operation_id
                         , anchor
                         , role
                         , account
                         , commodity
                         , quantity ))
                    * int64 "canonical"
                    * int64 "operation_id"
                    * int64 "anchor_event_id"
                    * text "role"
                    * option Rel.Type.text "account"
                    * text "commodity"
                    * text "quantity")))
         (json_ids chosen))
  in
  let anchors =
    List.map movement_rows ~f:(fun (_, _, anchor, _, _, _, _) -> anchor)
    |> List.dedup_and_sort ~compare:Int64.compare
  in
  let* reviews = reviews_of_events db anchors in
  let review_by_event =
    Hashtbl.of_alist_exn
      (module Int64)
      (List.map reviews ~f:(fun (r : bank_review) -> r.event_id, r))
  in
  let* movements =
    traverse
      (fun (canonical, operation_id, anchor_event_id, role, account, commodity, quantity) ->
         let* commodity = Json.decode Commodity.codec commodity in
         let* quantity = Or_error.try_with (fun () -> Quantity.of_string quantity) in
         Ok
           ( canonical
           , operation_id
           , { role
             ; account
             ; commodity
             ; quantity
             ; anchor_event_id
             ; review = Hashtbl.find review_by_event anchor_event_id
             } ))
      movement_rows
  in
  let by_group = Hashtbl.create (module Int64) in
  List.iter (List.rev movements) ~f:(fun (canonical, operation_id, m) ->
    Hashtbl.add_multi by_group ~key:canonical ~data:(operation_id, m));
  traverse
    (fun (id, occurred, anchor, institution, description, exchange, amount, currency) ->
       let* occurred = Or_error.try_with (fun () -> Temporal.of_string_exn occurred) in
       let movements = Option.value (Hashtbl.find by_group id) ~default:[] in
       let own =
         List.exists movements ~f:(fun (operation_id, _) -> Int64.equal operation_id id)
       in
       Ok
         { operation_id = id
         ; occurred
         ; institution
         ; description
         ; reported_amount = (if own then None else amount)
         ; reported_currency = (if own then None else currency)
         ; exchange_reviewed = exchange <> 0
         ; review = Option.bind anchor ~f:(Hashtbl.find review_by_event)
         ; movements = List.map movements ~f:snd
         })
    rows
;;

let operation_review db = operations db

type unmatched_transfer =
  { event_id : int64
  ; institution : string
  ; booked_on : Date.t
  ; principal : Quantity.t
  ; currency : string
  ; description : string
  ; kind : string
  ; related : int64 option
  ; related_decision : string option
  ; days : int option
  ; difference : Quantity.t option
  }

(* See [Sql.unmatched_transfers]. [offered] are the event IDs of current candidates. *)
let unmatched_transfers db ~offered =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           Sql.unmatched_transfers
           (text
            @-> ret
                  Row.(
                    unit
                      (fun
                          event_id
                           institution
                           booked_on
                           sign
                           magnitude
                           currency
                           description
                           kind
                           related
                           related_decision
                           days
                           difference
                         ->
                         ( event_id
                         , institution
                         , booked_on
                         , sign
                         , magnitude
                         , currency
                         , description
                         , kind
                         , related
                         , related_decision
                         , days
                         , difference ))
                    * int64 "event_id"
                    * text "institution"
                    * text "booked_on"
                    * text "principal_sign"
                    * text "principal_magnitude"
                    * text "currency"
                    * text "description"
                    * text "kind"
                    * option Rel.Type.int64 "related"
                    * option Rel.Type.text "related_decision"
                    * option Rel.Type.int "days"
                    * option Rel.Type.text "difference")))
         (json_ids offered))
  in
  traverse
    (fun ( event_id
         , institution
         , booked_on
         , sign
         , magnitude
         , currency
         , description
         , kind
         , related
         , related_decision
         , days
         , difference ) ->
       Or_error.try_with (fun () ->
         let magnitude = Quantity.of_string magnitude in
         { event_id
         ; institution
         ; booked_on = Date.of_string booked_on
         ; principal =
             (if String.equal sign "-" then Quantity.neg magnitude else magnitude)
         ; currency
         ; description
         ; kind
         ; related
         ; related_decision
         ; days
         ; difference = Option.map difference ~f:Quantity.of_string
         }))
    rows
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
          "SELECT id,bank_event_id,linked OR exchanged FROM movement_claims WHERE \
           operation_id=? AND role=?"
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
