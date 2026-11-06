open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Setup
open Sources
open Import

(* Stored movements, all of them or those of [event_ids]. *)
let movement_rows ?event_ids db =
  let select = "SELECT " ^ movement_columns ^ " FROM movements" in
  let* rows =
    collect
      db
      (match event_ids with
       | None -> statement select Db_schema.Movements.row
       | Some ids ->
         Rel_sql.Stmt.(
           func
             (select ^ " WHERE event_id IN (SELECT value FROM json_each(?))")
             (text @-> ret Db_schema.Movements.row))
           ("[" ^ String.concat ~sep:"," (List.map ids ~f:Int64.to_string) ^ "]"))
  in
  traverse
    (fun (r : Db_schema.Movements.t) ->
       let* m = movement_of_row r in
       Ok (r.event_id, m))
    rows
;;

(* Every stored movement by event ID, read in one query. *)
let movement_table db =
  let* rows = movement_rows db in
  Ok (Hashtbl.of_alist_exn (module Int64) rows)
;;

let movement db event_id =
  let* rows = movement_rows ~event_ids:[ event_id ] db in
  match rows with
  | [ (_, m) ] -> Ok m
  | _ -> Or_error.errorf "no stored movement for event %Ld" event_id
;;

let stored table event_id =
  Or_error.of_option
    (Hashtbl.find table event_id)
    ~error:(Error.createf "no stored movement for event %Ld" event_id)
;;

let event_of_row
      ({ id
       ; venue
       ; account
       ; kind
       ; source_id
       ; occurred_at = at
       ; occurred_on = on
       ; payload
       ; _
       } :
        Db_schema.Source_events.t)
  =
  let* e =
    match at, on with
    | Some occurred_at, None ->
      Source_event.create ~venue ~account ~kind ~source_id ~occurred_at ~payload ()
    | None, Some date ->
      let* occurred_on = Or_error.try_with (fun () -> Date.of_string date) in
      Source_event.create_dated ~venue ~account ~kind ~source_id ~occurred_on ~payload ()
    | _ -> Or_error.error_string "invalid source event occurrence"
  in
  Ok (id, e)
;;

type bank_review_order_column =
  | Review_date
  | Review_currency
  | Review_institution
  | Review_type
  | Review_event

let events_with_order ?order db =
  let sql =
    match order with
    | None ->
      "SELECT \
       id,venue,account,kind,source_id,occurred_at,occurred_on,retrieved_at,payload FROM \
       source_events ORDER BY coalesce(occurred_on, substr(occurred_at, 1, \
       10)),occurred_at,id"
    | Some (columns, ascending) ->
      let direction = if ascending then " ASC" else " DESC" in
      let column = function
        | Review_date -> "m.booked_on"
        | Review_currency -> "m.currency"
        | Review_institution -> "e.venue"
        | Review_type -> "coalesce(o.movement_type,m.movement_type,'-')"
        | Review_event -> "e.id"
      in
      let ordering =
        List.map (columns @ [ Review_event ]) ~f:(fun col -> column col ^ direction)
      in
      "SELECT \
       e.id,e.venue,e.account,e.kind,e.source_id,e.occurred_at,e.occurred_on,e.retrieved_at,e.payload \
       FROM source_events e LEFT JOIN movements m ON m.event_id=e.id LEFT JOIN \
       movement_type_overrides o ON o.event_id=e.id ORDER BY "
      ^ String.concat ~sep:"," ordering
  in
  let* rows = collect db (statement sql source_row) in
  let* decoded = traverse event_of_row rows in
  let* stored =
    collect
      db
      (statement
         "SELECT event_id,ordinal,name,value FROM source_event_fields ORDER BY \
          event_id,ordinal"
         Db_schema.Source_event_fields.row)
  in
  let by_event = Hashtbl.create (module Int64) in
  List.iter stored ~f:(fun (f : Db_schema.Source_event_fields.t) ->
    Hashtbl.add_multi by_event ~key:f.event_id ~data:(f.name, f.value));
  Ok
    (List.map decoded ~f:(fun (id, event) ->
       match Hashtbl.find by_event id with
       | None -> id, event
       | Some reversed -> id, Source_event.with_fields event (List.rev reversed)))
;;

let events db = events_with_order db

(* One stored source event row, as stored. *)
let stored_event_row db event_id =
  db_result
    (Db.first
       db.conn
       (Rel_sql.Stmt.(
          func
            "SELECT \
             id,venue,account,kind,source_id,occurred_at,occurred_on,retrieved_at,payload \
             FROM source_events WHERE id=?"
            (int64 @-> ret source_row))
          event_id))
;;

(* A source event's stored fields, by name, in source order. *)
let stored_event_fields db event_id =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT event_id,ordinal,name,value FROM source_event_fields WHERE event_id=? \
            ORDER BY ordinal"
           (int64 @-> ret Db_schema.Source_event_fields.row))
         event_id)
  in
  Ok (List.map rows ~f:(fun (f : Db_schema.Source_event_fields.t) -> f.name, f.value))
;;

(* One source event, if stored, without decoding the others. *)
let find_event db event_id =
  let* row = stored_event_row db event_id in
  match row with
  | None -> Ok None
  | Some row ->
    let* _, event = event_of_row row in
    let* fields = stored_event_fields db event_id in
    Ok (Some (Source_event.with_fields event fields))
;;

(* The event a saved decision names, by its identity: unique, so at most one. *)
let find_by_identity db ~venue ~account ~kind ~source_id =
  let* ids =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT id FROM source_events WHERE venue=? AND account=? AND kind=? AND \
            source_id=?"
           (text @-> text @-> text @-> text @-> ret Row.(t1 (int64 "id"))))
         venue
         account
         kind
         source_id)
  in
  let* events = traverse (fun id -> find_event db id) ids in
  Ok
    (List.filter_map (List.zip_exn ids events) ~f:(fun (id, e) ->
       Option.map e ~f:(fun e -> id, e)))
;;

(* The events of one place and time, as [Temporal.to_string] writes it: a date or an
   instant, never both. *)
let same_occurrence db ~venue ~account ~kind ~occurred =
  let date = String.length occurred = 10 in
  let* ids =
    collect
      db
      (Rel_sql.Stmt.(
         func
           (if date
            then
              "SELECT id FROM source_events WHERE venue=? AND account=? AND kind=? AND \
               occurred_on=?"
            else
              "SELECT id FROM source_events WHERE venue=? AND account=? AND kind=? AND \
               occurred_on IS NULL AND occurred_at=?")
           (text @-> text @-> text @-> text @-> ret Row.(t1 (int64 "id"))))
         venue
         account
         kind
         occurred)
  in
  let* events = traverse (fun id -> find_event db id) ids in
  Ok
    (List.filter_map (List.zip_exn ids events) ~f:(fun (id, e) ->
       Option.map e ~f:(fun e -> id, e)))
;;

let event_by_id db event_id =
  let* event = find_event db event_id in
  Or_error.of_option event ~error:(Error.of_string "unknown transaction event ID")
;;

type source_evidence_change =
  { event_id : int64
  ; institution : string
  ; description : string
  ; before : Source_evidence.t
  ; after : Source_evidence.t
  }

let preview_source_evidence ?institution db =
  let* source = events db in
  let* table = movement_table db in
  let* rows =
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
  let old = Hashtbl.create (module Int64) in
  List.iter rows ~f:(fun (id, card_reference, reversal) ->
    Hashtbl.set
      old
      ~key:id
      ~data:
        { Source_evidence.card_reference
        ; withdrawal_reversal = String.equal reversal "yes"
        });
  source
  |> List.filter ~f:(fun (_, e) ->
    Registry.is_movement db.registry e
    && Option.value_map institution ~default:true ~f:(String.equal e.Source_event.venue))
  |> traverse (fun (event_id, event) ->
    let* movement = stored table event_id in
    let* rules = source_evidence_rules db event.Source_event.venue in
    let after = Source_evidence.apply rules movement in
    let before =
      Option.value (Hashtbl.find old event_id) ~default:Source_evidence.empty
    in
    Ok
      (Option.some_if
         (not (Poly.equal before after))
         { event_id
         ; institution = event.venue
         ; description = String.concat ~sep:" | " movement.descriptions
         ; before
         ; after
         }))
  |> Result.map ~f:List.filter_opt
;;

let refresh_source_evidence ?institution db ~expected =
  transaction db (fun db ->
    Hashtbl.clear db.source_evidence_cache;
    let* current = preview_source_evidence ?institution db in
    if not (Poly.equal current expected)
    then Or_error.error_string "source evidence changed since preview; review it again"
    else
      let* () =
        traverse
          (fun change -> write_source_evidence db change.event_id change.after)
          current
        |> Result.map ~f:ignore
      in
      Ok (List.length current))
;;

let posting_row =
  Row.(
    t3
      (read_column Db_schema.Postings.account)
      (read_column Db_schema.Postings.commodity)
      (read_column Db_schema.Postings.quantity))
;;

let select_postings =
  Rel_sql.Stmt.(
    func
      "SELECT account,commodity,quantity FROM postings WHERE entry_id=? ORDER BY ordinal"
      (int64 @-> ret posting_row))
;;

let decode_entry date description rows =
  let* postings =
    traverse
      (fun (account, commodity, text) ->
         let* quantity = Or_error.try_with (fun () -> Quantity.of_string text) in
         Accounting.posting ~account ~commodity quantity)
      rows
  in
  let* date = Or_error.try_with (fun () -> Date.of_string date) in
  Accounting.entry ~date ~description postings
;;

let read_entry db id date description =
  let* rows = collect db (select_postings id) in
  decode_entry date description rows
;;

let find_entry =
  Rel_sql.Stmt.(
    func
      "SELECT id,date,description FROM journal_entries WHERE event_id=? AND \
       projection_key=?"
      (int64
       @-> text
       @-> ret
             Row.(
               t3
                 (read_column Db_schema.Journal_entries.id)
                 (read_column Db_schema.Journal_entries.date)
                 (read_column Db_schema.Journal_entries.description))))
;;

let insert_entry =
  let open Db_schema.Journal_entries in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_event_id v_projection_key v_date v_description ->
    insert
      Col.
        [ Value (event_id, v_event_id)
        ; Value (projection_key, v_projection_key)
        ; Value (date, v_date)
        ; Value (description, v_description)
        ]
;;

let insert_posting =
  let open Db_schema.Postings in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_entry_id v_ordinal v_account v_commodity v_quantity ->
    insert
      Col.
        [ Value (entry_id, v_entry_id)
        ; Value (ordinal, v_ordinal)
        ; Value (account, v_account)
        ; Value (commodity, v_commodity)
        ; Value (quantity, v_quantity)
        ]
;;

let same_entry (a : Accounting.entry) (b : Accounting.entry) =
  Date.equal a.date b.date
  && String.equal a.description b.description
  && List.length a.postings = List.length b.postings
  && List.for_all2_exn
       a.postings
       b.postings
       ~f:(fun (a : Accounting.posting) (b : Accounting.posting) ->
         String.equal a.account b.account
         && String.equal a.commodity b.commodity
         && Quantity.equal a.quantity b.quantity)
;;

let record_entry_in_transaction db ~event_id ~key (entry : Accounting.entry) =
  let* key = Validation.text "projection key" key in
  let* existing = db_result (Db.first db.conn (find_entry event_id key)) in
  match existing with
  | Some (id, date, description) ->
    let* previous = read_entry db id date description in
    if same_entry previous entry
    then Ok (Existing id)
    else
      Or_error.error_string
        "projection key already exists with different accounting entries"
  | None ->
    let* () =
      db_result
        (Db.exec
           db.conn
           (insert_entry event_id key (Date.to_string entry.date) entry.description))
    in
    let id = Db.last_insert_rowid db.conn in
    let rec write ordinal = function
      | [] -> Ok (Inserted id)
      | (p : Accounting.posting) :: rest ->
        let* () =
          db_result
            (Db.exec
               db.conn
               (insert_posting
                  id
                  ordinal
                  p.account
                  p.commodity
                  (Quantity.to_string p.quantity)))
        in
        write (ordinal + 1) rest
    in
    write 0 entry.postings
;;

let find_mapping =
  let open Db_schema.Bank_account_mappings in
  select_by_pair table institution source_account ledger_account
;;

let find_category =
  Rel_sql.Stmt.(
    func
      "SELECT account,reason FROM bank_categories WHERE event_id=?"
      (int64
       @-> ret
             Row.(
               t2
                 (read_column Db_schema.Bank_categories.account)
                 (read_column Db_schema.Bank_categories.reason))))
;;

let find_linked_transfer =
  Rel_sql.Stmt.(
    func
      "SELECT m.first_event_id,m.second_event_id,d.reason FROM bank_transfer_memberships \
       m JOIN bank_transfer_decisions d ON d.first_event_id=m.first_event_id AND \
       d.second_event_id=m.second_event_id WHERE m.event_id=? AND d.state='confirmed'"
      (int64
       @-> ret
             Row.(
               t3
                 (read_column Db_schema.Bank_transfer_memberships.first_event_id)
                 (read_column Db_schema.Bank_transfer_memberships.second_event_id)
                 (read_column Db_schema.Bank_transfer_decisions.reason))))
;;

let linked_transfer db event_id =
  db_result (Db.first db.conn (find_linked_transfer event_id))
;;

let category_account db event_id =
  let* category = db_result (Db.first db.conn (find_category event_id)) in
  Ok (Option.map category ~f:fst)
;;

let find_card_settlement =
  Rel_sql.Stmt.(
    func
      "SELECT ledger_account,reason FROM card_settlement_decisions WHERE event_id=?"
      (int64
       @-> ret
             Row.(
               t2
                 (read_column Db_schema.Card_settlement_decisions.ledger_account)
                 (read_column Db_schema.Card_settlement_decisions.reason))))
;;

let card_settlement db event_id =
  db_result (Db.first db.conn (find_card_settlement event_id))
;;

let linked_reversal db event_id =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT original,reversal,reason FROM reversal_pairs WHERE original=? OR \
         reversal=?"
        (int64
         @-> int64
         @-> ret Row.(t3 (int64 "original") (int64 "reversal") (text "reason"))))
  in
  db_result (Db.first db.conn (query event_id event_id))
;;

let counter_account db event_id =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT posting_account FROM reviews WHERE event_id=?"
        (int64 @-> ret Row.(t1 (option Rel.Type.text "posting_account"))))
  in
  let* row = db_result (Db.first db.conn (query event_id)) in
  Ok (Option.join row)
;;

let mapped_bank_entry db event_id (event : Source_event.t) =
  let* bank_account =
    db_result (Db.first db.conn (find_mapping event.venue event.account))
  in
  let* counter_account = counter_account db event_id in
  let* movement = movement db event_id in
  Bank_projection.entry ?bank_account ?counter_account ~institution:event.venue movement
;;

let record_entry db ~event_id ~key entry =
  transaction db (fun db -> record_entry_in_transaction db ~event_id ~key entry)
;;

let entries db =
  let row =
    Row.(
      t5
        (read_column Db_schema.Journal_entries.id)
        (read_column Db_schema.Journal_entries.event_id)
        (read_column Db_schema.Journal_entries.projection_key)
        (read_column Db_schema.Journal_entries.date)
        (read_column Db_schema.Journal_entries.description))
  in
  let* rows =
    collect
      db
      (statement
         "SELECT id,event_id,projection_key,date,description FROM journal_entries ORDER \
          BY date,event_id,projection_key"
         row)
  in
  let* postings =
    collect
      db
      (statement
         "SELECT entry_id,account,commodity,quantity FROM postings ORDER BY \
          entry_id,ordinal"
         Row.(t4 (int64 "entry_id") (text "account") (text "commodity") (text "quantity")))
  in
  let by_entry = Hashtbl.create (module Int64) in
  List.iter postings ~f:(fun (id, account, commodity, quantity) ->
    Hashtbl.add_multi by_entry ~key:id ~data:(account, commodity, quantity));
  traverse
    (fun (id, event_id, key, date, description) ->
       let rows = Option.value (Hashtbl.find by_entry id) ~default:[] |> List.rev in
       let* entry = decode_entry date description rows in
       Ok (event_id, key, entry))
    rows
;;

let movements db =
  let* source = events db in
  let* table = movement_table db in
  traverse
    (fun (id, event) ->
       let* movement = stored table id in
       Ok (id, event, movement))
    (List.filter source ~f:(fun (_, e) -> Registry.is_movement db.registry e))
;;

type journal_movement =
  { event_id : int64
  ; value_on : Date.t option
  ; description : string
  }

let journal_movements db =
  let* rows =
    collect
      db
      (statement
         "SELECT r.kind,r.event_id,m.value_on,r.description FROM reviews r JOIN \
          movements m ON m.event_id=r.event_id ORDER BY r.event_id"
         Row.(
           t4
             (text "kind")
             (int64 "event_id")
             (option Rel.Type.text "value_on")
             (text "description")))
  in
  List.filter rows ~f:(fun (kind, _, _, _) -> Registry.is_movement_kind db.registry kind)
  |> traverse (fun (_, event_id, value_on, description) ->
    let* value_on = Or_error.try_with (fun () -> Option.map value_on ~f:Date.of_string) in
    Ok { event_id; value_on; description })
;;

type projection_report =
  { inserted : int
  ; existing : int
  }

let project_bank db =
  transaction db (fun db ->
    let* source_events = events db in
    let rec project inserted existing = function
      | [] -> Ok { inserted; existing }
      | (event_id, (event : Source_event.t)) :: rest ->
        if not (Registry.is_movement db.registry event)
        then project inserted existing rest
        else
          let* entry = mapped_bank_entry db event_id event in
          let* result =
            record_entry_in_transaction db ~event_id ~key:Bank_projection.key entry
          in
          (match result with
           | Inserted _ -> project (inserted + 1) existing rest
           | Existing _ -> project inserted (existing + 1) rest)
    in
    project 0 0 source_events)
;;

let bank_accounts_from db source_events ~movement =
  let bank_events =
    List.filter source_events ~f:(fun (_, event) ->
      Registry.is_movement db.registry event)
    |> List.dedup_and_sort ~compare:(fun (_, a) (_, b) ->
      match String.compare a.Source_event.venue b.venue with
      | 0 -> String.compare a.account b.account
      | n -> n)
  in
  traverse
    (fun (event_id, (event : Source_event.t)) ->
       let* transaction = movement event_id in
       let account = transaction.Movement.account in
       let* mapped =
         db_result (Db.first db.conn (find_mapping event.venue event.account))
       in
       let* ledger_account =
         Or_error.of_option mapped ~error:(Error.of_string "account has no ledger name")
       in
       Bank_account_mapping.create
         ~institution:event.venue
         ~bank_id:account.bank_id
         ~account_id:account.account_id
         ~account_type:account.account_type
         ~ledger_account)
    bank_events
;;

(* Each account's first movement in listing order, among the kinds the registry reads as
   movements: the one that stands for the account, without reading every event. SQLite
   takes a row's other columns from the row its [min] picked. *)
let account_events db =
  let* rows =
    collect
      db
      (statement
         "SELECT e.id,e.venue,e.account,e.kind, \
          coalesce(e.occurred_on,substr(e.occurred_at,1,10)),e.occurred_at, \
          min(coalesce(e.occurred_on,substr(e.occurred_at,1,10))||char(31)|| \
          coalesce(e.occurred_at,'')||char(31)||printf('%019d',e.id)) FROM source_events \
          e JOIN movements m ON m.event_id=e.id GROUP BY e.venue,e.account,e.kind"
         Row.(
           unit (fun id venue account kind day at _ -> id, venue, account, kind, day, at)
           * int64 "id"
           * text "venue"
           * text "account"
           * text "kind"
           * text "day"
           * option Rel.Type.text "occurred_at"
           * text "first"))
  in
  let firsts =
    List.filter rows ~f:(fun (_, _, _, kind, _, _) ->
      Registry.is_movement_kind db.registry kind)
    |> List.sort
         ~compare:
           (fun
             (id, venue, account, _, day, at) (id', venue', account', _, day', at') ->
           [%compare: string * string * string * string option * int64]
             (venue, account, day, at, id)
             (venue', account', day', at', id'))
    |> List.remove_consecutive_duplicates
         ~which_to_keep:`First
         ~equal:(fun (_, venue, account, _, _, _) (_, venue', account', _, _, _) ->
           String.equal venue venue' && String.equal account account')
  in
  traverse
    (fun (id, _, _, _, _, _) ->
       let* event = find_event db id in
       Or_error.of_option
         (Option.map event ~f:(fun e -> id, e))
         ~error:(Error.createf "movement %Ld has no source event" id))
    firsts
;;

let bank_accounts db =
  let* source = account_events db in
  bank_accounts_from db source ~movement:(movement db)
;;

let upsert_mapping =
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       bank_account_mappings(institution,source_account,ledger_account,public_id) VALUES \
       (?,?,?,?) ON CONFLICT(institution,source_account) DO UPDATE SET \
       ledger_account=excluded.ledger_account"
      (text @-> text @-> text @-> text @-> unit))
;;

let audit_mapping =
  let open Db_schema.Bank_account_mapping_changes in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_institution v_source_account v_previous_ledger_account v_ledger_account ->
    insert
      Col.
        [ Value (institution, v_institution)
        ; Value (source_account, v_source_account)
        ; Value (previous_ledger_account, v_previous_ledger_account)
        ; Value (ledger_account, v_ledger_account)
        ]
;;

let update_posting v_account v_entry_id v_ordinal =
  let open Db_schema.Postings in
  Rel_sql.update
    Db.dialect
    table
    ~set:Col.[ Value (account, v_account) ]
    ~where:Col.[ Value (entry_id, v_entry_id); Value (ordinal, v_ordinal) ]
;;

let rename_posting account entry_id = update_posting account entry_id 0

let set_bank_account_mappings_in_transaction db mappings =
  let* mappings = Bank_account_mapping.of_json (Bank_account_mapping.to_json mappings) in
  let account_movements =
    Rel_sql.Stmt.(
      func
        "SELECT e.id,e.kind FROM source_events e JOIN movements m ON m.event_id=e.id \
         WHERE e.venue=? AND e.account=? ORDER BY \
         coalesce(e.occurred_on,substr(e.occurred_at,1,10)),e.occurred_at,e.id"
        (text @-> text @-> ret Row.(t2 (int64 "id") (text "kind"))))
  in
  let rec apply changed = function
    | [] -> Ok changed
    | (mapping : Bank_account_mapping.t) :: rest ->
      let source_account = Bank_account_mapping.source_account mapping in
      let* rows = collect db (account_movements mapping.institution source_account) in
      let* matching =
        List.filter rows ~f:(fun (_, kind) -> Registry.is_movement_kind db.registry kind)
        |> traverse (fun (id, _) ->
          let* event = find_event db id in
          Or_error.of_option
            (Option.map event ~f:(fun e -> id, e))
            ~error:(Error.createf "movement %Ld has no source event" id))
      in
      if List.is_empty matching
      then
        Or_error.error_string
          "mapping references an unknown source account; import its records first"
      else
        let* previous =
          db_result (Db.first db.conn (find_mapping mapping.institution source_account))
        in
        if Option.equal String.equal previous (Some mapping.ledger_account)
        then apply changed rest
        else
          let open Rel_sql.Stmt in
          let count =
            func
              "SELECT count(*) FROM card_settlement_decisions WHERE card_institution=? \
               AND card_source_account=?"
              (text @-> text @-> ret integer)
          in
          let* settlement_count =
            db_result (Db.first db.conn (count mapping.institution source_account))
          in
          let* () =
            if Option.value settlement_count ~default:0 > 0
            then Or_error.error_string "cannot rename a card with confirmed settlements"
            else Ok ()
          in
          let* previous_name =
            Or_error.of_option
              previous
              ~error:(Error.of_string "account has no ledger name")
          in
          let* _ =
            traverse
              (fun (event_id, (event : Source_event.t)) ->
                 let* existing =
                   db_result (Db.first db.conn (find_entry event_id Bank_projection.key))
                 in
                 match existing with
                 | None -> Ok ()
                 | Some (id, date, description) ->
                   let* stored = read_entry db id date description in
                   let* counter_account = counter_account db event_id in
                   let* movement = movement db event_id in
                   let* expected =
                     Bank_projection.entry
                       ~bank_account:previous_name
                       ?counter_account
                       ~institution:event.venue
                       movement
                   in
                   if not (same_entry stored expected)
                   then
                     Or_error.error_string
                       "account mapping conflicts with an existing bank projection"
                   else
                     db_result
                       (Db.exec db.conn (rename_posting mapping.ledger_account id)))
              matching
          in
          let* () =
            db_result
              (Db.exec
                 db.conn
                 (upsert_mapping
                    mapping.institution
                    source_account
                    mapping.ledger_account
                    (Bank_account_mapping.id mapping)))
          in
          let* () =
            db_result
              (Db.exec
                 db.conn
                 (audit_mapping
                    mapping.institution
                    source_account
                    previous
                    mapping.ledger_account))
          in
          apply (changed + 1) rest
  in
  apply 0 mappings
;;

let set_bank_account_mappings db mappings =
  transaction db (fun db -> set_bank_account_mappings_in_transaction db mappings)
;;

type companion_observation =
  { row : Companion.row
  ; matching : Bank_match.result option
  }

type companion_report =
  { matched : int
  ; unmatched : int
  ; ambiguous : int
  ; conflicts : int
  ; changed : int
  }

let csv_match_row row_id event_id state reason candidates_json policy =
  Row.(
    unit (fun row_id event_id state reason candidates policy ->
      row_id, event_id, state, reason, candidates, policy)
    * read_column row_id
    * read_column event_id
    * read_column state
    * read_column reason
    * read_column candidates_json
    * read_column policy)
;;

let decode_csv_match (row_id, event_id, state, reason, candidates, policy) =
  let* state = Bank_match.state_of_string state in
  let* candidates = Json.decode (Jsont.list Json.int64_codec) candidates in
  Ok ({ Bank_match.row_id; event_id; state; reason; candidates }, policy)
;;

(* One table holds every bank's companion observations, told apart by format. *)
(* One table holds every bank's companion observations, told apart by
   institution and format. *)
type stated_balance =
  { institution : string
  ; account : string
  ; balance_type : string
  ; commodity : Commodity.t
  ; quantity : Quantity.t
  ; as_of : Temporal.t
  }

let stated_balances ?institution db =
  let* rows =
    collect
      db
      Rel_sql.Stmt.(
        func
          "SELECT DISTINCT \
           institution,account,balance_type,commodity_json,quantity,as_of FROM \
           stated_balances ORDER BY as_of,institution,account,balance_type"
          (ret
             Row.(
               t6
                 (text "institution")
                 (text "account")
                 (text "balance_type")
                 (text "commodity_json")
                 (text "quantity")
                 (Rel.Col.make "as_of" Db_schema.statement_time Fn.id))))
  in
  traverse
    (fun (institution, account, balance_type, commodity, quantity, as_of) ->
       let* commodity = Json.decode Commodity.codec commodity in
       Ok
         { institution
         ; account
         ; balance_type
         ; commodity
         ; quantity = Quantity.of_string quantity
         ; as_of
         })
    (List.filter rows ~f:(fun (name, _, _, _, _, _) ->
       Option.for_all institution ~f:(String.equal name)))
;;

let companion_rows db ~institution ~format =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT \
            r.id,r.document_id,r.institution,r.row_number,r.fields_json,c.context_json \
            FROM companion_rows r LEFT JOIN document_contexts c ON \
            c.document_id=r.document_id WHERE r.institution=? AND r.format=? ORDER BY \
            r.document_id,r.row_number"
           (text
            @-> text
            @-> ret
                  Row.(
                    t6
                      (int64 "id")
                      (int64 "document_id")
                      (text "institution")
                      (int "row_number")
                      (text "fields_json")
                      (option Rel.Type.text "context_json"))))
         institution
         format)
  in
  traverse
    (fun (row_id, document_id, institution, row_number, fields, context) ->
       let* fields = decode_fields fields in
       let* context =
         match context with
         | None -> Ok []
         | Some json -> Json.decode Json.string_map json
       in
       Ok
         { Companion.row_id
         ; document_id
         ; institution
         ; format
         ; row_number
         ; fields
         ; context
         })
    rows
;;

let companion_matches db ~institution ~format =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT m.row_id,m.event_id,m.state,m.reason,m.candidates_json,m.policy FROM \
            companion_matches m JOIN companion_rows r ON r.id=m.row_id WHERE \
            r.institution=? AND r.format=? ORDER BY m.row_id"
           (text
            @-> text
            @-> ret
                  Db_schema.Companion_matches.(
                    csv_match_row row_id event_id state reason candidates_json policy)))
         institution
         format)
  in
  traverse decode_csv_match rows
;;

let upsert_companion_match =
  Rel_sql.Stmt.(
    func
      "INSERT INTO \
       companion_matches(row_id,event_id,state,reason,candidates_json,policy) VALUES \
       (?,?,?,?,?,?) ON CONFLICT(row_id) DO UPDATE SET \
       event_id=excluded.event_id,state=excluded.state,reason=excluded.reason,candidates_json=excluded.candidates_json,policy=excluded.policy,updated_at=(strftime('%Y-%m-%dT%H:%M:%f','now') \
       || '000000000Z')"
      (int64 @-> option int64 @-> text @-> text @-> text @-> text @-> unit))
;;

let audit_companion_match =
  let open Db_schema.Companion_match_changes in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_row_id
    v_previous_event_id
    v_event_id
    v_previous_state
    v_state
    v_reason
    v_candidates_json
    v_policy ->
    insert
      Col.
        [ Value (row_id, v_row_id)
        ; Value (previous_event_id, v_previous_event_id)
        ; Value (event_id, v_event_id)
        ; Value (previous_state, v_previous_state)
        ; Value (state, v_state)
        ; Value (reason, v_reason)
        ; Value (candidates_json, v_candidates_json)
        ; Value (policy, v_policy)
        ]
;;

let save_csv_results
      db
      ~matching_policy
      (previous_rows : (Bank_match.result * string) list)
      results
  =
  let previous = Hashtbl.create (module Int64) in
  List.iter previous_rows ~f:(fun (result, policy) ->
    Hashtbl.set previous ~key:result.row_id ~data:(result, policy));
  let equal (a : Bank_match.result) (b : Bank_match.result) =
    Option.equal Int64.equal a.event_id b.event_id
    && Poly.equal a.state b.state
    && String.equal a.reason b.reason
    && List.equal Int64.equal a.candidates b.candidates
  in
  let rec save changed = function
    | [] -> Ok changed
    | (result : Bank_match.result) :: rest ->
      let old = Hashtbl.find previous result.row_id in
      if
        Option.exists old ~f:(fun (row, policy) ->
          equal row result && String.equal policy matching_policy)
      then save changed rest
      else (
        let candidates = Json.encode (Jsont.list Json.int64_codec) result.candidates in
        let state = Bank_match.state_name result.state in
        let* () =
          db_result
            (Db.exec
               db.conn
               (upsert_companion_match
                  result.row_id
                  result.event_id
                  state
                  result.reason
                  candidates
                  matching_policy))
        in
        let* () =
          db_result
            (Db.exec
               db.conn
               (audit_companion_match
                  result.row_id
                  (Option.bind old ~f:(fun (row, _) -> row.event_id))
                  result.event_id
                  (Option.map old ~f:(fun (row, _) -> Bank_match.state_name row.state))
                  state
                  result.reason
                  candidates
                  matching_policy))
        in
        save (changed + 1) rest)
  in
  let* changed = save 0 results in
  let count state =
    List.count results ~f:(fun result -> Poly.equal result.Bank_match.state state)
  in
  Ok
    { matched = count Matched
    ; unmatched = count Unmatched
    ; ambiguous = count Ambiguous
    ; conflicts = count Conflict
    ; changed
    }
;;

(* The columns of the companion rows matched to each movement, by event, in row order. *)
let companion_columns ?institution db =
  let* matches =
    collect
      db
      (statement
         "SELECT row_id,event_id FROM companion_matches WHERE state='matched' AND \
          event_id IS NOT NULL"
         Row.(t2 (int64 "row_id") (int64 "event_id")))
  in
  let event_of = Hashtbl.of_alist_exn (module Int64) matches in
  let by_event = Hashtbl.create (module Int64) in
  let* () =
    traverse
      (fun (owner, (companion : Companion.t)) ->
         let* rows = companion_rows db ~institution:owner ~format:companion.format in
         traverse
           (fun (row : Companion.row) ->
              match Hashtbl.find event_of row.row_id with
              | None -> Ok ()
              | Some event_id ->
                let* columns = companion.fields row in
                Hashtbl.add_multi by_event ~key:event_id ~data:columns;
                Ok ())
           rows
         |> Result.map ~f:ignore)
      (registered_companions ?institution db)
    |> Result.map ~f:ignore
  in
  Ok (Hashtbl.map by_event ~f:(fun lists -> List.concat (List.rev lists)))
;;

(* A movement's type is derived: the first type rule that matches its own fields and
   the columns of its matched companion rows. Recompute it after anything that changes
   the rules or the matches. When [reason] is given, each change is also written to the
   type history. Returns how many movements changed type. *)
let retype ?institution ?reason db =
  let* source = events db in
  let source =
    List.filter source ~f:(fun (_, (e : Source_event.t)) ->
      Registry.is_movement db.registry e
      && Option.for_all institution ~f:(String.equal e.venue))
  in
  let* columns = companion_columns ?institution db in
  let* stored =
    collect
      db
      (statement
         "SELECT event_id,movement_type FROM movements"
         Row.(t2 (int64 "event_id") (option Rel.Type.text "movement_type")))
  in
  let stored = Hashtbl.of_alist_exn (module Int64) stored in
  let update =
    Rel_sql.Stmt.(
      func
        "UPDATE movements SET movement_type=? WHERE event_id=?"
        (option text @-> int64 @-> unit))
  in
  let log =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         movement_type_changes(movement_id,event_id,previous_type,movement_type,reason) \
         VALUES (?,?,?,?,?)"
        (int64 @-> int64 @-> option text @-> option text @-> text @-> unit))
  in
  let* changed =
    traverse
      (fun (event_id, (event : Source_event.t)) ->
         let* empty = Source_typing.empty event.venue in
         let* parsed = Registry.movement ~source_typing:empty db.registry event in
         let parsed =
           match Hashtbl.find columns event_id with
           | None -> parsed
           | Some more -> Movement.with_fields parsed (parsed.fields @ more)
         in
         let* profile = source_typing_profile db event.venue in
         let after =
           (Source_typing.apply profile parsed).movement_type
           |> Option.map ~f:Movement_type.to_string
         in
         let before = Option.join (Hashtbl.find stored event_id) in
         if Option.equal String.equal before after
         then Ok 0
         else
           let* () = db_result (Db.exec db.conn (update after event_id)) in
           let* () =
             match reason with
             | None -> Ok ()
             | Some reason ->
               let* movement_id = bank_primary_movement_id db event_id in
               db_result (Db.exec db.conn (log movement_id event_id before after reason))
           in
           Ok 1)
      source
  in
  Ok (List.fold changed ~init:0 ~f:( + ))
;;

let match_companions ?institution db =
  transaction db (fun db ->
    let* source = events db in
    let* reports =
      traverse
        (fun (owner, (companion : Companion.t)) ->
           let* rows = companion_rows db ~institution:owner ~format:companion.format in
           let own =
             List.filter source ~f:(fun (_, (e : Source_event.t)) ->
               String.equal e.venue owner && Registry.is_movement db.registry e)
           in
           let* results = companion.match_rows rows own in
           let* previous =
             companion_matches db ~institution:owner ~format:companion.format
           in
           save_csv_results db ~matching_policy:companion.policy previous results)
        (registered_companions ?institution db)
    in
    let* (_ : int) = retype ?institution db in
    Ok
      (List.fold
         reports
         ~init:{ matched = 0; unmatched = 0; ambiguous = 0; conflicts = 0; changed = 0 }
         ~f:(fun (a : companion_report) (b : companion_report) ->
           { matched = a.matched + b.matched
           ; unmatched = a.unmatched + b.unmatched
           ; ambiguous = a.ambiguous + b.ambiguous
           ; conflicts = a.conflicts + b.conflicts
           ; changed = a.changed + b.changed
           })))
;;

(* Types follow the rules, so a change to the rules retypes the movements it can
   affect, in a second transaction of its own. *)
let retype_after db ?institution reason =
  transaction db (fun db -> retype ?institution ~reason db) |> Result.map ~f:ignore
;;

let add_classification_rule_in_transaction = add_classification_rule

let add_classification_rule db (rule : Classification_rule.t) =
  let* () = add_classification_rule db rule in
  retype_after db ?institution:rule.institution (Printf.sprintf "rule %s added" rule.name)
;;

let replace_classification_rule db (rule : Classification_rule.t) =
  let* () = replace_classification_rule db rule in
  retype_after
    db
    ?institution:rule.institution
    (Printf.sprintf "rule %s changed" rule.name)
;;

let delete_classification_rule db name =
  let* () = delete_classification_rule db name in
  retype_after db (Printf.sprintf "rule %s deleted" name)
;;

let set_classification_rule_mode db name mode =
  let* () = set_classification_rule_mode db name mode in
  retype_after db (Printf.sprintf "rule %s mode changed" name)
;;

let rename_classification_rule db ~old_name ~new_name =
  let* () = rename_classification_rule db ~old_name ~new_name in
  retype_after db (Printf.sprintf "rule %s renamed to %s" old_name new_name)
;;

let set_institution_definition
      ?(dry_run = false)
      db
      ~name
      ~definition
      ~expected
      ~expected_rules
      ~rules
      ~source_unchanged
  =
  let* result =
    set_institution_definition
      ~dry_run
      db
      ~name
      ~definition
      ~expected
      ~expected_rules
      ~rules
      ~source_unchanged
  in
  let* () =
    if dry_run then Ok () else retype_after db ~institution:name "definition installed"
  in
  Ok result
;;

let companion_observations ?institution db =
  let* lists =
    traverse
      (fun (owner, (companion : Companion.t)) ->
         let* rows = companion_rows db ~institution:owner ~format:companion.format in
         let* matches =
           companion_matches db ~institution:owner ~format:companion.format
         in
         let matching = Hashtbl.create (module Int64) in
         List.iter matches ~f:(fun (result, _) ->
           Hashtbl.set matching ~key:result.Bank_match.row_id ~data:result);
         Ok
           (List.map rows ~f:(fun (row : Companion.row) ->
              { row; matching = Hashtbl.find matching row.row_id })))
      (registered_companions ?institution db)
  in
  Ok (List.concat lists)
;;

type journal_companions =
  { rows : (int64 * Companion.row) list
  ; total : int
  ; review : int
  }

let journal_companions db =
  let context_cache = Hashtbl.create (module Int64) in
  let* groups =
    traverse
      (fun (institution, (companion : Companion.t)) ->
         let counts =
           Rel_sql.Stmt.(
             func
               "SELECT count(*),coalesce(sum(CASE WHEN m.state IS NULL OR m.state <> \
                'matched' THEN 1 ELSE 0 END),0) FROM companion_rows r LEFT JOIN \
                companion_matches m ON m.row_id=r.id WHERE r.institution=? AND \
                r.format=?"
               (text @-> text @-> ret Row.(t2 (int "total") (int "review"))))
         in
         let* total, review =
           db_result (Db.first db.conn (counts institution companion.format))
           |> Result.map ~f:(fun value -> Option.value_exn value)
         in
         let row =
           Row.(
             unit (fun event_id row_id document_id row_number fields context ->
               event_id, row_id, document_id, row_number, fields, context)
             * int64 "event_id"
             * int64 "row_id"
             * int64 "document_id"
             * int "row_number"
             * text "fields_json"
             * option Rel.Type.text "context_json")
         in
         let query =
           Rel_sql.Stmt.(
             func
               "SELECT \
                m.event_id,r.id,r.document_id,r.row_number,r.fields_json,c.context_json \
                FROM companion_rows r JOIN companion_matches m ON m.row_id=r.id LEFT \
                JOIN document_contexts c ON c.document_id=r.document_id WHERE \
                r.institution=? AND r.format=? AND m.state='matched' AND m.event_id IS \
                NOT NULL ORDER BY r.document_id,r.row_number"
               (text @-> text @-> ret row))
         in
         let* rows = collect db (query institution companion.format) in
         let* rows =
           traverse
             (fun (event_id, row_id, document_id, row_number, fields, context) ->
                let* fields = decode_fields fields in
                let* context =
                  match Hashtbl.find context_cache document_id with
                  | Some value -> Ok value
                  | None ->
                    let* value =
                      match context with
                      | None -> Ok []
                      | Some json -> Json.decode Json.string_map json
                    in
                    Hashtbl.set context_cache ~key:document_id ~data:value;
                    Ok value
                in
                Ok
                  ( event_id
                  , { Companion.row_id
                    ; document_id
                    ; institution
                    ; format = companion.format
                    ; row_number
                    ; fields
                    ; context
                    } ))
             rows
         in
         Ok (rows, total, review))
      (registered_companions db)
  in
  Ok
    { rows = List.concat_map groups ~f:(fun (rows, _, _) -> rows)
    ; total = List.sum (module Int) groups ~f:(fun (_, total, _) -> total)
    ; review = List.sum (module Int) groups ~f:(fun (_, _, review) -> review)
    }
;;
