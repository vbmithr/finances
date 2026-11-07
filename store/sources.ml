open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Setup

let source_row = Db_schema.Source_events.row

let find_source =
  let open Db_schema.Source_events in
  let open Rel_query.Syntax in
  let select =
    Rel_query.Sql.(
      func
        (text
         @-> text
         @-> text
         @-> text
         @-> ret row (fun v_source_id v_kind v_account v_venue ->
           let* r = Bag.table table in
           Bag.where
             (Text.(r#.venue = v_venue)
              && Text.(r#.account = v_account)
              && Text.(r#.kind = v_kind)
              && Text.(r#.source_id = v_source_id))
             (Bag.yield r))))
  in
  select
;;

let insert_source =
  let open Db_schema.Source_events in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_venue v_account v_kind v_source_id v_occurred_at v_occurred_on v_payload ->
    insert
      Col.
        [ Value (venue, v_venue)
        ; Value (account, v_account)
        ; Value (kind, v_kind)
        ; Value (source_id, v_source_id)
        ; Value (occurred_at, v_occurred_at)
        ; Value (occurred_on, v_occurred_on)
        ; Value (payload, v_payload)
        ]
;;

let name_account =
  Rel_sql.Stmt.(
    func
      "INSERT OR IGNORE INTO \
       bank_account_mappings(institution,source_account,ledger_account,public_id) VALUES \
       (?,?,?,?)"
      (text @-> text @-> text @-> text @-> unit))
;;

let insert_movement =
  let open Db_schema.Movements in
  let insert = Rel_sql.insert_into_cols Db.dialect table in
  fun v_event_id (e : Source_event.t) (m : Movement.t) ->
    let principal = Movement.principal m in
    let negative = Quantity.(principal < zero) in
    let status, iban =
      match m.own_iban with
      | Known iban -> "known", Some (Iban.to_string iban)
      | Unknown -> "unknown", None
      | Not_applicable -> "not_applicable", None
    in
    insert
      Col.
        [ Value (event_id, v_event_id)
        ; Value (venue, e.venue)
        ; Value (account, e.account)
        ; Value (bank_id, m.account.bank_id)
        ; Value (account_id, m.account.account_id)
        ; Value (account_type, m.account.account_type)
        ; Value (currency, Iso4217.to_alpha_code m.account.currency)
        ; Value (own_iban_status, status)
        ; Value (own_iban, iban)
        ; Value (booked_on, Date.to_string m.booked_on)
        ; Value (value_on, Option.map m.value_on ~f:Date.to_string)
        ; Value (amount, Quantity.to_string m.amount)
        ; Value (fee, Quantity.to_string m.fee)
        ; Value
            ( principal_magnitude
            , Quantity.to_string (if negative then Quantity.neg principal else principal)
            )
        ; Value (principal_sign, if negative then "-" else "+")
        ; Value (counterparty_iban, Option.map m.counterparty_iban ~f:Iban.to_string)
        ; Value (descriptions, Json.encode (Jsont.list Jsont.string) m.descriptions)
        ; Value
            ( Db_schema.Movements.movement_type
            , Option.map m.movement_type ~f:Movement_type.to_string )
        ; Value (Db_schema.Movements.main_description, m.main_description)
        ]
;;

(* Every column or tag the source carried, in order, beside the event. *)
let store_event_fields db event_id fields =
  let insert =
    Rel_sql.Stmt.(
      func
        "INSERT INTO source_event_fields(event_id,ordinal,name,value) VALUES (?,?,?,?)"
        (int64 @-> int @-> text @-> text @-> unit))
  in
  List.mapi fields ~f:(fun ordinal (name, value) -> ordinal, name, value)
  |> List.fold_result ~init:() ~f:(fun () (ordinal, name, value) ->
    db_result (Db.exec db.conn (insert event_id ordinal name value)))
;;

(* Preserve the existing one-record bank interpretation as an operation. The
   account mapping remains authoritative and is deliberately not copied here. *)
let store_bank_operation db event_id (event : Source_event.t) (movement : Movement.t) =
  let insert_operation =
    Rel_sql.Stmt.(
      func
        "INSERT INTO operations(occurred,bank_event_id,source_event_id) VALUES (?,?,?)"
        (text @-> int64 @-> int64 @-> unit))
  in
  let insert_source =
    Rel_sql.Stmt.(
      func
        "INSERT INTO operation_sources(operation_id,event_id) VALUES (?,?)"
        (int64 @-> int64 @-> unit))
  in
  let insert_movement =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         operation_movements(operation_id,anchor_event_id,role,account,commodity,quantity) \
         VALUES (?,?,'primary',NULL,?,?)"
        (int64 @-> int64 @-> text @-> text @-> unit))
  in
  let insert_evidence =
    Rel_sql.Stmt.(
      func
        "INSERT INTO operation_movement_sources(movement_id,operation_id,event_id) \
         VALUES (?,?,?)"
        (int64 @-> int64 @-> int64 @-> unit))
  in
  let* () =
    db_result
      (Db.exec
         db.conn
         (insert_operation (Temporal.to_string event.occurred) event_id event_id))
  in
  let operation_id = Db.last_insert_rowid db.conn in
  let* () = db_result (Db.exec db.conn (insert_source operation_id event_id)) in
  let commodity =
    Commodity.fiat movement.account.currency |> Json.encode Commodity.codec
  in
  let* () =
    db_result
      (Db.exec
         db.conn
         (insert_movement
            operation_id
            event_id
            commodity
            (Quantity.to_string movement.amount)))
  in
  let movement_id = Db.last_insert_rowid db.conn in
  db_result (Db.exec db.conn (insert_evidence movement_id operation_id event_id))
;;

let bank_primary_movement_id db event_id =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT id FROM operation_movements WHERE anchor_event_id=? AND role='primary'"
        (int64 @-> ret Row.(t1 (int64 "id"))))
  in
  let* id = db_result (Db.first db.conn (query event_id)) in
  Or_error.of_option id ~error:(Error.of_string "bank primary movement is missing")
;;

(* The movement stored at import, rebuilt and validated again. *)
let movement_of_row (r : Db_schema.Movements.t) =
  Or_error.tag
    ~tag:(Printf.sprintf "stored movement %Ld" r.event_id)
    (let* currency = Validation.currency "currency" r.currency in
     let iban label s =
       Result.map_error (Iban.of_string s) ~f:(fun e -> Error.tag e ~tag:label)
     in
     let* own_iban =
       match r.own_iban_status, r.own_iban with
       | "known", Some s -> Result.map (iban "own IBAN" s) ~f:(fun i -> Movement.Known i)
       | "unknown", None -> Ok Movement.Unknown
       | "not_applicable", None -> Ok Movement.Not_applicable
       | _ -> Or_error.error_string "inconsistent own IBAN status"
     in
     let* counterparty_iban =
       match r.counterparty_iban with
       | None -> Ok None
       | Some s -> Result.map (iban "counterparty IBAN" s) ~f:Option.some
     in
     let* descriptions = Json.decode (Jsont.list Jsont.string) r.descriptions in
     let* booked_on, value_on, amount, fee =
       Or_error.try_with (fun () ->
         ( Date.of_string r.booked_on
         , Option.map r.value_on ~f:Date.of_string
         , Quantity.of_string r.amount
         , Quantity.of_string r.fee ))
     in
     Movement.create
       ~account:
         { bank_id = r.bank_id
         ; account_id = r.account_id
         ; account_type = r.account_type
         ; currency
         }
       ~own_iban
       ~booked_on
       ?value_on
       ~amount
       ~fee
       ?counterparty_iban
       ?movement_type:(Option.map r.movement_type ~f:Movement_type.of_string)
       ~main_description:r.main_description
       descriptions)
;;

let movement_columns =
  "event_id,venue,account,bank_id,account_id,account_type,currency,own_iban_status,own_iban,booked_on,value_on,amount,fee,principal_magnitude,principal_sign,counterparty_iban,descriptions,movement_type,main_description"
;;

let occurrence_columns (e : Source_event.t) =
  match e.occurred with
  | Temporal.Instant instant -> Some instant, None
  | Temporal.Date date -> None, Some (Date.to_string date)
;;

let source_typing_profile db institution =
  match Hashtbl.find db.source_typing_cache institution with
  | Some profile -> Ok profile
  | None ->
    let query =
      Rel_sql.Stmt.(
        func
          "SELECT pattern,account_type,case_mode,CASE WHEN action_kind='type-category' \
           THEN action_type ELSE target END,match_type,field FROM classification_rules \
           WHERE mode='automatic' AND action_kind IN ('type','type-category') AND \
           (institution IS NULL OR institution=?) ORDER BY priority,name"
          (text
           @-> ret
                 Row.(
                   unit (fun a b c d e f -> a, b, c, d, e, f)
                   * option Rel.Type.text "pattern"
                   * option Rel.Type.text "account_type"
                   * text "case_mode"
                   * text "target"
                   * option Rel.Type.text "match_type"
                   * option Rel.Type.text "field")))
    in
    let* rows =
      db_result (Db.fold db.conn (query institution) (fun row acc -> row :: acc) [])
    in
    let* rules =
      List.rev rows
      |> List.map ~f:(fun (pattern, account_type, case_mode, target, match_type, field) ->
        Result.map
          (Source_typing.make_rule
             ?field
             (Option.value pattern ~default:".*")
             account_type
             (String.equal case_mode "sensitive")
             target)
          ~f:(fun r -> Source_typing.with_match_type r match_type))
      |> Or_error.combine_errors
    in
    let profile = Source_typing.create ~institution rules in
    Hashtbl.set db.source_typing_cache ~key:institution ~data:profile;
    Ok profile
;;

let source_evidence_rules db institution =
  match Hashtbl.find db.source_evidence_cache institution with
  | Some rules -> Ok rules
  | None ->
    let query =
      Rel_sql.Stmt.(
        func
          "SELECT pattern,account_type,case_mode,action_kind,match_type FROM \
           classification_rules WHERE mode='automatic' AND action_kind IN \
           ('card-reference','withdrawal-reversal') AND (institution IS NULL OR \
           institution=?) ORDER BY priority,name"
          (text
           @-> ret
                 Row.(
                   t5
                     (text "pattern")
                     (option Rel.Type.text "account_type")
                     (text "case_mode")
                     (text "action_kind")
                     (option Rel.Type.text "match_type"))))
    in
    let* rows =
      db_result (Db.fold db.conn (query institution) (fun row acc -> row :: acc) [])
    in
    let* rules =
      List.rev rows
      |> List.map ~f:(fun (pattern, account_type, case_mode, kind, match_type) ->
        let action =
          if String.equal kind "card-reference"
          then Source_evidence.Card_reference
          else Withdrawal_reversal
        in
        Source_evidence.make_rule
          ~action
          ~pattern
          ~case_sensitive:(String.equal case_mode "sensitive")
          ~account_type
          ~match_type)
      |> Or_error.combine_errors
    in
    Hashtbl.set db.source_evidence_cache ~key:institution ~data:rules;
    Ok rules
;;

let write_source_evidence db event_id (evidence : Source_evidence.t) =
  let query =
    Rel_sql.Stmt.(
      func
        "INSERT INTO movement_evidence(event_id,card_reference,withdrawal_reversal) \
         VALUES (?,?,?) ON CONFLICT(event_id) DO UPDATE SET \
         card_reference=excluded.card_reference,withdrawal_reversal=excluded.withdrawal_reversal"
        (int64 @-> option text @-> text @-> unit))
  in
  db_result
    (Db.exec
       db.conn
       (query
          event_id
          evidence.card_reference
          (if evidence.withdrawal_reversal then "yes" else "no")))
;;

let apply_automatic_category
      db
      event_id
      (event : Source_event.t)
      (raw : Movement.t)
      (movement : Movement.t)
  =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT name,pattern,account_type,case_mode,target,match_type,action_kind FROM \
         classification_rules WHERE mode='automatic' AND action_kind IN \
         ('category','type-category') AND (institution IS NULL OR institution=?) ORDER \
         BY priority,name"
        (text
         @-> ret
               Row.(
                 unit (fun a b c d e f g -> a, b, c, d, e, f, g)
                 * text "name"
                 * option Rel.Type.text "pattern"
                 * option Rel.Type.text "account_type"
                 * text "case_mode"
                 * text "target"
                 * option Rel.Type.text "match_type"
                 * text "action_kind")))
  in
  let* rows =
    db_result (Db.fold db.conn (query event.venue) (fun row acc -> row :: acc) [])
  in
  let description =
    Option.value_map (List.hd movement.descriptions) ~default:"" ~f:String.strip
  in
  let rec select = function
    | [] -> Ok ()
    | (name, pattern, account_type, case_mode, target, match_type, action_kind) :: rest ->
      let* pattern_matches =
        match pattern with
        | None -> Ok true
        | Some text ->
          Result.map
            (Transaction_pattern.compile
               ~case_sensitive:(String.equal case_mode "sensitive")
               text)
            ~f:(fun p ->
              Transaction_pattern.matches
                p
                ~display_name:None
                ~descriptions:[ description ])
      in
      let matched_type =
        if String.equal action_kind "type-category"
        then raw.movement_type
        else movement.movement_type
      in
      let type_matches =
        Option.value_map match_type ~default:true ~f:(fun expected ->
          match expected, matched_type with
          | "unknown", None -> true
          | "unknown", Some _ -> false
          | _, Some kind -> String.equal expected (Movement_type.to_string kind)
          | _, None -> false)
      in
      if
        not
          (pattern_matches
           && type_matches
           && Option.value_map
                account_type
                ~default:true
                ~f:(String.equal movement.account.account_type))
      then select rest
      else (
        let reason = "Automatic rule " ^ name in
        let* movement_id = bank_primary_movement_id db event_id in
        let insert =
          Rel_sql.Stmt.(
            func
              "INSERT INTO bank_categories(movement_id,event_id,account,reason) VALUES \
               (?,?,?,?)"
              (int64 @-> int64 @-> text @-> text @-> unit))
        in
        let audit =
          Rel_sql.Stmt.(
            func
              "INSERT INTO \
               bank_category_changes(movement_id,event_id,previous_account,account,reason) \
               VALUES (?,?,NULL,?,?)"
              (int64 @-> int64 @-> text @-> text @-> unit))
        in
        let catalog =
          Rel_sql.Stmt.(
            func
              "INSERT OR IGNORE INTO category_catalog(account) VALUES (?)"
              (text @-> unit))
        in
        let* () = db_result (Db.exec db.conn (catalog target)) in
        let* () =
          db_result (Db.exec db.conn (insert movement_id event_id target reason))
        in
        db_result (Db.exec db.conn (audit movement_id event_id target reason)))
  in
  select (List.rev rows)
;;

let add_operation_with_sources ?source_event_id db by_id (operation : Operation.t) =
  let* source_ids = traverse operation_event_id operation.evidence in
  let* () =
    match source_event_id with
    | Some id when not (List.mem source_ids id ~equal:Int64.equal) ->
      Or_error.error_string "operation source event must be part of its evidence"
    | _ -> Ok ()
  in
  let* () =
    List.map source_ids ~f:(fun id ->
      if Hashtbl.mem by_id id
      then Ok ()
      else Or_error.error_string "unknown operation source event")
    |> Or_error.combine_errors_unit
  in
  let insert_operation =
    Rel_sql.Stmt.(
      func
        "INSERT INTO operations(occurred,source_event_id) VALUES (?,?)"
        (text @-> option int64 @-> unit))
  in
  let insert_source =
    Rel_sql.Stmt.(
      func
        "INSERT INTO operation_sources(operation_id,event_id) VALUES (?,?)"
        (int64 @-> int64 @-> unit))
  in
  let insert_movement =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         operation_movements(operation_id,anchor_event_id,role,account,commodity,quantity) \
         VALUES (?,?,?,?,?,?)"
        (int64 @-> int64 @-> text @-> option text @-> text @-> text @-> unit))
  in
  let insert_movement_source =
    Rel_sql.Stmt.(
      func
        "INSERT INTO operation_movement_sources(movement_id,operation_id,event_id) \
         VALUES (?,?,?)"
        (int64 @-> int64 @-> int64 @-> unit))
  in
  let* () =
    db_result
      (Db.exec
         db.conn
         (insert_operation (Temporal.to_string operation.occurred) source_event_id))
  in
  let operation_id = Db.last_insert_rowid db.conn in
  let* () =
    List.fold_result source_ids ~init:() ~f:(fun () id ->
      db_result (Db.exec db.conn (insert_source operation_id id)))
  in
  let* () =
    List.fold_result operation.movements ~init:() ~f:(fun () movement ->
      let* identity =
        Or_error.of_option
          movement.Operation.identity
          ~error:(Error.of_string "persisted movement needs a stable source identity")
      in
      let* movement_sources = traverse operation_event_id movement.evidence in
      let anchors =
        List.filter movement_sources ~f:(fun id ->
          Option.value_map (Hashtbl.find by_id id) ~default:false ~f:(fun event ->
            match Movement_identity.of_event event ~role:identity.role with
            | Ok actual -> Movement_identity.equal actual identity
            | Error _ -> false))
      in
      let* anchor =
        match anchors with
        | [ id ] -> Ok id
        | [] ->
          Or_error.error_string "movement identity is not backed by its source evidence"
        | _ -> Or_error.error_string "movement identity matches multiple source events"
      in
      let* () =
        db_result
          (Db.exec
             db.conn
             (insert_movement
                operation_id
                anchor
                identity.role
                movement.account
                (Json.encode Commodity.codec movement.commodity)
                (Quantity.to_string movement.quantity)))
      in
      let movement_id = Db.last_insert_rowid db.conn in
      List.fold_result movement_sources ~init:() ~f:(fun () id ->
        db_result (Db.exec db.conn (insert_movement_source movement_id operation_id id))))
  in
  Ok operation_id
;;

(* The operation an adapter proposes for a non-movement event, decoded from
   the stored event alone. *)
let derive_operation_event db ~event_id (event : Source_event.t) =
  let* rows = Registry.operation_rows db.registry event in
  match rows with
  | [ row ] ->
    let evidence = [ Operation.Stored_event event_id ] in
    let* movements =
      traverse
        (fun (leg : Operation_import.leg) ->
           let* identity =
             Movement_identity.of_event row.Operation_import.source ~role:leg.role
           in
           Operation.movement
             ~identity
             ~commodity:leg.commodity
             ~quantity:leg.quantity
             ~evidence
             ())
        row.legs
    in
    let* operation =
      Operation.create ~occurred:row.source.occurred ~evidence ~movements
    in
    Ok (row, operation)
  | _ -> Or_error.error_string "operation adapters must propose one row per event"
;;

(* A non-movement event whose adapter interprets it as an operation with
   role-labelled movements: decode the proposal from the stored event and
   persist it like any other operation. *)
let persist_operation_event db ~event_id (event : Source_event.t) =
  let* _, operation = derive_operation_event db ~event_id event in
  let by_id = Hashtbl.of_alist_exn (module Int64) [ event_id, event ] in
  let* _ = add_operation_with_sources ~source_event_id:event_id db by_id operation in
  Ok ()
;;

(* A replayed operation event must still match what is stored for it: the
   occurrence, and each movement's role, commodity, quantity and source link.
   Stored decisions are never touched; a discrepancy is an error. *)
let verify_stored_operation_event db ~event_id (event : Source_event.t) =
  let* _, derived = derive_operation_event db ~event_id event in
  let mismatch what =
    Or_error.errorf
      "stored operation for this source row differs from the importer's interpretation \
       (%s); the database or the importer changed, rebuild or review the stored \
       operation before reimporting"
      what
  in
  let* operations =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT id,occurred FROM operations WHERE source_event_id=?"
           (int64 @-> ret Row.(t2 (int64 "id") (text "occurred"))))
         event_id)
  in
  match operations with
  | [ (operation_id, occurred) ] ->
    if not (String.equal occurred (Temporal.to_string derived.Operation.occurred))
    then mismatch "occurrence"
    else
      let* stored =
        collect
          db
          (Rel_sql.Stmt.(
             func
               "SELECT m.id,m.role,m.commodity,m.quantity FROM operation_movements m \
                WHERE m.operation_id=? ORDER BY m.id"
               (int64
                @-> ret
                      Row.(
                        t4 (int64 "id") (text "role") (text "commodity") (text "quantity"))
               ))
             operation_id)
      in
      let* stored =
        traverse
          (fun (id, role, commodity, quantity) ->
             let* commodity = Json.decode Commodity.codec commodity in
             let* quantity = Or_error.try_with (fun () -> Quantity.of_string quantity) in
             let* sources =
               collect
                 db
                 (Rel_sql.Stmt.(
                    func
                      "SELECT event_id FROM operation_movement_sources WHERE \
                       movement_id=? ORDER BY event_id"
                      (int64 @-> ret Row.(t1 (int64 "event_id"))))
                    id)
             in
             Ok (role, commodity, quantity, sources))
          stored
      in
      let* expected =
        traverse
          (fun (m : Operation.movement) ->
             let* identity =
               Or_error.of_option
                 m.identity
                 ~error:(Error.of_string "derived movement lacks an identity")
             in
             Ok (identity.role, m.commodity, m.quantity, [ event_id ]))
          derived.movements
      in
      let normalize =
        List.sort ~compare:(fun (a, _, _, _) (b, _, _, _) -> String.compare a b)
      in
      let equal (r1, c1, q1, s1) (r2, c2, q2, s2) =
        String.equal r1 r2
        && Commodity.equal c1 c2
        && Quantity.equal q1 q2
        && List.equal Int64.equal s1 s2
      in
      if List.equal equal (normalize stored) (normalize expected)
      then Ok ()
      else mismatch "movements"
  | _ -> mismatch "operation is missing or duplicated"
;;

let ingest_in_transaction db (e : Source_event.t) =
  let at, on = occurrence_columns e in
  let* existing =
    db_result (Db.first db.conn (find_source e.venue e.account e.kind e.source_id))
  in
  match existing with
  | Some previous ->
    if
      Option.equal Ptime.equal at previous.occurred_at
      && Option.equal String.equal on previous.occurred_on
      && Records.restates ~stored:previous.payload e.payload
    then
      let* () =
        if Registry.is_operation_kind db.registry e.kind
        then verify_stored_operation_event db ~event_id:previous.id e
        else Ok ()
      in
      Ok (Existing previous.id)
    else
      Or_error.error_string
        "source identity already exists with different timestamp/date or payload"
  | None ->
    let* () =
      db_result
        (Db.exec
           db.conn
           (insert_source e.venue e.account e.kind e.source_id at on e.payload))
    in
    let id = Db.last_insert_rowid db.conn in
    let* () = store_event_fields db id e.fields in
    let* () = Event_digests.store db id e in
    (* Movements are decoded once, here, and stored for queries. *)
    let* () =
      if Registry.is_movement db.registry e
      then
        let* profile = source_typing_profile db e.venue in
        let raw_profile = Source_typing.create ~institution:e.venue [] in
        let* raw = Registry.movement ~source_typing:raw_profile db.registry e in
        let m = Source_typing.apply profile raw in
        let* () = db_result (Db.exec db.conn (insert_movement id e m)) in
        (* An account's ledger name is fixed when the account first appears, so that queries
           read it and a later naming rule never renames it. *)
        let* name = Bank_projection.ledger_account ~institution:e.venue m.account in
        let* mapping =
          Bank_account_mapping.create
            ~institution:e.venue
            ~bank_id:m.account.bank_id
            ~account_id:m.account.account_id
            ~account_type:m.account.account_type
            ~ledger_account:name
        in
        let* () =
          db_result
            (Db.exec
               db.conn
               (name_account e.venue e.account name (Bank_account_mapping.id mapping)))
        in
        let* () = store_bank_operation db id e m in
        let* rules = source_evidence_rules db e.venue in
        let evidence = Source_evidence.apply rules m in
        let* () = write_source_evidence db id evidence in
        apply_automatic_category db id e raw m
      else if Registry.is_operation_kind db.registry e.kind
      then persist_operation_event db ~event_id:id e
      else Ok ()
    in
    Ok (Inserted id)
;;

let ingest db e = transaction db (fun db -> ingest_in_transaction db e)
