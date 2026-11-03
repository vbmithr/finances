open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Sources
open Ledger
open Review
open Restore
open Transfers
open Explain

let add_operation_in_transaction db operation =
  let* source = events db in
  add_operation_with_sources db (Hashtbl.of_alist_exn (module Int64) source) operation
;;

let add_operation db operation =
  transaction db (fun db -> add_operation_in_transaction db operation)
;;

let get_operation_in_transaction db operation_id =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT occurred FROM operations WHERE id=?"
        (int64 @-> ret Row.(t1 (text "occurred"))))
  in
  let* occurred = db_result (Db.first db.conn (query operation_id)) in
  let* occurred =
    Or_error.of_option occurred ~error:(Error.of_string "unknown operation")
  in
  let* occurred = Or_error.try_with (fun () -> Temporal.of_string_exn occurred) in
  let sources =
    Rel_sql.Stmt.(
      func
        "SELECT event_id FROM operation_sources WHERE operation_id=? ORDER BY event_id"
        (int64 @-> ret Row.(t1 (int64 "event_id"))))
  in
  let* source_ids = collect db (sources operation_id) in
  let* source = events db in
  let by_id = Hashtbl.of_alist_exn (module Int64) source in
  let rows =
    Rel_sql.Stmt.(
      func
        "SELECT \
         m.id,m.operation_id,m.anchor_event_id,m.role,COALESCE(r.account,m.account) AS \
         account, COALESCE(r.commodity,m.commodity) AS commodity,m.quantity FROM \
         operation_movements m LEFT JOIN operation_movement_resolutions r ON \
         r.movement_id=m.id WHERE m.operation_id=? ORDER BY m.id"
        (int64 @-> ret Db_schema.Operation_movements.row))
  in
  let* rows = collect db (rows operation_id) in
  let movement_sources =
    Rel_sql.Stmt.(
      func
        "SELECT event_id FROM operation_movement_sources WHERE movement_id=? ORDER BY \
         event_id"
        (int64 @-> ret Row.(t1 (int64 "event_id"))))
  in
  let* movements =
    traverse
      (fun (row : Db_schema.Operation_movements.t) ->
         let* anchor =
           Or_error.of_option
             (Hashtbl.find by_id row.anchor_event_id)
             ~error:(Error.of_string "operation movement anchor is missing")
         in
         let* identity = Movement_identity.of_event anchor ~role:row.role in
         let* commodity = Json.decode Commodity.codec row.commodity in
         let* quantity = Or_error.try_with (fun () -> Quantity.of_string row.quantity) in
         let* source_ids = collect db (movement_sources row.id) in
         let evidence = List.map source_ids ~f:(fun id -> Operation.Stored_event id) in
         if not (List.mem source_ids row.anchor_event_id ~equal:Int64.equal)
         then Or_error.error_string "movement anchor is not in its source evidence"
         else
           Operation.movement
             ~identity
             ?account:row.account
             ~commodity
             ~quantity
             ~evidence
             ())
      rows
  in
  Operation.create
    ~occurred
    ~evidence:(List.map source_ids ~f:(fun id -> Operation.Stored_event id))
    ~movements
;;

let operation_group db operation_id =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT bank_event_id FROM operations WHERE id=?"
        (int64 @-> ret Row.(t1 (option Rel.Type.int64 "bank_event_id"))))
  in
  let* row = db_result (Db.first db.conn (query operation_id)) in
  let* _ = Or_error.of_option row ~error:(Error.of_string "unknown operation") in
  let* links = operation_transfer_links db in
  match
    List.find links ~f:(fun link ->
      Int64.equal link.first_operation_id operation_id
      || Int64.equal link.second_operation_id operation_id)
  with
  | None -> Ok (operation_id, None)
  | Some link -> Ok (link.first_operation_id, Some link.second_operation_id)
;;

let grouped_operation_in_transaction db operation_id =
  let* first_id, other = operation_group db operation_id in
  let* first = get_operation_in_transaction db first_id in
  match other with
  | None -> Ok (first_id, first)
  | Some other_id ->
    let* second = get_operation_in_transaction db other_id in
    let evidence =
      List.dedup_and_sort (first.evidence @ second.evidence) ~compare:Poly.compare
    in
    let* operation =
      Operation.create
        ~occurred:first.occurred
        ~evidence
        ~movements:(first.movements @ second.movements)
    in
    Ok (first_id, operation)
;;

let get_operation db operation_id =
  transaction db (fun db ->
    let* _, operation = grouped_operation_in_transaction db operation_id in
    Ok operation)
;;

let operations_for_event db event_id =
  transaction db (fun db ->
    let query =
      Rel_sql.Stmt.(
        func
          "SELECT operation_id FROM operation_sources WHERE event_id=? ORDER BY \
           operation_id"
          (int64 @-> ret Row.(t1 (int64 "operation_id"))))
    in
    let* ids = collect db (query event_id) in
    let* groups = traverse (operation_group db) ids in
    let canonical =
      List.map groups ~f:fst |> List.dedup_and_sort ~compare:Int64.compare
    in
    traverse (grouped_operation_in_transaction db) canonical)
;;

type general_transfer_leg =
  { operation_id : int64
  ; movement_id : int64
  ; event_id : int64
  ; role : string
  ; account : string
  ; commodity : Commodity.t
  ; symbol : string
  ; quantity : Quantity.t
  ; date : Date.t
  ; bank : bool
  }

type general_transfer_preview =
  { first : general_transfer_leg
  ; second : general_transfer_leg
  ; entry : Accounting.entry
  }

let general_transfer_leg ?(allow_linked = false) db ~operation_id ~role =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT m.id,m.anchor_event_id,o.bank_event_id, EXISTS(SELECT 1 FROM \
         operation_transfer_memberships t WHERE t.movement_id=m.id), EXISTS(SELECT 1 \
         FROM operation_exchange_decisions x WHERE x.operation_id=o.id) FROM \
         operation_movements m JOIN operations o ON o.id=m.operation_id WHERE o.id=? AND \
         m.role=?"
        (int64
         @-> text
         @-> ret
               Row.(
                 t5
                   (int64 "id")
                   (int64 "event_id")
                   (option Rel.Type.int64 "bank_event_id")
                   (bool "linked")
                   (bool "exchange"))))
  in
  let* found = db_result (Db.first db.conn (query operation_id role)) in
  let* movement_id, event_id, bank_event_id, linked, exchange =
    Or_error.of_option found ~error:(Error.of_string "operation movement not found")
  in
  let* () =
    if (linked && not allow_linked) || exchange
    then
      Or_error.error_string "movement is already part of a reviewed transfer or exchange"
    else Ok ()
  in
  let* operation = get_operation_in_transaction db operation_id in
  let* proposal =
    match operation.movements with
    | [ movement ] -> Ok movement
    | _ ->
      Or_error.error_string
        "transfer linking currently requires one movement per operation"
  in
  let* () =
    match operation.evidence with
    | [ Operation.Stored_event id ] when Int64.equal id event_id -> Ok ()
    | _ ->
      Or_error.error_string "transfer linking currently requires one source per operation"
  in
  let* date =
    Or_error.try_with (fun () ->
      Date.of_string (String.prefix (Temporal.to_string operation.occurred) 10))
  in
  let* account, symbol =
    match bank_event_id with
    | Some id ->
      let* source = event_by_id db id in
      let* movement = movement db id in
      let* () =
        if not (Quantity.is_zero movement.fee)
        then Or_error.error_string "transfers with bank fees require separate review"
        else Ok ()
      in
      let* reviews = bank_review db in
      let* row =
        List.find reviews ~f:(fun r -> Int64.equal r.event_id id)
        |> Or_error.of_option ~error:(Error.of_string "bank movement review missing")
      in
      let* () =
        if
          Poly.equal row.classification Unclassified
          || (allow_linked && linked && Poly.equal row.classification Transfer)
        then Ok ()
        else Or_error.error_string "bank movement is already classified"
      in
      let* mapping =
        db_result (Db.first db.conn (find_mapping source.venue source.account))
      in
      let* account =
        match mapping with
        | Some account -> Ok account
        | None ->
          Bank_projection.ledger_account ~institution:source.venue movement.account
      in
      let* symbol =
        match movement.account.currency with
        | c -> Ok (Iso4217.to_alpha_code c)
      in
      Ok (account, symbol)
    | None ->
      let* account =
        Or_error.of_option
          proposal.account
          ~error:(Error.of_string "resolve the movement account first")
      in
      let query =
        Rel_sql.Stmt.(
          func
            "SELECT symbol FROM commodity_symbols WHERE commodity=?"
            (text @-> ret Row.(t1 (text "symbol"))))
      in
      let* symbol =
        db_result
          (Db.first db.conn (query (Json.encode Commodity.codec proposal.commodity)))
      in
      let* symbol =
        Or_error.of_option
          symbol
          ~error:(Error.of_string "resolve the movement commodity first")
      in
      Ok (account, symbol)
  in
  Ok
    { operation_id
    ; movement_id
    ; event_id
    ; role
    ; account
    ; commodity = proposal.commodity
    ; symbol
    ; quantity = proposal.quantity
    ; date
    ; bank = Option.is_some bank_event_id
    }
;;

let preview_general_transfer_internal
      ?(allow_linked = false)
      db
      ~first_operation
      ~first_role
      ~second_operation
      ~second_role
  =
  let* () =
    if Int64.equal first_operation second_operation
    then Or_error.error_string "transfer requires different operations"
    else Ok ()
  in
  let* first =
    general_transfer_leg ~allow_linked db ~operation_id:first_operation ~role:first_role
  in
  let* second =
    general_transfer_leg ~allow_linked db ~operation_id:second_operation ~role:second_role
  in
  let* () =
    if first.bank && second.bank
    then Or_error.error_string "use fin transfer review for bank-to-bank transfers"
    else Ok ()
  in
  let date =
    if Date.compare first.date second.date <= 0 then first.date else second.date
  in
  let* entry =
    Operation_accounting.transfer
      ~date
      ~description:"Reviewed transfer"
      [ first.account, first.commodity, first.symbol, first.quantity
      ; second.account, second.commodity, second.symbol, second.quantity
      ]
  in
  Ok { first; second; entry }
;;

let preview_general_transfer
      db
      ~first_operation
      ~first_role
      ~second_operation
      ~second_role
  =
  preview_general_transfer_internal
    db
    ~first_operation
    ~first_role
    ~second_operation
    ~second_role
;;

let general_transfer_entries db =
  let* links = operation_transfer_links db in
  let bank =
    Rel_sql.Stmt.(
      func
        "SELECT bank_event_id FROM operations WHERE id=?"
        (int64 @-> ret Row.(t1 (option Rel.Type.int64 "bank_event_id"))))
  in
  let* entries =
    traverse
      (fun link ->
         let* first_bank = db_result (Db.first db.conn (bank link.first_operation_id)) in
         let* second_bank =
           db_result (Db.first db.conn (bank link.second_operation_id))
         in
         match first_bank, second_bank with
         | Some (Some _), Some (Some _) -> Ok None
         | Some _, Some _ ->
           let* preview =
             preview_general_transfer_internal
               ~allow_linked:true
               db
               ~first_operation:link.first_operation_id
               ~first_role:link.first_movement.role
               ~second_operation:link.second_operation_id
               ~second_role:link.second_movement.role
           in
           Ok (Some preview)
         | _ -> Or_error.error_string "transfer operation is missing")
      links
  in
  Ok (List.filter_opt entries)
;;

let confirm_general_transfer_internal ?(in_transaction = false) db ~expected =
  (if in_transaction then fun f -> f db else transaction db) (fun db ->
    let* current =
      preview_general_transfer
        db
        ~first_operation:expected.first.operation_id
        ~first_role:expected.first.role
        ~second_operation:expected.second.operation_id
        ~second_role:expected.second.role
    in
    if not (Poly.equal current expected)
    then Or_error.error_string "transfer preview changed; review it again"
    else (
      let first = current.first.movement_id
      and second = current.second.movement_id in
      let insert =
        Rel_sql.Stmt.(
          func
            "INSERT INTO \
             operation_transfer_links(first_movement_id,second_movement_id,reason) \
             VALUES (?,?,'Reviewed transfer')"
            (int64 @-> int64 @-> unit))
      in
      let member =
        Rel_sql.Stmt.(
          func
            "INSERT INTO \
             operation_transfer_memberships(movement_id,first_movement_id,second_movement_id) \
             VALUES (?,?,?)"
            (int64 @-> int64 @-> int64 @-> unit))
      in
      let audit =
        Rel_sql.Stmt.(
          func
            "INSERT INTO \
             operation_transfer_changes(first_movement_id,second_movement_id,action) \
             VALUES (?,?,'confirmed')"
            (int64 @-> int64 @-> unit))
      in
      let* () = db_result (Db.exec db.conn (insert first second)) in
      let* () = db_result (Db.exec db.conn (member first first second)) in
      let* () = db_result (Db.exec db.conn (member second first second)) in
      db_result (Db.exec db.conn (audit first second))))
;;

let confirm_general_transfer db ~expected = confirm_general_transfer_internal db ~expected

let undo_general_transfer db operation_id =
  transaction db (fun db ->
    let* links = operation_transfer_links db in
    let* link =
      List.find links ~f:(fun l ->
        Int64.equal l.first_operation_id operation_id
        || Int64.equal l.second_operation_id operation_id)
      |> Or_error.of_option
           ~error:(Error.of_string "operation is not linked as a transfer")
    in
    let find =
      Rel_sql.Stmt.(
        func
          "SELECT m.id,o.bank_event_id FROM operation_movements m JOIN operations o ON \
           o.id=m.operation_id WHERE o.id=? AND m.role=?"
          (int64
           @-> text
           @-> ret Row.(t2 (int64 "id") (option Rel.Type.int64 "bank_event_id"))))
    in
    let* first =
      db_result (Db.first db.conn (find link.first_operation_id link.first_movement.role))
    in
    let* second =
      db_result
        (Db.first db.conn (find link.second_operation_id link.second_movement.role))
    in
    let* first_id, first_bank =
      Or_error.of_option first ~error:(Error.of_string "first transfer movement missing")
    in
    let* second_id, second_bank =
      Or_error.of_option
        second
        ~error:(Error.of_string "second transfer movement missing")
    in
    if Option.is_some first_bank && Option.is_some second_bank
    then Or_error.error_string "use fin transfer undo for bank-to-bank transfers"
    else (
      let members =
        Rel_sql.Stmt.(
          func
            "DELETE FROM operation_transfer_memberships WHERE first_movement_id=? AND \
             second_movement_id=?"
            (int64 @-> int64 @-> unit))
      in
      let remove =
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
             VALUES (?,?,'undone')"
            (int64 @-> int64 @-> unit))
      in
      let* () = db_result (Db.exec db.conn (members first_id second_id)) in
      let* () = db_result (Db.exec db.conn (remove first_id second_id)) in
      db_result (Db.exec db.conn (audit first_id second_id))))
;;

let exchange_projection_hash (entry : Accounting.entry) =
  let postings =
    List.map entry.postings ~f:(fun (p : Accounting.posting) ->
      Json.encode
        (Jsont.list Jsont.string)
        [ p.account; p.commodity; Quantity.to_string p.quantity ])
  in
  Digestif.SHA256.(
    to_hex
      (digest_string
         (Json.encode
            (Jsont.list Jsont.string)
            (Date.to_string entry.date :: entry.description :: postings))))
;;

let transfer_projection_hash (entry : Accounting.entry) =
  let postings =
    List.map entry.postings ~f:(fun (p : Accounting.posting) ->
      Json.encode
        (Jsont.list Jsont.string)
        [ p.account; p.commodity; Quantity.to_string p.quantity ])
    |> List.sort ~compare:String.compare
  in
  Digestif.SHA256.(
    to_hex
      (digest_string
         (Json.encode (Jsont.list Jsont.string) (Date.to_string entry.date :: postings))))
;;

type portable_transfer_preview =
  { decisions : Bank_transfer.decision list
  ; general_decisions : general_transfer_preview list
  ; unchanged : int
  ; problems : string list
  }

let transfer_snapshot db =
  transaction db (fun db ->
    let* events = events db in
    let* reviews = bank_review db in
    let* anchors = bank_operation_anchors db in
    let* operation_links = operation_transfer_links db in
    let* general = general_transfer_entries db in
    let reference (identity : Movement_identity.t) =
      let matches =
        List.filter events ~f:(fun (_, (event : Source_event.t)) ->
          String.equal event.venue identity.venue
          && String.equal event.account identity.source_account
          && String.equal event.kind identity.source_kind
          && String.equal event.source_id identity.source_id)
      in
      let* id, event =
        match matches with
        | [ entry ] -> Ok entry
        | _ -> Or_error.error_string "transfer movement source is missing or ambiguous"
      in
      let* () =
        match List.find reviews ~f:(fun r -> Int64.equal r.event_id id) with
        | Some row -> verify_bank_operation_anchor anchors row
        | None -> Ok ()
      in
      Ok
        Category_portable.
          { institution = event.venue
          ; source_account = event.account
          ; kind = event.kind
          ; source_id = event.source_id
          ; occurred = Temporal.to_string event.occurred
          ; payload_sha256 = Digestif.SHA256.(to_hex (digest_string event.payload))
          ; fields_sha256 = category_fields_hash event
          ; movement_role = identity.role
          ; account = None
          ; reason = None
          ; type_override = None
          }
    in
    let* links =
      operation_links
      |> traverse (fun link ->
        let* first = reference link.first_movement in
        let* second = reference link.second_movement in
        let reason = link.reason in
        let projection_sha256 =
          List.find general ~f:(fun p ->
            Int64.equal p.first.operation_id link.first_operation_id
            && Int64.equal p.second.operation_id link.second_operation_id)
          |> Option.map ~f:(fun p -> transfer_projection_hash p.entry)
        in
        Ok Transfer_portable.{ first; second; reason; projection_sha256 })
    in
    Ok Transfer_portable.{ links })
;;

let preview_transfer_restore db (snapshot : Transfer_portable.t) =
  let* events = events db in
  let* reviews = bank_review db in
  let* anchors = bank_operation_anchors db in
  let* existing = bank_transfer_decisions db in
  let* existing_links = operation_transfer_links db in
  let* general_entries = general_transfer_entries db in
  let* stored_movements =
    collect
      db
      (statement
         "SELECT m.anchor_event_id,m.role,m.operation_id,o.bank_event_id FROM \
          operation_movements m JOIN operations o ON o.id=m.operation_id WHERE \
          EXISTS(SELECT 1 FROM operation_movement_sources s WHERE s.movement_id=m.id AND \
          s.operation_id=m.operation_id AND s.event_id=m.anchor_event_id)"
         Row.(
           t4
             (int64 "event_id")
             (text "role")
             (int64 "operation_id")
             (option Rel.Type.int64 "bank_event_id")))
  in
  let problems = ref []
  and unchanged = ref 0 in
  let problem message = problems := message :: !problems in
  let identity (r : Category_portable.decision) =
    r.institution, r.source_account, r.kind, r.source_id, r.movement_role
  in
  let all_movements =
    List.concat_map snapshot.links ~f:(fun link ->
      [ identity link.first; identity link.second ])
  in
  if
    List.contains_dup
      all_movements
      ~compare:[%compare: string * string * string * string * string]
  then problem "a movement occurs in more than one transfer link";
  let resolve (r : Category_portable.decision) =
    let label = r.institution ^ "/" ^ r.source_id in
    let matches =
      List.filter events ~f:(fun (_, e) ->
        String.equal e.Source_event.venue r.institution
        && String.equal e.account r.source_account
        && String.equal e.kind r.kind
        && String.equal e.source_id r.source_id)
    in
    if
      Option.is_some r.account
      || Option.is_some r.reason
      || Option.is_some r.type_override
    then (
      problem ("unsupported movement reference: " ^ label);
      None)
    else (
      match matches with
      | [ (id, e) ] ->
        let evidence_count =
          List.count events ~f:(fun (_, other) ->
            String.equal other.Source_event.venue e.venue
            && String.equal other.account e.account
            && String.equal other.kind e.kind
            && String.equal
                 (Temporal.to_string other.occurred)
                 (Temporal.to_string e.occurred)
            &&
            if List.is_empty e.fields
            then String.equal other.payload e.payload
            else String.equal (category_fields_hash other) (category_fields_hash e))
        in
        if
          not
            (String.equal (Temporal.to_string e.occurred) r.occurred
             && String.equal
                  Digestif.SHA256.(to_hex (digest_string e.payload))
                  r.payload_sha256
             && String.equal (category_fields_hash e) r.fields_sha256)
        then (
          problem ("source evidence differs: " ^ label);
          None)
        else if evidence_count > 1
        then (
          problem ("ambiguous source evidence: " ^ label);
          None)
        else (
          match
            List.find stored_movements ~f:(fun (event, role, _, _) ->
              Int64.equal event id && String.equal role r.movement_role)
          with
          | None ->
            problem ("movement not found: " ^ label);
            None
          | Some (_, _, operation_id, bank_event_id) ->
            (match bank_event_id with
             | Some _ ->
               (match List.find reviews ~f:(fun row -> Int64.equal row.event_id id) with
                | None ->
                  problem ("bank movement missing: " ^ label);
                  None
                | Some row ->
                  (match verify_bank_operation_anchor anchors row with
                   | Error e ->
                     problem (label ^ ": " ^ Error.to_string_hum e);
                     None
                   | Ok () -> Some (id, operation_id, true)))
             | None -> Some (id, operation_id, false)))
      | [] ->
        problem ("source not found: " ^ label);
        None
      | _ ->
        problem ("ambiguous source identity: " ^ label);
        None)
  in
  let matched =
    List.filter_map snapshot.links ~f:(fun link ->
      let first = resolve link.first
      and second = resolve link.second in
      match first, second with
      | Some (first_event_id, _, true), Some (second_event_id, _, true) ->
        (match
           Bank_transfer.create
             ~first_event_id
             ~second_event_id
             ~state:Confirmed
             ~reason:link.reason
         with
         | Error e ->
           problem (Error.to_string_hum e);
           None
         | Ok d ->
           (match
              List.find existing ~f:(fun old ->
                Int64.equal old.first_event_id d.first_event_id
                && Int64.equal old.second_event_id d.second_event_id)
            with
            | Some old
              when Poly.equal old.state d.state && String.equal old.reason d.reason ->
              Int.incr unchanged;
              None
            | Some _ ->
              problem "existing transfer decision differs";
              None
            | None -> Some (`Bank d)))
      | Some (_, first_operation, _), Some (_, second_operation, _) ->
        let existing_link =
          List.find existing_links ~f:(fun l ->
            (Int64.equal l.first_operation_id first_operation
             && Int64.equal l.second_operation_id second_operation)
            || (Int64.equal l.first_operation_id second_operation
                && Int64.equal l.second_operation_id first_operation))
        in
        (match existing_link with
         | Some current ->
           (match
              ( List.find general_entries ~f:(fun p ->
                  (Int64.equal p.first.operation_id first_operation
                   && Int64.equal p.second.operation_id second_operation)
                  || (Int64.equal p.first.operation_id second_operation
                      && Int64.equal p.second.operation_id first_operation))
              , link.projection_sha256 )
            with
            | Some p, Some hash
              when String.equal (transfer_projection_hash p.entry) hash
                   && String.equal current.reason link.reason ->
              Int.incr unchanged;
              None
            | _ ->
              problem "existing general transfer differs";
              None)
         | None ->
           (match
              ( preview_general_transfer
                  db
                  ~first_operation
                  ~first_role:link.first.movement_role
                  ~second_operation
                  ~second_role:link.second.movement_role
              , link.projection_sha256 )
            with
            | Ok preview, Some hash
              when String.equal (transfer_projection_hash preview.entry) hash ->
              Some (`General preview)
            | Error e, _ ->
              problem (Error.to_string_hum e);
              None
            | _ ->
              problem "general transfer projection differs or is missing";
              None))
      | _ -> None)
  in
  let decisions =
    List.filter_map matched ~f:(function
      | `Bank d -> Some d
      | _ -> None)
  in
  let general_decisions =
    List.filter_map matched ~f:(function
      | `General d -> Some d
      | _ -> None)
  in
  let pairs =
    List.map decisions ~f:(fun d -> d.Bank_transfer.first_event_id, d.second_event_id)
  in
  if List.contains_dup pairs ~compare:[%compare: int64 * int64]
  then problem "duplicate transfer links";
  Ok
    { decisions
    ; general_decisions
    ; unchanged = !unchanged
    ; problems = List.rev !problems
    }
;;

let restore_transfers db ~expected snapshot =
  transaction db (fun db ->
    let* current = preview_transfer_restore db snapshot in
    if not (Poly.equal current expected)
    then Or_error.error_string "restore preview changed; review it again"
    else if not (List.is_empty current.problems)
    then Or_error.error_string "resolve transfer restore problems before applying"
    else
      let* bank_changes =
        set_bank_transfers_internal
          ~require_unreviewed:true
          ~in_transaction:true
          db
          current.decisions
      in
      let* _ =
        traverse
          (fun expected ->
             confirm_general_transfer_internal ~in_transaction:true db ~expected)
          current.general_decisions
      in
      Ok (bank_changes + List.length current.general_decisions))
;;

let exchange_projection db operation_id =
  let query =
    Rel_sql.Stmt.(
      func
        "SELECT o.bank_event_id,o.source_event_id, (SELECT count(*) FROM \
         operation_movements m JOIN operation_movement_resolutions r ON \
         r.movement_id=m.id WHERE m.operation_id=o.id), EXISTS(SELECT 1 FROM \
         operation_movements m JOIN operation_transfer_memberships t ON \
         t.movement_id=m.id WHERE m.operation_id=o.id) FROM operations o WHERE o.id=?"
        (int64
         @-> ret
               Row.(
                 t4
                   (option Rel.Type.int64 "bank_event_id")
                   (option Rel.Type.int64 "source_event_id")
                   (int "resolved")
                   (bool "linked"))))
  in
  let* metadata = db_result (Db.first db.conn (query operation_id)) in
  let* bank, source, resolved, linked =
    Or_error.of_option metadata ~error:(Error.of_string "unknown operation")
  in
  let* source =
    Or_error.of_option source ~error:(Error.of_string "exchange needs one stored source")
  in
  let* () =
    if Option.is_some bank
    then Or_error.error_string "bank movement exchange review is not supported yet"
    else if linked
    then Or_error.error_string "operation is already linked as a transfer"
    else if resolved <> 2
    then Or_error.error_string "resolve both exchange movements first"
    else Ok ()
  in
  let* operation = get_operation_in_transaction db operation_id in
  let* () =
    match operation.evidence with
    | [ Operation.Stored_event id ] when Int64.equal id source -> Ok ()
    | _ -> Or_error.error_string "exchange needs exactly one matching source record"
  in
  let* movements =
    traverse
      (fun (m : Operation.movement) ->
         let* account =
           Or_error.of_option
             m.account
             ~error:(Error.of_string "exchange movement has no account")
         in
         let symbol_query =
           Rel_sql.Stmt.(
             func
               "SELECT symbol FROM commodity_symbols WHERE commodity=?"
               (text @-> ret Row.(t1 (text "symbol"))))
         in
         let* symbol =
           db_result
             (Db.first db.conn (symbol_query (Json.encode Commodity.codec m.commodity)))
         in
         let* symbol =
           Or_error.of_option
             symbol
             ~error:(Error.of_string "resolved commodity has no journal symbol")
         in
         let* _ = validate_commodity_symbol m.commodity symbol in
         Ok (account, m.commodity, symbol, m.quantity))
      operation.movements
  in
  let* fields = operation_fields db operation_id in
  let description =
    List.find_map
      [ "Transaction Description"; "Description"; "description" ]
      ~f:(fun name -> List.Assoc.find fields name ~equal:String.equal)
    |> Option.filter ~f:(fun text -> not (String.is_empty (String.strip text)))
    |> Option.value ~default:"Exchange"
  in
  let* date =
    Or_error.try_with (fun () ->
      Date.of_string (String.prefix (Temporal.to_string operation.occurred) 10))
  in
  let* entry = Operation_accounting.exchange ~date ~description movements in
  Ok (source, entry)
;;

let operation_exchange_preview db operation_id = exchange_projection db operation_id

let set_operation_exchange_internal
      ?(in_transaction = false)
      db
      ~operation_id
      ~confirm
      ~expected
  =
  (if in_transaction then fun f -> f db else transaction db) (fun db ->
    let query =
      Rel_sql.Stmt.(
        func
          "SELECT reason FROM operation_exchange_decisions WHERE operation_id=?"
          (int64 @-> ret Row.(t1 (text "reason"))))
    in
    let* existing = db_result (Db.first db.conn (query operation_id)) in
    if Bool.equal confirm (Option.is_some existing)
    then Ok false
    else
      let* () =
        if confirm
        then
          let* current = exchange_projection db operation_id in
          if Option.exists expected ~f:(fun expected -> Poly.equal current expected)
          then Ok ()
          else Or_error.error_string "exchange preview changed; review it again"
        else Ok ()
      in
      let change =
        if confirm
        then
          Rel_sql.Stmt.(
            func
              "INSERT INTO operation_exchange_decisions(operation_id,reason) VALUES \
               (?,'Reviewed exchange')"
              (int64 @-> unit))
        else
          Rel_sql.Stmt.(
            func
              "DELETE FROM operation_exchange_decisions WHERE operation_id=?"
              (int64 @-> unit))
      in
      let audit =
        Rel_sql.Stmt.(
          func
            "INSERT INTO operation_exchange_changes(operation_id,action) VALUES (?,?)"
            (int64 @-> text @-> unit))
      in
      let* () = db_result (Db.exec db.conn (change operation_id)) in
      let* () =
        db_result
          (Db.exec
             db.conn
             (audit operation_id (if confirm then "confirmed" else "undone")))
      in
      Ok true)
;;

let set_operation_exchange db ~operation_id ~confirm ~expected =
  set_operation_exchange_internal db ~operation_id ~confirm ~expected
;;

let operation_exchange_entries db =
  let* ids =
    collect
      db
      (statement
         "SELECT operation_id FROM operation_exchange_decisions ORDER BY operation_id"
         Row.(t1 (int64 "operation_id")))
  in
  traverse
    (fun id ->
       let* source, entry = exchange_projection db id in
       Ok (source, "operation:exchange", entry))
    ids
;;

let exchange_snapshot db =
  transaction db (fun db ->
    let query =
      statement
        "SELECT o.id,o.source_event_id,m.role FROM operation_exchange_decisions d JOIN \
         operations o ON o.id=d.operation_id JOIN operation_movements m ON m.id=(SELECT \
         min(m2.id) FROM operation_movements m2 WHERE m2.operation_id=o.id) ORDER BY \
         o.id"
        Row.(
          t3
            (int64 "operation_id")
            (option Rel.Type.int64 "source_event_id")
            (text "role"))
    in
    let* rows = collect db query in
    let* source_events = events db in
    let* exchanges =
      traverse
        (fun (operation_id, source, role) ->
           let* source =
             Or_error.of_option source ~error:(Error.of_string "exchange source missing")
           in
           let* checked_source, entry = exchange_projection db operation_id in
           let* () =
             if Int64.equal source checked_source
             then Ok ()
             else Or_error.error_string "exchange source changed"
           in
           let* event =
             List.Assoc.find source_events source ~equal:Int64.equal
             |> Or_error.of_option ~error:(Error.of_string "exchange source missing")
           in
           let source =
             Category_portable.
               { institution = event.venue
               ; source_account = event.account
               ; kind = event.kind
               ; source_id = event.source_id
               ; occurred = Temporal.to_string event.occurred
               ; payload_sha256 = Digestif.SHA256.(to_hex (digest_string event.payload))
               ; fields_sha256 = category_fields_hash event
               ; movement_role = role
               ; account = None
               ; reason = None
               ; type_override = None
               }
           in
           Ok
             Exchange_portable.
               { source; projection_sha256 = exchange_projection_hash entry })
        rows
    in
    Ok Exchange_portable.{ exchanges })
;;

type exchange_restore_preview =
  { exchange_decisions : (int64 * (int64 * Accounting.entry)) list
  ; exchange_unchanged : int
  ; exchange_problems : string list
  }

let preview_exchange_restore db (snapshot : Exchange_portable.t) =
  let* source_events = events db in
  let problems = ref []
  and unchanged = ref 0 in
  let problem s = problems := s :: !problems in
  let identities =
    List.map snapshot.exchanges ~f:(fun (x : Exchange_portable.exchange) ->
      let d = x.source in
      d.institution, d.source_account, d.kind, d.source_id)
  in
  if List.contains_dup identities ~compare:[%compare: string * string * string * string]
  then problem "duplicate exchange source";
  let lookup =
    Rel_sql.Stmt.(
      func
        "SELECT o.id,EXISTS(SELECT 1 FROM operation_exchange_decisions d WHERE \
         d.operation_id=o.id) FROM operations o JOIN operation_movements m ON \
         m.operation_id=o.id WHERE o.source_event_id=? AND m.anchor_event_id=? AND \
         m.role=?"
        (int64 @-> int64 @-> text @-> ret Row.(t2 (int64 "id") (bool "reviewed"))))
  in
  let* decisions =
    traverse
      (fun (x : Exchange_portable.exchange) ->
         let d = x.source in
         let label = d.institution ^ "/" ^ d.source_id in
         let matches =
           List.filter source_events ~f:(fun (_, e) ->
             String.equal e.Source_event.venue d.institution
             && String.equal e.account d.source_account
             && String.equal e.kind d.kind
             && String.equal e.source_id d.source_id)
         in
         match matches with
         | [ (id, event) ]
           when String.equal (Temporal.to_string event.occurred) d.occurred
                && String.equal
                     Digestif.SHA256.(to_hex (digest_string event.payload))
                     d.payload_sha256
                && String.equal (category_fields_hash event) d.fields_sha256 ->
           let duplicate =
             List.count source_events ~f:(fun (_, e) ->
               String.equal e.Source_event.venue event.venue
               && String.equal e.account event.account
               && String.equal e.kind event.kind
               && String.equal (Temporal.to_string e.occurred) d.occurred
               &&
               if List.is_empty event.fields
               then String.equal e.payload event.payload
               else String.equal (category_fields_hash e) d.fields_sha256)
             > 1
           in
           if
             duplicate
             || Option.is_some d.account
             || Option.is_some d.reason
             || Option.is_some d.type_override
           then (
             problem ("ambiguous or invalid exchange source: " ^ label);
             Ok None)
           else
             let* found = db_result (Db.first db.conn (lookup id id d.movement_role)) in
             (match found with
              | None ->
                problem ("exchange movement not found: " ^ label);
                Ok None
              | Some (operation_id, reviewed) ->
                (match exchange_projection db operation_id with
                 | Error e ->
                   problem (label ^ ": " ^ Error.to_string_hum e);
                   Ok None
                 | Ok (_, entry)
                   when not
                          (String.equal
                             (exchange_projection_hash entry)
                             x.projection_sha256) ->
                   problem ("exchange projection differs: " ^ label);
                   Ok None
                 | Ok projection ->
                   if reviewed
                   then (
                     Int.incr unchanged;
                     Ok None)
                   else Ok (Some (operation_id, projection))))
         | [] ->
           problem ("exchange source not found: " ^ label);
           Ok None
         | [ _ ] ->
           problem ("exchange source evidence differs: " ^ label);
           Ok None
         | _ ->
           problem ("ambiguous exchange source: " ^ label);
           Ok None)
      snapshot.exchanges
  in
  Ok
    { exchange_decisions = List.filter_opt decisions
    ; exchange_unchanged = !unchanged
    ; exchange_problems = List.rev !problems
    }
;;

let restore_exchanges db ~expected snapshot =
  transaction db (fun db ->
    let* current = preview_exchange_restore db snapshot in
    if not (Poly.equal current expected)
    then Or_error.error_string "exchange restore preview changed; review it again"
    else if not (List.is_empty current.exchange_problems)
    then Or_error.error_string "resolve exchange restore problems before applying"
    else
      let* changes =
        traverse
          (fun (operation_id, projection) ->
             set_operation_exchange_internal
               ~in_transaction:true
               db
               ~operation_id
               ~confirm:true
               ~expected:(Some projection))
          current.exchange_decisions
      in
      Ok (List.count changes ~f:Fn.id))
;;
