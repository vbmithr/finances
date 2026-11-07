open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Setup
open Import
open Ledger
open Review
open Accounts

type type_rule_outcome =
  | Newly_typed
  | Changed
  | Unchanged
  | Shadowed of string
  | Overridden

type type_rule_row =
  { event_id : int64
  ; institution : string
  ; description : string
  ; before : Movement_type.t option
  ; after : Movement_type.t option
  ; outcome : type_rule_outcome
  }

type type_rule_test =
  { scanned : int
  ; matches : type_rule_row list
  ; untyped_before : int
  ; untyped_after : int
  }

let typing_rule (rule : Classification_rule.t) kind =
  Source_typing.make_rule
    ?field:rule.field
    (Option.value rule.pattern ~default:".*")
    rule.account_type
    rule.case_sensitive
    (Movement_type.to_string kind)
  |> Or_error.map ~f:(fun typing ->
    Source_typing.with_match_type
      typing
      (Option.map rule.match_type ~f:Classification_rule.condition_name))
;;

let test_type_rule db (draft : Classification_rule.t) =
  let* draft_kind =
    Classification_rule.action_type draft.action
    |> Or_error.of_option
         ~error:(Error.of_string "only rules that set a movement type can be tested")
  in
  let* draft_typing = typing_rule draft draft_kind in
  let* saved = classification_rules db in
  let* saved =
    List.filter saved ~f:(fun (r : Classification_rule.t) ->
      (match r.mode with
       | Automatic -> true
       | Review | Disabled -> false)
      && Option.is_some (Classification_rule.action_type r.action))
    |> List.map ~f:(fun (r : Classification_rule.t) ->
      let* typing =
        typing_rule r (Option.value_exn (Classification_rule.action_type r.action))
      in
      Ok (r, (r.priority, r.name, typing)))
    |> Or_error.combine_errors
  in
  let ordered entries =
    List.sort entries ~compare:(fun (p1, n1, _) (p2, n2, _) ->
      match Int.compare p1 p2 with
      | 0 -> String.compare n1 n2
      | c -> c)
  in
  (* The saved version of the draft's rule is part of "before" and absent from
     "after", so editing a rule shows what the edit changes. *)
  let for_venue ~without_draft venue =
    List.filter_map saved ~f:(fun ((r : Classification_rule.t), entry) ->
      Option.some_if
        (Option.value_map r.institution ~default:true ~f:(String.equal venue)
         && not (without_draft && String.equal r.name draft.name))
        entry)
  in
  let draft_applies venue =
    Option.value_map draft.institution ~default:true ~f:(String.equal venue)
  in
  let* source = movement_source db in
  let source =
    List.filter source ~f:(fun (_, (e : Source_event.t)) -> draft_applies e.venue)
  in
  let* columns = companion_columns db in
  let* override_rows =
    collect
      db
      (statement
         "SELECT movement_id,event_id,movement_type FROM movement_type_overrides"
         Db_schema.Movement_type_overrides.row)
  in
  let overridden =
    Set.of_list
      (module Int64)
      (List.map override_rows ~f:(fun (r : Db_schema.Movement_type_overrides.t) ->
         r.event_id))
  in
  let winner rules movement =
    List.find rules ~f:(fun (_, _, r) -> Source_typing.matches r movement)
  in
  let type_of (movement : Movement.t) = function
    | None -> movement.movement_type
    | Some (_, _, (r : Source_typing.rule)) -> Some r.kind
  in
  let* results =
    traverse
      (fun (event_id, (event : Source_event.t)) ->
         let base = Source_typing.create ~institution:event.venue [] in
         let* movement = Registry.movement ~source_typing:base db.registry event in
         let movement =
           match Hashtbl.find columns event_id with
           | None -> movement
           | Some more -> Movement.with_fields movement (movement.fields @ more)
         in
         let before_rules = ordered (for_venue ~without_draft:false event.venue) in
         let after_rules =
           ordered
             ((draft.priority, draft.name, draft_typing)
              :: for_venue ~without_draft:true event.venue)
         in
         let before_winner = winner before_rules movement in
         let after_winner = winner after_rules movement in
         let before = type_of movement before_winner in
         let after = type_of movement after_winner in
         let is_overridden = Set.mem overridden event_id in
         let row =
           if not (Source_typing.matches draft_typing movement)
           then None
           else (
             let outcome =
               if is_overridden
               then Overridden
               else (
                 match after_winner with
                 | Some (_, name, _) when not (String.equal name draft.name) ->
                   Shadowed name
                 | _ ->
                   (match before with
                    | None -> Newly_typed
                    | Some _ when Option.equal Poly.equal before after -> Unchanged
                    | Some _ -> Changed))
             in
             Some
               { event_id
               ; institution = event.venue
               ; description = String.concat ~sep:" | " movement.descriptions
               ; before
               ; after
               ; outcome
               })
         in
         Ok
           ( row
           , (not is_overridden) && Option.is_none before
           , (not is_overridden) && Option.is_none after ))
      source
  in
  Ok
    { scanned = List.length source
    ; matches = List.filter_map results ~f:(fun (row, _, _) -> row)
    ; untyped_before = List.count results ~f:(fun (_, b, _) -> b)
    ; untyped_after = List.count results ~f:(fun (_, _, a) -> a)
    }
;;

type description_gap =
  { institution : string
  ; key : string
  ; count : int
  ; first_on : Date.t
  ; last_on : Date.t
  ; debits : int
  ; credits : int
  ; example : string
  ; pattern : string
  }

let description_gaps ?institution db =
  let* reviews = bank_review db in
  let groups = Hashtbl.create (module String) in
  List.iter reviews ~f:(fun (r : bank_review) ->
    if
      Option.is_none r.movement_type
      && Option.value_map institution ~default:true ~f:(String.equal r.institution)
    then (
      let description =
        Option.value
          (List.hd (Movement.field_values r.transaction "description"))
          ~default:""
      in
      let key = Description_key.key description in
      Hashtbl.add_multi
        groups
        ~key:(r.institution ^ "\000" ^ key)
        ~data:(r.institution, key, description, r.transaction)));
  Hashtbl.data groups
  |> List.map ~f:(fun members ->
    let institution, key, example, _ = List.hd_exn members in
    let dates = List.map members ~f:(fun (_, _, _, (m : Movement.t)) -> m.booked_on) in
    let debits =
      List.count members ~f:(fun (_, _, _, (m : Movement.t)) ->
        Quantity.(m.amount < zero))
    in
    { institution
    ; key
    ; count = List.length members
    ; first_on = List.min_elt dates ~compare:Date.compare |> Option.value_exn
    ; last_on = List.max_elt dates ~compare:Date.compare |> Option.value_exn
    ; debits
    ; credits = List.length members - debits
    ; example
    ; pattern = Description_key.pattern key
    })
  |> List.sort ~compare:(fun a b ->
    match Int.compare b.count a.count with
    | 0 ->
      (match String.compare a.institution b.institution with
       | 0 -> String.compare a.key b.key
       | c -> c)
    | c -> c)
  |> fun gaps -> Ok gaps
;;

type institution_summary =
  { movements : int
  ; first_on : string option
  ; last_on : string option
  ; by_type : (Movement_type.t option * int) list
  }

let institution_summary db institution =
  let* rows =
    db_result
      (Db.fold
         db.conn
         (Rel_sql.Stmt.(
            func
              "SELECT m.event_id,m.booked_on,m.movement_type,o.movement_type FROM \
               movements m JOIN source_events e ON e.id=m.event_id LEFT JOIN \
               movement_type_overrides o ON o.event_id=m.event_id WHERE e.venue=?"
              (text
               @-> ret
                     Row.(
                       t4
                         (int64 "event_id")
                         (text "booked_on")
                         (option Rel.Type.text "movement_type")
                         (option Rel.Type.text "override"))))
            institution)
         (fun row acc -> row :: acc)
         [])
  in
  (* The stored type already follows the rules, companion columns included. *)
  let effective (_, _, primary, override) =
    Option.map (Option.first_some override primary) ~f:Movement_type.of_string
  in
  let counts = Hashtbl.create (module String) in
  List.iter rows ~f:(fun row ->
    let kind = effective row in
    Hashtbl.update
      counts
      (Option.value_map kind ~default:"" ~f:Movement_type.to_string)
      ~f:(fun n -> kind, 1 + Option.value_map n ~default:0 ~f:snd));
  let dates = List.map rows ~f:(fun (_, d, _, _) -> d) in
  Ok
    { movements = List.length rows
    ; first_on = List.min_elt dates ~compare:String.compare
    ; last_on = List.max_elt dates ~compare:String.compare
    ; by_type =
        Hashtbl.data counts
        |> List.sort ~compare:(fun (a, x) (b, y) ->
          match Int.compare y x with
          | 0 ->
            String.compare
              (Option.value_map a ~default:"" ~f:Movement_type.to_string)
              (Option.value_map b ~default:"" ~f:Movement_type.to_string)
          | c -> c)
    }
;;

type operation_summary =
  { operations : int
  ; first_on : string option
  ; last_on : string option
  ; unresolved : int
  }

(* Operations whose source is a non-bank event (no bank movement anchors them),
   such as exchange rows. An operation is unresolved while one of its movements
   still has no account. *)
let operation_summary db institution =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT o.id,substr(o.occurred,1,10) FROM operations o JOIN source_events e \
            ON e.id=o.source_event_id WHERE o.bank_event_id IS NULL AND e.venue=?"
           (text @-> ret Row.(t2 (int64 "id") (text "occurred"))))
         institution)
  in
  let* unresolved =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT COUNT(DISTINCT o.id) FROM resolved_movements m CROSS JOIN operations \
            o ON o.id=m.operation_id JOIN source_events e ON e.id=o.source_event_id \
            WHERE o.bank_event_id IS NULL AND e.venue=? AND m.account IS NULL AND NOT \
            m.ignored"
           (text @-> ret integer))
         institution)
  in
  let dates = List.map rows ~f:snd in
  Ok
    { operations = List.length rows
    ; first_on = List.min_elt dates ~compare:String.compare
    ; last_on = List.max_elt dates ~compare:String.compare
    ; unresolved = List.hd unresolved |> Option.value ~default:0
    }
;;

let operation_institutions db =
  let* venues =
    collect
      db
      Rel_sql.Stmt.(
        func
          "SELECT DISTINCT e.venue FROM operations o JOIN source_events e ON \
           e.id=o.source_event_id WHERE o.bank_event_id IS NULL ORDER BY e.venue"
          (ret Row.(t1 (text "venue"))))
  in
  traverse
    (fun venue ->
       let* summary = operation_summary db venue in
       Ok (venue, summary))
    venues
;;

(* The columns of the companion rows matched to one movement. *)
let companion_columns_of_event db event_id =
  let* rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT r.id,r.document_id,r.row_number,r.institution,r.format,r.fields_json \
            FROM companion_matches c JOIN companion_rows r ON r.id=c.row_id WHERE \
            c.event_id=? AND c.state='matched' ORDER BY r.id"
           (int64
            @-> ret
                  Row.(
                    unit (fun a b c d e f -> a, b, c, d, e, f)
                    * int64 "id"
                    * int64 "document_id"
                    * int "row_number"
                    * text "institution"
                    * text "format"
                    * text "fields_json")))
         event_id)
  in
  traverse
    (fun (row_id, document_id, row_number, institution, format, fields_json) ->
       let* companion =
         List.find (registered_companions db) ~f:(fun (owner, (c : Companion.t)) ->
           String.equal owner institution && String.equal c.format format)
         |> Option.map ~f:snd
         |> Or_error.of_option ~error:(Error.of_string "unregistered companion format")
       in
       let* fields = decode_fields fields_json in
       let* context_rows =
         collect
           db
           (Rel_sql.Stmt.(
              func
                "SELECT context_json FROM document_contexts WHERE document_id=?"
                (int64 @-> ret Row.(t1 (text "context_json"))))
              document_id)
       in
       let* context =
         match context_rows with
         | [] -> Ok []
         | json :: _ -> Json.decode Json.string_map json
       in
       companion.fields
         { Companion.row_id
         ; document_id
         ; institution
         ; format
         ; row_number
         ; fields
         ; context
         })
    rows
  |> Result.map ~f:List.concat
;;

let explain_movement db event_id =
  let* event = event_by_id db event_id in
  let venue = event.Source_event.venue in
  let* empty = Source_typing.empty venue in
  let* parsed = Registry.movement ~source_typing:empty db.registry event in
  let* more = companion_columns_of_event db event_id in
  let parsed = Movement.with_fields parsed (parsed.fields @ more) in
  let* stored = movement db event_id in
  let* rules = classification_rules db in
  let automatic (r : Classification_rule.t) =
    (match r.mode with
     | Automatic -> true
     | Review | Disabled -> false)
    && Option.value_map r.institution ~default:true ~f:(String.equal venue)
  in
  let first_hit = ref false in
  let decide checks_for =
    (* Mark the first match as the decider and the later ones as shadowed. *)
    List.map checks_for ~f:(fun (name, pattern, input, outcome) ->
      let verdict =
        match outcome with
        | Error reason -> Explanation.Does_not_match reason
        | Ok kind ->
          if !first_hit
          then Also_matches
          else (
            first_hit := true;
            Decides kind)
      in
      { Explanation.name; pattern; input; verdict })
  in
  let after_code = parsed in
  (* Type rules over every field of the movement; the first that matches decides. *)
  let* description_entries =
    List.filter rules ~f:(fun r ->
      automatic r && Option.is_some (Classification_rule.action_type r.action))
    |> traverse (fun (r : Classification_rule.t) ->
      let kind = Option.value_exn (Classification_rule.action_type r.action) in
      let* typing = typing_rule r kind in
      let pattern = Option.value r.pattern ~default:"any description" in
      let field = Option.value r.field ~default:"description" in
      let values = Movement.field_values after_code field in
      let input =
        if List.is_empty values
        then Printf.sprintf "%s: this movement has no such field" field
        else Printf.sprintf "%s: %s" field (String.concat ~sep:" | " values)
      in
      let reason =
        if
          not
            (Option.value_map
               r.account_type
               ~default:true
               ~f:(String.equal after_code.account.account_type))
        then
          Some
            (Printf.sprintf
               "account type is %s, not %s"
               after_code.account.account_type
               (Option.value_exn r.account_type))
        else if
          not
            (Option.value_map r.match_type ~default:true ~f:(fun cond ->
               match cond, after_code.movement_type with
               | Classification_rule.Unknown, None -> true
               | Unknown, Some _ -> false
               | Is wanted, Some have -> Poly.equal wanted have
               | Is _, None -> false))
        then
          Some
            (Printf.sprintf
               "parser type is %s, the rule wants %s"
               (Option.value_map
                  after_code.movement_type
                  ~default:"unknown"
                  ~f:Movement_type.to_string)
               (Option.value_map
                  r.match_type
                  ~default:"any"
                  ~f:Classification_rule.condition_name))
        else if not (Source_typing.matches typing after_code)
        then
          Some
            (if List.is_empty values
             then Printf.sprintf "the movement has no field %s" field
             else Printf.sprintf "field %s does not match" field)
        else None
      in
      Ok
        ( r.name
        , pattern
        , input
        , match reason with
          | Some reason -> Error reason
          | None -> Ok (Some kind) ))
  in
  let checks = decide description_entries in
  let rules_type =
    match
      List.find_map checks ~f:(fun c ->
        match c.verdict with
        | Decides kind -> kind
        | _ -> None)
    with
    | Some kind -> Some kind
    | None -> after_code.movement_type
  in
  let* override_rows =
    db_result
      (Db.fold
         db.conn
         (Rel_sql.Stmt.(
            func
              "SELECT movement_type FROM movement_type_overrides WHERE event_id=?"
              (int64 @-> ret Row.(t1 (text "movement_type"))))
            event_id)
         (fun row acc -> row :: acc)
         [])
  in
  let override = List.hd override_rows |> Option.map ~f:Movement_type.of_string in
  let* category = db_result (Db.first db.conn (find_category event_id)) in
  let effective, source =
    match override, rules_type with
    | Some kind, _ -> Some kind, Explanation.Manual_override
    | None, Some kind -> Some kind, Rules
    | None, None -> None, Untyped
  in
  Ok
    { Explanation.event_id
    ; institution = venue
    ; booked_on = Date.to_string stored.booked_on
    ; amount = Quantity.to_string stored.amount
    ; currency = Iso4217.to_alpha_code stored.account.currency
    ; account_type = stored.account.account_type
    ; descriptions = stored.descriptions
    ; override
    ; checks
    ; rules_type
    ; effective
    ; source
    ; category
    }
;;

let document_detail db document_id =
  let* docs =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT sha256,imported_at FROM import_documents WHERE id=?"
           (int64
            @-> ret
                  Row.(
                    t2
                      (text "sha256")
                      (read_column Db_schema.Import_documents.imported_at))))
         document_id)
  in
  let* paths =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT path FROM bank_import_attempts WHERE document_id=? ORDER BY id"
           (int64 @-> ret Row.(t1 (text "path"))))
         document_id)
  in
  match docs with
  | [ (sha256, imported_at) ] ->
    Ok
      { Transaction_detail.id = document_id
      ; sha256
      ; imported_at = Timestamp.to_rfc3339 imported_at
      ; paths = List.dedup_and_sort paths ~compare:String.compare
      }
  | _ -> Or_error.errorf "no stored document %Ld" document_id
;;

let show_movement db event_id =
  let* explanation = explain_movement db event_id in
  let* source = stored_event_row db event_id in
  let* (event : Db_schema.Source_events.t) =
    Or_error.of_option source ~error:(Error.of_string "unknown transaction event ID")
  in
  let* document_ids =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT document_id FROM import_document_events WHERE event_id=? ORDER BY \
            document_id"
           (int64 @-> ret Row.(t1 (int64 "document_id"))))
         event_id)
  in
  let* documents = traverse (document_detail db) document_ids in
  let* statement_rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT \
            s.reference,s.source_account,s.currency,s.period_start,s.period_end,s.opening,s.closing \
            FROM statement_events e JOIN statements s ON s.id=e.statement_id WHERE \
            e.event_id=? ORDER BY s.id"
           (int64
            @-> ret
                  Row.(
                    unit (fun a b c d e f g -> a, b, c, d, e, f, g)
                    * option Rel.Type.text "reference"
                    * text "source_account"
                    * text "currency"
                    * read_column Db_schema.Statements.period_start
                    * read_column Db_schema.Statements.period_end
                    * text "opening"
                    * text "closing")))
         event_id)
  in
  let statements =
    List.map
      statement_rows
      ~f:
        (fun
          (reference, source_account, currency, period_start, period_end, opening, closing)
        ->
        { Transaction_detail.reference
        ; source_account
        ; currency
        ; period =
            Temporal.to_string period_start ^ " to " ^ Temporal.to_string period_end
        ; opening
        ; closing
        })
  in
  let* companion_rows =
    collect
      db
      (Rel_sql.Stmt.(
         func
           "SELECT \
            r.id,r.document_id,r.row_number,r.institution,r.format,r.fields_json,c.state,c.reason,c.policy \
            FROM companion_matches c JOIN companion_rows r ON r.id=c.row_id WHERE \
            c.event_id=? ORDER BY r.id"
           (int64
            @-> ret
                  Row.(
                    unit (fun a b c d e f g h i -> a, b, c, d, e, f, g, h, i)
                    * int64 "id"
                    * int64 "document_id"
                    * int "row_number"
                    * text "institution"
                    * text "format"
                    * text "fields_json"
                    * text "state"
                    * text "reason"
                    * text "policy")))
         event_id)
  in
  let* companions =
    traverse
      (fun ( row_id
           , document_id
           , row_number
           , institution
           , format
           , fields_json
           , state
           , reason
           , policy ) ->
         let* companion =
           List.find (registered_companions db) ~f:(fun (owner, (c : Companion.t)) ->
             String.equal owner institution && String.equal c.format format)
           |> Option.map ~f:snd
           |> Or_error.of_option ~error:(Error.of_string "unregistered companion format")
         in
         let* raw = decode_fields fields_json in
         let* context_rows =
           collect
             db
             (Rel_sql.Stmt.(
                func
                  "SELECT context_json FROM document_contexts WHERE document_id=?"
                  (int64 @-> ret Row.(t1 (text "context_json"))))
                document_id)
         in
         let* context =
           match context_rows with
           | [] -> Ok []
           | json :: _ -> Json.decode Json.string_map json
         in
         let row : Companion.row =
           { row_id; document_id; institution; format; row_number; fields = raw; context }
         in
         let* columns = companion.fields row in
         let* annotation = companion.annotate row in
         let* document = document_detail db document_id in
         Ok
           { Transaction_detail.row_id
           ; format
           ; row_number
           ; document
           ; fields = columns
           ; state
           ; reason
           ; policy
           ; display_name = annotation.display_name
           ; tags = annotation.tags
           })
      companion_rows
  in
  let* transfer = linked_transfer db event_id in
  let* stored_fields = stored_event_fields db event_id in
  let payload =
    match Json.decode Jsont.json event.payload with
    | Ok value -> Json.encode ~format:Jsont.Indent Jsont.json value
    | Error _ -> event.payload
  in
  Ok
    { Transaction_detail.explanation
    ; venue = event.venue
    ; account = event.account
    ; kind = event.kind
    ; source_id = event.source_id
    ; occurred =
        (match event.occurred_at, event.occurred_on with
         | Some at, _ -> Timestamp.to_rfc3339 at
         | None, Some on -> on
         | None, None -> "")
    ; retrieved_at = Timestamp.to_rfc3339 event.retrieved_at
    ; payload
    ; fields = stored_fields
    ; documents
    ; statements
    ; companions
    ; transfer
    }
;;
