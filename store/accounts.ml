open Base
open Rel
module Db = Rel_sqlite3
open Handle
open Import
open Ledger
open Review
open Restore
open Transfers

type classification_rule_match =
  { transaction : bank_review
  ; eligible : bool
  ; apply_type : bool
  ; apply_account : bool
  ; settlement_ledger : string option
  ; explanation : string option
  }

let preview_classification_rule ?(force = false) db (rule : Classification_rule.t) =
  let* pattern =
    match rule.pattern with
    | None -> Ok None
    | Some text ->
      let* p = Transaction_pattern.compile ~case_sensitive:rule.case_sensitive text in
      Ok (Some p)
  in
  let* rows = bank_review db in
  let* settlement_card =
    match rule.action with
    | Settlement card_id ->
      let* accounts = bank_account_info db in
      let* card =
        match
          List.filter accounts ~f:(fun info ->
            String.equal (Bank_account_mapping.id info.mapping) card_id)
        with
        | [ card ] -> Ok card
        | [] -> Or_error.error_string "rule references an unknown card ID"
        | _ -> Or_error.error_string "rule card ID is ambiguous"
      in
      if
        not
          (List.mem
             [ "CREDITCRD"; "credit-card" ]
             card.mapping.account_type
             ~equal:String.equal)
      then Or_error.error_string "rule settlement target is not a credit card"
      else if not (String.is_prefix card.mapping.ledger_account ~prefix:"liabilities:")
      then Or_error.error_string "map the rule's card to a liabilities account first"
      else if Option.is_none card.settlement_account
      then Or_error.error_string "link the rule's card to its settlement account first"
      else Ok (Some card)
    | _ -> Ok None
  in
  let* raw_types =
    match rule.mode, rule.action, rule.match_type with
    | Automatic, (Type _ | Type_and_category _), Some _ ->
      let* source = events db in
      source
      |> List.filter ~f:(fun (_, event) -> Registry.is_movement db.registry event)
      |> traverse (fun (id, event) ->
        let profile = Source_typing.create ~institution:event.Source_event.venue [] in
        let* movement = Registry.movement ~source_typing:profile db.registry event in
        Ok (id, movement.movement_type))
    | _ -> Ok []
  in
  let matched =
    List.filter rows ~f:(fun row ->
      let matched_type =
        match rule.mode, rule.action, rule.match_type with
        | Automatic, (Type _ | Type_and_category _), Some _ ->
          Option.value
            (List.Assoc.find raw_types row.event_id ~equal:Int64.equal)
            ~default:None
        | _ -> row.movement_type
      in
      Option.value_map rule.institution ~default:true ~f:(String.equal row.institution)
      && Option.value_map
           rule.account_type
           ~default:true
           ~f:(String.equal row.transaction.account.account_type)
      && Option.value_map rule.match_type ~default:true ~f:(function
        | Classification_rule.Unknown -> Option.is_none matched_type
        | Is kind -> Option.value_map matched_type ~default:false ~f:(Poly.equal kind))
      && Option.value_map pattern ~default:true ~f:(fun p ->
        let display_name, descriptions =
          match rule.mode with
          | Automatic ->
            ( None
            , [ Option.value_map
                  (List.hd row.transaction.descriptions)
                  ~default:""
                  ~f:String.strip
              ] )
          | Review | Disabled -> None, [ row.description ]
        in
        Transaction_pattern.matches p ~display_name ~descriptions))
  in
  Ok
    (List.map matched ~f:(fun row ->
       let type_requested =
         Option.is_some (Classification_rule.action_type rule.action)
       in
       let account_requested =
         match rule.action with
         | Category _ | Settlement _ | Type_and_category _ -> true
         | Type _ | Card_reference | Withdrawal_reversal_hint -> false
       in
       let type_block = type_requested && Option.is_some row.movement_type_override in
       let already_reviewed =
         (not (String.equal row.counter_account "equity:unclassified"))
         || Option.is_some row.reason
       in
       let account_reason =
         if not account_requested
         then None
         else if
           force
           && Poly.equal row.classification Category
           &&
           match rule.action with
           | Category account | Type_and_category (_, account) ->
             not (String.equal row.counter_account account)
           | _ -> false
         then None
         else if already_reviewed
         then Some "already reviewed"
         else (
           match settlement_card with
           | None -> None
           | Some card ->
             let movement = row.transaction in
             let linked = Option.value_exn card.settlement_account in
             let account = movement.account in
             if
               not
                 (String.equal row.institution linked.institution
                  && String.equal account.bank_id linked.bank_id
                  && String.equal account.account_id linked.account_id
                  && String.equal account.account_type linked.account_type)
             then Some "not from the linked settlement account"
             else if
               (not (Iso4217.equal card.currency account.currency))
               || Quantity.(movement.amount >= zero)
               || not (Quantity.is_zero movement.fee)
             then Some "not a fee-free debit in the card currency"
             else if
               List.exists rows ~f:(fun other ->
                 let tx = other.transaction in
                 String.equal other.institution card.mapping.institution
                 && String.equal tx.account.bank_id card.mapping.bank_id
                 && String.equal tx.account.account_id card.mapping.account_id
                 && String.equal tx.account.account_type card.mapping.account_type
                 && Iso4217.equal tx.account.currency account.currency
                 && Quantity.equal tx.amount (Quantity.neg movement.amount)
                 && Int.abs (Date.diff tx.booked_on movement.booked_on) <= 7)
             then Some "matching card-side payment exists; review as a transfer"
             else None)
       in
       let type_already_matches =
         Option.value_map
           (Classification_rule.action_type rule.action)
           ~default:false
           ~f:(fun kind ->
             Option.value_map row.movement_type ~default:false ~f:(Poly.equal kind))
       in
       let apply_type = type_requested && (not type_block) && not type_already_matches in
       let apply_account = account_requested && Option.is_none account_reason in
       let reasons =
         List.filter_opt
           [ Option.some_if type_block "type override already reviewed"
           ; Option.some_if
               (type_requested && (not type_block) && type_already_matches)
               "type already matches"
           ; account_reason
           ]
       in
       { transaction = row
       ; eligible = apply_type || apply_account
       ; apply_type
       ; apply_account
       ; settlement_ledger =
           Option.map settlement_card ~f:(fun card -> card.mapping.ledger_account)
       ; explanation =
           Option.some_if (not (List.is_empty reasons)) (String.concat ~sep:"; " reasons)
       }))
;;

let apply_classification_rule ?(force = false) db ~expected ~selected =
  let event_ids = List.map selected ~f:(fun m -> m.transaction.event_id) in
  if List.contains_dup event_ids ~compare:Int64.compare
  then Or_error.error_string "duplicate rule match"
  else
    transaction db (fun db ->
      let* rules = classification_rules db in
      let* current =
        List.find rules ~f:(fun r ->
          String.equal r.Classification_rule.name expected.Classification_rule.name)
        |> Or_error.of_option ~error:(Error.of_string "rule no longer exists")
      in
      if not (Poly.equal current expected)
      then Or_error.error_string "rule changed since preview; review it again"
      else
        let* matches = preview_classification_rule ~force db current in
        let* () =
          traverse
            (fun old ->
               match
                 List.find matches ~f:(fun m ->
                   Int64.equal m.transaction.event_id old.transaction.event_id)
               with
               | Some now when old.eligible && Poly.equal old now -> Ok ()
               | _ ->
                 Or_error.error_string
                   "a rule match changed since preview; review it again")
            selected
          |> Result.map ~f:ignore
        in
        let reason =
          Printf.sprintf
            "Rule %s%s"
            current.name
            (Option.value_map current.pattern ~default:"" ~f:(fun p -> ": " ^ p))
        in
        let* () =
          match Classification_rule.action_type current.action with
          | None -> Ok ()
          | Some kind ->
            let* _ =
              traverse
                (fun m ->
                   if not m.apply_type
                   then Ok false
                   else
                     write_movement_type_override
                       db
                       ~event_id:m.transaction.event_id
                       ~previous:m.transaction.movement_type_override
                       ~movement_type:(Some kind)
                       ~reason)
                selected
            in
            Ok ()
        in
        let account_ids =
          List.filter_map selected ~f:(fun m ->
            Option.some_if m.apply_account m.transaction.event_id)
        in
        match current.action with
        | Card_reference | Withdrawal_reversal_hint ->
          Or_error.error_string
            "evidence rules apply during import; refresh stored evidence to update \
             existing transactions"
        | Type _ -> Ok (List.length selected)
        | Category account | Type_and_category (_, account) ->
          let* decisions =
            traverse
              (fun event_id -> Bank_category.create ~event_id ~account ~reason)
              account_ids
          in
          let* _ =
            set_bank_categories_in_transaction ~only_unclassified:(not force) db decisions
          in
          Ok (List.length selected)
        | Settlement card_id ->
          let* source = events db in
          let source = Hashtbl.of_alist_exn (module Int64) source in
          let* card =
            match account_ids with
            | [] -> Ok None
            | first :: _ ->
              let* _, card = card_settlement_preview ~card_id db first in
              Ok (Some card)
          in
          let* () =
            List.fold_result account_ids ~init:() ~f:(fun () event_id ->
              let* event =
                Hashtbl.find source event_id
                |> Or_error.of_option
                     ~error:(Error.of_string "unknown transaction event ID")
              in
              confirm_card_settlement_in_transaction
                ~card_id
                ~reason
                ?validated_card:card
                ~source_event:event
                db
                event_id)
          in
          Ok (List.length selected))
;;

let bank_overview db =
  let* source = movement_source db in
  let* pairs = candidate_pairs db in
  let needed = account_representatives source in
  List.iter pairs ~f:(fun (a, b, _, _) ->
    Hash_set.add needed a;
    Hash_set.add needed b);
  let* table = selected_movements db needed in
  let* summaries, movements, unclassified = status_summaries db source in
  let* accounts = bank_account_info_from ~summaries db source table in
  let* candidates = transfer_candidates_from db source table pairs in
  Ok (accounts, movements, unclassified, List.length candidates)
;;

let set_bank_account_identities_in_transaction db identities =
  let* identities =
    traverse
      (fun (i : Account_identity.t) ->
         Account_identity.create
           ~institution:i.institution
           ~bank_id:i.bank_id
           ~account_id:i.account_id
           ~account_type:i.account_type
           ~iban:i.iban)
      identities
  in
  if
    List.contains_dup
      (List.map identities ~f:(fun i ->
         i.Account_identity.institution, Account_identity.source_account i))
      ~compare:[%compare: string * string]
  then Or_error.error_string "duplicate account identity"
  else
    let* accounts = bank_accounts db in
    let* source = events db in
    let upsert =
      Rel_sql.Stmt.(
        func
          "INSERT INTO bank_account_identities(institution,source_account,iban) VALUES \
           (?,?,?) ON CONFLICT(institution,source_account) DO UPDATE SET \
           iban=excluded.iban"
          (text @-> text @-> text @-> unit))
    in
    let audit =
      let open Db_schema.Bank_account_identity_changes in
      let insert = Rel_sql.insert_into_cols Db.dialect table in
      fun v_institution v_source_account v_previous_iban v_iban ->
        insert
          Col.
            [ Value (institution, v_institution)
            ; Value (source_account, v_source_account)
            ; Value (previous_iban, v_previous_iban)
            ; Value (iban, v_iban)
            ]
    in
    let rec apply count = function
      | [] -> Ok count
      | i :: rest ->
        let identity = Account_identity.source_account i in
        let account =
          List.find accounts ~f:(fun a ->
            String.equal a.Bank_account_mapping.institution i.institution
            && String.equal (Bank_account_mapping.source_account a) identity)
        in
        let* account =
          Or_error.of_option
            account
            ~error:(Error.of_string "unknown account identity; import the account first")
        in
        let* own_iban = own_iban db source account in
        let* () =
          match own_iban with
          | Not_applicable ->
            Or_error.error_string
              "credit-card accounts have no IBAN; register the associated bank account \
               instead"
          | Known old when not (Iban.equal old i.iban) ->
            Or_error.error_string "IBAN contradicts imported account identifier"
          | Known _ | Unknown -> Ok ()
        in
        let* previous =
          db_result (Db.first db.conn (find_account_iban i.institution identity))
        in
        (* The previous value stays text, so an entry that today's rules reject
           can still be corrected and is audited exactly as stored. *)
        let iban = Iban.to_string i.iban in
        if Option.equal String.equal previous (Some iban)
        then apply count rest
        else
          let* () = db_result (Db.exec db.conn (upsert i.institution identity iban)) in
          let* () =
            db_result
              (Db.exec db.conn (audit i.institution identity previous (Some iban)))
          in
          apply (count + 1) rest
    in
    let* changed = apply 0 identities in
    let* transfers = bank_transfer_decisions db in
    let* _ =
      traverse
        (fun (d : Bank_transfer.decision) ->
           if Poly.equal d.state Rejected
           then Ok ()
           else (
             let get id =
               Or_error.of_option
                 (List.Assoc.find source id ~equal:Int64.equal)
                 ~error:(Error.of_string "missing transfer source")
             in
             let* a = get d.first_event_id in
             let* b = get d.second_event_id in
             let* ta = movement db d.first_event_id in
             let* tb = movement db d.second_event_id in
             let* compatible, _, _ = transfer_iban_evidence db source a ta b tb in
             if compatible
             then Ok ()
             else
               Or_error.error_string
                 "new identity contradicts a confirmed transfer; reject the transfer \
                  before correcting identities"))
        transfers
    in
    Ok changed
;;

let set_bank_account_identities db identities =
  transaction db (fun db -> set_bank_account_identities_in_transaction db identities)
;;

type account_change =
  { id : int64
  ; kind : string
  ; institution : string
  ; source_account : string
  ; previous_value : string option
  ; value : string option
  ; changed_at : Ptime.t
  }

let bank_account_history db =
  let row =
    Row.(
      unit (fun id kind institution source_account previous_value value changed_at ->
        { id; kind; institution; source_account; previous_value; value; changed_at })
      * int64 "id"
      * text "kind"
      * text "institution"
      * text "source_account"
      * col "previous_value" Type.(option text)
      * col "value" Type.(option text)
      * Col.make "changed_at" Db_schema.instant Col.no_proj)
  in
  collect
    db
    (statement
       "SELECT id,'ledger_account' AS \
        kind,institution,source_account,previous_ledger_account AS \
        previous_value,ledger_account AS value,changed_at FROM \
        bank_account_mapping_changes UNION ALL SELECT \
        id,'iban',institution,source_account,previous_iban,iban,changed_at FROM \
        bank_account_identity_changes UNION ALL SELECT \
        id,'settlement_account',institution,source_account,previous_settlement,settlement_account,changed_at \
        FROM bank_account_settlement_changes ORDER BY changed_at,kind,id"
       row)
;;

type account_config_report =
  { mapping_changes : int
  ; identity_changes : int
  ; settlement_changes : int
  ; changes : account_change list
  }

let export_bank_accounts db =
  let* accounts = bank_account_info db in
  Ok
    (List.map accounts ~f:(fun info ->
       let a = info.mapping in
       let reference =
         { Bank_account_config.institution = a.institution
         ; bank_id = a.bank_id
         ; account_id = a.account_id
         ; account_type = a.account_type
         }
       in
       { Bank_account_config.reference
       ; ledger_account = Some a.ledger_account
       ; iban =
           Option.value_map info.iban ~default:Bank_account_config.Clear ~f:(fun s ->
             Set s)
       ; settlement_account =
           Option.value_map
             info.settlement_account
             ~default:Bank_account_config.Clear
             ~f:(fun r -> Set r)
       }))
;;

let apply_bank_accounts_in_transaction ?(dry_run = false) db config =
  let* config = Bank_account_config.of_json (Bank_account_config.to_json config) in
  let* () =
    if dry_run then db_result (Db.exec_sql db.conn "SAVEPOINT account_preview") else Ok ()
  in
  let* before_history = bank_account_history db in
  let* info = bank_account_info db in
  let find r =
    List.find info ~f:(fun i ->
      String.equal i.mapping.institution r.Bank_account_config.institution
      && String.equal
           (Bank_account_mapping.source_account i.mapping)
           (Bank_account_config.source_account r))
  in
  let describe (r : Bank_account_config.reference) =
    Printf.sprintf "%s %s %s" r.institution r.account_type r.account_id
  in
  let resolve r =
    Or_error.of_option
      (find r)
      ~error:
        (Error.of_string
           (Printf.sprintf
              "%s has no imported statements; import them with fin import %s FOLDER \
               before importing the account settings"
              (describe r)
              r.institution))
  in
  let* () =
    match
      List.filter config ~f:(fun row ->
        Option.is_none (find row.Bank_account_config.reference))
    with
    | [] -> Ok ()
    | missing ->
      Or_error.errorf
        "%d configured account%s ha%s no imported statements: %s. Import each one's \
         statement folder with fin import BANK PATH, then import the account settings \
         again."
        (List.length missing)
        (if List.length missing = 1 then "" else "s")
        (if List.length missing = 1 then "s" else "ve")
        (String.concat
           ~sep:"; "
           (List.map missing ~f:(fun row -> describe row.Bank_account_config.reference)))
  in
  let* mappings =
    traverse
      (fun row ->
         match row.Bank_account_config.ledger_account with
         | None -> Ok None
         | Some ledger_account ->
           let* current = resolve row.reference in
           if String.equal current.mapping.ledger_account ledger_account
           then Ok None
           else (
             let r = row.reference in
             let* mapping =
               Bank_account_mapping.create
                 ~institution:r.institution
                 ~bank_id:r.bank_id
                 ~account_id:r.account_id
                 ~account_type:r.account_type
                 ~ledger_account
             in
             Ok (Some mapping)))
      config
  in
  let* mapping_changes =
    set_bank_account_mappings_in_transaction db (List.filter_opt mappings)
  in
  let remove_iban =
    let open Db_schema.Bank_account_identities in
    fun v_institution v_source_account ->
      Rel_sql.delete_from
        Db.dialect
        table
        ~where:
          Col.
            [ Value (institution, v_institution)
            ; Value (source_account, v_source_account)
            ]
  in
  let audit_iban =
    let open Db_schema.Bank_account_identity_changes in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    fun v_institution v_source_account v_previous_iban ->
      insert
        Col.
          [ Value (institution, v_institution)
          ; Value (source_account, v_source_account)
          ; Value (previous_iban, Some v_previous_iban)
          ; Value (iban, None)
          ]
  in
  let* clears =
    traverse
      (fun row ->
         match row.Bank_account_config.iban with
         | Keep | Set _ -> Ok 0
         | Clear ->
           let r = row.reference in
           let* info = resolve r in
           let* () =
             if String.equal info.iban_source "imported"
             then Or_error.error_string "an imported source IBAN cannot be cleared"
             else Ok ()
           in
           let source = Bank_account_config.source_account r in
           let* old =
             db_result (Db.first db.conn (find_account_iban r.institution source))
           in
           (match old with
            | None -> Ok 0
            | Some iban ->
              let* () = db_result (Db.exec db.conn (remove_iban r.institution source)) in
              let* () =
                db_result (Db.exec db.conn (audit_iban r.institution source iban))
              in
              Ok 1))
      config
  in
  let* sets =
    traverse
      (fun row ->
         match row.Bank_account_config.iban with
         | Keep | Clear -> Ok None
         | Set iban ->
           let* current = resolve row.reference in
           if Option.equal Iban.equal current.iban (Some iban)
           then Ok None
           else (
             let r = row.reference in
             let* identity =
               Account_identity.create
                 ~institution:r.institution
                 ~bank_id:r.bank_id
                 ~account_id:r.account_id
                 ~account_type:r.account_type
                 ~iban
             in
             Ok (Some identity)))
      config
  in
  let* identity_sets =
    set_bank_account_identities_in_transaction db (List.filter_opt sets)
  in
  let set =
    Rel_sql.Stmt.(
      func
        "INSERT INTO \
         bank_account_settlements(institution,source_account,settlement_account) VALUES \
         (?,?,?) ON CONFLICT(institution,source_account) DO UPDATE SET \
         settlement_account=excluded.settlement_account"
        (text @-> text @-> text @-> unit))
  in
  let remove =
    let open Db_schema.Bank_account_settlements in
    fun v_institution v_source_account ->
      Rel_sql.delete_from
        Db.dialect
        table
        ~where:
          Col.
            [ Value (institution, v_institution)
            ; Value (source_account, v_source_account)
            ]
  in
  let audit =
    let open Db_schema.Bank_account_settlement_changes in
    let insert = Rel_sql.insert_into_cols Db.dialect table in
    fun v_institution v_source_account v_previous_settlement v_settlement_account ->
      insert
        Col.
          [ Value (institution, v_institution)
          ; Value (source_account, v_source_account)
          ; Value (previous_settlement, v_previous_settlement)
          ; Value (settlement_account, v_settlement_account)
          ]
  in
  let* settlements =
    traverse
      (fun row ->
         match row.Bank_account_config.settlement_account with
         | Keep -> Ok 0
         | update ->
           let r = row.reference in
           let source = Bank_account_config.source_account r in
           let* child = resolve r in
           let* value =
             match update with
             | Keep | Clear -> Ok None
             | Set parent ->
               let* parent_info = resolve parent in
               if
                 (not (String.equal child.iban_source "not_applicable"))
                 || String.equal parent_info.iban_source "not_applicable"
                 || not (Iso4217.equal child.currency parent_info.currency)
               then
                 Or_error.error_string
                   "card settlement must link a credit-card account to a non-card \
                    account in the same currency"
               else Ok (Some (Json.encode Bank_account_config.reference_jsont parent))
           in
           let* previous =
             db_result (Db.first db.conn (find_settlement r.institution source))
           in
           if Option.equal String.equal previous value
           then Ok 0
           else
             let* () =
               match value with
               | None -> db_result (Db.exec db.conn (remove r.institution source))
               | Some value ->
                 db_result (Db.exec db.conn (set r.institution source value))
             in
             let* () =
               db_result (Db.exec db.conn (audit r.institution source previous value))
             in
             Ok 1)
      config
  in
  let* after_history = bank_account_history db in
  let changes =
    List.filter after_history ~f:(fun change ->
      not
        (List.exists before_history ~f:(fun old ->
           String.equal old.kind change.kind && Int64.equal old.id change.id)))
  in
  let result =
    { mapping_changes
    ; identity_changes = identity_sets + List.sum (module Int) clears ~f:Fn.id
    ; settlement_changes = List.sum (module Int) settlements ~f:Fn.id
    ; changes
    }
  in
  let* () =
    if dry_run
    then
      db_result
        (Db.exec_sql db.conn "ROLLBACK TO account_preview; RELEASE account_preview")
    else Ok ()
  in
  Ok result
;;

let apply_bank_accounts ?(dry_run = false) db config =
  transaction db (fun db -> apply_bank_accounts_in_transaction ~dry_run db config)
;;

let confirm_card_settlement_with_setup
      db
      ~event_id
      ~card_id
      ~expected_ledger
      ~expected_link
      ~setup
  =
  transaction db (fun db ->
    let* accounts = bank_account_info db in
    let* card =
      match
        List.filter accounts ~f:(fun info ->
          String.equal (Bank_account_mapping.id info.mapping) card_id)
      with
      | [ card ] -> Ok card
      | [] -> Or_error.error_string "card no longer exists"
      | _ -> Or_error.error_string "card ID collision; review is refused"
    in
    let equal_reference
          (a : Bank_account_config.reference)
          (b : Bank_account_config.reference)
      =
      String.equal a.Bank_account_config.institution b.institution
      && String.equal
           (Bank_account_config.source_account a)
           (Bank_account_config.source_account b)
    in
    let* () =
      if
        String.equal
          setup.Bank_account_config.reference.institution
          card.mapping.institution
        && String.equal
             (Bank_account_config.source_account setup.reference)
             (Bank_account_mapping.source_account card.mapping)
      then Ok ()
      else Or_error.error_string "card setup does not match the selected card"
    in
    let* () =
      if
        String.equal card.mapping.ledger_account expected_ledger
        && Option.equal equal_reference card.settlement_account expected_link
      then Ok ()
      else Or_error.error_string "card setup changed since the preview; review it again"
    in
    let* _ = apply_bank_accounts_in_transaction db [ setup ] in
    confirm_card_settlement_in_transaction ~card_id db event_id)
;;

let registry t = t.registry

(* Deletion deliberately removes immutable evidence only within one transaction;
   the original triggers are restored before commit and rollback restores them on
   every error. Refuse relationships that would affect another account. *)
type account_deletion =
  { transactions : int
  ; documents : int
  }

let delete_account ?(dry_run = false) db (mapping : Bank_account_mapping.t) =
  transaction db (fun db ->
    let quote value =
      "'" ^ String.substr_replace_all value ~pattern:"'" ~with_:"''" ^ "'"
    in
    let institution = quote mapping.institution in
    let account = quote (Bank_account_mapping.source_account mapping) in
    let exec sql = db_result (Db.exec_sql db.conn sql) in
    let scalar sql =
      let* value = db_result (Db.first db.conn (statement sql integer)) in
      Or_error.of_option value ~error:(Error.of_string "account deletion count missing")
    in
    let events =
      "SELECT id FROM source_events WHERE venue="
      ^ institution
      ^ " AND account="
      ^ account
    in
    let docs =
      "SELECT DISTINCT document_id FROM import_document_events WHERE event_id IN ("
      ^ events
      ^ ")"
    in
    let* transactions =
      scalar ("SELECT count(*) FROM source_events WHERE id IN (" ^ events ^ ")")
    in
    let* settlements =
      scalar
        ("SELECT count(*) FROM card_settlement_decisions WHERE event_id IN ("
         ^ events
         ^ ") OR (card_institution="
         ^ institution
         ^ " AND card_source_account="
         ^ account
         ^ ")")
    in
    let* reversals =
      scalar
        ("SELECT count(*) FROM reversal_pairs WHERE original IN ("
         ^ events
         ^ ") OR reversal IN ("
         ^ events
         ^ ")")
    in
    let* documents =
      scalar ("SELECT count(*) FROM import_documents WHERE id IN (" ^ docs ^ ")")
    in
    if reversals > 0
    then
      Or_error.error_string
        "account has confirmed reversals; undo them before deleting it"
    else if settlements > 0
    then
      Or_error.error_string "account has confirmed card settlements; deletion is refused"
    else if transactions = 0
    then Or_error.error_string "account has no imported transactions"
    else
      let* shared =
        scalar
          ("SELECT count(*) FROM import_document_events WHERE document_id IN ("
           ^ docs
           ^ ") AND event_id NOT IN ("
           ^ events
           ^ ")")
      in
      let* statements_shared =
        scalar
          ("SELECT count(*) FROM statements WHERE document_id IN ("
           ^ docs
           ^ ") AND (institution<>"
           ^ institution
           ^ " OR source_account<>"
           ^ account
           ^ ")")
      in
      let* companions =
        scalar ("SELECT count(*) FROM companion_rows WHERE document_id IN (" ^ docs ^ ")")
      in
      let* matches =
        scalar
          ("SELECT (SELECT count(*) FROM companion_matches WHERE event_id IN ("
           ^ events
           ^ ") OR EXISTS (SELECT 1 FROM json_each(candidates_json) WHERE CAST(value AS \
              INTEGER) IN ("
           ^ events
           ^ "))) + (SELECT count(*) FROM companion_match_changes WHERE event_id IN ("
           ^ events
           ^ ") OR previous_event_id IN ("
           ^ events
           ^ ") OR EXISTS (SELECT 1 FROM json_each(candidates_json) WHERE CAST(value AS \
              INTEGER) IN ("
           ^ events
           ^ ")))")
      in
      let* transfers =
        scalar
          ("SELECT (SELECT count(*) FROM bank_transfer_decisions WHERE first_event_id IN \
            ("
           ^ events
           ^ ") OR second_event_id IN ("
           ^ events
           ^ ")) + (SELECT count(*) FROM bank_transfer_changes WHERE first_event_id IN ("
           ^ events
           ^ ") OR second_event_id IN ("
           ^ events
           ^ ")) + (SELECT count(*) FROM operation_transfer_memberships t JOIN \
              operation_movements m ON m.id=t.movement_id WHERE m.anchor_event_id IN ("
           ^ events
           ^ ")) + (SELECT count(*) FROM operation_transfer_changes t JOIN \
              operation_movements a ON a.id=t.first_movement_id JOIN operation_movements \
              b ON b.id=t.second_movement_id WHERE a.anchor_event_id IN ("
           ^ events
           ^ ") OR b.anchor_event_id IN ("
           ^ events
           ^ "))")
      in
      let* reference =
        Bank_account_config.reference
          ~institution:mapping.institution
          ~bank_id:mapping.bank_id
          ~account_id:mapping.account_id
          ~account_type:mapping.account_type
      in
      let target = quote (Json.encode Bank_account_config.reference_jsont reference) in
      let* settlement_refs =
        scalar
          ("SELECT (SELECT count(*) FROM bank_account_settlements WHERE \
            settlement_account="
           ^ target
           ^ " AND NOT (institution="
           ^ institution
           ^ " AND source_account="
           ^ account
           ^ ")) + (SELECT count(*) FROM bank_account_settlement_changes WHERE \
              (settlement_account="
           ^ target
           ^ " OR previous_settlement="
           ^ target
           ^ ") AND NOT (institution="
           ^ institution
           ^ " AND source_account="
           ^ account
           ^ "))")
      in
      let* shared_inventories =
        scalar
          ("SELECT count(*) FROM day_inventories WHERE document_id IN ("
           ^ docs
           ^ ") AND (institution<>"
           ^ institution
           ^ " OR source_account<>"
           ^ account
           ^ ")")
      in
      let* shared_operations =
        scalar
          ("SELECT count(DISTINCT s.operation_id) FROM operation_sources s WHERE \
            s.event_id IN ("
           ^ events
           ^ ") AND EXISTS (SELECT 1 FROM operation_sources other WHERE \
              other.operation_id=s.operation_id AND other.event_id NOT IN ("
           ^ events
           ^ "))")
      in
      if
        shared
        + statements_shared
        + companions
        + matches
        + transfers
        + settlement_refs
        + shared_inventories
        + shared_operations
        > 0
      then
        Or_error.error_string
          "account has shared documents, companion matches, transfer decisions, \
           operations or settlement references; deletion cannot safely affect other \
           accounts"
      else if dry_run
      then Ok { transactions; documents }
      else
        let* () = exec "CREATE TEMP TABLE fin_delete_events(id INTEGER PRIMARY KEY)" in
        let* () = exec ("INSERT INTO fin_delete_events " ^ events) in
        let* () = exec "CREATE TEMP TABLE fin_delete_docs(id INTEGER PRIMARY KEY)" in
        let* () = exec ("INSERT INTO fin_delete_docs " ^ docs) in
        let* () =
          exec "CREATE TEMP TABLE fin_delete_operations(id INTEGER PRIMARY KEY)"
        in
        let* () =
          exec
            "INSERT INTO fin_delete_operations SELECT DISTINCT operation_id FROM \
             operation_sources WHERE event_id IN (SELECT id FROM fin_delete_events)"
        in
        let tables =
          [ "source_events"
          ; "movements"
          ; "import_documents"
          ; "statements"
          ; "stated_balances"
          ; "day_inventories"
          ; "bank_import_attempts"
          ; "document_contexts"
          ; "bank_category_changes"
          ; "bank_account_mapping_changes"
          ; "bank_account_identity_changes"
          ; "bank_account_settlement_changes"
          ; "reversal_changes"
          ; "movement_type_changes"
          ; "operation_resolution_changes"
          ; "operation_exchange_changes"
          ]
        in
        let rec remove_triggers acc = function
          | [] -> Ok acc
          | table :: rest ->
            let name = table ^ "_no_delete" in
            let* sql =
              db_result
                (Db.first
                   db.conn
                   (statement
                      ("SELECT sql FROM sqlite_schema WHERE type='trigger' AND name="
                       ^ quote name)
                      Row.(t1 (text "sql"))))
            in
            (match sql with
             | None -> Or_error.errorf "missing immutable trigger %s" name
             | Some sql ->
               let* () = exec ("DROP TRIGGER " ^ name) in
               remove_triggers (sql :: acc) rest)
        in
        let* triggers = remove_triggers [] tables in
        let event_ids = "SELECT id FROM fin_delete_events"
        and doc_ids = "SELECT id FROM fin_delete_docs" in
        let local = "institution=" ^ institution ^ " AND source_account=" ^ account in
        let statements =
          [ "DELETE FROM postings WHERE entry_id IN (SELECT id FROM journal_entries \
             WHERE event_id IN ("
            ^ event_ids
            ^ "))"
          ; "DELETE FROM journal_entries WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM bank_categories WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM bank_category_changes WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM movement_type_overrides WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM movement_type_changes WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM operation_exchange_changes WHERE operation_id IN (SELECT id \
             FROM fin_delete_operations)"
          ; "DELETE FROM operation_exchange_decisions WHERE operation_id IN (SELECT id \
             FROM fin_delete_operations)"
          ; "DELETE FROM operation_resolution_changes WHERE movement_id IN (SELECT id \
             FROM operation_movements WHERE operation_id IN (SELECT id FROM \
             fin_delete_operations))"
          ; "DELETE FROM operation_movement_resolutions WHERE movement_id IN (SELECT id \
             FROM operation_movements WHERE operation_id IN (SELECT id FROM \
             fin_delete_operations))"
          ; "DELETE FROM operation_movement_sources WHERE movement_id IN (SELECT id FROM \
             operation_movements WHERE operation_id IN (SELECT id FROM \
             fin_delete_operations))"
          ; "DELETE FROM operation_movements WHERE operation_id IN (SELECT id FROM \
             fin_delete_operations)"
          ; "DELETE FROM operation_sources WHERE operation_id IN (SELECT id FROM \
             fin_delete_operations)"
          ; "DELETE FROM operations WHERE id IN (SELECT id FROM fin_delete_operations)"
          ; "DELETE FROM reversal_changes WHERE original IN ("
            ^ event_ids
            ^ ") OR reversal IN ("
            ^ event_ids
            ^ ")"
          ; "DELETE FROM statement_events WHERE event_id IN ("
            ^ event_ids
            ^ ") OR statement_id IN (SELECT id FROM statements WHERE document_id IN ("
            ^ doc_ids
            ^ "))"
          ; "DELETE FROM statements WHERE document_id IN (" ^ doc_ids ^ ")"
          ; "DELETE FROM stated_balances WHERE document_id IN (" ^ doc_ids ^ ")"
          ; "DELETE FROM import_document_events WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM day_inventories WHERE " ^ local
          ; "DELETE FROM bank_import_attempts WHERE document_id IN (" ^ doc_ids ^ ")"
          ; "DELETE FROM document_contexts WHERE document_id IN (" ^ doc_ids ^ ")"
          ; "DELETE FROM import_documents WHERE id IN (" ^ doc_ids ^ ")"
          ; "DELETE FROM movement_evidence WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM movements WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM source_event_fields WHERE event_id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM source_events WHERE id IN (" ^ event_ids ^ ")"
          ; "DELETE FROM bank_account_mappings WHERE " ^ local
          ; "DELETE FROM bank_account_mapping_changes WHERE " ^ local
          ; "DELETE FROM bank_account_identities WHERE " ^ local
          ; "DELETE FROM bank_account_identity_changes WHERE " ^ local
          ; "DELETE FROM bank_account_settlements WHERE " ^ local
          ; "DELETE FROM bank_account_settlement_changes WHERE " ^ local
          ; "DROP TABLE fin_delete_events"
          ; "DROP TABLE fin_delete_docs"
          ; "DROP TABLE fin_delete_operations"
          ]
        in
        let* () = List.fold_result statements ~init:() ~f:(fun () sql -> exec sql) in
        let* () = List.fold_result triggers ~init:() ~f:(fun () sql -> exec sql) in
        Ok { transactions; documents })
;;
