open Base
open Finances

let timestamp =
  Jsont.map Jsont.string ~dec:Timestamp.of_rfc3339_exn ~enc:Timestamp.to_rfc3339
;;

let temporal = Jsont.map Jsont.string ~dec:Temporal.of_string_exn ~enc:Temporal.to_string

let outcome =
  Jsont.map
    Jsont.string
    ~dec:(fun s -> Bank_import.outcome_of_string s |> Or_error.ok_exn)
    ~enc:Bank_import.outcome_name
;;

let transfer_state =
  Jsont.map
    Jsont.string
    ~dec:(fun s -> Bank_transfer.state_of_string s |> Or_error.ok_exn)
    ~enc:Bank_transfer.state_name
;;

let print codec value = Stdio.print_endline (Json.encode ~format:Jsont.Indent codec value)

let event =
  let field name codec (enc : int64 * Source_event.t -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "id" Json.int64_codec (fun r -> fst r)
  |> field "venue" Jsont.string (fun r -> (snd r).venue)
  |> field "account" Jsont.string (fun r -> (snd r).account)
  |> field "kind" Jsont.string (fun r -> (snd r).kind)
  |> field "source_id" Jsont.string (fun r -> (snd r).source_id)
  |> field "occurred_at" (Jsont.option timestamp) (fun r ->
    match (snd r).occurred with
    | Temporal.Instant t -> Some t
    | Temporal.Date _ -> None)
  |> field "occurred_on" (Jsont.option Json.date) (fun r ->
    match (snd r).occurred with
    | Temporal.Date d -> Some d
    | Temporal.Instant _ -> None)
  |> Jsont.Object.finish
;;

let account =
  let field name codec (enc : Store.bank_account_info -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "id" Jsont.string (fun r -> Bank_account_mapping.id r.mapping)
  |> field "institution" Jsont.string (fun r -> r.mapping.institution)
  |> field "bank_id" Jsont.string (fun r -> r.mapping.bank_id)
  |> field "account_id" Jsont.string (fun r -> r.mapping.account_id)
  |> field "account_type" Jsont.string (fun r -> r.mapping.account_type)
  |> field "iban" (Jsont.option Json.iban) (fun r -> r.iban)
  |> field "iban_source" Jsont.string (fun r -> r.iban_source)
  |> field "ledger_account" Jsont.string (fun r -> r.mapping.ledger_account)
  |> field "currency" Json.currency (fun r -> r.currency)
  |> field "movements" Json.integer (fun r -> r.movements)
  |> field "first_on" Json.date (fun r -> r.first_on)
  |> field "last_on" Json.date (fun r -> r.last_on)
  |> field
       "settlement_account"
       (Jsont.option Bank_account_config.reference_jsont)
       (fun r -> r.settlement_account)
  |> Jsont.Object.finish
;;

let latest_import =
  let field name codec (enc : Bank_import.file_report -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "attempted_at" timestamp (fun r -> r.attempted_at)
  |> field "institution" Jsont.string (fun r -> r.institution)
  |> field "path" Jsont.string (fun r -> r.path)
  |> field "outcome" outcome (fun r -> r.outcome)
  |> field "message" (Jsont.option Jsont.string) (fun r -> r.message)
  |> Jsont.Object.finish
;;

let status =
  let field name codec (enc : string * int * Finances_bank.Bank_status.t -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "database" Jsont.string (fun r ->
    let database, _, _ = r in
    database)
  |> field "banks" Json.integer (fun r ->
    let _, banks, _ = r in
    banks)
  |> field "accounts" (Jsont.list account) (fun r ->
    let _, _, report = r in
    report.accounts)
  |> field "movements" Json.integer (fun r ->
    let _, _, report = r in
    report.movements)
  |> field "unclassified_movements" Json.integer (fun r ->
    let _, _, report = r in
    report.unclassified_movements)
  |> field "transfer_candidate_pairs" Json.integer (fun r ->
    let _, _, report = r in
    report.transfer_candidate_pairs)
  |> field "latest_import" (Jsont.option latest_import) (fun r ->
    let _, _, report = r in
    report.latest_import)
  |> Jsont.Object.finish
;;

let import =
  let field name codec (enc : Bank_import.file_report -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "attempt_id" Json.int64_codec (fun r -> r.attempt_id)
  |> field "institution" Jsont.string (fun r -> r.institution)
  |> field "path" Jsont.string (fun r -> r.path)
  |> field "format" Jsont.string (fun r -> r.format)
  |> field "outcome" outcome (fun r -> r.outcome)
  |> field "document_id" (Jsont.option Json.int64_codec) (fun r -> r.document_id)
  |> field "sha256" (Jsont.option Jsont.string) (fun r -> r.sha256)
  |> field "inserted" Json.integer (fun r -> r.inserted)
  |> field "existing" Json.integer (fun r -> r.existing)
  |> field "message" (Jsont.option Jsont.string) (fun r -> r.message)
  |> field "attempted_at" timestamp (fun r -> r.attempted_at)
  |> Jsont.Object.finish
;;

let statement =
  let field name codec (enc : Store.statement_reconciliation -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "statement_id" Json.int64_codec (fun r -> r.statement_id)
  |> field "document_id" Json.int64_codec (fun r -> r.document_id)
  |> field "institution" Jsont.string (fun r -> r.institution)
  |> field "source_account" Jsont.string (fun r -> r.source_account)
  |> field "reference" (Jsont.option Jsont.string) (fun r -> r.reference)
  |> field "currency" Json.currency (fun r -> r.currency)
  |> field "period_start" temporal (fun r -> r.period_start)
  |> field "period_end" temporal (fun r -> r.period_end)
  |> field "opening" Json.quantity (fun r -> r.opening)
  |> field "closing" Json.quantity (fun r -> r.closing)
  |> field "movements" Json.quantity (fun r -> r.movements)
  |> field "entries" Json.integer (fun r -> r.entries)
  |> field "difference" Json.quantity (fun r -> r.difference)
  |> field "reconciled" Jsont.bool (fun r -> Quantity.is_zero r.difference)
  |> Jsont.Object.finish
;;

let statements =
  let field name codec (enc : Store.statement_reconciliation list -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "statements" (Jsont.list statement) (fun r -> r)
  |> Jsont.Object.finish
;;

let review =
  let field name codec (enc : Store.bank_review -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "event_id" Json.int64_codec (fun r -> r.event_id)
  |> field "institution" Jsont.string (fun r -> r.institution)
  |> field "source_account" Jsont.string (fun r -> r.transaction.account.account_id)
  |> field "booked_on" Json.date (fun r -> r.transaction.booked_on)
  |> field "amount" Json.quantity (fun r -> r.transaction.amount)
  |> field "fee" Json.quantity (fun r -> r.transaction.fee)
  |> field "currency" Json.currency (fun r -> r.transaction.account.currency)
  |> field "description" Jsont.string (fun r ->
    String.concat ~sep:" | " r.transaction.descriptions)
  |> field "display_name" (Jsont.option Jsont.string) (fun r -> r.display_name)
  |> field "movement_type" (Jsont.option Jsont.string) (fun r ->
    Option.map r.movement_type ~f:Movement_type.to_string)
  |> field "detail_sources" (Jsont.list Json.int64_codec) (fun r -> r.detail_sources)
  |> field "account" Jsont.string (fun r -> r.counter_account)
  |> field "reason" (Jsont.option Jsont.string) (fun r -> r.reason)
  |> Jsont.Object.finish
;;

let category_similar =
  let field name codec (enc : Category_similarity.t -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "match" Jsont.string (fun _ ->
    "same institution and card merchant, or description; ignoring ASCII case and \
     whitespace")
  |> field "example" review (fun r -> r.seed)
  |> field "candidates" (Jsont.list review) (fun r -> r.candidates)
  |> field "conflicts" (Jsont.list review) (fun r -> r.conflicts)
  |> Jsont.Object.finish
;;

type transaction =
  { account_id : string
  ; review : Store.bank_review
  }

let transaction =
  let field name codec (enc : transaction -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "event_id" Json.int64_codec (fun r -> r.review.event_id)
  |> field "account_id" Jsont.string (fun r -> r.account_id)
  |> field "institution" Jsont.string (fun r -> r.review.institution)
  |> field "source_account" Jsont.string (fun r ->
    r.review.transaction.account.account_id)
  |> field "booked_on" Json.date (fun r -> r.review.transaction.booked_on)
  |> field "amount" Json.quantity (fun r -> r.review.transaction.amount)
  |> field "fee" Json.quantity (fun r -> r.review.transaction.fee)
  |> field "currency" Json.currency (fun r -> r.review.transaction.account.currency)
  |> field "description" Jsont.string (fun r ->
    String.concat ~sep:" | " r.review.transaction.descriptions)
  |> field "display_name" (Jsont.option Jsont.string) (fun r -> r.review.display_name)
  |> field "movement_type" (Jsont.option Jsont.string) (fun r ->
    Option.map r.review.movement_type ~f:Movement_type.to_string)
  |> field "detail_sources" (Jsont.list Json.int64_codec) (fun r ->
    r.review.detail_sources)
  |> field "counter_account" Jsont.string (fun r -> r.review.counter_account)
  |> field "classification" Jsont.string (fun r ->
    match r.review.classification with
    | Store.Unclassified -> "unclassified"
    | Category -> "category"
    | Transfer -> "transfer"
    | Settlement -> "settlement"
    | Reversal -> "reversal")
  |> field "reason" (Jsont.option Jsont.string) (fun r -> r.review.reason)
  |> Jsont.Object.finish
;;

let card_settlement_change =
  let field name codec (enc : Store.card_settlement_change -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "id" Json.int64_codec (fun r -> r.id)
  |> field "event_id" Json.int64_codec (fun r -> r.event_id)
  |> field "card_institution" Jsont.string (fun r -> r.card_institution)
  |> field "card_source_account" Jsont.string (fun r -> r.card_source_account)
  |> field "ledger_account" Jsont.string (fun r -> r.ledger_account)
  |> field "action" Jsont.string (fun r ->
    match r.action with
    | Confirmed -> "confirmed"
    | Undone -> "undone")
  |> field "changed_at" timestamp (fun r -> r.changed_at)
  |> Jsont.Object.finish
;;

let classification_rule =
  let field name codec (enc : Classification_rule.t -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "name" Jsont.string (fun r -> r.name)
  |> field "pattern" Jsont.string (fun r -> r.pattern)
  |> field "institution" (Jsont.option Jsont.string) (fun r -> r.institution)
  |> field "case_sensitive" Jsont.bool (fun r -> r.case_sensitive)
  |> field "action" Jsont.string (fun r -> Classification_rule.action_name r.action)
  |> field "target" Jsont.string (fun r -> Classification_rule.action_target r.action)
  |> Jsont.Object.finish
;;

let classification_rule_match =
  let field name codec (enc : Store.classification_rule_match -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "transaction" review (fun r -> r.transaction)
  |> field "eligible" Jsont.bool (fun r -> r.eligible)
  |> field "explanation" (Jsont.option Jsont.string) (fun r -> r.explanation)
  |> Jsont.Object.finish
;;

let category_change =
  let field name codec (enc : Store.category_change -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "id" Json.int64_codec (fun r -> r.id)
  |> field "event_id" Json.int64_codec (fun r -> r.event_id)
  |> field "previous_account" (Jsont.option Jsont.string) (fun r -> r.previous_account)
  |> field "account" Jsont.string (fun r -> r.account)
  |> field "reason" Jsont.string (fun r -> r.reason)
  |> field "changed_at" timestamp (fun r -> r.changed_at)
  |> Jsont.Object.finish
;;

let leg =
  let field name codec (enc : int64 * Source_event.t * Movement.t -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "event_id" Json.int64_codec (fun r ->
    let id, _, _ = r in
    id)
  |> field "institution" Jsont.string (fun r ->
    let _, event, _ = r in
    event.venue)
  |> field "source_account" Jsont.string (fun r ->
    let _, event, _ = r in
    event.account)
  |> field "booked_on" Json.date (fun r ->
    let _, _, tx = r in
    tx.booked_on)
  |> field "currency" Json.currency (fun r ->
    let _, _, tx = r in
    tx.account.currency)
  |> field "bank_amount" Json.quantity (fun r ->
    let _, _, tx = r in
    tx.amount)
  |> field "principal" Json.quantity (fun r ->
    let _, _, tx = r in
    Movement.principal tx)
  |> field "fee" Json.quantity (fun r ->
    let _, _, tx = r in
    tx.fee)
  |> field "description" Jsont.string (fun r ->
    let _, _, tx = r in
    String.concat ~sep:" | " tx.descriptions)
  |> Jsont.Object.finish
;;

let candidate =
  let field name codec (enc : Store.transfer_candidate -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "first" leg (fun r -> r.first_event_id, r.first, r.first_transaction)
  |> field "second" leg (fun r -> r.second_event_id, r.second, r.second_transaction)
  |> field "days_apart" Json.integer (fun r -> r.days_apart)
  |> field "unique_both_directions" Jsont.bool (fun r -> r.unique)
  |> field "iban_compatible" Jsont.bool (fun r -> r.iban_compatible)
  |> field "iban_match" Jsont.bool (fun r -> r.iban_match)
  |> field "iban_evidence" (Jsont.list Jsont.string) (fun r -> r.iban_evidence)
  |> field "state" Jsont.string (fun r ->
    Option.value_map r.state ~default:"unreviewed" ~f:Bank_transfer.state_name)
  |> field "reason" (Jsont.option Jsont.string) (fun r -> r.reason)
  |> Jsont.Object.finish
;;

let transfer_change =
  let field name codec (enc : Store.transfer_change -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "id" Json.int64_codec (fun r -> r.id)
  |> field "first_event_id" Json.int64_codec (fun r -> r.first_event_id)
  |> field "second_event_id" Json.int64_codec (fun r -> r.second_event_id)
  |> field "previous_state" (Jsont.option Jsont.string) (fun r -> r.previous_state)
  |> field "state" transfer_state (fun r -> r.state)
  |> field "reason" Jsont.string (fun r -> r.reason)
  |> field "changed_at" timestamp (fun r -> r.changed_at)
  |> Jsont.Object.finish
;;

let evidence =
  let field name codec (enc : Transfer_ranking.evidence -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "code" Jsont.string (fun r -> r.code)
  |> field "points" Json.integer (fun r -> r.points)
  |> field "detail" Jsont.string (fun r -> r.detail)
  |> Jsont.Object.finish
;;

let ranked =
  let field name codec (enc : Transfer_ranking.ranked -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "first" leg (fun r ->
    let r = r.candidate in
    r.first_event_id, r.first, r.first_transaction)
  |> field "second" leg (fun r ->
    let r = r.candidate in
    r.second_event_id, r.second, r.second_transaction)
  |> field "score" Json.integer (fun r -> r.score)
  |> field "confidence" Jsont.string (fun r -> r.confidence)
  |> field "iban_match" Jsont.bool (fun r -> r.candidate.iban_match)
  |> field "evidence" (Jsont.list evidence) (fun r -> r.evidence)
  |> field "iban_evidence" (Jsont.list Jsont.string) (fun r -> r.candidate.iban_evidence)
  |> field "component" Json.integer (fun r -> r.assignment.component)
  |> field "assignment_status" Jsont.string (fun r -> r.assignment.status)
  |> field "component_best_score" (Jsont.option Json.integer) (fun r ->
    r.assignment.best_score)
  |> field "exclusion_margin" (Jsont.option Json.integer) (fun r ->
    r.assignment.exclusion_margin)
  |> field "inclusion_penalty" (Jsont.option Json.integer) (fun r ->
    r.assignment.inclusion_penalty)
  |> Jsont.Object.finish
;;

let excluded =
  let field name codec (enc : Store.transfer_candidate -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "first_event_id" Json.int64_codec (fun r -> r.first_event_id)
  |> field "second_event_id" Json.int64_codec (fun r -> r.second_event_id)
  |> field "reasons" (Jsont.list Jsont.string) (fun r -> r.iban_evidence)
  |> Jsont.Object.finish
;;

let proposed =
  let field name codec (enc : Transfer_ranking.ranked -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "first_event_id" Json.int64_codec (fun r -> r.candidate.first_event_id)
  |> field "second_event_id" Json.int64_codec (fun r -> r.candidate.second_event_id)
  |> field "confidence" Jsont.string (fun r -> r.confidence)
  |> field "review_required" Jsont.bool (fun _r -> true)
  |> Jsont.Object.finish
;;

let preview =
  let field
        name
        codec
        (enc : Transfer_ranking.ranked list * Store.transfer_candidate list -> _)
        map
    =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "policy" Jsont.string (fun _r -> Transfer_ranking.policy)
  |> field "notice" Jsont.string (fun _r ->
    "Heuristic evidence, not calibrated probabilities. Conditional on imported records, \
     registered identities and the seven-day candidate policy. Review is required; \
     nothing is confirmed automatically.")
  |> field "component_vertex_limit" Json.integer (fun _r ->
    Transfer_ranking.max_component_vertices)
  |> field "excluded_pairs" (Jsont.list excluded) (fun r ->
    List.filter (snd r) ~f:(fun c -> not c.Store.iban_compatible))
  |> field "all_pairs" (Jsont.list candidate) snd
  |> field "proposed_pairs" (Jsont.list proposed) (fun r ->
    List.filter (fst r) ~f:(fun r ->
      String.equal r.Transfer_ranking.assignment.status "supported"))
  |> field "candidates" (Jsont.list ranked) (fun r -> fst r)
  |> Jsont.Object.finish
;;

type change_value =
  | Text of string
  | Reference of Bank_account_config.reference

let change_value =
  let text =
    Jsont.map
      Jsont.string
      ~dec:(fun s -> Text s)
      ~enc:(function
        | Text s -> s
        | Reference _ -> assert false)
  in
  let reference =
    Jsont.map
      Bank_account_config.reference_jsont
      ~dec:(fun r -> Reference r)
      ~enc:(function
        | Reference r -> r
        | Text _ -> assert false)
  in
  Jsont.any
    ~dec_string:text
    ~dec_object:reference
    ~enc:(function
      | Text _ -> text
      | Reference _ -> reference)
    ()
;;

let change_value_of_text (r : Store.account_change) value =
  Option.map value ~f:(fun text ->
    if String.equal r.kind "settlement_account"
    then
      Reference (Json.decode Bank_account_config.reference_jsont text |> Or_error.ok_exn)
    else Text text)
;;

let account_change =
  let field name codec (enc : Store.account_change -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "id" Json.int64_codec (fun r -> r.id)
  |> field "kind" Jsont.string (fun r -> r.kind)
  |> field "institution" Jsont.string (fun r -> r.institution)
  |> field "source_account" Jsont.string (fun r -> r.source_account)
  |> field "previous_value" (Jsont.option change_value) (fun r ->
    change_value_of_text r r.previous_value)
  |> field "value" (Jsont.option change_value) (fun r -> change_value_of_text r r.value)
  |> field "changed_at" timestamp (fun r -> r.changed_at)
  |> Jsont.Object.finish
;;

let account_config =
  let field name codec (enc : bool * Store.account_config_report -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "dry_run" Jsont.bool (fun r -> fst r)
  |> field "mapping_changes" Json.integer (fun r -> (snd r).mapping_changes)
  |> field "identity_changes" Json.integer (fun r -> (snd r).identity_changes)
  |> field "settlement_changes" Json.integer (fun r -> (snd r).settlement_changes)
  |> field "changes" (Jsont.list account_change) (fun r -> (snd r).changes)
  |> Jsont.Object.finish
;;

let paths =
  let field name codec (enc : App_paths.t -> _) map =
    Jsont.Object.mem name codec ~enc map
  in
  Jsont.Object.enc_only ()
  |> field "data_dir" Jsont.string (fun r -> r.data_dir)
  |> field "database" Jsont.string (fun r -> r.database)
  |> Jsont.Object.finish
;;

let companion =
  Jsont.Object.enc_only ()
  |> Jsont.Object.mem "row_id" Json.int64_codec ~enc:(fun (o, _) -> o.Store.row.row_id)
  |> Jsont.Object.mem "document_id" Json.int64_codec ~enc:(fun (o, _) ->
    o.Store.row.document_id)
  |> Jsont.Object.mem "row_number" Json.integer ~enc:(fun (o, _) ->
    o.Store.row.row_number)
  |> Jsont.Object.mem "institution" Jsont.string ~enc:(fun (o, _) ->
    o.Store.row.institution)
  |> Jsont.Object.mem "format" Jsont.string ~enc:(fun (o, _) -> o.Store.row.format)
  |> Jsont.Object.mem "event_id" (Jsont.option Json.int64_codec) ~enc:(fun (o, _) ->
    Option.bind o.Store.matching ~f:(fun m -> m.event_id))
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun (o, _) ->
    Option.value_map o.Store.matching ~default:"pending" ~f:(fun m ->
      Bank_match.state_name m.state))
  |> Jsont.Object.mem "reason" (Jsont.option Jsont.string) ~enc:(fun (o, _) ->
    Option.map o.Store.matching ~f:(fun m -> m.reason))
  |> Jsont.Object.mem
       "candidate_event_ids"
       (Jsont.list Json.int64_codec)
       ~enc:(fun (o, _) ->
         Option.value_map o.Store.matching ~default:[] ~f:(fun m -> m.candidates))
  |> Jsont.Object.keep_unknown (Json.members (Jsont.option Jsont.string)) ~enc:snd
  |> Jsont.Object.finish
;;
