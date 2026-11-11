-- What deleting the account in deleted_account would touch, after account_deletion_scope.
-- The first four count what it removes or what refuses it outright; every count after them
-- is something another account shares, which must be zero.
WITH account AS (SELECT * FROM deleted_account),
events AS (SELECT id FROM deleted_events),
documents AS (SELECT id FROM deleted_documents)
SELECT
  (SELECT count(*) FROM events) AS transactions,
  (SELECT count(*) FROM documents) AS documents,
  (SELECT count(*) FROM reversal_pairs
   WHERE original IN events OR reversal IN events) AS reversals,
  (SELECT count(*) FROM card_settlement_decisions, account a
   WHERE event_id IN events
     OR (card_institution = a.institution AND card_source_account = a.source_account))
    AS settlements,
  -- Documents that also carried another account's events, statements or inventories.
  (SELECT count(*) FROM import_document_events
   WHERE document_id IN documents AND event_id NOT IN events)
  + (SELECT count(*) FROM statements s, account a
     WHERE s.document_id IN documents
       AND (s.institution <> a.institution OR s.source_account <> a.source_account))
  + (SELECT count(*) FROM statement_totals t, account a
     WHERE t.document_id IN documents
       AND (t.institution <> a.institution OR t.source_account <> a.source_account))
  + (SELECT count(*) FROM statement_total_events l JOIN statement_totals t ON t.id = l.total_id
     WHERE (l.event_id IN events AND t.document_id NOT IN documents)
        OR (t.document_id IN documents AND l.event_id NOT IN events))
  + (SELECT count(*) FROM day_inventories d, account a
     WHERE d.document_id IN documents
       AND (d.institution <> a.institution OR d.source_account <> a.source_account))
  -- Companion rows and their matches, current or past.
  + (SELECT count(*) FROM companion_rows WHERE document_id IN documents)
  + (SELECT count(*) FROM companion_matches
     WHERE event_id IN events
       OR EXISTS (SELECT 1 FROM json_each(candidates_json)
                  WHERE CAST(value AS INTEGER) IN events))
  + (SELECT count(*) FROM companion_match_changes
     WHERE event_id IN events OR previous_event_id IN events
       OR EXISTS (SELECT 1 FROM json_each(candidates_json)
                  WHERE CAST(value AS INTEGER) IN events))
  -- Transfer decisions and links, current or past.
  + (SELECT count(*) FROM bank_transfer_decisions
     WHERE first_event_id IN events OR second_event_id IN events)
  + (SELECT count(*) FROM bank_transfer_changes
     WHERE first_event_id IN events OR second_event_id IN events)
  + (SELECT count(*) FROM operation_transfer_memberships t
     JOIN operation_movements m ON m.id = t.movement_id
     WHERE m.anchor_event_id IN events)
  + (SELECT count(*) FROM operation_transfer_changes t
     JOIN operation_movements a ON a.id = t.first_movement_id
     JOIN operation_movements b ON b.id = t.second_movement_id
     WHERE a.anchor_event_id IN events OR b.anchor_event_id IN events)
  -- Other accounts settled through this one, now or before.
  + (SELECT count(*) FROM bank_account_settlements s, account a
     WHERE s.settlement_account = a.settlement_target
       AND NOT (s.institution = a.institution AND s.source_account = a.source_account))
  + (SELECT count(*) FROM bank_account_settlement_changes s, account a
     WHERE (s.settlement_account = a.settlement_target
            OR s.previous_settlement = a.settlement_target)
       AND NOT (s.institution = a.institution AND s.source_account = a.source_account))
  -- Operations built from this account's events and another's.
  + (SELECT count(DISTINCT s.operation_id) FROM operation_sources s
     WHERE s.event_id IN events
       AND EXISTS (SELECT 1 FROM operation_sources other
                   WHERE other.operation_id = s.operation_id
                     AND other.event_id NOT IN events))
    AS shared
