-- What a batch of category decisions is checked against, for the event IDs in the JSON array
-- ?1, in that order: the event, what already decides its movement, its current category and
-- primary movement, and its bank journal entry (projection key ?2).
SELECT v.value AS event_id, e.venue, e.account, e.kind,
  EXISTS (SELECT 1 FROM bank_transfer_memberships m
          JOIN bank_transfer_decisions d
            ON d.first_event_id = m.first_event_id AND d.second_event_id = m.second_event_id
          WHERE m.event_id = v.value AND d.state = 'confirmed') AS transfer,
  EXISTS (SELECT 1 FROM movement_claims WHERE anchor_event_id = v.value AND linked)
    AS operation_linked,
  EXISTS (SELECT 1 FROM card_settlement_decisions WHERE event_id = v.value) AS settlement,
  EXISTS (SELECT 1 FROM reversal_pairs WHERE original = v.value OR reversal = v.value)
    AS reversal,
  c.account AS previous_account, c.reason AS previous_reason,
  (SELECT id FROM operation_movements WHERE anchor_event_id = v.value AND role = 'primary')
    AS movement_id,
  j.id AS entry_id, j.date, j.description
FROM json_each(?1) v
LEFT JOIN source_events e ON e.id = v.value
LEFT JOIN bank_categories c ON c.event_id = v.value
LEFT JOIN journal_entries j ON j.event_id = v.value AND j.projection_key = ?2
ORDER BY v.key
