-- The operations a listing shows: each group once, under its canonical operation. ?1 is a
-- JSON object of filters; an absent member filters nothing. The caller appends the order
-- and the limit.
SELECT v.id, v.occurred, v.anchor, v.institution, v.description,
  x.operation_id IS NOT NULL AS exchange,
  (SELECT value FROM operation_fields WHERE operation_id = v.id AND name = 'Amount'
   ORDER BY event_id, ordinal LIMIT 1) AS reported_amount,
  (SELECT value FROM operation_fields WHERE operation_id = v.id AND name = 'Currency'
   ORDER BY event_id, ordinal LIMIT 1) AS reported_currency
FROM operation_rows v
LEFT JOIN operation_exchange_decisions x ON x.operation_id = v.id
WHERE v.canonical = v.id
  AND (?1 ->> '$.from_date' IS NULL OR substr(v.occurred, 1, 10) >= ?1 ->> '$.from_date')
  AND (?1 ->> '$.to_date' IS NULL OR substr(v.occurred, 1, 10) <= ?1 ->> '$.to_date')
  AND (?1 ->> '$.search' IS NULL
    OR instr(lower(v.description), lower(?1 ->> '$.search')) > 0)
  -- The remaining filters hold when any movement of the group passes.
  AND (?1 ->> '$.institution' IS NULL OR v.institution = ?1 ->> '$.institution'
    OR v.id IN (SELECT canonical FROM group_reviews
                WHERE institution = ?1 ->> '$.institution'))
  AND (?1 ->> '$.account_institution' IS NULL
    OR v.id IN (SELECT canonical FROM group_reviews
                WHERE institution = ?1 ->> '$.account_institution'
                  AND source = ?1 ->> '$.account_source'))
  -- '-' selects movements with no type.
  AND (?1 ->> '$.movement_type' IS NULL
    OR v.id IN (SELECT canonical FROM group_reviews
                WHERE coalesce(movement_type, '-') = ?1 ->> '$.movement_type'))
  AND (?1 ->> '$.decision' IS NULL
    OR v.id IN (SELECT canonical FROM group_reviews WHERE decision = ?1 ->> '$.decision'))
  AND (?1 ->> '$.counter_account' IS NULL
    OR v.id IN (SELECT canonical FROM group_reviews
                WHERE counter_account = ?1 ->> '$.counter_account'))
