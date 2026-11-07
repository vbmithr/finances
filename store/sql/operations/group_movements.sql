-- The movements of the groups whose canonical IDs are in the JSON array ?1, each group's
-- canonical operation first.
SELECT coalesce(g.canonical, m.operation_id) AS canonical, m.operation_id, m.anchor_event_id,
  m.role, m.account, m.commodity, m.quantity, m.ignored
FROM resolved_movements m
LEFT JOIN grouped_operations g ON g.member = m.operation_id
WHERE coalesce(g.canonical, m.operation_id) IN (SELECT value FROM json_each(?1))
ORDER BY canonical, coalesce(g.position, 0), m.operation_id, m.id
