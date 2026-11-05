-- The companion rows matched to each movement.
SELECT event_id, row_id
FROM companion_matches
WHERE state = 'matched' AND event_id IS NOT NULL
ORDER BY row_id
