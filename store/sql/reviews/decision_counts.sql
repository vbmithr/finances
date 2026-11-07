-- Bank movements counted by event kind, decision, category and type. The caller keeps the
-- kinds its registry reads as movements.
SELECT kind, decision,
  CASE WHEN decision = 'category' THEN counter_account ELSE '' END AS category,
  coalesce(movement_type, 'unknown') AS type, count(*) AS count
FROM reviews
GROUP BY 1, 2, 3, 4
