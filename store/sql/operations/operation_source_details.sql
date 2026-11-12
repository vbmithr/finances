-- Source fields for the selected operation groups, in list and source order.
-- A source shared by several member operations is shown once per group.
WITH wanted AS MATERIALIZED (
  SELECT CAST(key AS INTEGER) AS position, CAST(value AS INTEGER) AS operation_id
  FROM json_each(?1)
), members AS (
  SELECT position, operation_id, operation_id AS member FROM wanted
  UNION
  SELECT w.position, w.operation_id, g.member
  FROM grouped_operations g JOIN wanted w ON w.operation_id = g.canonical
), sources AS (
  SELECT DISTINCT m.position, m.operation_id, s.event_id
  FROM members m JOIN operation_sources s ON s.operation_id = m.member
)
SELECT sources.operation_id, e.id AS event_id, e.venue,
       coalesce(e.occurred_at, e.occurred_on) AS occurred, f.name, f.value
FROM sources
JOIN source_events e ON e.id = sources.event_id
LEFT JOIN source_event_fields f ON f.event_id = e.id
ORDER BY sources.position, e.id, f.ordinal;
