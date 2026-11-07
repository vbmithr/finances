-- Operations as listings show them, and their groups' reviews.
-- Every connection creates these views when it opens, file by file in name order, so a
-- view comes after the views it reads. They hold no data: changing one needs no rebuild.

-- The provider's own columns of an operation's source rows.
CREATE TEMP VIEW operation_fields(operation_id, event_id, ordinal, name, value) AS
SELECT s.operation_id, f.event_id, f.ordinal, f.name, f.value
FROM operation_sources s
JOIN source_event_fields f ON f.event_id = s.event_id;

-- Each stored operation with its group's canonical ID and its anchor, the primary
-- movement first.
CREATE TEMP VIEW operation_anchors(id, occurred, canonical, anchor) AS
SELECT o.id, o.occurred, coalesce(g.canonical, o.id),
  (SELECT anchor_event_id FROM operation_movements
   WHERE operation_id = o.id ORDER BY role <> 'primary', id LIMIT 1)
FROM operations o
LEFT JOIN grouped_operations g ON g.member = o.id;

-- Each stored operation as a listing shows it: its anchor's review, or the provider's own
-- columns when there is none.
CREATE TEMP VIEW operation_rows AS
SELECT p.id, p.occurred, p.canonical, p.anchor, r.event_id, r.booked_on, r.amount,
  r.currency, r.movement_type, r.counter_account, r.description AS review_description,
  a.public_id AS account_id,
  CASE WHEN r.event_id IS NOT NULL THEN r.institution ELSE
    (SELECT CASE WHEN count(DISTINCT venue) = 1 THEN min(venue) ELSE '' END
     FROM (SELECT e.venue FROM operation_sources s JOIN source_events e ON e.id = s.event_id
           WHERE s.operation_id = p.id
           UNION ALL
           SELECT e.venue FROM operation_movements m
           JOIN source_events e ON e.id = m.anchor_event_id
           WHERE m.operation_id = p.id))
  END AS institution,
  CASE WHEN r.event_id IS NOT NULL THEN r.description ELSE coalesce(
    (SELECT value FROM operation_fields WHERE operation_id = p.id
       AND name = 'Transaction Description' ORDER BY event_id, ordinal LIMIT 1),
    (SELECT value FROM operation_fields WHERE operation_id = p.id
       AND name = 'Description' ORDER BY event_id, ordinal LIMIT 1),
    (SELECT value FROM operation_fields WHERE operation_id = p.id
       AND name = 'description' ORDER BY event_id, ordinal LIMIT 1),
    (SELECT CAST(e.payload AS TEXT) ->> '$.record.details.descriptions[0]'
     FROM source_events e WHERE e.id = p.anchor AND e.kind = 'records-operation'),
    '')
  END AS description
FROM operation_anchors p
LEFT JOIN reviews r ON r.event_id = p.anchor
LEFT JOIN bank_account_mappings a
  ON a.institution = r.institution AND a.source_account = r.source;

-- A review of every movement an operation's group holds, under the group's ID.
CREATE TEMP VIEW group_reviews AS
SELECT coalesce(g.canonical, m.operation_id) AS canonical, x.*
FROM reviews x
JOIN operation_movements m ON m.anchor_event_id = x.event_id
LEFT JOIN grouped_operations g ON g.member = m.operation_id;
