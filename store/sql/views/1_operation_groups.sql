-- Which operations read as one: confirmed transfer links, and the groups records state.
-- Every connection creates these views when it opens, file by file in name order, so a
-- view comes after the views it reads. They hold no data: changing one needs no rebuild.

-- Each confirmed transfer link between operations, with both movements.
CREATE TEMP VIEW operation_links AS
SELECT l.first_movement_id, l.second_movement_id, l.reason,
  a.anchor_event_id AS first_event_id, b.anchor_event_id AS second_event_id,
  a.operation_id AS first_operation_id, b.operation_id AS second_operation_id,
  a.role AS first_role, b.role AS second_role
FROM operation_transfer_links l
JOIN operation_movements a ON a.id = l.first_movement_id
JOIN operation_movements b ON b.id = l.second_movement_id;

-- Operations a confirmed transfer link joins, as (canonical, member).
CREATE TEMP VIEW linked_operations(first, second) AS
SELECT first_operation_id, second_operation_id FROM operation_links;

-- Operations whose records state a group, outside any link: a decision outranks the source.
CREATE TEMP VIEW stated_operations(venue, group_key, operation_id, supporting) AS
SELECT DISTINCT e.venue, e.payload ->> '$.record.group', o.id,
  coalesce(e.payload ->> '$.record.supporting', 0) <> 0
FROM operations o
JOIN operation_sources s ON s.operation_id = o.id
JOIN source_events e ON e.id = s.event_id
WHERE CASE WHEN json_valid(e.payload)
  THEN e.payload ->> '$.format' = 'fin-records-row'
    AND e.payload ->> '$.record.group' IS NOT NULL
  ELSE 0 END
  AND o.id NOT IN (SELECT first FROM linked_operations UNION SELECT second FROM linked_operations);

-- Each operation that reads under another's ID, with its place in the group after the
-- canonical one. An operation in no row is alone.
CREATE TEMP VIEW grouped_operations(member, canonical, position) AS
SELECT second, first, 1 FROM linked_operations
UNION ALL
SELECT operation_id, canonical, position - 1
FROM (
  SELECT operation_id,
    first_value(operation_id) OVER grp AS canonical,
    row_number() OVER grp AS position
  FROM stated_operations
  WINDOW grp AS (PARTITION BY venue, group_key ORDER BY supporting, operation_id)
)
WHERE position > 1;
