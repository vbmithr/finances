-- Views every connection's queries share. They are temporary because they hold no data:
-- changing one needs neither a schema version nor a rebuild.

-- Operations a confirmed transfer link joins, as (canonical, member).
CREATE TEMP VIEW linked_operations(first, second) AS
SELECT a.operation_id, b.operation_id
FROM operation_transfer_links l
JOIN operation_movements a ON a.id = l.first_movement_id
JOIN operation_movements b ON b.id = l.second_movement_id;

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

-- A bank movement linked to an operation without one: the reason and the account resolved
-- on that side.
CREATE TEMP VIEW operation_transfer_sides(event_id, reason, account) AS
SELECT own.anchor_event_id, l.reason, r.account
FROM operation_movements own
JOIN operations o ON o.id = own.operation_id AND o.bank_event_id = own.anchor_event_id
JOIN operation_transfer_memberships t ON t.movement_id = own.id
JOIN operation_transfer_links l
  ON l.first_movement_id = t.first_movement_id AND l.second_movement_id = t.second_movement_id
JOIN operation_movements other
  ON other.id = CASE WHEN own.id = l.first_movement_id
    THEN l.second_movement_id ELSE l.first_movement_id END
JOIN operations oo ON oo.id = other.operation_id AND oo.bank_event_id IS NULL
JOIN operation_movement_resolutions r ON r.movement_id = other.id;

-- One row per bank movement: its decision, counter-account and effective description,
-- the name its matched companion rows agree on or else its main description. This is the
-- one place a movement's decision is stated; everything that counts or lists decisions
-- reads it from here.
CREATE TEMP VIEW reviews AS
SELECT m.event_id, e.kind, m.venue AS institution, m.account AS source, m.booked_on,
  m.amount, m.currency, e.occurred_at, e.occurred_on,
  coalesce(
    (SELECT CASE WHEN count(DISTINCT c.display_name) = 1 THEN min(c.display_name) END
     FROM companion_matches cm JOIN companion_rows c ON c.id = cm.row_id
     WHERE cm.event_id = m.event_id AND cm.state = 'matched'),
    m.descriptions ->> ('$[' || m.main_description || ']'),
    '') AS description,
  o.movement_type AS override,
  coalesce(o.movement_type, m.movement_type) AS movement_type,
  CASE
    WHEN r.original IS NOT NULL THEN 'reversal'
    WHEN d.state = 'confirmed' OR m.link_account IS NOT NULL THEN 'transfer'
    WHEN s.event_id IS NOT NULL THEN 'settlement'
    WHEN coalesce(c.account, 'equity:unclassified') <> 'equity:unclassified' THEN 'category'
    ELSE 'unclassified'
  END AS decision,
  CASE
    WHEN r.original IS NOT NULL
      THEN 'assets:reversals:pair-' || r.original || '-' || r.reversal
    WHEN d.state = 'confirmed' THEN g.ledger_account
    WHEN m.link_account IS NOT NULL THEN m.link_account
    WHEN s.event_id IS NOT NULL THEN s.ledger_account
    ELSE coalesce(c.account, 'equity:unclassified')
  END AS counter_account,
  -- What the journal posts against: a bank transfer goes through its pair's account, and
  -- a link to an operation is posted with that operation, not here.
  CASE
    WHEN r.original IS NOT NULL
      THEN 'assets:reversals:pair-' || r.original || '-' || r.reversal
    WHEN d.state = 'confirmed'
      THEN 'assets:transfers:pair-' || min(d.first_event_id, d.second_event_id) || '-'
        || max(d.first_event_id, d.second_event_id)
    WHEN s.event_id IS NOT NULL THEN s.ledger_account
    ELSE c.account
  END AS posting_account,
  coalesce(r.reason, CASE WHEN d.state = 'confirmed' THEN d.reason END, m.link_reason, s.reason,
    c.reason) AS reason,
  v.card_reference, v.withdrawal_reversal
FROM (
  -- A link to an operation without a bank movement, read by event: a view on the right of
  -- a LEFT JOIN would be computed whole for every lookup.
  SELECT *,
    (SELECT account FROM operation_transfer_sides x WHERE x.event_id = movements.event_id)
      AS link_account,
    (SELECT reason FROM operation_transfer_sides x WHERE x.event_id = movements.event_id)
      AS link_reason
  FROM movements) m
JOIN source_events e ON e.id = m.event_id
LEFT JOIN movement_type_overrides o ON o.event_id = m.event_id
LEFT JOIN bank_categories c ON c.event_id = m.event_id
LEFT JOIN card_settlement_decisions s ON s.event_id = m.event_id
-- A confirmed transfer with another bank movement, and that movement's account.
LEFT JOIN bank_transfer_memberships bm ON bm.event_id = m.event_id
LEFT JOIN bank_transfer_decisions d
  ON d.first_event_id = bm.first_event_id AND d.second_event_id = bm.second_event_id
LEFT JOIN movements other
  ON other.event_id = CASE WHEN bm.event_id = bm.first_event_id
    THEN bm.second_event_id ELSE bm.first_event_id END
LEFT JOIN bank_account_mappings g
  ON g.institution = other.venue AND g.source_account = other.account
LEFT JOIN reversal_pairs r ON r.original = m.event_id OR r.reversal = m.event_id
LEFT JOIN movement_evidence v ON v.event_id = m.event_id;

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
