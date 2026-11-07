-- The decision rule: what each bank movement is, once, for everything that counts or lists it.
-- Every connection creates these views when it opens, file by file in name order, so a
-- view comes after the views it reads. They hold no data: changing one needs no rebuild.

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
  -- Listings order by this day, then the instant, then the event ID.
  coalesce(e.occurred_on, substr(e.occurred_at, 1, 10)) AS listed_on,
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
