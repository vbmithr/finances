-- For each unclassified transfer, deposit or withdrawal not offered as a candidate (the
-- event IDs in the JSON array ?1), the nearest thing to a partner: the opposite principal
-- within 7 days (decided, rejected or otherwise excluded), within 30 days, or a principal
-- within a fee (2 or 2%) within 7 days.
WITH own AS MATERIALIZED (
  SELECT x.event_id, x.institution, x.description, m.booked_on, m.currency, m.venue,
    m.account, m.principal_sign, m.principal_magnitude
  FROM reviews x JOIN movements m ON m.event_id = x.event_id
  WHERE x.decision = 'unclassified'
    AND x.movement_type IN ('transfer', 'deposit', 'withdrawal')
    AND x.event_id NOT IN (SELECT value FROM json_each(?1))
),
-- Movements on the other side: same currency, opposite sign, another account.
opposite AS (
  SELECT r.event_id, o.event_id AS other, o.principal_magnitude AS magnitude,
    r.principal_magnitude AS own_magnitude,
    CAST(abs(julianday(o.booked_on) - julianday(r.booked_on)) AS INTEGER) AS days
  FROM own r
  JOIN movements o ON o.currency = r.currency
    AND o.booked_on BETWEEN date(r.booked_on, '-30 days') AND date(r.booked_on, '+30 days')
  WHERE o.principal_sign <> r.principal_sign
    AND NOT (o.venue = r.venue AND o.account = r.account)
),
exact AS (
  SELECT event_id, other, days,
    row_number() OVER (PARTITION BY event_id ORDER BY days, other) AS rank
  FROM opposite
  WHERE magnitude = own_magnitude
),
near AS (
  SELECT event_id, other, days, difference,
    row_number() OVER (PARTITION BY event_id ORDER BY days, other) AS rank
  FROM (
    SELECT *, CASE WHEN decimal_cmp(gap, '0') < 0 THEN decimal_sub('0', gap) ELSE gap END
      AS difference
    FROM (SELECT *, decimal_sub(magnitude, own_magnitude) AS gap FROM opposite)
    WHERE days <= 7 AND event_id NOT IN (SELECT event_id FROM exact)
  )
  WHERE decimal_cmp(difference, '2') <= 0
    OR decimal_cmp(difference, decimal_mul(own_magnitude, '0.02')) <= 0
)
SELECT r.event_id, r.institution, r.booked_on, r.principal_sign, r.principal_magnitude,
  r.currency, r.description,
  CASE
    WHEN e.days <= 7 THEN CASE
      WHEN ox.decision <> 'unclassified' THEN 'decided'
      WHEN EXISTS (SELECT 1 FROM bank_transfer_decisions d
                   WHERE d.state = 'rejected'
                     AND d.first_event_id = min(r.event_id, e.other)
                     AND d.second_event_id = max(r.event_id, e.other)) THEN 'rejected'
      ELSE 'excluded' END
    WHEN e.other IS NOT NULL THEN 'days'
    WHEN n.other IS NOT NULL THEN 'amount'
    ELSE 'none'
  END AS kind,
  coalesce(e.other, n.other) AS related,
  ox.decision AS related_decision,
  coalesce(e.days, n.days) AS days,
  n.difference
FROM own r
LEFT JOIN exact e ON e.event_id = r.event_id AND e.rank = 1
LEFT JOIN near n ON n.event_id = r.event_id AND n.rank = 1
LEFT JOIN reviews ox ON ox.event_id = e.other
ORDER BY r.booked_on DESC, r.event_id DESC
