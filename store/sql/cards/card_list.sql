-- Every card fin has seen, on a bank row naming it or as a card account's own number: the
-- card account it belongs to and how fin knows, the bank rows naming it, and from when its
-- account has purchases of its own.
WITH named AS (
  SELECT m.venue AS institution, v.card_reference, m.booked_on, r.decision
  FROM movement_evidence v
  JOIN movements m ON m.event_id = v.event_id
  JOIN reviews r ON r.event_id = v.event_id
  WHERE v.card_reference IS NOT NULL
),
card_rows AS (
  SELECT institution, card_reference, count(*) AS bank_rows, min(booked_on) AS first_seen,
    max(booked_on) AS last_seen, sum(decision = 'settlement') AS settled,
    sum(decision = 'unclassified') AS undecided
  FROM named
  GROUP BY institution, card_reference
),
cards AS (
  SELECT institution, card_reference FROM card_rows
  UNION
  SELECT institution, card_reference FROM card_accounts
)
SELECT k.institution, k.card_reference, c.known_by, c.card_source_account, g.public_id,
  g.ledger_account,
  coalesce(d.bank_rows, 0) AS bank_rows, d.first_seen, d.last_seen,
  coalesce(d.settled, 0) AS settled, coalesce(d.undecided, 0) AS undecided,
  (SELECT min(booked_on) FROM movements p
   WHERE p.venue = c.institution AND p.account = c.card_source_account) AS purchases_from
FROM cards k
LEFT JOIN card_rows d
  ON d.institution = k.institution AND d.card_reference = k.card_reference
LEFT JOIN card_accounts c
  ON c.institution = k.institution AND c.card_reference = k.card_reference
LEFT JOIN bank_account_mappings g
  ON g.institution = c.institution AND g.source_account = c.card_source_account
ORDER BY k.institution, d.first_seen IS NULL, d.first_seen, k.card_reference
