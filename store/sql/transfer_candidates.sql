-- Movement pairs that may be one transfer between owned accounts: exactly opposite
-- principals in one currency, on different accounts, booked at most seven days apart.
-- Bank_transfer.eligible states the same rule for confirmations.
--
-- ?1 is 0 for pairs still to review: both sides undecided and the pair never decided. It is 1
-- to add reviewed pairs: those decided either way, and those with a side already decided
-- elsewhere, except a categorized side the pair was never decided for. A settled or reversed
-- movement is never a candidate. Whether the accounts' IBANs allow the pair is checked by
-- the caller.
SELECT a.event_id AS first, b.event_id AS second, d.state, d.reason
FROM movements a
JOIN movements b
  ON b.currency = a.currency AND b.principal_magnitude = a.principal_magnitude
  AND b.principal_sign <> a.principal_sign
  AND b.booked_on BETWEEN date(a.booked_on, '-7 days') AND date(a.booked_on, '+7 days')
JOIN reviews ra ON ra.event_id = a.event_id
JOIN reviews rb ON rb.event_id = b.event_id
LEFT JOIN bank_transfer_decisions d
  ON d.first_event_id = a.event_id AND d.second_event_id = b.event_id
WHERE a.event_id < b.event_id
  AND a.principal_magnitude <> '0'
  AND NOT (a.venue = b.venue AND a.account = b.account)
  AND ra.decision NOT IN ('settlement', 'reversal')
  AND rb.decision NOT IN ('settlement', 'reversal')
  AND CASE WHEN ?1
    THEN d.state IS NOT NULL OR 'category' NOT IN (ra.decision, rb.decision)
    ELSE d.state IS NULL AND ra.decision = 'unclassified' AND rb.decision = 'unclassified'
  END
ORDER BY a.event_id, b.event_id
