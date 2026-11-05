-- Undecided card spending on a bank account, naming the card, that the card's own account
-- does not show being paid: no movement of the opposite amount in the same currency on
-- that card within seven days. The caller matches the bank and card accounts.
SELECT r.event_id
FROM reviews r
JOIN movements m ON m.event_id = r.event_id
WHERE r.card_reference IS NOT NULL
  AND r.decision = 'unclassified' AND r.reason IS NULL
  AND decimal_cmp(m.amount, '0') < 0 AND decimal_cmp(m.fee, '0') = 0
  AND NOT EXISTS (
    SELECT 1 FROM opposite_payments p
    WHERE p.event_id = m.event_id AND p.venue = m.venue AND p.bank_id = m.bank_id
      AND p.account_id = r.card_reference AND p.account_type = 'CREDITCRD')
ORDER BY coalesce(r.occurred_on, substr(r.occurred_at, 1, 10)), r.occurred_at, r.event_id
