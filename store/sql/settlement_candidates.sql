-- Undecided card spending on a bank account, with each card account the card it names
-- belongs to (see card_accounts), where that account does not show being paid: no movement
-- of the opposite amount in the same currency on it within seven days. The caller checks
-- the bank and card accounts.
SELECT r.event_id, c.card_source_account
FROM reviews r
JOIN movements m ON m.event_id = r.event_id
JOIN card_accounts c ON c.institution = m.venue AND c.card_reference = r.card_reference
WHERE r.decision = 'unclassified' AND r.reason IS NULL
  AND decimal_cmp(m.amount, '0') < 0 AND decimal_cmp(m.fee, '0') = 0
  AND NOT EXISTS (
    SELECT 1 FROM opposite_payments p
    WHERE p.event_id = m.event_id AND p.venue = c.institution
      AND p.account = c.card_source_account)
ORDER BY r.listed_on, r.occurred_at, r.event_id, c.card_source_account
