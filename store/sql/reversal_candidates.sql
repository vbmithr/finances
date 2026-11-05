-- Withdrawals a later deposit of the same amount on the same account reverses, where the
-- source marks the deposit as a withdrawal reversal. Both are undecided and have no fee.
-- Reversals in listing order, each with its possible originals in listing order.
SELECT o.event_id AS original, r.event_id AS reversal
FROM reviews r
JOIN movements rm ON rm.event_id = r.event_id
JOIN movements om
  ON om.venue = rm.venue AND om.bank_id = rm.bank_id AND om.account_id = rm.account_id
  AND om.account_type = rm.account_type AND om.currency = rm.currency
  AND om.booked_on <= rm.booked_on
JOIN reviews o ON o.event_id = om.event_id
WHERE r.decision = 'unclassified' AND r.reason IS NULL
  AND r.movement_type = 'withdrawal' AND r.withdrawal_reversal = 'yes'
  AND o.decision = 'unclassified' AND o.reason IS NULL AND o.movement_type = 'withdrawal'
  AND decimal_cmp(rm.amount, '0') > 0 AND decimal_cmp(om.amount, '0') < 0
  AND decimal_cmp(decimal_sub('0', om.amount), rm.amount) = 0
  AND decimal_cmp(rm.fee, '0') = 0 AND decimal_cmp(om.fee, '0') = 0
ORDER BY coalesce(r.occurred_on, substr(r.occurred_at, 1, 10)), r.occurred_at, r.event_id,
  coalesce(o.occurred_on, substr(o.occurred_at, 1, 10)), o.occurred_at, o.event_id
