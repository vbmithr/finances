-- Facts matching reads: card-side payments, and movements a link or exchange already holds.
-- Every connection creates these views when it opens, file by file in name order, so a
-- view comes after the views it reads. They hold no data: changing one needs no rebuild.

-- Each movement with every account showing the opposite amount in the same currency within
-- seven days. For card spending on a bank account, a row on the card's own account is the
-- card being paid: a transfer, not a settlement.
CREATE TEMP VIEW opposite_payments(event_id, venue, account, bank_id, account_id,
                                    account_type) AS
SELECT m.event_id, p.venue, p.account, p.bank_id, p.account_id, p.account_type
FROM movements m
JOIN movements p
  ON p.currency = m.currency
  AND p.booked_on BETWEEN date(m.booked_on, '-7 days') AND date(m.booked_on, '+7 days')
  AND decimal_cmp(p.amount, decimal_sub('0', m.amount)) = 0;

-- Each operation movement with what already claims it: a confirmed transfer link, or an
-- exchange decision on its operation. A claimed movement is not offered again.
CREATE TEMP VIEW movement_claims AS
SELECT m.id, m.operation_id, m.anchor_event_id, m.role, o.bank_event_id,
  EXISTS (SELECT 1 FROM operation_transfer_memberships t WHERE t.movement_id = m.id)
    AS linked,
  EXISTS (SELECT 1 FROM operation_exchange_decisions x WHERE x.operation_id = o.id)
    AS exchanged
FROM operation_movements m
JOIN operations o ON o.id = m.operation_id;
