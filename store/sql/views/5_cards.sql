-- Card numbers and the card accounts they belong to.
-- Every connection creates these views when it opens, file by file in name order, so a
-- view comes after the views it reads. They hold no data: changing one needs no rebuild.

-- Each card number with the card account it belongs to, and how fin knows: a card account's
-- own number is one of its cards ('statement'), and a person can assign others, such as a
-- replaced card's ('assigned'). Card numbers are as the institution prints them.
CREATE TEMP VIEW card_accounts(institution, card_reference, card_source_account, known_by) AS
SELECT DISTINCT venue, account_id, account, 'statement'
FROM movements
WHERE account_type IN ('CREDITCRD', 'credit-card')
UNION ALL
SELECT institution, card_reference, card_source_account, 'assigned'
FROM card_assignments;
