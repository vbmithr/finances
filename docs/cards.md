# Cards

Status: implemented 2026-11-06, except `fin card show`.

## The problem

A card account can outlive its cards. A bank replaces a card when it expires or is lost, and the new
card has a new number; the account behind it, and the bank account that pays it, stay the same. A
partner's card can share the account too.

Fin does not know this. It matches a card settlement — the monthly debit on a bank account that pays
a deferred-debit card — to the card account whose **account number equals the card number** in the
debit's text. That holds for the current card only. With three cards over ten years on one card
account, the debits of the two earlier cards match nothing and are left out of
`fin settlement candidates`:

| Card (fictional numbers) | Settlement debits on checking | Matched today |
|---|---|---|
| `4970XXXXXXXX1111` | 26, 2017–2020 | no |
| `4970XXXXXXXX2222` | 33, 2019–2021 | no |
| `4810XXXXXXXX3333`, the account's number | 54, 2022–2026 | yes |

The workaround is a settlement rule, `fin rule add-settlement NAME REGEX CARD-ID`, which targets a
card account by ID whatever number the text names. It works, but it stores a fact — this card belonged
to that account — as a pattern on descriptions, and nothing lists which cards fin has seen or where
each one belongs.

## What fin knows today

| Fact | Where | From |
|---|---|---|
| A card account | an imported account of type `CREDITCRD`, its ledger name in `bank_account_mappings` | the card's own statements |
| Which bank account pays it | `bank_account_settlements`, set by `fin account link-card` | the person |
| A card number on a bank debit | `movement_evidence.card_reference` | the institution's evidence rule, for BoursoBank the text after `Relevé différé Carte` |
| A recorded settlement | `card_settlement_decisions`: the debit, the card account, its ledger account | `fin settlement review` |
| A purchase | a row of the card account | the card's statements |

Purchase rows do not name the card. In BoursoBank exports the card account's number is the current
card's number, and the purchase descriptions carry no card number, so purchases belong to the card
**account**; when several cards share one in the same period, fin cannot tell which card bought what.

## Model

A card is a card number as statements print it, masked by the bank, belonging to at most one card
account. Fin never sees or stores a full card number.

Cards come from two places:

- **Stated**: a card account's own number is one of its cards. Derived from imported accounts, as
  matching does today, and never stored.
- **Assigned**: a person says that a number belongs to an account. Stored, audited and exported like
  other account settings.

A view joins both, so that matching reads one place:

```sql
-- Each card number with the card account it belongs to, and how fin knows.
CREATE TEMP VIEW card_accounts(institution, card_reference, card_institution,
                               card_source_account, known_by) AS ...
```

When a card was used is derived from the debits that name it, never stored.

## Commands

`fin card list [--json]` lists every card number fin has seen, on a bank row or as a card account:
the number as printed, its account (ID and ledger name) or `unassigned`, how it is known, the first
and last bank row naming it (its payments, and any refund), and how many are settled or undecided. It also shows from when the
account has purchases, so that a card used before its account's statements begin stands out.

`fin card assign CARD ACCOUNT-ID` assigns a card number, given as printed or by a unique ending
such as its last digits, to a card account, after a preview listing
the settlement debits that would become candidates; it asks before writing. It refuses an account
that is not a card account, an account at another institution, and a number that is already another
account's own number or assigned elsewhere.

`fin card unassign CARD` removes an assignment, refusing while settlements recorded through it
remain; undo those first, as for other decisions.

Later, `fin card show CARD`: the card's settlements and its account's purchases over the card's
period, saying when another card shared the account then.

`fin account link-card` stays where it is: it links a card *account* to the bank account that pays
it, which is a setting of accounts, not of cards.

## Matching

Settlement matching reads `card_accounts` instead of comparing a card number with an account number:

- `settlement_candidates.sql` joins `card_accounts` on the debit's card reference, and finds the card
  side's payment in `opposite_payments` by the card account it returns, instead of
  `p.account_id = r.card_reference`;
- the OCaml in `bin/settlement_cmd.ml` that searches accounts for `account_id = hint` goes; it keeps
  the checks on the account itself (a card account, linked to the paying bank account, in its
  currency).

Settlement rules stay, for debits that name no card at all.

## Decisions and schema

Assignments are decisions: `fin decisions export` gains an optional `cards` section, restored before
settlements, which need it to match. An export without it restores as before.

Two tables, `card_assignments` (institution, card reference, card account) and
`card_assignment_changes` for the history, make schema 37: one rebuild through `fin decisions`.

## What it does not solve

Purchases before a card account's statements begin. If a bank exports a card account's history only
from its current card on, the earlier cards' settlements, once assigned, pay into a card account that
holds no purchases for those years, and its balance is off by their total. `fin card list` makes that
period visible, but the purchases have to come from the bank.

## Open questions

- Without those purchases, record the earlier settlements as unitemized spending (a category, not a
  settlement), or settle them and balance the period with one adjustment entry? The first is simpler;
  the second keeps the card account's settlements complete.
- Should `fin card list` include card numbers seen only on purchase rows of other institutions'
  exports, once an institution names cards there?
