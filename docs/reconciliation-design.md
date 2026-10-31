# Reconciling overlapping statement files

How to combine several exports of the same account (OFX, CAMT, MT940, CSV),
each possibly overlapping the others and containing duplicates. It is design
background. The import model in use remains primary/companion; the alternative
explored here is deferred in [observation model](observation-model.md).

## 1. Observations are not transactions

Store every file row as an immutable *observation*: raw fields, source
document, row number. A transaction is derived by grouping observations that
describe the same real-world movement. Fin already keeps companion rows apart
from their matches (`companion_rows`, `companion_matches`). The proposed change
is that unmatched rows also yield a transaction, instead of waiting for a
structured event.

## 2. Identity key per observation

- **Stable bank ID** (OFX FITID, CAMT reference): key is `(account, id)`.
  Re-imports and overlapping exports collapse exactly (`Source_ids`).
- **No ID** (most CSVs, MT940 without references): build a natural key from
  account, date, amount, currency and normalized description. This key is weak
  because it is not unique.

## 3. Duplicates and occurrence numbers

Two real purchases can share a natural key (two identical coffees on one day).

- Within one file, number identical keys 1st, 2nd, ... and use
  `key + occurrence` as identity.
- Across overlapping files, take the **maximum** count per key, not the sum.
  Two files showing two coffees mean two coffees; a file showing three means
  three.
- A partially covered day breaks the max rule, which is why `Day_inventory`
  refuses partial or corrected days for review.

## 4. Cross-format matching in tiers

1. Exact match on account, amount, currency and booking date, with a
   compatible description (`Csv_exact_match`).
2. A single candidate on each side: accept.
3. Several candidates on either side: mark the row **ambiguous**, do not merge.

Never guess. An unmerged observation stays its own transaction and is flagged
for review. A wrong merge hides money; a missed merge is a visible duplicate.

## 5. Enrich, do not replace

On merge the structured record stays authoritative for amount, date and ID.
Extra fields from the other source (category, value date, fees) are applied as
annotations. Disagreement yields `Conflict` and the annotation is withheld.

## 6. Order independence

Matching must be a pure function of the stored observations, re-run after each
import, so "CSV then OFX" and "OFX then CSV" converge to the same state.

In an immutable-evidence store the cost is retiring the standalone event
created from a CSV row once a structured event arrives. Options:

| Option | Mechanism | Trade-offs |
| --- | --- | --- |
| 1. Superseded marker (preferred) | `superseded_events(event_id, by_event_id)`, schema 27; queries on `movements`/`source_events` exclude superseded events | No deletion, auditable; categories on the standalone event are not carried over; every direct query must filter |
| 2. Delete on merge | Drop `*_no_delete` triggers in a transaction (as `delete_account` does) and migrate category/type state | No dead rows; riskier, touches many tables |
| 3. Narrow first step | Create standalone transactions only when no structured event exists; refuse or flag later overlapping structured imports | Small; not order-independent |

## Decisions

Revised 2026-10-31. Statements are normally obtained as the same account and
period in several formats, so a primary and its companions cover the same dates.

- Keep one primary source per institution, chosen by its profile. Allow several
  companions per primary (for a bank whose formats need it; Wise uses CAMT and
  one CSV companion).
- Companion rows with no match yet stay pending and match when the primary
  arrives, in either import order. Ambiguous matches stay unmerged and are
  flagged `ambiguous`.
- Improve matching, not storage: institution-scoped reference normalization and
  a mutual-unique candidate rule with a one-day date tolerance. Report
  disagreeing fields as conflicts.
- A bank offering only one format makes that format its primary.
- Superseded earlier decision: "every record creates a transaction immediately".
  It was driven by periods covered by only some formats, which is not the normal
  case. Revisit if it becomes one.
- Wise MT940 is not parsed: it adds nothing beyond CAMT and CSV (see
  [statement observations](statement-observations.md#outcome-wise-mt940-is-not-parsed)).

## Deferred design: links, not retirement

Status: deferred 2026-10-31; see the decisions above.

### The merge-retirement problem

A CSV row creates transaction A, which the user may categorize. A later CAMT
import of the same movement creates transaction B. A and B are one real movement,
so one must stop counting. The three options above all treat A as something to
retire, and every decision table (`bank_categories`, `bank_transfer_decisions`,
`bank_transfer_memberships`, `movement_type_overrides`,
`card_settlement_decisions`, `reversal_pairs`) is keyed on `event_id`. Option 1
strands those decisions and taxes every query with a filter. Option 2 gives up
immutability and the audit trail. Option 3 is not order-independent, which
contradicts a decision already taken. None is chosen.

### Deferred design

The model (observations, transactions that carry decisions, audited links
and assignment-based matching) is specified in
[observation model](observation-model.md). This document remains background and
the record of why options 1 to 3 were rejected.

