# Observation model

Status: **deferred** (decided 2026-10-31). Not implemented and not the current
plan. It records the design to adopt if the primary/companion model proves
insufficient; see "When to revisit" below. Evidence is in
[statement observations](statement-observations.md); background and the rejected
alternatives are in [reconciliation design](reconciliation-design.md).

## Why deferred

The primary/companion model fits how statements are obtained: the same account
and period are requested in several formats, so a structured primary and its
companions cover identical dates. Unmatched companion rows already wait as
pending and match when the primary arrives, so import order is largely handled.
The remaining problems are matching quality across formats (Wise reference
forms, the 8 ambiguous BoursoBank rows), not storage. This model is larger than
those problems justify. Parts of it (assignment-based resolution, tombstones,
pinned links, the per-source-kind invariant) were also designed ahead of any
observed need.

## When to revisit

Adopt this model, or the minimal core of it (observations, transactions that
carry decisions, links), if a real case needs one of:

- a period covered by only some formats, with the structured source arriving
  later and decisions already made on the other;
- a bank with no format fit to be primary;
- several equally authoritative sources for one account.

Start with the minimal core, mutual-unique matching and a confirm/unlink pair,
and add the rest only when a case demands it.

## Principle

A statement file is a set of *observations* of things that happened. Fin stores
every observation as immutable evidence and *infers* transactions by linking
observations that describe the same movement. Everything the user decides about
a movement (category, transfer, type, settlement, reversal) attaches to the
transaction, never to an observation. Import order, repeats and the number of
formats must not change the result.

Wrongly merging two movements hides money; failing to merge shows a duplicate.
Fin prefers the visible duplicate, and every link can be inspected and undone.

## Entities

### Document and statement (existing, unchanged in role)

A *document* is one imported file (`import_documents`). A *statement* is a
document's account-scoped period with its opening and closing balances
(`statements`). Balances and boundaries belong to the statement and are never
merged across statements.

### Observation

One row of one document, as the source reported it. Immutable.

- `document`, `ordinal` (row position), `account` (institution plus source
  account identity), `format` (OFX, CAMT, MT940, CSV profile, ...).
- Source facts, kept separate and unnormalized: booking date, value date or other
  source dates, each as `Temporal.t`; signed amount; currency; description
  fields in source order; type code; fees if reported; the raw payload bytes.
- `source_id`: the bank's identifier if the format has a reliable one (OFX FITID,
  CAMT reference), otherwise absent. Wise MT940 is *not* absent: its `NONREF`
  `:61:` field is useless, but the continuation line carries a reference that the
  Wise adapter may expose as `source_id`.
- `occurrence`: for observations without `source_id`, the 1-based count of rows
  with the same natural key (account, booking date, amount, currency, normalized
  description) in this document.

### Observation identity and replay

Two sightings are the *same observation* when they share an identity key:

- with `source_id`: `(account, format, source_id)`;
- without: `(account, format, natural key, occurrence)`.

A re-imported or overlapping file whose row has an existing identity key adds a
membership (`statement_observations(statement, observation)`) and creates no
new observation. For keyless rows across overlapping documents, the count per
natural key is the **maximum** over documents, not the sum: two files that each
show two coffees mean two coffees.

This is distinct from *linking* below. Identity says "this row is one I have
already stored." Linking says "this stored row and that stored row, from
different sources, describe one movement."

### Transaction

A surrogate id plus the resolved account. The transaction row carries no
facts of its own; its amount, dates and description are projected from linked
observations (see Projection). Transaction ids are never reused.

### Link

`observation_links(observation PK, transaction, origin)`. Every observation is
linked to exactly one transaction. `origin` is `derived` (by the matcher, with
the evidence code and policy version that produced it) or `confirmed` (by the
user). A `*_changes` table audits every insertion, move and removal, as the
decision tables do today.

### Constraints

`link_constraints(a, b, kind)`, `kind` being `must` or `cannot`, are created
only by human decisions: confirming a link is a `must`; undoing or splitting a
link is a `cannot`. They are inputs to the matcher, not outputs.

### Merge record

`transaction_merges(absorbed, survivor, at, reason)`. When a link moves
observations from one transaction to another, the emptied transaction is kept
as a tombstone pointing at the survivor, so references and audit history resolve.

## Invariants

1. Observations are never updated or deleted.
2. Every observation has exactly one link.
3. A transaction has at most one observation per document, and at most one per
   `(format, source kind)`. Distinct rows in one statement stay distinct
   transactions, including identical ones. Several overlapping exports of one
   format share observations by identity rather than adding new ones.
4. Decision tables reference transactions only. Changing a link never changes a
   decision silently (see Conflicts).
5. Derived links are a deterministic function of the stored observations, the
   constraints and the policy version. Import order does not matter.
6. A link that carries a decision is *pinned*: the matcher may not change it
   without a visible conflict.

## Inference

Run after every import, in one SQLite transaction, over the affected accounts.
All observations of an account are considered, not only the new ones, which is
what makes the outcome order-independent.

### 1. Establish account identity

Matching never starts until observations of both sources belong to the same
account identity by an explicit, institution-scoped rule (for example IBAN, or
for Wise the currency within the balance). A suffix such as the Wise GBP
eight-digit identifier is evidence to show the user, not an identity rule.

### 2. Candidates

Between two observations of *different* source kinds only, in the same account:

| Tier | Evidence | Notes |
| --- | --- | --- |
| 1 | Equal normalized `source_id`, same currency, same signed amount | The normalization is per institution and named (for example the Wise reference suffix). No generic suffix rule. |
| 2 | Same currency, same signed amount, booking dates within one day, compatible description | Both dates are kept. In the Wise archives 16 of 1,972 reference-matched pairs differ by a day. |

Anything weaker produces no candidate. Each edge carries its tier and a score.

### 3. Resolution

Candidates form a graph. For each connected component, use the same
component-wise assignment as `Transfer_ranking` (forced edges, equal
alternatives, exclusion margin). An edge becomes a derived link only when it is
**forced** or **dominates** every alternative in its component by the configured
margin. Otherwise every observation in the component stays on its own
transaction and the component is reported as *ambiguous* with its candidates.
This makes a match safe from both sides, not merely unique in the current batch.
Components over the size limit are reported, not guessed.

### 4. Constraints first

`must` pairs are linked before assignment, and `cannot` pairs are removed from
the candidate graph. A constraint that contradicts another (for example a `must`
that also appears in a `cannot` through a third observation) is reported and
none of the group is changed.

### 5. Apply

Compare the result with the stored derived links and apply the difference, each
change audited:

- *Link*: move the observations onto one surviving transaction. The survivor is
  the one holding a decision; if neither does, the lowest id. The other becomes a
  tombstone in `transaction_merges`.
- *Dissolve*: a later import made a derived link ambiguous. If the link is not
  pinned, split the observations back to separate transactions (new ids for the
  split-off observations).
- *Conflict*: both transactions hold decisions that differ, or a pinned link
  would change. Nothing moves. The group is flagged with the competing
  decisions and the evidence.

Confirmed links are never changed by the matcher.

## Projection

A transaction's visible facts come from its observations by a per-institution
precedence: the structured source supplies amount, currency, booking date and
identifier; other sources contribute annotations (value date, the bank's own
type, fees, a richer description). Fields that disagree are not overwritten:

- a differing amount or currency is a **conflict** and blocks the automatic link
  (it would not have matched);
- differing dates, descriptions or types are shown side by side with their
  source, and the precedence value is used for ordering and reporting.

The presentation of conflicts in `fin transaction show` is still to design.

## Worked cases

**CSV first, then CAMT.** The CSV import yields observation O1 on transaction T1.
The user categorizes T1. CAMT later yields O2 on T2. Tier 1 or 2 links O1 and O2;
T1 holds a decision so T1 survives; T2 is a tombstone absorbed into T1. The
category stays. Importing in the opposite order produces the same links and the
same survivor, because only the presence of a decision, not the order, selects
it.

**Two identical coffees.** A CSV has the same date, amount and description
twice: two observations with occurrences 1 and 2, two transactions. A CAMT with
two coffees offers two candidates for each; the component is symmetric so no edge
dominates, and both pairs stay *ambiguous* unless the CAMT references (tier 1)
separate them. A second CSV covering the same day does not add observations:
its two rows resolve to the existing identity keys by maximum count.

**Wise MT940 against CAMT.** MT940 `:61:` carries `NONREF`; the reference sits on
the continuation line and is exposed as `source_id`. Currency, reference and
amount give 1,972 one-to-one candidates (1,956 same date, 16 one day apart).
The 31 MT940 entries without an exact reference stay unlinked, and are offered as
review candidates where a unique amount-and-currency neighbor exists. Equal
opening and closing balances do not merge the two statements.

**Wrong automatic link.** The user runs `fin ... unlink`. A `cannot` constraint
is stored, the observations move to separate transactions (the original keeps
its decisions), and no later import re-links them.

## What stays outside this model

Transfer matching between the user's own accounts, category classification and
card settlement remain separate decision layers, now keyed on transaction ids.
Statement balance reconciliation stays on statements.

## Implementation plan

1. Schema bump: `observations`, `transactions`, `observation_links`,
   `link_constraints`, `transaction_merges`, `statement_observations`; decision
   tables move to transaction ids. No migration, per project policy. Reproduce
   current BoursoBank OFX+CSV, Wise CAMT+CSV and Bunq MT940+CSV behavior and
   compare by rebuilding the personal database and diffing reports and payloads.
2. Property tests: shuffled import order, reimports, overlapping exports,
   repeated identical payments, refunds, conflicting fields, constraint
   contradictions.
3. Matcher with assignment resolution, the `fin import --dry-run` preview, and
   the link/unlink/confirm commands.
4. Wise MT940 reader and adapter, enabled through the matcher.

## Open questions

- Which institution profiles define reference normalization, and how users
  configure it for their own institutions.
- Whether derived links are stored (proposed above, for audit and cheap diffing)
  or recomputed on read.
- Treatment of refunds against the original purchase, and of a day covered only
  partially by one source (the `Day_inventory` problem).
- Conflict presentation, and how a pinned conflict is resolved by the user.
- Whether one transaction may hold several observations from the same source
  kind when a bank splits a single movement into several statement lines.
