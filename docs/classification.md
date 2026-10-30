# Transaction classification and review

Status: first implementation stage, 2026-10-29. Fin stores explicit reviewed
category decisions, lists transactions and reviews normalized card-merchant or
description matches. Saved rules can match source descriptions and effective
types, and can explicitly apply type overrides, categories or card settlements.
An experimental full-screen
settlement review is available; user-managed tags are not implemented.

## Unified rules

Bundled BoursoBank typing defaults and user rules share one ordered rule set.
Rules can run automatically on import, require review, or be disabled. Automatic
type and category actions apply to new movements; settlement actions require
review. Manual overrides and reviewed accounting decisions take precedence.
Changing a rule never silently rewrites previously imported movements: type
refresh and rule application preview historical changes and apply atomically.
See the [manual](manual.md#transaction-rules) for the current workflow.

## Purpose

Turn imported movements into useful personal accounts without requiring the same
categorization decision for every transaction. The user defines the meaning of
the books; fin should make repeated work easier and make each suggestion and
change understandable.

Classification assigns a counter-account such as `expenses:food:groceries` or
`income:salary`. It must not change the source movement, its amount, currency or
identity. The existing `equity:unclassified` posting remains the explicit place
for movements that have not been understood.

## Categories and accounting meaning

There is no fixed correct number of categories. Start with roughly 10–20 expense
categories and a few income categories, then split or merge them when a reporting
question calls for it. A possible starting point is housing, utilities, groceries,
restaurants, transport, health, subscriptions, software, travel, shopping, gifts,
taxes and other, with salary, interest and other income. These are examples, not a
mandatory chart or a limit. Hledger's account hierarchy supports broader totals
and more detailed child accounts when useful.

A category expresses the purpose of a purchase, not just its merchant. A merchant
may sell groceries, gifts and household goods, so a single payee need not imply a
single category. Refunds generally use the same expense account as the original
purchase, reducing the expense. Transfers between owned accounts are not income
or expenses; use fin's separate transfer review. Do not infer the missing balance
transfer between replacement cards from their purchase exports.

Optional, user-defined tags add dimensions across the account hierarchy. A train
ticket can be classified to `expenses:transport:train` and tagged `trip: lisbon`
and `purpose: holiday`. Tags support questions about the trip without changing the
accounting posting. They are not a substitute for a category, and a transaction
need not have any. Hledger supports tags on transactions and postings; choose the
scope explicitly when a split needs tags on only part of a transaction. Fin exports
some source-derived tags, but user-managed tags are not implemented.

Some transactions may need several categories, for example a mixed purchase.
That requires separate postings with amounts: two tags on the full amount do not
allocate the purchase between categories. Fin currently assigns a single
counter-account per movement; split classification is separate work and must
not be simulated by inventing source movements.

## Similar transactions and learned rules

Classifying a movement should make it easy to find and review other movements
that might share its category. Separate these operations:

- **Recognize a payee.** Normalize or match varying source descriptions to a
  payee while retaining the original text. Payee identity is not a category.
- **Suggest a category.** Use explicit user rules and previously reviewed
  decisions as evidence. Show the matching transactions and reasons for a
  suggestion, including disagreements.
- **Apply a decision.** Let the user select individual historical matches or a
  batch, preview the effects and apply them atomically. Do not overwrite an
  existing reviewed decision without an explicit correction.
- **Create a rule.** Offer to save a reusable, inspectable condition for future
  imports. The user should control its scope, edit it and disable it. Automatic
  application should be a deliberate choice, not a side effect of accepting a
  batch of suggestions.

The first implemented preview is `fin transaction category similar EVENT_ID`. The
example must have a reviewed income or expense category. Within the same
institution, it compares matched card merchants when available, or the ordered
description fields otherwise. Merchant and description keys are kept separate.
It ignores ASCII case and runs of
spaces, tabs and newlines. It reports unclassified matches and separately reports
matching transactions with a different reviewed category. Confirmed transfers are
excluded. This is evidence for review, not a probability or
an automatic decision; `--json` provides the same preview for scripts.

Start with exact and carefully normalized descriptions, then evaluate fuzzy
matching on real variations. Possible evidence includes provider, account,
direction, merchant text and, when useful, amount or recurrence. Do not make a
single merchant default authoritative for mixed-purpose merchants. Conflicting
reviewed examples should produce alternatives or an unresolved result, not a
silent guess. A refund should not be treated as unrelated merely because its
sign differs, but pairing it with a purchase needs evidence.

Keep explicit rules deterministic, with documented precedence and a visible
reason when rules conflict. Record whether a decision came from the user or a
rule, the relevant rule version and its supporting evidence. Preserve the
existing audit history and correction workflow. Suggestions are not decisions:
never feed unreviewed predictions back as if the user had confirmed them.

## Review interface

A full-screen review interface can keep the selected transaction, proposed
changes and pending state visible while navigating a queue. The experimental
`fin settlement review --tui` prototypes this for settlements using a pinned
[Notty fork](https://github.com/vbmithr/notty) as a Git submodule. Cancellation
discards the pending decision; confirmation commits the card setup and settlement
together. The
fork keeps its upstream history and local compatibility fix. Extend
and evaluate the interface for classification; noninteractive commands and JSON
workflows must remain available for scripts.

The initial `fin transaction category review EVENT_ID` workflow presents an
unclassified movement, offers previously used categories or a new category name,
and lets the user select exact matches and confirm a transactional batch. It
shows conflicting classifications and leaves cancellation without changes. The
event ID can be found with `fin transaction list`. This first interface uses
terminal prompts; it does not yet provide tags or category search. Separately,
`fin rule` saves conditions and actions for automatic import or explicit preview
and batch application; see the [manual](manual.md#transaction-rules).

The fuller workflow should present one unclassified movement, let the user search
or choose a category, add optional tags, inspect source details or skip it. After
classification, show similar unclassified transactions as a selectable group and
preview the batch before applying it. Offer a reusable rule only as a separate,
explicit decision. Tags and categories should be independently selectable for a
batch: transactions from one merchant can share a category without sharing a tag.

A useful review screen could show the current movement, its source account,
description, amount, current classification and provenance, alongside a short
list of suggested categories and similar reviewed transactions. It should let the
user search and select categories, inspect a match group, select or exclude rows,
preview a batch, apply or skip it, and correct earlier decisions. A rule proposal
should show its conditions and the transactions it would affect before saving.

The interface should work with a keyboard, resize cleanly, handle long and
untrusted descriptions safely, and make cancellation leave unapplied changes
untouched. It should be clear when a result is a suggestion, a reviewed decision
or a rule-applied classification. The classification engine and transactional
store operations belong below the UI, so they can be tested independently and
used by both terminal and noninteractive clients.

## Evaluation and rollout

Start with a small, recent period of real transactions. Measure how many items
are matched, how many suggested categories the user accepts or corrects, and
how much manual work is saved. Keep a later period separate when evaluating
future-transaction predictions; don't evaluate on the decisions used to build a
rule. Test ambiguous merchants, noisy descriptions, refunds, repeated identical
transactions, conflicting examples, rule precedence, batch rollback and reimport.

Deliver in steps:

- Define the user's initial chart and exercise existing manual review on a small
  period. Confirm transfers separately. Design user-managed tags as optional
  metadata distinct from accounting categories.
- Extend the initial exact normalized-description preview with richer matching
  when real cases demonstrate a need, without automatic classification.
- Extend reusable rules with versioning and conflict handling if needed.
- Evaluate the vendored Notty settlement prototype and extend the review
  interface to classification if it improves the workflow.
- Consider local embeddings or other models only after measuring where simple
  matching fails. Keep any external model or service optional and explicit about
  what transaction data leaves the machine.

## References

- [Actual Budget: payees](https://actualbudget.org/docs/transactions/payees/)
  describes payee matching and category defaults.
- [Actual Budget: rules](https://actualbudget.org/docs/budgeting/rules/) describes
  editable conditions, automatic learning and rule precedence.
- [YNAB: categorizing transactions](https://support.ynab.com/en_us/categorizing-transactions-a-guide-HyRl60sks)
  describes payee defaults, suggestions and disabling automation for mixed payees.
- [Scalable and Weakly Supervised Bank Transaction Classification](https://arxiv.org/abs/2305.18430)
  describes a research approach using embeddings and weak supervision. Results on
  a bank dataset do not establish accuracy for fin's users or their categories.
