# Transaction classification and review

Status: updated 2026-10-31. Fin stores explicit reviewed category decisions,
lists transactions, previews similar movements and applies reusable regular-
expression rules to imports and existing transactions (see "Rules today"). Rule
provenance, bundled institutions as shareable definitions, a rule-authoring workflow, user-managed tags and
the full-screen interface are not implemented.

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
  imports. The user controls its scope, can edit it and disable it. Automatic
  application is a deliberate choice, not a side effect of accepting a batch.
  Rules exist (below); creating one from a reviewed batch does not yet.

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

## Rules today

Rules are stored in the `classification_rules` table and managed with `fin rule`
(`add`, `list`, `mode`, `preview`, `apply`, `rename`, `add-evidence`,
`add-reported-type`). A rule has:

- a **condition**: a regular expression on the first trimmed source description
  (case-insensitive by default), optionally limited by institution, account type
  and a movement-type condition (`unknown` or a given type). The engine is `Re`,
  which has no backtracking, so a pattern cannot cause pathological matching;
- an **action**: a category, a movement type, both, a settlement, or one of the
  source-evidence actions (card reference, withdrawal reversal, reported-type
  mapping);
- a **mode**: `automatic` (applied to new imports), `review` (applied only after
  `rule preview` and `rule apply`) or `disabled`. Settlement rules cannot be
  automatic, and evidence rules cannot be `review`;
- a **priority** (lower runs first, then name). The first matching type action
  and the first matching category action win independently, and typing runs
  before categorizing.

Category targets must be expense or income accounts. Manual overrides and
reviewed decisions remain authoritative. Editing or deleting a rule affects
future imports only; existing results change through an explicit refresh or
`rule apply`. Institution definitions may carry `rules` (type, evidence and
reported-type actions); bundled defaults live in each bundled institution's definition
(`institutions/<institution>.json`) and are installed only into a fresh database.

Gaps:

- Rules are keyed by name alone. They do not record where they came from (bundled,
  user, a definition) or a version, so an updated default cannot be told apart
  from a user's edit.
- Conditions cannot test the amount or direction of a movement.
- There is no workflow for writing a rule beyond the command line, and no summary
  of what a draft rule would change.

## Shared definitions and a rule editor

Status: design for discussion, 2026-10-31; not implemented. The goal is a rule
editor for one's own rules, and definitions that others can write for their
institutions and distribute with fin or separately.

### Scope: interpretation rules only

Decided 2026-10-31. Only **interpretation rules** (type, evidence, reported-type)
are shared or shipped. They state how an institution words its movements, are
objective, and apply to everyone with that institution. **Accounting rules**
(category, type and category) depend on a person's own chart of accounts and are
strictly personal: fin never bundles, exports for sharing or installs them from a
definition. A definition therefore cannot contain a category action, and
installation rejects one.

### How the existing rules came about

Nothing generated them. They were written by hand, a few at a time, from real
exports: an unrecognized description prefix turned up, a regular expression was
added to the bundled defaults (first in code, then as institution typing
profiles, then as unified rules in `institutions/*_defaults.json`, now in each
institution's definition), and
`fin transaction refresh-types` re-applied them. `CHANGES.md` records the
increments: card payments, interest labels, fees, cheques, malformed
deferred-card labels "found in exports". Today the defaults cover BoursoBank
description prefixes (ten patterns), BoursoBank evidence (card reference,
withdrawal reversal) and Wise reported codes. The editor should support this
loop directly: find movements with no type, group their descriptions, draft a
pattern, test it, save it, refresh.

### Institution definitions are the sharing format

Updated 2026-10-31. A separate ruleset format would duplicate what exists. An
institution definition (`fin institution definition show/check/set`) is already a
versioned JSON file holding a source profile (primary CSV, OFX with CSV companion,
CAMT with CSV, running-balance CSV or MT940 with CSV) and `rules` limited to
interpretation actions (`type`, `card-reference`, `withdrawal-reversal`,
`reported-type`, `reported-other`). That file is the unit that describes an
institution's customization, and the way to share one.

What is missing:

- Bundled institutions are definitions (done 2026-10-31): `institutions/boursobank.json`,
  `wise.json`, `bunq.json` and `revolut.json` each hold the source profile and the
  default rules, and `fin institution definition show` prints the same file.
  Contributing a bank and contributing rules are therefore the same operation.
- Definitions carry no metadata or examples. Add an id, definition version, title,
  authors, license and `examples` (a description or reported code and its expected
  type, and near misses that must not match). `fin institution definition check`
  runs them. This is the quality bar for contributions and detects a rule that
  stopped matching.
- A rules-only definition that extends a bundled institution without restating its
  source is not possible (open question below).

Accounting rules never appear in a definition.

### Provenance and updates

Each rule records its definition, definition version and origin (bundled,
imported, user), and names are unique within a definition rather than globally.
Installing or updating a definition previews added, changed and removed rules and conflicts with
existing ones. A rule the user has edited is kept and reported, not overwritten.
This also fixes the bundled defaults being installed only for a fresh database.

### Rule editor

Build the engine before an interface, so both scripts and a terminal editor use
it:

- `fin rule test` evaluates a draft against stored transactions without saving:
  how many it matches, how many would be newly classified, which existing rules
  overlap it and which would win, conflicts with reviewed decisions, and a sample
  of matches and near misses.
- Pattern helpers build the condition from a chosen transaction: starts with,
  contains or exact, escaped and compiled to the regular expression, which is
  always shown.
- `fin rule gaps` (name provisional) lists movements with no type, grouped by
  normalized description prefix with counts and examples, so the next pattern is
  chosen from evidence. A group can seed a draft.
- A terminal front end (the Notty option above) comes later on the same engine.
  Authors can also edit a definition file and run `fin institution definition check`.

### First use: Revolut

Checked 2026-10-31 on the two real Revolut exports (843 rows; counts only).
`rule gaps` is the wrong lever there. The CSV carries the bank's own `Type`:
Card Payment 668, Deposit 70, Transfer 57, ATM 46, Card Refund 1, Charge 1. The
profile reads the column but no movement type is set from it. Descriptions work
for the small types (`Top-up by`, `Payment from`, `Cash withdrawal`,
`Transfer to`, `Transfer from`, `Balance migration`) but not for the 668 card
payments, whose descriptions are merchants (372 distinct two-word keys). Typing
Revolut therefore needs the reported `Type` column, as Wise's companion CSV codes
already are, not description rules. The existing `reported-type` rules (an
institution, a code expression and a type) are the right shape. Implemented
2026-10-31: a movement carries the bank's own code (`Movement.reported_code`),
and `Source_typing` applies the institution's reported-type rules to it as the
base type, so description rules and their `--match-type` conditions see it.
Revolut's definition maps Card Payment and Card Refund to `card`, Deposit to
`deposit`, Transfer to `transfer`, ATM to `withdrawal` and Charge to `fee`, and
keeps other codes as `revolut:CODE`. Revolut's REVERTED rows are handled
separately.

### A simpler rule model (proposal)

Status: design for discussion, 2026-10-31; not implemented. `fin rule explain`
shows the problem: to say why a movement has its type, fin walks four stages (a
manual override, the bank's code, the first description, the companion file's code)
with a caveat on the last. The model has these moving parts:

- eight rule actions: type, category, type and category, settlement, card
  reference, withdrawal reversal, reported type, reported other;
- a different matching input per kind and mode: the first description for
  automatic type rules, any description or the companion's display name for review
  rules, the bank's code for reported-type rules;
- types saved as a snapshot at import and changed by an explicit refresh, with
  companion codes arriving later, when files are matched.

The proposal is one sentence: **when FIELD matches REGEX, set a type or a
category.**

- **Every column of every source is a field, named as the source names it.** A
  rule says "field `TransferWise ID` matches `^BALANCE_CASHBACK-`", with no role,
  preset or adapter code for that column. For a CSV the names are its headers
  (Revolut's `Product` and `State`, Wise's `Payer Name` and `Note`); for a
  structured source they are its own tags (OFX name and memo, the CAMT bank
  transaction code and additional entry information, the MT940 `:86:`
  subfields). Fin adds a few canonical fields for every movement so rules stay
  portable across banks: `description` (a rule matches when any description does),
  `direction`, `amount`, `date`, the account type and the type so far. A matched
  companion file's fields are available once its rows match, under a prefix naming
  the source when two sources share a name. A definition then says only how to
  ingest (which columns give date, amount, currency, description and identity);
  its role columns for merchant, payee, details type and payment reference become
  ordinary fields, and the display name becomes a setting that names one field.
  The existing conditions stay: institution, account type, and the type so far.
- **Two actions for users.** `type` (interpretation; bundled and shareable) and
  `category` (personal; never shipped). Reported-type and reported-other become
  type rules on the `code` field. Settlement and the two evidence rules stay as
  advanced workflows with their own commands.
- **Modes say when a rule runs, not what it sees.** `automatic` rules run on every
  import and rule change; `review` rules run on demand with a preview. Both match
  the same fields.
- **A type is derived, not snapshotted.** The effective type is the manual
  override, else the first matching type rule in priority order, else none. A
  companion is no longer a separate stage: its fields are fields, available once
  its rows match. Editing a rule changes types, so `refresh-types` and
  `refresh-reported-types` go away; the type history records what each recompute
  changed.

What it fixes: the Wise fee lines (`Wise Charges for…`, 187 rows typed `card`,
`conversion` or `transfer` by their code) become an ordinary rule on `description`
that outranks the `code` mapping by priority. `rule explain` becomes one table of
rules against fields. Bundled definitions describe every rule the same way, which
is what sharing rules between users needs.

What it costs:

- **Derived types need a recompute strategy.** Recomputing on every read would slow
  `status` and listings (today about 75 ms on 4,400 movements). Recompute after an
  import or a rule edit and store the result, which keeps reads fast but makes
  every import and edit pay for it.
- **Audit.** The stored type snapshot and its history table exist so a change is
  visible. Recomputing must log which rule change caused which type change.
- **Companion timing and disagreement.** Fields from a companion exist only after
  its rows match, so rules on them must re-run after matching, and several matched
  rows may disagree on a field (today's consensus rule: they must agree).
- **Priority replaces stages.** Today the primary's own type beats the companion's
  by stage. With one list, a `description` rule that should beat a `code` rule must
  have a lower priority number, and bundled and user priorities need a convention.
- **Breaking change.** Fin is in beta, so no compatibility is kept: a schema bump,
  rewritten bundled definitions and a rebuild. Existing databases still need the
  bundled rules added, which is the provenance and update work.

Open questions:

- A rule over `description`: does it match any description, or does it need
  `description[1]` for the first (today's automatic behaviour)?
- How `reported-other` keeps the code as a provider-specific type (`wise:CODE`)
  without a separate action: a `{code}` template in the target?
- Whether `direction` (credit or debit) and `amount` conditions come with the first
  step or later.
- Whether `type and category` stays one action or becomes two rules.
- Where recomputed types are stored and how their history is kept.
- How primary sources keep all their fields. A companion row already stores its raw
  cells, and the definition knows the header. A primary event does not: the Revolut
  payload drops `Product`, `State` and `Balance`, and the OFX and CAMT payloads are
  fixed subsets. Capture them in the payload or in a side table, without changing
  source identity, with a generic flattening for the tag-based formats.
- How fields are prefixed when two sources share a name, and what a rule does when
  several matched companion rows give different values for one field.
- Export headers vary by language and over time, so a rule on a missing field must
  be visible: `rule test` and `rule explain` should report how many movements lack
  it. Field values are strings; numeric conditions need their own handling.

Staging, each step usable alone:

1. Companion fields (implemented 2026-10-31). Every companion exposes its raw
   columns by header name, and reported-type rules name the field they match
   (default: the bank's code), so a rule can match `TransferWise ID`. This needed a
   schema bump (a `field` column) and no new storage, since companions already keep
   their cells. On the real Wise archives it types 187 fee lines and 29 cashback
   rows. The rule still has the reported-type action and applies to companion rows
   only; folding it into the single rule form is step 3.
2. Primary fields. Decided 2026-10-31: `description` is the main text the adapter
   declares, not the first text; `text` is any description; every text and column
   the source carries has its own name; and columns that a payload drops are kept in
   a side table (`source_event_fields`), which leaves payloads and source identity
   untouched.
   - 2a, implemented 2026-10-31: named fields that the payload already holds (OFX
     `name` and `memo`, Bunq `name` and `counterparty`, the Wise CAMT `reference`,
     Revolut `type`), the `description` and `text` split, and `--field` on
     automatic type rules, with `rule test` and `rule explain` showing the field
     and value. No schema change.
   - 2b, CSV part implemented 2026-10-31: the `source_event_fields` side table,
     populated at import with every column of a CSV primary by its header (Revolut
     `Product`, `State`, `Balance`, and every column of a user-defined bank), loaded
     with the event and merged into the movement's fields. The stored movement also
     keeps which text is the main one, so `rule gaps` groups by it. Schema 28.
   - 2b, remaining: the tag-based formats (CAMT, OFX, MT940) through a generic
     flattening. Deferred until a rule needs a tag that is not exposed; today only
     the named fields above are.
   - Canonical fields `direction`, `amount` and `date` come with 2b or later.
3. One rule form (implemented 2026-10-31): `reported-type` and `reported-other` are
   gone. A bank's own code is an ordinary column, so one `type` rule with `--field`
   serves both a transaction's own source and a matched companion file; the first
   matching rule in priority order decides; and a target may contain `{value}`, which
   takes the matched text (`wise:{value}`). The bundled Wise and Revolut rules were
   rewritten as `type` rules on their columns, bundled rules keep their own names and
   priorities when installed, and `Movement.reported_code` is removed. Two separate
   evaluation sites remain, the stored type at import and the companion rows' types,
   which step 4 merged; and a definition's role columns for merchant, payee, details
   type and payment reference are still there to drop.
4. Derive types (implemented 2026-10-31, schema 30): `movements.movement_type` is the
   derived result, recomputed by `Store.retype` over the movement's fields plus its
   matched companion columns after every import, companion match, rule change and
   definition change. It is the one column of `movements` that may be updated, and
   each change is logged in `movement_type_changes` with the reason. The refresh
   commands and `companion_reported_types` are gone, and `rule explain` has two
   stages, override and rules.
5. Remaining: drop a definition's role columns for merchant, payee, details type and
   payment reference, and remove any leftover old action kinds.

### Distribution and trust

Bundled definitions live in the repository, reviewed by pull request and
installed by the package. Third-party definitions are installed from a file by
`fin institution definition set`, as today; nothing is fetched automatically. A
definition is data, never code. Validate its size and patterns. Authors must not
publish patterns or examples derived from personal data such as names, IBANs or
card numbers; `definition check` should warn about IBAN-like, card-like and long
digit sequences.

### Exporting and upstreaming rules

Planned workflow for contributing rules found with `rule gaps` and `rule test`:

1. Save the rule locally with `fin rule add`.
2. `fin rule export --institution NAME [RULE...]` writes the selected
   interpretation rules as the `rules` of a definition fragment. It refuses
   category, settlement and other personal actions. It never writes descriptions
   from the database; it may print candidate descriptions to the terminal so the
   author can write anonymized examples by hand. The repository is public.
3. The author adds the rules and synthetic examples to the institution's bundled
   definition and runs `fin institution definition check`.
4. A dune test runs `check` over every bundled definition, so CI covers the
   examples and the patterns.
5. Open a pull request. A contributed rule should be anchored and as narrow as the
   evidence allows, state its case flag, restrict `account_type` where the wording
   differs between account types, have at least one positive example and one near
   miss, come from a real export (noted without pasting data) and belong to an
   institution that has an adapter in fin.

Bundled rules reach a fresh database only, which is acceptable while fin is in
beta and databases are rebuilt. Updating existing databases depends on rule
provenance above. Bundled priorities need a convention that does not clash with
the local default of -1000.

### Open questions

- Whether to add amount, direction and recurrence conditions, and whether the
  need is real beyond cases like salary credits.
- Whether to offer plain conditions (starts with, contains) as the stored form,
  with the regular expression only as an advanced option.
- Whether a rules-only definition may extend a bundled institution, or only a full
  definition may replace it.
- Whether and when ML suggestions rank candidate rules; they must not bypass
  rule precedence or become decisions.
- Whether the personal category rules need an editor of their own, separate from
  the shared interpretation rules.

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
