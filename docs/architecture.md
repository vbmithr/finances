# Kernel architecture

## Ownership

SQLite owns source evidence and accounting entries. Position observations are
currently in-memory domain values. Future importers and position reconstruction
will use source evidence as their common input.

```text
source records
      |
      v
source_events (SQLite, immutable)
      |
      +--> accounting policy --> journal_entries + postings (SQLite)
      |
      +--> position reconstruction (future)

journal_entries + postings
      |
      v
finances.hledger adapter --> generated .journal --> hledger queries
```

The core library has no dependency on exchange APIs, network clients, or hledger.
The CLI imports opaque payload files or OFX XML bank statements, projects bank
movements into accounting entries, and exports journals through a separate
`finances.hledger` adapter. It does not categorize bank entries or interpret an
exchange's financial semantics.

## Domain boundaries

`Source_event` describes external evidence. Its identity is
`(venue, account, kind, source_id)`. An occurrence is either nonnegative 64-bit Unix
milliseconds (`At_ms`) or a calendar date (`On_date`); SQLite requires exactly one
of `occurred_at_ms` and `occurred_on`. Bank booking dates do not fabricate a UTC
timestamp. SQLite assigns the internal ID and retrieval timestamp. Payload bytes
are stored as a BLOB, including any embedded NUL characters.

`Accounting` describes postings and balanced entries. Quantities are
`Bigdecimal.t`, dates are `Core.Date.t`, and fallible APIs return `Or_error.t`.
Private record types ensure callers construct values through validation functions.
Every entry has at least two postings and sums to exactly zero per commodity.
Cross-commodity trades therefore need explicit conversion postings or another
future accounting policy; a USD amount and a EUR amount cannot cancel each other.

`Instrument` describes a linear perpetual's venue, market, base, quote,
settlement commodity, and positive contract multiplier. The multiplier converts
contract quantity into base exposure. `Position` stores signed contract quantity
and a positive average entry price. Long quantities are positive and shorts are
negative. Its calculation is:

```text
unrealized P&L in quote = quantity * multiplier * (mark - entry price)
```

The calculation has no division or rounding. It does not imply that the quote and
settlement commodities have equal value. Funding, fees, settlement conversion,
mark timestamps, and collateral are separate concerns. This observation model does
not yet calculate average entry price from fills or model inverse contracts.

## Persistence and replay

`Db_schema` defines the 32 persistent row records, their typed Rel columns, rows,
tables, keys, defaults and indexes. These definitions drive table creation and
Store's inserts and read projections, so column types are shared rather than
repeated in an independent SQL schema. Domain types still validate and represent
financial concepts; persistence records represent the stored columns.

`Sqlite_schema` adds checks, strictness and immutability beside each Rel table.
Checks use the table's typed columns; trigger names and targets come from the
table itself. The pinned Rel SQLite generator emits `BIGINT` for OCaml `int64`,
so the helper changes those column declarations to SQLite `INTEGER` for `STRICT`
tables and rowid allocation. This changes the SQL type spelling, not the OCaml
value type or SQLite's 64-bit storage. Rel generates the remaining DDL, including
foreign keys and indexes. There is no separately maintained SQL schema.
Native strict-table generation is tracked in
[Rel issue 5](https://github.com/dbuenzli/rel/issues/5).

`Store` uses Rel's SQLite backend, generated inserts, updates and deletes, and
the query language for simple lookups. Query parameters are bound. Rel's curried
query builder supplies predicate parameters in reverse order; lookup helpers
keep their public arguments in key order. Raw SQL remains for ordered reads,
joins, conflict-targeted upserts and queries that are substantially clearer as
SQL. Their result projections reuse table columns where applicable; computed
fields and outer joins have their own result descriptions.

Schema version 10 contains source events, journal entries, postings, immutable
import documents, document/event links, append-only bank import attempts, persisted
bank account mappings, an append-only mapping change log, immutable CSV rows,
derived CSV matches, and append-only match change history. Decimal posting
quantities use text; no financial amount uses SQLite REAL.

Schema initialization runs inside an immediate transaction. `PRAGMA user_version`
must be 0 (a fresh database) or 10 (the supported baseline). Other experiments are
rejected without alteration. There is no historical migration chain. Initializer
failures roll back all partial DDL. Original records and account configuration can
be reimported when resetting a database. Transaction decisions require renewed
review because internal event IDs can change after reimport.
Foreign keys are enabled on every connection and a five-second busy timeout is
configured. The initial concurrency model uses a single writer and SQLite's
default rollback journal. Connections must not be shared between threads.

Source ingestion is transactional and compares existing evidence before inserting.
The same identity with changed timestamp/date or payload is a conflict. Importers must
choose a stable source record payload and handle exchange corrections explicitly.
There is no silent replacement or correction workflow yet.

Entry insertion is a separate transaction: both the header and every posting
commit together. Each entry references a source event. Multiple entries can derive
from one event using distinct projection keys. Retrying the same key compares the
date, description, posting order, accounts, commodities, and exact quantities.
Changed accounting projections are rejected by the general entry writer. The
explicit account-mapping operation can rename only verified generated bank
postings; broader reclassification still needs a future operation.

The balance invariant is enforced by the OCaml constructors and rechecked when
reading entries. SQLite foreign keys and uniqueness constraints enforce links and
identity; SQLite does not independently verify arbitrary decimal balances for SQL
written outside the library.

Read APIs currently materialize their results. Source records are ordered by their
calendar date (using UTC for timestamped records), then timestamp and internal ID;
date-only events precede timestamped events within a date. Journal entries are
ordered by date, source event ID, and projection key.
An internal ID breaks ties deterministically within one database; it does not
establish an exchange's causal ordering. Exchange sequence numbers will be needed
for reliable fill replay.

## Bank source ingestion

`Bank_ofx` parses XML using Xmlm and uses `Bigdecimal.of_string` directly on source
amounts. It handles multiple statements under one response, as seen in the
BoursoBank export, and keeps account identities, booking dates, names/memos, raw
`DTUSER`, and optional available/ledger balance observations distinct. Unsupported
message sets, missing transaction IDs, malformed booking dates/amounts, unsuccessful
responses, and correction instructions produce errors.

`Store.import_ofx` archives the complete document, creates or reuses dated events,
and links the archive to those events in a single transaction. Archives are
identified by institution and SHA-256 of the original bytes. Bank event identity
uses the institution, a JSON tuple of bank ID/account ID/account type, event kind,
and FITID. Account fields are encoded as a tuple so delimiters cannot produce
identity collisions. Currency is included in the canonical event payload and a
changed currency under the same identity causes a conflict.

Canonical transaction payloads include account context and the XML transaction
tree as JSON. XML entities are decoded, insignificant whitespace is normalized,
and DTUSER is omitted. In the supplied export every DTUSER is an invalid calendar
date; it cannot safely be used as a value date. The raw field remains available in
the parsed observation and archived document. Other changed transaction fields
cause an explicit conflict, including changes to names or memos. A changed balance
snapshot does not change transaction identity or overwrite earlier snapshots.

This stage supplies parsed domain values and persistent source evidence. It does
not yet create a separate normalized bank transaction table. CSV observations add
value dates and bank metadata without creating movements. OFX balance observations are preserved in the archived statement;
they are not yet used to reconcile account balances. Wise reconciliation is
implemented separately below.

## Bank accounting and journal export

`Bank_ofx.source_transaction` reconstructs a typed bank transaction from the
canonical source payload. It validates the payload's account identity, FITID, and
booking date against the enclosing source event, so journal generation can work
entirely from the database without reopening source files.

`Bank_projection` implements the initial `bank-unclassified-v1` policy. Each event
produces exact postings for its signed bank movement and opposite principal in
`equity:unclassified` by default. Revolut adds a separate explicit fee posting.
Reviewed categories or confirmed transfers replace only the principal counter-account. This does not claim a withdrawal is an expense or
a deposit is income. Ledger account names include the institution and the bank's
complete account identity. Components outside the supported token alphabet, and
components starting with the reserved `x-` prefix, are reversibly hex encoded.

`Store.project_bank` runs the complete projection batch in one SQLite transaction.
It shares the internal entry writer with `record_entry` without nesting
transactions. Identical projections reuse existing entries; a conflict or malformed
source event rolls back all new entries in that batch. Non-bank events are skipped.
No schema migration is needed because the existing entry and posting tables own
the accounting projection.

`finances.hledger` converts persisted entries to the OCaml hledger binding's AST
and delegates rendering to its printer. The core library does not depend on that
binding. The adapter declares every account and commodity, preserves exact decimal
precision, adds source event IDs and projection keys, and sanitizes description
characters that could change journal syntax. No generated timestamp is included,
so repeated exports of the same stored entries are byte-identical.

Bank journals do not infer opening balances. Bank-category tags remain suggestions;
explicit reviewed categories and transfer decisions update verified projections.
Available balance observations do not automatically supply ledger opening balances.
Corrections to source evidence, splits and FX transfers need a future workflow;
generated journals remain a rendering of SQLite accounting.

## Inbox sync and mapping decisions

The bank workflow design in [bank-workflow.md](bank-workflow.md) specifies the
implemented folder milestone and later phases. The separate finances.bank library
owns filesystem scanning and sync orchestration. Format detection, mapping types,
and durable import/history operations remain in the core library.

Readable files are archived even when unsupported or rejected. A successful file
and its attempt row commit together; a conflicting file first rolls back its
transaction import, then its bytes and rejection are recorded in another
transaction. Source files are never moved or edited. Import history is append-only,
and repeated filenames or byte-identical copies create observation history without
duplicating source evidence. History is not a completeness assertion.

Account mappings are explicit SQLite decisions keyed by institution and the full
observed source account identity. Applying a mapping validates all affected old
projections, renames their bank postings, upserts the decision, and appends an audit
record in one transaction. Future projections use that mapping. Multiple identities
may share a ledger name for reporting, but this does not merge source identities
or deduplicate differently masked observations of one underlying account.

Sync validates database/config/output paths, imports files independently, applies
mappings, matches CSV observations, projects accepted bank events, and atomically
publishes a journal. A
failed mapping or projection leaves earlier successful source imports committed
and the previous journal untouched. Readable unsupported companions are pending
work; rejected and unreadable files cause a nonzero CLI exit after the successful
subset is published and reported.

## CSV enrichment

Bank_csv parses the observed BoursoBank layout with the OCaml CSV library. Exact
amounts and dates are validated before any row commits. Original decoded fields
and normalized observation columns are stored under document ID and logical row
number, with the archived input retaining the original bytes.

Bank_csv_match indexes OFX movements by institution/account/date/exact amount and
requires a normalized original-description match. It never uses suggested labels
or row ordering to invent an identity. Repeated identical CSV rows in one document
cannot claim a single OFX movement. Nonempty metadata disagreements across linked
observations are conflicts. All observations remain immutable.

Store.match_bank_csv recomputes derived links transactionally, retaining candidate
IDs and reasons and auditing only changed results. It runs after all files are
scanned, so CSV/OFX filename order does not affect results. Matching can become
ambiguous when more OFX evidence arrives. The review API exposes pending or
unresolved observations; there is no manual link-approval command yet.

The journal adapter uses only uncontested matches for secondary value dates and
bank suggestion tags. It leaves stored accounting entries unchanged. CSV pointage
and reported balances do not imply cleared transactions or opening balances.
Coverage counts accompany generated output; unresolved rows retain their base OFX
movements without enrichment.

## Next milestones

Finish banking before returning to exchange integrations:

- Resolve source account identity aliases explicitly, including changed masking.
- Persist reviewed categories and versioned rules.
- Extend reviewed transfers to FX, splits and deferred-debit card settlements.
- Establish verified opening balances and reconcile statement balances.
- Produce reports that distinguish reviewed totals from unresolved evidence.

Paradex position reconstruction, funding, and exchange reconciliation are deferred
until the bank workflow is complete. No API credentials or live exchange
operations are needed by the current kernel.

## Wise bank adapter

Wise_camt reads the observed CAMT.053.001.10 shape, validates booked status,
currency, interval, entry count when present, distinct event identities, and exact
opening/movements/closing reconciliation before import. A statement and all source
movements commit atomically. Rejected documents retain bytes and an import attempt.
Wise statements retain their balance observations and links to every movement,
including overlapping statement membership; balances never create postings.

Bank_transaction provides the shared projection view over OFX and Wise payloads.
Wise events use kind `wise-transaction`, a full-precision timestamp/reference/direction
identity, and a payload containing booking clock, amount, account/currency and
optional value date. The generic event stores UTC milliseconds; the identity and
payload preserve finer precision. Journal dates follow the original local date.

Wise_csv preserves all 23 fields and matches only Wise CAMT events using normalized
reference, local millisecond clock, amount and currency. Multiple balance accounts
with indistinguishable candidates remain ambiguous. Separate observation and match
tables keep BoursoBank's account/date/description matching policy independent.
Both adapters share matching-state persistence and append-only decision history.
The folder workflow recomputes both match sets before generic bank projection.

`bank wise status` recomputes statement reconciliation from stored event memberships
using Bigdecimal and exposes every Wise CSV row's match state. The journal adds
uncontested merchant, payment reference and reported-fee metadata, without fee or
FX postings. Real samples currently validate EUR movements with zero fees.

## Bank adapter layout

Bank-specific adapters live in `lib/banks/boursobank/` (OFX parsing, BoursoBank CSV
parsing and matching), `lib/banks/wise/` (CAMT parsing and Wise CSV parsing and
matching), `lib/banks/bunq/` (MT940 parsing, movement identities and CSV matching),
and `lib/banks/revolut/` (primary CSV movement parsing and fee evidence).
Dune's `include_subdirs unqualified` builds these directories into the
existing `finances` library, preserving its public module names.

The adapters share `Bank_match` for matching states and results; Wise does not
depend on the BoursoBank matcher. `Bank_csv_match` re-exports these types for
compatibility. Source events, database persistence, account mappings,
the common bank transaction view, and accounting projection stay in `lib/`.
Folder scanning and sync orchestration remain in `bank/`, and journal rendering
remains in `journal/`. This is source organization within one library; adding a
bank still requires wiring its importer and persistence into the workflow.

`dune fmt` applies the repository's ocamlformat configuration (Jane Street profile)
to all adapter and shared OCaml files, including nested directories.

## Reviewed bank categories

`Bank_category` validates explicit per-event decisions with an expense/income
counter-account and a nonempty reason; `equity:unclassified` resets a decision.
The baseline includes current `bank_categories` and append-only
`bank_category_changes` without rewriting original evidence or entries.

Applying a batch validates bank event identities, rejects repeated IDs, compares
any existing entry against its expected projection, and updates only posting
ordinal 1 before saving the decision and audit row in the same transaction. A
conflict rolls back the entire batch. Identical account/reason retries add no audit
row. Bank account mappings compare against the reviewed projection too, preserving
categories while renaming posting ordinal 0.

Projection reads the persisted category for each event. The existing projection
key `bank-unclassified-v1` is retained as its stable identity even after explicit
classification; a review decision updates that entry rather than duplicating the
movement. Journal rendering includes the latest decision ID and sanitized reason.
No bank suggestion automatically becomes a decision, and no extra journal entry
is created for a category change. Review and history CLI commands expose these
states; rendering must run again after applying decisions.

## Bunq adapter

`Bunq_mt940` parses the observed Bunq MT940 form and verifies exact opening balance
plus movements equals closing balance. `Bunq_transaction` builds canonical payloads
and identities including occurrence numbers, so identical repeated payments remain
separate. Its generic source events use kind `bunq-transaction` and dated booking
occurrences. `Bank_transaction` supplies the shared projection view.

The baseline includes immutable
Bunq day inventories, statement balances, CSV rows and audited derived matches.
Import compares the sorted canonical movement inventory for every already observed
account/day. Any mismatch rolls back the entire source import before its rejection
is archived. This conservatively rejects partial-day overlaps or corrections; no
stable upstream IDs exist to resolve those safely. Statement membership uses the
existing document/event links. The reconciliation API sums those persisted events.

`Bunq_csv` treats the currency-free CSV as companion observations only. Normalized
account, dates, amount, counterparty, name and description form a matching key.
Both sources must supply equal occurrence counts per key within each document;
mismatches or currency ambiguity remain review items. CSV arrival order does not
matter: sync recomputes its matches after scanning all files and before projection.
Shared audited match persistence saves transitions without rewriting evidence.

## Revolut and transfer decisions

The baseline includes Revolut document/account context,
immutable daily movement inventories, current transfer decisions, unique per-event
confirmed memberships, and append-only transfer changes.

`Revolut_csv` supports the observed completed Current-account CSV. Its caller must
provide an account label because the file cannot identify the account. Canonical
payloads retain local start/completion clocks, signed principal and nonnegative fee;
booking is a calendar completion date. Source identities include occurrence numbers
and exclude running balance observations. Per-currency running balance steps must
equal principal minus fee, and repeated account/day inventories must agree.

The common transaction view adds an explicit fee (zero for existing adapters).
Projection records bank net amount, opposite principal and, where present, a
positive `expenses:bank-fees` posting. Reviewed categories and transfer decisions
change only the principal counter-account, preserving fees and exact bank quantities.

`Bank_transfer` defines validated decisions and the same-currency, opposite-principal,
seven-day candidate policy. Candidate generation never mutates accounting or infers
ownership. Confirm/reject operations verify both existing projections before changes,
then update pair state, per-event memberships, counter-postings and audit in a single
transaction. Membership uniqueness prevents double assignment across either side of
any pair. Confirming a pair asserts ownership; previously reviewed expense/income
categories require an explicit reset before confirmation. Rejection of a confirmed
pair restores its unclassified counters. Unrelated rejection leaves other links intact.

Effective counter-account resolution checks confirmed transfer membership before
manual categories. Projection, account remapping and bank review all use that view.
Journal generation includes active pair/reason tags. The immutable evidence and
stable projection identity are preserved; no transfer duplicates a bank movement.

## Account identity and configuration

Per-source-account IBAN aliases have append-only change history.
Source identifiers remain unchanged: a Revolut label or domestic account number
can have a registered IBAN without reidentifying movements. Source identifiers
that are themselves valid IBANs provide imported evidence. Card accounts have no
IBAN; their relationship to a bank account is modeled separately.

Identity history supports explicit clearing; card settlement metadata has its own
audit history. `Bank_account_config` defines a version 1 format
with source references, optional ledger names, IBAN updates and settlement links.
Omitted properties retain state; explicit null clears optional curated metadata.
Imported IBANs cannot be changed. Settlement links connect a card source account
to a known non-card account in the same currency and never create accounting entries.

`accounts export/list/apply/history` exposes this single workflow. Applying a
configuration validates source references, composes ledger/identity/relationship
mutations inside one transaction, and checks existing projections and confirmed
transfers. A dry run uses a savepoint and rolls back every update and audit row,
while returning concrete planned changes. Sync accepts the same versioned file
through `--accounts`. SQLite remains authoritative; configuration is an optional
explicit input. Only versioned configuration and grouped CLI commands are supported.

## Explainable transfer ranking

Structured Bunq counterparty IBANs are compared with inferred or registered own
account IBANs. Contradictory pairs are diagnostic-only and cannot be confirmed.
Identity updates that contradict existing confirmations roll back atomically.
Known identities can exclude false counterparts even when their actual matching
movements are outside the imported date range.

`Transfer_ranking` scores compatible, unreviewed candidates using the documented
versioned evidence policy. Same-day booking is strongly weighted, while identity
contradictions are hard exclusions. Numeric scores are not calibrated probabilities.
The graph solver computes exact maximum-weight matchings with unmatched vertices
allowed, independently per connected component. Edge inclusion/exclusion solves
identify forced edges, equal alternatives and score margins without arbitrary tie
breaking. Components above 18 vertices remain unevaluated. `transfers preview`
returns evidence, confidence bands, assignment diagnostics, excluded pairs and
review-required proposals; it never mutates transfer decisions or journal entries.

## Local Arch package

The Arch build helper compiles/tests the checkout in a temporary disk-backed build
directory and generates a native binary payload. Its explicit public-file allowlist
excludes the workspace's personal records. The PKGBUILD installs the native CLI,
license, documentation and man pages with only glibc, GMP and SQLite runtime
requirements. A concrete PKGBUILD and SHA-256 checksums accompany the local artifact.
No system installation or opam switch mutation occurs during packaging.

## CLI identity and XDG paths

The public executable and Cmdliner root name are `fin`; OCaml libraries and the
Arch package retain the `finances` name. `App_paths` resolves absolute XDG data and
config overrides with HOME fallbacks. The database defaults to `fin/fin.sqlite3`
under XDG_DATA_HOME, and account configuration to `fin/accounts.json` under
XDG_CONFIG_HOME. Invalid relative overrides are ignored. With HOME unavailable,
the CLI falls back to the user's passwd home directory.

Commands take an optional `--database/-d` rather than a positional database.
`accounts apply` accepts an optional config filename, defaulting to the XDG config.
`accounts export --output FILE` atomically writes a private configuration and
rejects output paths referring to its own database. Missing database parents are
created with private permissions; library callers still pass explicit database
paths. `fin paths` reports defaults without opening the database. Statement inboxes
remain caller-selected, and sync does not silently apply a config file.
