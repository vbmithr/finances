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

`Store` uses Rel's typed statement bindings and SQLite backend. SQL parameters are
bound rather than interpolated. `Db_schema` defines versioned SQL migrations.
Schema version 5 contains source events, journal entries, postings, immutable
import documents, document/event links, append-only bank import attempts, persisted
bank account mappings, an append-only mapping change log, immutable CSV rows,
derived CSV matches, and append-only match change history. Decimal posting
quantities use text; no financial amount uses SQLite REAL.

Schema initialization runs inside an immediate transaction. The schema version is
tracked through `PRAGMA user_version`; a newer or unsupported version is rejected.
Migration from version 1 rebuilds the event table and its referencing journal and
posting tables inside one transaction, preserving primary keys, retrieval times,
source payload bytes, and posting quantities. Foreign keys stay enabled. A failure
rolls back both schema changes and the version increment. Version 2 to 3 adds
history and mapping tables without rewriting existing evidence or entries. Fresh
and older databases apply the migration chain in one transaction. Version 3 to 4
adds CSV observations and match/audit tables without changing prior source evidence,
accounting entries, mappings, or import history. Version 4 to 5 adds Wise statement
balances and membership links, Wise CSV observations and match history, and expands
the import-attempt format constraint while preserving its rows.
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
produces two explicit, exact postings: its signed bank movement and the opposite
amount in `equity:unclassified`. This does not claim a withdrawal is an expense or
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

Bank journals currently have no inferred opening balances, accepted expense or
income classifications, or transfer matching. Bank-category tags retain suggestions. They support register and period movement
queries. Available balance observations cannot safely supply ledger opening
balances. Corrections and reclassification still need an explicit future workflow;
generated journals do not become a second editable accounting source of truth.

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
- Match owned-account transfers and deferred-debit card settlements.
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

`wise-status` recomputes statement reconciliation from stored event memberships
using Bigdecimal and exposes every Wise CSV row's match state. The journal adds
uncontested merchant, payment reference and reported-fee metadata, without fee or
FX postings. Real samples currently validate EUR movements with zero fees.

## Bank adapter layout

Bank-specific adapters live in `lib/banks/boursobank/` (OFX parsing, BoursoBank CSV
parsing and matching) and `lib/banks/wise/` (CAMT parsing and Wise CSV parsing and
matching). Dune's `include_subdirs unqualified` builds these directories into the
existing `finances` library, preserving its public module names.

The adapters share `Bank_match` for matching states and results; Wise does not
depend on the BoursoBank matcher. `Bank_csv_match` re-exports these types for
compatibility. Source events, database persistence and migrations, account mappings,
the common bank transaction view, and accounting projection stay in `lib/`.
Folder scanning and sync orchestration remain in `bank/`, and journal rendering
remains in `journal/`. This is source organization within one library; adding a
bank still requires wiring its importer and persistence into the workflow.

`dune fmt` applies the repository's ocamlformat configuration (Jane Street profile)
to all adapter and shared OCaml files, including nested directories.
