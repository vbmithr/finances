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
Schema version 2 contains `source_events`, `journal_entries`, `postings`,
`import_documents`, and `import_document_events`, plus indexes and triggers that
reject updates or deletions of source events and archived documents. Decimal
posting quantities use text; no financial amount uses SQLite `REAL`.

Schema initialization runs inside an immediate transaction. The schema version is
tracked through `PRAGMA user_version`; a newer or unsupported version is rejected.
Migration from version 1 rebuilds the event table and its referencing journal and
posting tables inside one transaction, preserving primary keys, retrieval times,
source payload bytes, and posting quantities. Foreign keys stay enabled. A failure
rolls back both schema changes and the version increment.
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
Changed accounting projections are rejected; editing or rebuilding them will need
an explicit future operation.

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
not yet create a separate normalized bank transaction table or import CSV value
dates or categories. Balance observations are preserved in the archived statement;
they are not yet used to reconcile account balances.

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

Bank journals currently have no inferred opening balances, expense categories,
income categories, or transfer matching. They support register and period movement
queries. Available balance observations cannot safely supply ledger opening
balances. Corrections and reclassification still need an explicit future workflow;
generated journals do not become a second editable accounting source of truth.

## Next milestone

Implement a Paradex adapter for one account and a bounded historical period:

- Store source records and pagination checkpoints atomically in import batches.
- Normalize fills, transfers, fees, funding, and correction events while retaining
  provenance and source sequence numbers.
- Reconstruct position quantities and average entry prices, covering partial
  closes and reversals.
- Apply an explicit accounting policy for collateral and realized flows.
- Store timestamped exchange snapshots and reconcile collateral and positions.

Portfolio valuation, categorization, and verified opening balances remain separate
next steps. No API credentials or live exchange operations are needed by the
current kernel.
