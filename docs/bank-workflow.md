# Bank workflow design

## Goal

Finish the personal banking workflow before adding exchange integrations. Drop
exports into a folder, import evidence into SQLite, resolve a small review queue,
and generate reproducible hledger journals and useful financial reports.

The workflow is complete when another month's records can be added without
duplicate postings, reviewed decisions survive regeneration, and account balances
can be reconciled against bank statements.

## Ownership and storage

SQLite owns original evidence, import history, account mappings, review decisions,
and accounting entries. hledger journals and reports are generated outputs. Inputs
are never moved or modified by ingestion.

```text
personal/
  inbox/                 bank exports, including overlapping periods
  config/
    accounts.json        optional editable mapping input
    rules.json           future categorization rules
  finances.sqlite3       evidence, decisions, and accounting
  bank.journal           generated output
  reports/               generated reports
```

This directory is durable personal data and is ignored by Git. Task scratch files
and test databases belong in disk-backed scratch storage.

## Milestones

| Milestone | Deliverable | Acceptance criterion |
|---|---|---|
| Folder ingestion and account mapping | Explicit folder sync, durable per-file outcomes, readable ledger names | Repeated and overlapping exports create no duplicate postings; one bad file does not stop unrelated imports |
| CSV enrichment | BoursoBank value dates, suggested labels, and categories attached to existing OFX transactions | OFX plus CSV describes each movement once; ambiguous matches are visible |
| Accounting rules and review | Persistent manual decisions and versioned categorization rules | Regenerating a journal retains reviewed choices and their provenance |
| Transfer matching | Explicit links between movements in owned accounts | Reviewed internal transfers do not inflate income or expenses |
| Reconciliation and reporting | Verified opening balances, statement checks, and reports | Every balance discrepancy is visible and explainable |

The first milestone is implemented: folder sync, import history, and persisted
ledger naming are available. CSV enrichment is also implemented, including exact
matching, persisted review states, and journal metadata. A background watcher,
source account identity resolution, Excel/QIF parsing, categorization, transfer
matching, and balance reconciliation are later work. Unsupported files must be reported rather than silently accepted.

## Folder ingestion

The initial entry point is an explicit command:

```sh
finances sync-bank personal/finances.sqlite3 personal/inbox \
  --journal personal/bank.journal
```

One invocation uses one explicit institution identifier, defaulting to
`boursobank`. It does not guess the institution from filenames. Separate inboxes
or invocations can be used for different banks.

Scan regular files recursively in deterministic path order. Do not follow symbolic
links. Ignore hidden files/directories and names ending in `.part`, `.tmp`, or `~`;
report ignored paths. Run sync after copying files has finished. Detect changes in
size, identity, or modification time during a read and request a retry. A stable
read cannot prove that a paused writer has finished.

Read each file once and hash the exact bytes passed to the parser. Detect OFX from
its content, accepting renamed exports. File extensions can identify an intended
OFX input when malformed; they cannot make arbitrary content valid. Identify CSV,
QIF, and spreadsheets for explicit unsupported-format reporting.

Archive readable input bytes in the existing immutable `import_documents` table,
including rejected and unsupported inputs. Append a `bank_import_attempts` row for
every observed file, including repeated files and unreadable paths. Each outcome
records its path, institution, format, archive reference when available, counts,
diagnostic, and timestamp. An unreadable file cannot have a content archive.

| Outcome | Meaning |
|---|---|
| Imported | A newly accepted document; it may contain already known transactions |
| Unchanged | The same bytes were already accepted, even under another filename |
| Rejected | Malformed OFX/CSV or conflicting transaction evidence |
| Unsupported | Readable input whose format is not implemented |
| Unreadable | The file could not be read consistently or exceeds the read limit |

Successful import and its attempt record commit together. A conflicting file
rolls back all new source transactions and links from that file; its original bytes
and rejection are then recorded separately. Files commit independently, allowing
later files to proceed. Database failures that prevent recording an outcome abort
the command rather than claiming success.

Hashes identify documents, not economic movements. Transactions retain their
institution/account/FITID identity. Date and amount alone cannot distinguish
duplicate observations from legitimate identical payments. Changes to existing
evidence require an explicit correction workflow.

## Account mapping

Discover bank accounts from imported evidence and expose their complete source
identity and current ledger name:

```sh
finances bank-accounts personal/finances.sqlite3 > personal/config/accounts.json
```

Each mapping contains `institution`, `bank_id`, `account_id`, `account_type`, and
`ledger_account`. Users can replace the generated ledger name with a readable
`assets:bank:...` name, then apply the file explicitly or during sync:

```sh
finances map-bank-accounts personal/finances.sqlite3 personal/config/accounts.json
finances sync-bank personal/finances.sqlite3 personal/inbox \
  --accounts personal/config/accounts.json --journal personal/bank.journal
```

Mappings are validated, persisted in SQLite, and audited. The JSON file is an input
to a decision, not a second authoritative store. Missing mappings in a partial file
do not delete stored decisions. Unknown source accounts are errors, catching typos;
import an account before mapping it. Duplicate source identities in one config are
rejected. Multiple source identities may explicitly map to the same ledger name
for consolidated reporting. This is a ledger naming decision, not source identity
resolution: it does not deduplicate two exports that present the same underlying
account under different masked identifiers. Such identity aliases require a later
reviewed resolution step before their movements can safely be merged.

Mapping changes rename only the bank posting in existing entries generated by
`bank-unclassified-v1`. Verify the old projection before modifying it; conflicting
or manually changed entries prevent the whole mapping update. Amounts, dates,
descriptions, source evidence, and unclassified counter-postings are preserved.
Mapping rows, audit rows, and posting renames commit atomically. Future projections
use the stored name, so rerunning them does not create another entry.

## Sync and publication

Validate control paths and parse the optional mapping file before import. Keep the
database, config, and journal outside the inbox. Refuse a journal destination that
aliases the database or config. The scanner excludes control-file identities, including hard links, so it does
not ingest its own database or outputs.

After per-file imports, apply mappings, project all accepted bank events, serialize
the journal, and publish it with an atomic rename. Stage output in the configured
disk-backed `TMPDIR`; publication requires it to share a filesystem with the
journal. When TMPDIR is unset, staging uses the destination directory. If mapping,
projection, or publication fails, the existing journal stays
intact. Earlier successful source imports remain committed and can be resumed.

Sync publishes the successfully imported subset even when other files need review.
Its summary lists outcomes and reports transaction, mapping, and projection counts.
Rejected or unreadable inputs produce a nonzero CLI exit after reporting results;
unsupported companions are reported as pending work. CSV match coverage and review
counts are reported separately without blocking valid OFX accounting. A history command provides
durable diagnostics. A generated journal does not establish that the inbox or date
range is complete.

## CSV enrichment and ambiguity

BoursoBank OFX currently supplies usable transaction IDs and booking dates. Its
observed `DTUSER` values are invalid calendar dates. CSV supplies useful value dates,
categories, and suggested labels but no proven stable transaction identity.

Treat OFX as the transaction identity backbone and CSV as additional observations.
Match within a resolved account using booking date, exact amount, and available
description/reference evidence. Do not merge ambiguous matches automatically.
CSV-only records need an explicit identity policy before they can create postings.
Preserve every source observation and record enrichment provenance.

The implemented CSV reader supports the observed 12-column UTF-8 BoursoBank
layout. The first Solde column is a French-decimal movement amount; the second is
an independently reported balance with dot decimals. It handles quoted semicolons,
multiline fields, CRLF and BOM, and validates dates and decimals without floats.
A malformed row rejects the whole CSV document while retaining its archived bytes.

Immutable CSV observations use document ID and logical row number as provenance,
not movement identity. Automatic matching requires institution, exact source
account ID, booking date, exact amount, and normalized original description. It
requires uniqueness in both directions within a document. Identical observations
across different exports can link to one OFX movement. No implicit currency
conversion or account-mask normalization is performed.

After every folder scan, recompute derived links against all available OFX events.
Persist matched, unmatched, ambiguous, and conflicting states, candidate event IDs,
reasons, and the matching policy. Append an audit record only when a result changes.
Contradictory nonempty value dates, categories, suggested descriptions, or comments
require review; balances and pointage remain separate observations. A new OFX
record can invalidate a formerly unique automatic link without rewriting evidence.

Uncontested CSV value dates and bank suggestions are available in journal secondary
dates and tags. They do not change booking dates, amounts, account classifications,
or clearing status. The journal includes coverage counts, and inspection is exposed
through csv-enrichments with an optional --review flag. Manual approval of links
and conflicting metadata remains a later review-workflow feature.

## Decisions, transfers, and corrections

Manual review decisions belong in SQLite with the affected transaction, previous
decision, reason, and rule version where applicable. Rules may suggest expense or
income categories; unresolved records remain unclassified. Distinguish suggestions
from accepted decisions. Corrections and reclassification need controlled
projection updates rather than another posting of the original movement.

An owned-account transfer requires matching two separately observed bank movements.
Use resolved account ownership, currency, signed amounts, dates, and references to
propose links. Handle fees, FX, deferred-debit card settlement, split movements, and
missing counterparts explicitly. Do not infer ownership from a familiar name.

## Reconciliation and reporting

Use verified ledger balances and statement boundaries to establish opening balances
and reconcile subsequent movements. An available balance is not an opening ledger
balance. Preserve distinctions among booking dates, value dates, observation times,
and precise timestamps.

Until classification and opening balances are verified, reports describe account
movements, not current balances or definitive spending/income. Transfers can appear
in gross inflow/outflow totals. Later reports should expose unresolved records,
unmatched transfers, and reconciliation discrepancies alongside reviewed totals.

## Validation

Use synthetic fixtures for automated tests. Cover nested folders, renamed and
duplicate files, overlapping periods, same-amount legitimate transactions,
malformed files between valid ones, unsupported companions, append-only history,
account aliases, mapping changes after projection, rollback, schema migration,
output-path protection, and atomic journal publication. Verify journal syntax and
amounts with the installed hledger CLI when running the end-to-end workflow.

## Wise adapter sample findings

The supplied CAMT.053.001.10 and CSV pair describes the same booked movements;
opening balance plus movements reconciles to the closing booked balance. CAMT
preserves higher timestamp precision and booked balances, while CSV adds useful
merchant, card, and counterparty metadata.

Wise transaction IDs in this sample are not unique per movement: an ID can be
reused for opposite-sign entries at different times. Standard entry references
are incomplete. Proprietary bank transaction codes carry the corresponding Wise
IDs, with cashback format differences. An adapter must preserve distinct events
and verify reimport stability without deduplicating on ID alone.

The adapter is implemented: identity combines normalized reference, full UTC
nanosecond booking instant, and debit/credit direction within the institution and
currency balance account. Each statement must reconcile exactly before acceptance.
SQLite retains statement balances and movement membership for independent review.
CSV adds metadata only, matched by reference/local millisecond clock/amount/currency
with conservative ambiguity and conflict handling. Reimport and overlap reuse events.

Use separate inboxes with explicit `--institution wise` and
`--institution boursobank`, sharing the same database and journal output.
`wise-status DATABASE` reports statement differences and CSV matching decisions.
The combined journal includes every institution already stored. Opening balance
postings, fee postings, and FX accounting are not inferred. Real FX and nonzero-fee
cases need additional coverage; synthetic tests exercise fee metadata conflicts,
currency separation, reversals, and multi-bank replay.
