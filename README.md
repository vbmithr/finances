# finances

Personal finance kernel in OCaml, using Jane Street Core, Bigdecimal, and SQLite
through Rel. SQLite is the source of truth.

## Current scope

- Immutable source payloads with venue, account, event kind, source ID, either an
  event timestamp or a calendar date, and database-generated retrieval time.
- Idempotent ingestion: identical retries return the existing ID; conflicting
  timestamps or payloads return an error. Payloads are preserved byte for byte.
- Accounting entries with typed dates and exact, independently balanced commodity
  amounts. Entries link to their source evidence and a stable projection key.
- Atomic persistence of entries and postings, foreign keys, and versioned schema
  initialization. Unknown schema versions are rejected.
- A linear perpetual instrument and position observation model. Unrealized P&L is
  calculated in the quote commodity using signed quantity and contract multiplier.
- A small CLI for database initialization and importing source evidence.
- OFX 2.x XML bank imports covering all statement blocks and accounts, with exact
  amounts, original document archives, and duplicate detection across exports.
- Bank movement accounting entries against `equity:unclassified`, with atomic,
  repeatable projection from stored events.
- Deterministic hledger journal export through the separate OCaml hledger binding.

The kernel does not yet fetch Paradex data, derive positions from fills, persist
position observations, calculate funding, import CSV, categorize transactions, or
infer opening balances. Those operations can be added around the core APIs.

## Setup

Use an opam switch with OCaml 5.1 or later and SQLite development headers
(SQLite 3.38.5 or later). Rel is pinned in `finances.opam` to upstream commit
`f196804c51031870327ecc482d0b97f052941081` from
[dbuenzli/rel](https://github.com/dbuenzli/rel). This is an immutable Git revision,
not a branch pin. `conf-sqlite3` is required so Rel builds its SQLite backend.
The OCaml `hledger` binding is also pinned to upstream commit
`8ae0e0e48652af83549f857be6eee3d79f54db07` of
[ocaml-hledger](https://github.com/vbmithr/ocaml-hledger). This opam package is
distinct from the hledger command-line program, which is used to query journals.

From this directory, in your chosen switch:

```sh
mkdir -p "$HOME/.cache/codex/scratch/finances"
export TMPDIR="$HOME/.cache/codex/scratch/finances"
opam pin add --no-action rel \
  'git+https://github.com/dbuenzli/rel.git#f196804c51031870327ecc482d0b97f052941081'
opam pin add --no-action hledger \
  'git+https://github.com/vbmithr/ocaml-hledger.git#8ae0e0e48652af83549f857be6eee3d79f54db07'
opam install . --deps-only --with-test
opam exec -- dune build
opam exec -- dune runtest
```

Tests require `TMPDIR` and remove their temporary databases. Formatting uses
ocamlformat 0.29.0 with the Jane Street profile:

```sh
opam install ocamlformat.0.29.0
opam exec -- dune fmt
```

## CLI

```sh
dune exec finances -- init personal.sqlite3
dune exec finances -- ingest personal.sqlite3 examples/source-event.json \
  --venue paradex --account main --kind transfer --id example-deposit-1 \
  --at-ms 1792886400000
dune exec finances -- events personal.sqlite3
```

The example is a synthetic source record, not a Paradex API response. `ingest`
stores its input as opaque evidence; it does not create postings or positions.
Running the same import again prints `existing` and the original internal ID.
Changing the payload under the same source identity is an error, including changes
to whitespace. `events` prints metadata as JSON without printing source payloads.

## Bank imports

```sh
dune exec finances -- import-ofx personal.sqlite3 /path/to/export.ofx
```

The institution identifier defaults to `boursobank`; use `--institution NAME` for
another institution. Keep this identifier consistent across imports. A successful
import prints statement, inserted transaction, existing transaction, and document
counts/IDs. All accounts in the file are imported.

Original document bytes are archived in `import_documents` with a SHA-256 digest.
Each transaction becomes a dated source event identified by institution, bank
account, and FITID. `import_document_events` links transactions to every document
that contains them. An identical file reuses its archive; overlapping files reuse
transactions and preserve their separate statement observations. The complete
import is atomic: conflicting transaction evidence rolls back new events and the
new document together. No accounting entries are generated automatically.

`Bank_ofx.parse` supplies typed statements, exact amounts, booking dates, optional
names/memos, and separately identified available and ledger balance observations.
Its scope is OFX 2.x XML bank messages; OFX 1.x SGML, credit-card and investment
message sets, and transaction corrections are rejected explicitly.

The supplied BoursoBank OFX and CSV contained the same 21 dated amounts across two
accounts. The CSV also supplied categories, suggested descriptions, and value
dates. OFX `DTUSER` was an invalid calendar date in every transaction, so the
parser keeps it as opaque metadata. It is omitted from the canonical transaction
payload used for replay; its original value remains in the archived document.
Formatting whitespace is normalized in transaction payloads, while other changes
to a transaction's canonical XML cause a conflict. CSV enrichment is a future step.

`Source_event.occurrence` distinguishes `At_ms` from `On_date`. The `events` command
prints `occurred_at_ms: null` and a populated `occurred_on` for bank records.
Opening an existing schema version 1 database migrates it atomically to version 2,
preserving source IDs, evidence, journal entries, and exact posting quantities.
The bank test fixture is synthetic; personal exports are not test fixtures.

## From bank export to a queryable journal

The following workflow stores the source data and accounting entries in SQLite,
then generates a journal. `personal/` is ignored by Git and holds durable local
financial data, not scratch files.

```sh
mkdir -p personal
dune exec finances -- import-ofx personal/finances.sqlite3 /path/to/export.ofx
dune exec finances -- project-bank personal/finances.sqlite3
dune exec finances -- export-hledger personal/finances.sqlite3 > personal/bank.journal
hledger --no-conf -f personal/bank.journal check --strict
```

`project-bank` reads the stored canonical source events; it does not need the OFX
file again. It creates one balanced entry per bank transaction, with the signed
amount posted to the bank account and its opposite to `equity:unclassified`.
Reimporting or projecting again creates no duplicate entries. A projection conflict
or invalid source event rolls back the complete projection batch.

Bank ledger names are
`assets:bank:INSTITUTION:BANK_ID:ACCOUNT_TYPE:ACCOUNT_ID`. Components containing
characters such as masking asterisks are encoded as `x-` followed by their UTF-8
bytes in hexadecimal. Literal components starting with `x-` are also encoded,
preventing collisions. Original identifiers remain in SQLite. Names and memos are
used in descriptions; control characters become spaces and literal semicolons and
pipes within source fields use their fullwidth equivalents to avoid changing
journal syntax. Source text remains intact in the database archive.

Journal entries contain a `source-event-id` tag and projection key for tracing back
to SQLite. Accounts and commodities are declared, and commodity precision retains
every stored decimal digit. The export is stable across repeated runs. Regenerate
it after changing the database; journal edits do not flow back into SQLite.

Useful [hledger queries](https://hledger.org/hledger.html):

```sh
# All bank transactions, with descriptions and running movement totals
hledger --no-conf -f personal/bank.journal register assets:bank

# Monthly net movements, grouped by account
hledger --no-conf -f personal/bank.journal balance assets:bank -M --tree

# Monthly money leaving or entering bank accounts
hledger --no-conf -f personal/bank.journal balance assets:bank 'amt:<0' -M --tree
hledger --no-conf -f personal/bank.journal balance assets:bank 'amt:>0' -M --tree

# Search descriptions; replace TEXT with a merchant or other search expression
hledger --no-conf -f personal/bank.journal register assets:bank 'desc:TEXT'
```

These reports describe the imported period's movements. They are not current
balances without verified opening balances, and outflows/inflows include transfers
and other unclassified movements. No expense or income category is guessed. The
observed export supplies available balances, which are not used as ledger opening
balances. Adding CSV categories and reviewed transfer matching is a later step.

The core `finances` library remains independent of hledger. The
`finances.hledger` adapter and CLI use the pinned OCaml binding to render the
journal; the external hledger CLI validates and queries that output.

## Library

```ocaml
open Core
open Finances

let collateral_transfer =
  let open Or_error.Let_syntax in
  let amount = Bigdecimal.of_string "1000.00000001" in
  let%bind incoming =
    Accounting.posting ~account:"assets:paradex:collateral" ~commodity:"USDC" amount
  in
  let%bind outgoing =
    Accounting.posting ~account:"assets:wallet" ~commodity:"USDC"
      (Bigdecimal.neg amount)
  in
  Accounting.entry ~date:(Date.of_string "2026-10-25")
    ~description:"Collateral transfer" [ incoming; outgoing ]
```

`Store.with_open` scopes the connection, initializes or migrates the schema, and
returns `Or_error.t`. `Store.ingest` returns `Inserted id` or `Existing id`.
`Store.record_entry db ~event_id ~key entry` atomically persists a balanced entry
and all its postings. The key identifies one accounting projection of one source
event, for example `collateral:v1`. The same entry can be replayed without creating
duplicates; a different entry under that key is rejected.

Amounts use `Bigdecimal.t` directly and are stored with
`Bigdecimal.to_string_no_sn` in SQLite `TEXT` columns. Do monetary arithmetic in
OCaml: SQLite numeric casts and `SUM` on these strings would lose exactness.
Account names are nonempty colon-separated tokens, and commodities are nonempty
tokens; tokens allow letters, digits, underscores, and hyphens.

## Design

See [the architecture notes](docs/architecture.md) for storage ownership,
invariants, and the next steps toward Paradex reconciliation.

## License

[ISC](LICENSE.md).
