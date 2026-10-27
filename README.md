# finances

Personal finance kernel in OCaml, using Jane Street Base, decimal, Ptime, Bos, and SQLite
through Rel. SQLite is the source of truth.

Where the project stands and what comes next: [docs/plan.md](docs/plan.md). What
changed: [CHANGES.md](CHANGES.md).

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
- Bank movement accounting entries with atomic, repeatable projection and explicit
  reviewed categories or transfer counter-accounts.
- Deterministic hledger journal export through the separate OCaml hledger binding.
- Recursive inbox sync with content detection, per-file import history, and
  atomic journal publication.
- Persisted bank account names with audited updates to verified generated entries.
- BoursoBank CSV enrichment with conservative OFX matching and a review report for
  unmatched, ambiguous, or conflicting observations.
- Wise CAMT, Bunq MT940 and Revolut CSV imports with exact accounting, companion
  matching where available, and explicit fee handling for Revolut.
- Audited confirm/reject transfer review with one confirmed counterpart per movement.

The kernel does not yet fetch Paradex data, derive positions from fills, persist
position observations, calculate funding, automatically categorize transactions, or
infer opening balances. Those operations can be added around the core APIs.

## CLI and XDG storage

Start with the [user manual](docs/manual.md) for setup, bank imports, account
configuration, review and hledger queries.

The executable is `fin`; the OCaml libraries and Arch package remain named
`finances`. No database argument is needed for normal use:

```sh
fin
fin init
fin status
fin paths
fin accounts list
fin bank statements --institution bunq
fin transfers preview
```

Running `fin` without arguments shows a welcome and useful first steps, exits
successfully, and creates no files. `fin init` creates the database and prints its
location, the optional account-config location, and an import example. It is the
only command that creates a database; the others fail with "fin is not
initialized" until it has run. It is safe to repeat and preserves existing data and
configuration. Use `fin init --database FILE` to initialize a different database.
No account questionnaire is needed: accounts appear from imported statements.
The lower-level `fin db init` remains available for quiet database initialization.

`fin status` summarizes observed account date ranges, imported movements,
unclassified movements, unreviewed transfer candidate pairs and the latest inbox
attempt. It creates no database when none exists. Date ranges are observations,
not proof of complete coverage; financial reports come from the exported hledger
journal. `accounts list`, `bank review`, `transfers candidates` and
`transfers preview` now display readable text by default. Add `--json` to those
commands or `status` for scripts; the existing inspection JSON shapes are retained.
`bank history` also displays readable file import attempts by default, with
`--json` for scripts. Other history and provider reconciliation commands emit JSON.

The [XDG Base Directory specification](https://specifications.freedesktop.org/basedir/latest/)
provides the base paths. Defaults are:

| Purpose | Default | Override |
| --- | --- | --- |
| Database | `~/.local/share/fin/fin.sqlite3` | `$XDG_DATA_HOME/fin/fin.sqlite3` |
| Account config | `~/.config/fin/accounts.json` | `$XDG_CONFIG_HOME/fin/accounts.json` |

Empty or relative XDG environment values are ignored. `fin paths` prints expanded
locations. First use creates database parent directories with mode 0700 where
needed. Existing directory permissions are retained. Configuration export writes
atomically with mode 0600. SQLite is authoritative; configuration remains an input.
Sync uses saved account settings unless `--accounts FILE` is explicitly supplied.

```sh
fin accounts list --database /path/to/experiment.sqlite3
fin accounts export -o "${XDG_CONFIG_HOME:-$HOME/.config}/fin/accounts.json"
fin accounts apply --dry-run  # reads the default XDG account config
fin accounts apply           # persist those edits
fin accounts apply /path/to/other-accounts.json --database /path/to/experiment.sqlite3
```

`--database` (or `-d`) is an option on commands that access SQLite. Input files and
inbox directories are positional; the old positional database argument is removed.
Use `dune exec fin -- ...` when running from the checkout. Journals default to
`bank.journal` beside the selected database when syncing. Original statement inboxes
are ordinary user-chosen directories, independent of the DB/config defaults.

## Install an Arch Linux test package

Build the current checkout using a prepared opam switch, and install it:

```sh
make install OPAM_SWITCH=5.5.0
fin --version
fin accounts list
fin bank statements --institution bunq
```

`make install` builds the package as `make package` does, then installs it with
`sudo pacman -U`, which asks for your password and confirmation. `make package`
only builds it: it generates `dist/arch/PKGBUILD` from
`packaging/arch/PKGBUILD.in`, fills in the version, architecture and SHA-256 digest,
and builds `dist/arch/finances-VERSION-PKGREL-x86_64.pkg.tar.zst`. To produce only the concrete recipe and its matching native
payload archive (without running the package build):

```sh
make pkgbuild OPAM_SWITCH=5.5.0
```

Run make from the project root. Omit `OPAM_SWITCH` to use the active switch.
`make help` lists targets, including `build`, `test` and `fmt`. Override
`ARCH_OUTPUT` or `SCRATCH_ROOT` to change output or disk-backed scratch locations.
The underlying commands remain `python packaging/arch/build.py --switch 5.5.0`
and that command with `--prepare-only`, respectively. The generated PKGBUILD uses
the generated binary archive, so copying the template alone is insufficient.

The helper compiles and tests a native release executable, stages only the binary,
public docs, license and generated man pages, then invokes makepkg. Its working
files use disk-backed cache storage and are removed afterward. `dist/arch/` holds
the package, a checksummed binary payload and a concrete PKGBUILD for repackaging.
This is a local experimental binary package, not an AUR source-build recipe.
It requires makepkg/fakeroot, Python, zstd and the prepared opam build dependencies.
The installed executable requires only glibc, GMP (used through Zarith/decimal),
and SQLite; opam and OCaml are not needed at runtime. The hledger CLI is optional
for querying rendered journals. No personal records or configuration enter the package.
`packaging/arch/build.py` never runs sudo or installs anything; only `make install`
does, after building.

The CLI provides `init`, `status` and `paths`, with grouped commands under `db`,
`accounts`, `bank`, `transfers`, and `journal`:

```sh
fin db init
fin bank sync bunq personal/bunq/inbox
fin transfers preview
fin journal export
```

## Setup

Use an opam switch with OCaml 5.1 or later and SQLite development headers
(SQLite 3.38.5 or later). Rel is pinned in `finances.opam` to upstream commit
`f196804c51031870327ecc482d0b97f052941081` from
[dbuenzli/rel](https://github.com/dbuenzli/rel). This is an immutable Git revision,
not a branch pin. `conf-sqlite3` is required so Rel builds its SQLite backend.
The OCaml `hledger` binding is also pinned to upstream commit
`cad809fa47876142cfdf361bde2a2e506ebdadad` of
[ocaml-hledger](https://github.com/vbmithr/ocaml-hledger). This opam package is
distinct from the hledger command-line program, which is used to query journals.

From this directory, in your chosen switch:

```sh
mkdir -p "$HOME/.cache/codex/scratch/finances"
export TMPDIR="$HOME/.cache/codex/scratch/finances"
opam pin add --no-action rel \
  'git+https://github.com/dbuenzli/rel.git#f196804c51031870327ecc482d0b97f052941081'
opam pin add --no-action hledger \
  'git+https://github.com/vbmithr/ocaml-hledger.git#cad809fa47876142cfdf361bde2a2e506ebdadad'
opam install . --deps-only --with-test
opam exec -- dune build
opam exec -- dune runtest
```

Dependencies are installed per opam switch. After changing switches, rerun
`opam install . --deps-only --with-test` in this directory. This also installs the
project's pinned Rel and hledger dependencies. If Dune reports a missing library
such as `csv`, check that installation and execution use the same switch:

```sh
opam switch show
opam install --switch=5.5.0 . --deps-only --with-test
opam exec --switch=5.5.0 -- dune exec fin -- bank statements --institution bunq
```

Replace `5.5.0` with your chosen switch. To use plain `dune` commands in your shell,
select that switch and refresh the shell environment with `eval "$(opam env)"`.

Tests require `TMPDIR` and remove their temporary databases. Formatting uses
ocamlformat 0.29.0 with the Jane Street profile:

```sh
opam install ocamlformat.0.29.0
opam exec -- dune fmt
```

Enable the version-controlled pre-commit formatting check in each checkout:

```sh
git config --local core.hooksPath .githooks
```

The hook checks all staged OCaml sources with ocamlformat using the repository's
configuration. It never rewrites files. If it blocks a commit, run `opam exec --
dune fmt`, stage the formatted files, and retry. Opam and the pinned ocamlformat
version must be installed. The hook is enabled in this checkout.


## CLI

```sh
dune exec fin -- db init
dune exec fin -- db ingest examples/source-event.json \
  --venue paradex --account main --kind transfer --id example-deposit-1 \
  --at-ms 1792886400000
dune exec fin -- db events
```

The example is a synthetic source record, not a Paradex API response. `ingest`
stores its input as opaque evidence; it does not create postings or positions.
Running the same import again prints `existing` and the original internal ID.
Changing the payload under the same source identity is an error, including changes
to whitespace. `events` prints metadata as JSON without printing source payloads.

## Bank imports

```sh
dune exec fin -- bank sync boursobank /path/to/statements
```

Every import goes through `bank sync BANK PATH...`. `BANK` is required and must name a
registered bank (`boursobank`, `bunq`, `revolut` or `wise`); files imported under
any other identifier are rejected, with the attempt recorded. The institution's
adapter recognizes each file's format; a file another bank recognizes is rejected
with the institution to use instead. Keep this identifier consistent across
imports. Each file's outcome lists inserted and existing movements; all accounts
in a file are imported.

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
to a transaction's canonical XML cause a conflict. CSV enrichment is described below.

`Source_event.occurrence` distinguishes `At_ms` from `On_date`. The `events` command
prints `occurred_at_ms: null` and a populated `occurred_on` for OFX records.
Wise records retain an instant and project using the local booking date.
This experimental build supports only the current schema baseline (11). Fresh
files initialize it atomically; other schema versions are rejected without changes.
There are no historical migrations or schema fixtures. Reimport original statements
and reapply saved account configuration when resetting an experiment. Category and
transfer review files use internal event IDs: inspect the new records before
recreating those decisions instead of blindly reusing old numeric IDs.
Banks have no tables of their own: statements, companion observations, daily
inventories and import contexts share generic tables keyed by institution and
format.
Tables are created from typed Rel definitions in `lib/db_schema.ml`. The same
columns supply Store's inserts and read projections. SQLite checks, strictness
and immutability are declared alongside those tables through `Sqlite_schema`;
there is no separate SQL schema to keep synchronized. See
[Persistence and replay](docs/architecture.md#persistence-and-replay) for details.
The bank test fixture is synthetic; personal exports are not test fixtures.

## From bank export to a queryable journal

The following workflow stores the source data and accounting entries in SQLite,
then generates a journal. `personal/` is ignored by Git and holds durable local
financial data, not scratch files.

```sh
mkdir -p personal
dune exec fin -- init
dune exec fin -- bank sync boursobank /path/to/statements
dune exec fin -- bank project
dune exec fin -- journal export > personal/bank.journal
hledger --no-conf -f personal/bank.journal check --strict
```

`bank project` reads the stored canonical source events; it does not need the OFX
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
balances. Reviewed categorization and explicit transfer reconciliation are available below.

The core `finances` library remains independent of hledger. The
`finances.hledger` adapter and CLI use the pinned OCaml binding to render the
journal; the external hledger CLI validates and queries that output.

## Folder workflow

See [the bank workflow design](docs/bank-workflow.md) for the complete plan and
milestones. The first milestone is available:

~~~sh
mkdir -p personal/{boursobank,wise,bunq,revolut}/inbox personal/reviews personal/reports
# Copy bank exports into personal/boursobank/inbox, then run after copying has completed.
dune exec fin -- init
dune exec fin -- bank sync boursobank personal/boursobank/inbox \
  --journal personal/bank.journal
dune exec fin -- bank history --limit 100
~~~

Sync traverses regular files recursively in path order and detects OFX content
even when an export has a different extension. It ignores symbolic links, hidden
paths, and names ending in .part, .tmp, or ~. Readable rejected and unsupported
files are archived in SQLite with append-only attempt history. A bad file does not
prevent unrelated files from importing. The default per-file limit is 64 MiB;
--max-file-mib accepts 1–1024. Repeated or overlapping exports reuse existing
evidence and entries. Input files are never modified or moved.

The summary distinguishes imported documents, unchanged documents, rejected inputs,
unsupported formats, unreadable files, and ignored paths. The observed BoursoBank
CSV layout, Wise 23-column CSV and Bunq seven-column CSV are companion observations.
Revolut ten-column CSV supplies primary movements. Excel, QIF, and other CSV layouts are
reported as unsupported. Companion CSV observations never create a second
transaction stream; Revolut CSV is its own primary movement source.

Successful files commit independently. Sync generates a journal from successfully
imported data even if some files need review, and returns a nonzero exit for
rejected or unreadable inputs. Unsupported companions are visible in the summary
without failing sync. The CSV summary separately counts matched, unmatched,
ambiguous, and conflicting observations; review items do not prevent valid OFX
entries from being exported. Import history is readable text, newest attempt
first; use `fin bank history --json` for the original JSON layout. It lists file
import attempts, rather than individual bank movements. Use `--limit N` to choose
how many recent attempts to display (100 by default).

To give bank accounts readable ledger names:

~~~sh
dune exec fin -- accounts export \
  -o "${XDG_CONFIG_HOME:-$HOME/.config}/fin/accounts.json"
# Edit ledger_account fields, retaining each account's source identity.
dune exec fin -- accounts apply
dune exec fin -- bank sync boursobank personal/boursobank/inbox \
  --accounts ~/.config/fin/accounts.json --journal personal/bank.journal
~~~

The version 1 configuration manages ledger names, IBANs and card settlement links
as described below. Source identifiers remain immutable. Names stay below
`assets:bank:` and renames verify existing projections before changing postings.
Keep the database, mapping config, and journal outside the inbox. Sync rejects
paths that would ingest its own outputs or overwrite a control file, and skips
hard links to its control files. Journal publication uses an atomic rename after
all accounting work succeeds; mapping,
projection, or output failures leave the old journal intact. Previously accepted
source files remain in SQLite for retry. The staging directory is TMPDIR (or the
journal directory when unset) and must share the journal's filesystem.

## Manage bank accounts

SQLite is authoritative for account names, registered IBANs, and card settlement
relationships. Configure accounts already discovered from imported statements:

```sh
fin accounts list
fin accounts export -o "${XDG_CONFIG_HOME:-$HOME/.config}/fin/accounts.json"
# Edit the exported file, then preview and apply:
fin accounts apply --dry-run
fin accounts apply
fin accounts history
```

`list` returns JSON with source identifiers, ledger account, IBAN and its provenance,
currency, movement count, first/last imported movement dates, and settlement links.
These dates describe observed records, not proof of complete statement coverage.
IBAN provenance is `imported`, `registered`, `missing`, or `not_applicable` for a
credit-card account.

The canonical private configuration has a version and an accounts array. This
example is illustrative; keep the source identifiers from your actual export:

```json
{
  "version": 1,
  "accounts": [
    {
      "institution": "example-bank",
      "bank_id": "EXAMPLE",
      "account_id": "current",
      "account_type": "CHECKING",
      "ledger_account": "assets:bank:daily",
      "iban": "FR1420041010050500013M02606",
      "settlement_account": null
    }
  ]
}
```

The four source identity fields are immutable selectors; changing them does not
rename or merge a source account. `ledger_account` changes its display/accounting
name after verifying existing projections. `iban` supplies an account identifier
missing from exports; spaces and case are normalized, then the country code,
that country's length and BBAN structure from the SWIFT IBAN Registry, and the
MOD 97-10 check digits are validated (ISO 13616-1).
An IBAN already present as a source identifier cannot be replaced or cleared.
IBAN registration does not change movement IDs or historical source evidence.

For a credit card, leave `iban` null and set `settlement_account` to the associated
bank account's reference: an object with `institution`, `bank_id`, `account_id`,
and `account_type`. Settlement targets must be known non-card accounts in the same
currency. This is an informational relationship: it does not copy the parent's IBAN
onto the card, create postings, or infer deferred-debit settlement transfers.

Omitted accounts and optional properties keep their current settings. In version 1
configuration, `iban: null` clears a manually registered IBAN and
`settlement_account: null` removes a link. Clearing and updates are audited.
`--dry-run` returns the planned changes and counts without persisting data or audit
rows. A failure anywhere rolls back all account edits and posting renames together.
Identity edits that contradict confirmed transfers are rejected for review first.

The file is an input, not a second database or watched source of truth. Re-export
from SQLite when creating a fresh editable snapshot. Keep it in the XDG config directory, outside version control. To apply this same configuration during folder sync, use
`--accounts ~/.config/fin/accounts.json`; source imports happen first, then
the account configuration applies atomically before projection and journal output.
Import every referenced source account before applying a complete multi-bank file.

Only the versioned format and grouped commands are supported. Old experimental
flat commands and mapping-array inputs have been removed.

## Review and categorize bank movements

List movements that still use `equity:unclassified`:

```sh
fin bank review --json > personal/reports/bank-review.json
fin bank review --all
```

Use the reported `event_id` values to write a JSON decision file, for example
`personal/reviews/categories.json`. The ID below is illustrative; select the actual
movement from the review output:

```json
[
  {"event_id": 1, "account": "expenses:groceries", "reason": "Reviewed receipt"}
]
```

Apply the entire batch atomically, then regenerate the accounting projection and
journal:

```sh
fin bank categories apply personal/reviews/categories.json
fin bank project
fin journal export > personal/bank.journal
fin bank categories history
hledger -f personal/bank.journal --strict check
hledger -f personal/bank.journal balance expenses --monthly
```

Decisions explicitly assign the movement's counter-account to `expenses:...` or
`income:...`. Set it to `equity:unclassified` with a reason to return a movement to
review. Refunds can use the same expense account as the original purchase. Amount
signs are preserved; classification does not infer or reverse them.

SQLite retains the current account and reason plus append-only change history.
Repeated identical decisions are harmless. Decisions survive sync, reimports and
account renames, and can precede first projection. Existing generated entries are
verified before updating only their counter-account; dates, bank postings, amounts
and source evidence remain intact. An invalid or conflicting decision rejects the
whole batch. Each generated journal includes the latest decision ID and reason as
tags. `bank categories apply` changes SQLite; regenerate the journal to publish it.

This first review step uses explicit decisions. Bank CSV categories remain
suggestions; automatic category rules and split allocations are future work. Explicit
owned-account transfer review is available below. Do not classify transfers as income or spending
merely to remove them from the review queue.

## Bank statement folders

Use the same bank-prefixed layout for every institution:

| Bank | Statement folder | Import support |
| --- | --- | --- |
| BoursoBank | `personal/boursobank/inbox/` | OFX movements, optional CSV enrichment |
| Wise | `personal/wise/inbox/` | CAMT.053 XML movements, optional CSV enrichment |
| Bunq | `personal/bunq/inbox/` | MT940 movements, optional seven-column CSV companion |
| Revolut | `personal/revolut/inbox/` | Completed Current-account CSV, explicit account label |

Keep the shared database, journal, configuration, and reports outside these inboxes.
Pass the institution explicitly when syncing. All `personal/` contents are
ignored by Git. Existing import history retains the paths used at import time;
moving an inbox does not change source identities or create new movements.

## Wise and multiple banks

Wise CAMT.053.001.10 XML supplies movements, full booking timestamps, account
currency, and opening/closing booked balances. The matching 23-column Wise CSV is
optional enrichment. Keep a separate inbox per institution and sync both into the
same database and journal:

```sh
fin bank sync boursobank personal/boursobank/inbox \
  --journal personal/bank.journal
fin bank sync wise personal/wise/inbox \
  --journal personal/bank.journal
fin bank statements --institution wise
hledger -f personal/bank.journal --strict check
hledger -f personal/bank.journal register assets:bank:wise
```

Each sync regenerates the journal from all stored bank entries. Default account
names include institution, bank, account type, and account ID; Wise balance account
types include currency. Existing account mappings also work for Wise. Wise exports
name `wise` as the bank, like every other import. CSV and XML may arrive in either order.

Wise references can repeat for reversals. Identity therefore includes the
normalized proprietary transaction reference, full UTC nanosecond booking instant,
and debit/credit direction, scoped by institution and balance account. Cashback
UUID formatting is normalized. Amount is evidence, so a changed amount for an
existing identity rejects the whole import. Indistinguishable duplicate movements
within a statement are rejected rather than collapsed. Original bytes remain archived.

The parser requires booked entries and exact reconciliation of opening balance
plus signed movements with closing balance. `bank statements` independently computes
that difference from stored statement/event links and lists CSV matching states.
Statement balances are observations; the journal contains movements without
inferred opening-balance entries.

Wise CSV matching requires reference, local millisecond booking clock, exact signed
amount, and currency to agree uniquely in both directions within a document. CSV
has no timezone or reliable balance-account identity, so ambiguity remains visible.
Fees and FX metadata disagreements across exports are audited as conflicts.
Uncontested matches add `wise-merchant`, `wise-payment-reference`,
`wise-reported-fees`, and `companion-row-ids` journal tags where available. Fees remain
metadata: no extra fee posting or currency conversion is inferred. The supplied
samples cover EUR with zero fees; synthetic tests cover fee metadata and currency
isolation, but real FX and nonzero-fee exports need further validation.

## Bunq statements

Put Bunq's MT940 export (including exports with no filename extension) and its
optional seven-column CSV into `personal/bunq/inbox/`:

```sh
fin bank sync bunq personal/bunq/inbox \
  --journal personal/bank.journal
fin bank statements --institution bunq
```

The MT940 reader supports the observed Bunq layout: a BUNQ header, account/currency,
`:28:` statement sequence, final opening/closing balances, paired `:61:`/`:86:`
entries with `NONREF`, and IBAN/name/remittance metadata. It reads exact comma
decimals, handles multiline descriptions, preserves booking and interest dates,
and rejects unsupported fields, date ranges, currencies or unbalanced statements.
Amounts never pass through floating point. Interest dates become journal secondary
dates. `bank statements` independently reconciles persisted statement/event links.

Bunq supplies no movement IDs in these exports. Identity uses normalized account,
currency, booking/interest dates, signed amount, counterparty, name and description,
plus an occurrence number for identical movements. Identical repeated movements
are preserved. An immutable inventory of each imported account/day prevents
changed evidence, corrected amounts or partial-day overlaps from silently creating
or collapsing movements. Exact overlapping day inventories are reusable; a changed
inventory rejects the whole statement for review. Export complete days when
extending the history. Resolving a changed day is a future correction workflow.

The companion CSV has `Date`, `Interest Date`, `Amount`, `Account`, `Counterparty`,
`Name`, and `Description` columns. It supports Bunq's comma decimals and grouped
thousands. It carries no currency, so it never creates events or supplies guessed
currency: rows wait for matching MT940 evidence. Exact normalized fields and
per-document occurrence counts must agree; partial duplicate groups or competing
currencies remain ambiguous. Raw rows and matching history are retained, and
uncontested matches add CSV provenance and counterparty tags to the journal.
Bunq uses the same persisted account mappings and reviewed categories as other banks.

## Revolut CSV import

Place the ten-column Revolut CSV export in `personal/revolut/inbox/`:

```sh
fin bank sync revolut personal/revolut/inbox \
  --account-id main --journal personal/bank.journal
```

The CSV has no account identifier. Supply a stable label (`main` here) for that
specific account every time; different accounts need different labels and inboxes.
A previously imported document cannot be reassigned to another label. Currency is
part of the account identity. Only completed `Current` transactions are supported;
pending, reversed, or other products reject the file for review.

The adapter validates exact decimal fields and each consecutive running balance
per currency against **Amount − Fee**. The first opening balance is not independently
reported, so this validates the running balance sequence without asserting an
external opening balance. Source booking dates use completion dates; the CSV's
local clocks are retained without assigning a timezone.

Each bank posting uses the net movement, the counter-posting uses the opposite
principal amount, and a nonzero fee creates a separate `expenses:bank-fees` posting.
These three postings balance exactly. This fee policy applies to Revolut's observed
CSV form; Wise fees remain metadata pending evidence for their own accounting.
Identity uses account, currency, clocks, type, description, amount and fee plus an
occurrence ordinal. Running balances stay document observations. As with Bunq,
immutable account/day inventories reject changed or partial-day overlapping exports
rather than guessing identities. Exact reimports and overlapping complete days
reuse movements, including identical repeated payments.

## Review transfers between your accounts

List candidate pairs with both movements, fees, dates, currencies and descriptions:

```sh
fin transfers candidates --json > personal/reports/transfer-candidates.json
fin transfers candidates --all
```

Candidates require distinct source accounts, equal opposite principal amounts in
the same currency, and booking dates within seven calendar days. Multiple candidates
remain visible; uniqueness is only an amount/date observation. Confirm only after
checking that both accounts belong to you and that the movements describe the same
transfer. Imported records or a familiar counterparty name alone do not prove that.

Write explicit decisions to a private JSON file, using actual event IDs from the
candidate report. These IDs are illustrative:

```json
[
  {"first_event_id": 1, "second_event_id": 2,
   "decision": "confirm", "reason": "Checked both owned accounts and reference"}
]
```

```sh
fin transfers review personal/reviews/transfers.json
fin bank project
fin journal export > personal/bank.journal
fin transfers history
hledger -f personal/bank.journal --strict check
hledger -f personal/bank.journal register assets:transfers
```

Confirmation assigns both principal counter-postings to a shared
`assets:transfers:pair-ID-ID` account. They cancel once both dates are included,
while an outstanding leg remains visible between booking dates. Bank amounts,
explicit Revolut fees and original evidence remain intact. No new movement or
extra accounting entry is created. Journal tags record the pair and review reason.

Each movement can belong to only one confirmed pair. A substantive expense/income
category must first be reset to `equity:unclassified`; confirmation never overwrites
it silently. Confirmed legs cannot be categorized until their transfer is rejected.
Use `"decision": "reject"` to hide a proposed pair or undo a confirmation. Undo
restores each leg's unclassified counter-account and retains the full audit history.
Identical decisions are idempotent, and batches commit atomically after verifying
existing projections. Account remapping, sync and reimports preserve decisions.
Default candidate output hides reviewed pairs and movements already confirmed
elsewhere; `--all` includes them for inspection. FX, splits and delayed transfers
outside the seven-day window need a future explicit policy.

## Transfer evidence and assignment preview

```sh
fin transfers preview --json > personal/reports/transfer-preview.json
fin transfers candidates --all
```

Structured counterparty IBAN evidence currently comes from Bunq. It is compared
with imported account IBANs and registered identities. A counterparty IBAN belonging
to another known account, or differing from the proposed counterpart's IBAN, rules
out that pair. Such contradictions are excluded from default candidates, retained
with reasons under `--all`, and block confirmation. The preview lists excluded
pairs separately. Missing identity data is unknown evidence, not a contradiction.

The versioned `transfer-evidence-v1` score is an explainable heuristic, not a
calibrated probability or percentage. Its additive points are:

| Evidence | Points |
| --- | ---: |
| Exact opposite principal and currency | 30 |
| Same booking day | 35 |
| One day apart | 20 |
| Two or more days apart | max(0, 16 − 3 × days) |
| Matching structured counterparty/account IBAN | 80 |
| No competing compatible counterpart | 10 |
| Explicit mention of the other bank | 8 |
| Shared explicit reference token in descriptions | 50 |

Same-day booking has strong weight; an IBAN contradiction always takes precedence.
No ownership is inferred from a name. Candidate generation still requires exact
principal/currency and the seven-day booking window. Estimates are conditional on
the imported records, available identities and this policy; missing records can
change the conclusion.

The preview solves maximum-weight one-to-one matching for each connected candidate
component, allowing movements to remain unmatched. `supported` means the edge is
in every highest-scoring assignment; `alternative` means equally good assignments
can choose different counterparts; `not_preferred` has a lower best possible total.
`exclusion_margin` is how much the best total falls if a pair is forbidden, and
`inclusion_penalty` is the loss if the pair is required. Equal optima remain
ambiguous, without tie-breaking by row order. Components larger than 18 movements
are explicitly `not_evaluated` and yield no proposed confirmations.

Supported pairs score `high` when score ≥65 and exclusion margin ≥15, with same-day,
IBAN or shared-reference support; other supported pairs are `medium`. Alternatives
and unevaluated pairs remain `ambiguous`. The proposed pairs are a preview requiring
review, never automatic confirmations. Use the existing `transfers review` workflow
after checking identities and evidence.

## Optional CSV enrichment

OFX is sufficient to import transactions. For BoursoBank's fuller data, put both
OFX and CSV exports for the same accounts and period in the inbox. Either may
arrive first; repeat sync when more evidence arrives. The current reader supports
the observed UTF-8 semicolon CSV layout, including quoted/multiline fields and an
optional BOM. The two columns named Solde are interpreted by their fixed positions:
column 7 is the exact French-decimal movement amount, and column 11 is a separate
reported balance using dot decimals. A different layout is not guessed.

CSV rows are immutable observations keyed by archived document and logical data
row number. They preserve value dates, original and suggested descriptions, bank
categories and parent categories, comments, account labels, reported balances, and
raw pointage. The exact input file stays archived. No CSV row creates a source
movement or posting.

Automatic linking requires an exact institution, source account ID, booking date,
amount, and whitespace-normalized original description match to OFX. It does not
use suggested labels as identity. Multiple OFX candidates, multiple rows claiming
one movement within one CSV document, or conflicting nonempty metadata across
exports require review. Account masks are not guessed or stripped. Currency comes
from the matched OFX movement; CSV does not supply a currency or perform FX
conversion. CSV-only observations wait for OFX.

~~~sh
# Recompute automatically during sync, or explicitly after other imports:
dune exec fin -- bank match
# Inspect all observations or just those still needing review:
dune exec fin -- bank companions --institution boursobank
dune exec fin -- bank companions --institution boursobank --review
~~~

Matching states and their changes are saved in SQLite. Overlapping CSV files can
observe the same movement without creating it again. Later OFX evidence can make
an earlier automatic match ambiguous; the rematch is audited, and uncertain
metadata is removed from the generated journal. This is a review report, not yet a
manual match-approval interface.

For uncontested matches, the journal includes CSV value dates as secondary dates,
bank category/parent-category/suggested-description tags, and CSV row IDs. Commas,
colons, semicolons, and control characters in source labels are rendered safely;
original text remains in SQLite. These tags are source suggestions, not accepted
expense or income classifications. Core accounting postings and booking dates are
unchanged, and CSV pointage does not mark entries cleared.

~~~sh
# Transactions with uniquely matched bank category metadata:
hledger --no-conf -f personal/bank.journal register assets:bank tag:bank-category
# Secondary dates use CSV value dates where available:
hledger --no-conf -f personal/bank.journal register assets:bank --date2
~~~

The journal header reports matched-event and review-row counts. Entries without a
matched CSV value date retain no secondary date; hledger falls back to the booking
date for those entries. Do not treat a partial match as complete value-date coverage.


## Library

```ocaml
open Base
open Finances

let collateral_transfer =
  let open Or_error.Let_syntax in
  let amount = Quantity.of_string "1000.00000001" in
  let%bind incoming =
    Accounting.posting ~account:"assets:paradex:collateral" ~commodity:"USDC" amount
  in
  let%bind outgoing =
    Accounting.posting ~account:"assets:wallet" ~commodity:"USDC"
      (Quantity.neg amount)
  in
  Accounting.entry ~date:(Date.of_string "2026-10-25")
    ~description:"Collateral transfer" [ incoming; outgoing ]
```

`Store.with_open` scopes the connection, initializes the current schema, and
returns `Or_error.t`. `Store.ingest` returns `Inserted id` or `Existing id`.
`Store.record_entry db ~event_id ~key entry` atomically persists a balanced entry
and all its postings. The key identifies one accounting projection of one source
event, for example `collateral:v1`. The same entry can be replayed without creating
duplicates; a different entry under that key is rejected.

Amounts use `Quantity.t`, an alias of `Decimal.t` from the `decimal` library,
and are stored as canonical plain decimal strings through `Quantity.to_string`
in SQLite `TEXT` columns. `Quantity` delegates arithmetic to decimal with local
operand-sized contexts and traps inexact results, preserving exact sums and
products independently of `Decimal.Context.default`. Dates use `Ptime.date`
through the small ISO/calendar-day helpers in `Date`. Base supplies general
utilities, and Bos handles high-level filesystem operations. Core, Core_unix,
Bigdecimal and `ppx_jane` are not dependencies. Do monetary arithmetic in OCaml:
SQLite numeric casts and `SUM` on these strings would lose exactness.
Account names are nonempty colon-separated tokens, and commodities are nonempty
tokens; tokens allow letters, digits, underscores, and hyphens.

## Design

See [the architecture notes](docs/architecture.md) for storage ownership,
invariants, and the bank workflow milestones. The
[bank adapter interface proposal](docs/bank-adapters.md) describes a common module
signature, registry and shared import model for future external bank adapters.

## License

[ISC](LICENSE.md).
