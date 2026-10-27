# fin user manual

`fin` imports bank statements into a local SQLite database, lets you review
accounts and transactions, and produces an hledger journal. It currently supports
BoursoBank, Wise, Bunq and Revolut exports. It does not connect to banks or fetch
statements for you.

## Getting started

Run `fin` for a short welcome, or `fin --help` for the command reference. Neither
command creates a database. Every command supports `--help`, including nested
commands such as `fin bank sync --help`. The installed package also provides
`man fin` and subcommand man pages.

Start with:

```sh
fin init
```

`fin init` creates the database and shows the next step. It is the only command
that creates one: every other command, imports included, fails with "fin is not
initialized" until you have run it, so a mistyped `--database` path cannot start
a second, empty database. Repeating `fin init` preserves existing records and
account configuration. It does not create a config file or ask you to enter
accounts. Accounts are discovered from imported records. `fin db init` provides
quiet initialization for scripts, and `fin paths` shows where everything lives.

## Checking progress

```sh
fin status
fin accounts list
fin bank review
fin transfers preview
```

`fin status` shows discovered banks and accounts, per-account observed date
ranges, movement counts, unclassified movements, unreviewed transfer candidate
pairs, and the latest inbox import attempt (including failures). Candidate pairs can overlap: their count is not a
count of distinct transfers. The latest attempt is not a summary of all unresolved
import failures; use `fin bank history` to inspect earlier attempts.

Dates describe the earliest and latest observed records, not continuous or
complete statement coverage. Status does not calculate bank balances or spending.
Use hledger for financial reports after exporting the journal.

Status, account listing, transaction review and transfer candidates/preview show
readable text by default. Add `--json` for machine-readable output:

```sh
fin status --json
fin accounts list --json
fin bank review --json > bank-review.json
fin transfers candidates --json > transfer-candidates.json
fin transfers preview --json > transfer-preview.json
```

The existing JSON layouts of the four inspection commands are unchanged; scripts
that previously parsed their default output now need `--json`. Bank import
history is readable too, with `--json` retaining its existing JSON layout. Other
history and provider reconciliation commands still output JSON. Configuration export remains
JSON, and journal export remains hledger text.

## Storage and backups

The default database is `~/.local/share/fin/fin.sqlite3`; the optional account
configuration is `~/.config/fin/accounts.json`. `XDG_DATA_HOME` and
`XDG_CONFIG_HOME` override those base directories when set to absolute paths.
`fin paths` shows the resolved locations.

fin keeps two things, and you never need their paths for normal use:

- The database, `fin.sqlite3`: everything fin knows. It holds a copy of every
  statement file you imported, the movements and accounting entries, account
  settings and review history.
- The account configuration, `accounts.json`: optional and yours to edit.
  `fin accounts apply` reads it; fin writes it only when you run
  `fin accounts export`.

`bank.journal`, beside the database, is not state: every `fin bank sync` rewrites
it from the database for hledger. Statement folders can live anywhere you
choose; fin only reads them.

Use `--database FILE` (or `-d FILE`) on each database command to work with an
alternative database:

```sh
fin init --database ~/fin-experiment.sqlite3
fin accounts list --database ~/fin-experiment.sqlite3
```

Back up and delete fin's state without knowing where it lives:

```sh
fin backup ~/fin-backups
fin delete
```

`fin backup [DIR]` creates `fin-backup-YYYYMMDDTHHMMSSZ` in `DIR` (default: the
current directory), readable only by you, holding a consistent copy of the
database (safe even while `fin` is running) and `accounts.json` if you have one.
To restore, run `fin delete`, then copy those files back to the paths it prints.
Keep your original bank exports too.

`fin delete` lists everything it will delete, the database, its `bank.journal`
and the account configuration, and asks you to type `yes`. Your statement folders
are never touched. In a script, `fin delete --yes` skips the question; without a
terminal and without `--yes` it refuses. With `--database FILE` it deletes that
database and keeps the account configuration, which belongs to the default one.

This experimental release accepts schema version 12; it does not migrate older
experimental databases, and refuses them without changing the file. After
installing a release with a new schema, back up, delete and rebuild:

```sh
fin backup ~/fin-backups
fin delete
fin init
fin bank sync boursobank ~/Statements/boursobank
fin bank sync wise ~/Statements/wise
fin bank sync bunq ~/Statements/bunq
fin bank sync revolut ~/Statements/revolut --account-id main
```

If you keep an account configuration, copy `accounts.json` from the backup back to
`~/.config/fin/accounts.json` and run `fin accounts apply` after every bank is
synced: it refers to accounts from all banks. Without one, accounts keep the names
fin gives them.
Review decisions contain database-specific event IDs, so inspect new IDs before
reapplying decisions to a rebuilt database.

## Importing statements

Keep a separate inbox for each bank. Supply the formats below:

| Bank | Primary transaction source | Optional companion |
| --- | --- | --- |
| BoursoBank | OFX 2.x XML | BoursoBank CSV |
| Wise | CAMT.053 XML | Wise CSV |
| Bunq | MT940 | Bunq seven-column CSV |
| Revolut | Supported ten-column Current-account CSV | None |

Companion CSV files enrich the primary records; they do not create a second
transaction stream. Currencies must be ISO 4217 alphabetic codes such as `EUR`;
a file using any other code, or a lowercase one, is rejected and appears in
`fin bank history`. Arbitrary CSV layouts, Excel, PDF and QIF are not supported
imports. Export the supported format from your bank instead.

For example, put your BoursoBank exports in `~/Statements/boursobank`, then run:

```sh
fin bank sync boursobank ~/Statements/boursobank
fin accounts list
fin bank review
```

Use the corresponding institution for your other inboxes:

```sh
fin bank sync wise ~/Statements/wise
fin bank sync bunq ~/Statements/bunq
fin bank sync revolut ~/Statements/revolut --account-id main
```

The first argument is the bank that produced the files: one of `boursobank`,
`bunq`, `revolut` or `wise` (listed in `fin bank sync --help`). It is required,
since a file does not always say which bank it came from: OFX, CAMT and MT940 are
shared formats. The arguments after it are files or directories, any number of
them. Directories are searched recursively, skipping hidden and partially
downloaded files; a file you name is always processed, and fails the command if
no bank recognizes it. A file that clearly comes from another bank is rejected
and appears in `fin bank history`.

For Revolut, choose a stable account label; use separate labels and inboxes for
different accounts. Sync scans the folder recursively, archives the inputs,
imports movements, projects accounting entries and publishes a combined
`bank.journal` beside the selected database. `--journal FILE` chooses another
output location. Syncing each bank into the same database produces one combined
view of your finances.

You can add more exports and repeat the command. Unchanged documents and known
transactions are recognized. Conflicting evidence is reported for review.
Successful files remain imported even if another file fails. Inspect the summary
and history when sync reports an error:

```sh
fin bank history
fin bank statements
fin bank companions --review
```

`fin bank statements` compares each bank statement's stated balances with the
movements it lists. `fin bank companions` lists the enrichment exports (the
BoursoBank, Wise and Bunq CSV files) with their fields and match state;
`--review` keeps the rows that still need attention. Both accept
`--institution` to show one bank. `fin bank match` recomputes the matches;
`fin bank sync` already does this.

`fin bank history` is a log of file import attempts, not a list of transactions.
It shows the bank, timestamp, filename, outcome, new/existing movement counts and
any diagnostic. Attempts appear newest first; an unchanged file can appear more
than once because each attempt is recorded. Use `fin bank history --limit 10` for
the ten latest attempts, or `fin bank history --json` for machine-readable output.
Use `fin bank review --all` to browse individual movements instead.

See [Bank import and review reference](../README.md#folder-workflow) for
additional detail, or use `fin bank sync --help` for current options.

## Accounts and configuration

```sh
fin accounts list
fin accounts export -o "${XDG_CONFIG_HOME:-$HOME/.config}/fin/accounts.json"
```

Export after importing your statements, then edit that JSON file to set ledger
account names, register known IBANs and associate a credit card with its settlement
account. A credit card does not need its own IBAN. IBANs may use spaces or lower
case; `fin` checks them against the country's registered length and structure
and the check digits, and refuses invalid ones. Keep the exported account
identity fields unchanged so settings refer to the correct imported account.
Export writes a new snapshot to the chosen file; keep existing edits before
exporting over it.

Preview and apply your edits:

```sh
fin accounts apply --dry-run
fin accounts apply
fin accounts history
```

These commands use the default config path; provide a positional JSON filename
to apply another file. Configuration is applied explicitly. Later imports use
settings already saved in SQLite; editing JSON alone does not update the database.
Apply is atomic and audited. Omitted settings are kept; explicit `null` clears
supported optional settings. IBANs inferred from primary records cannot be
overridden or cleared. See the
[account configuration reference](../README.md#manage-bank-accounts) for the
JSON field definitions.

## Reviewing transactions and transfers

```sh
fin bank review
fin transfers candidates
fin transfers preview
```

Unclassified movements initially use `equity:unclassified`. Imported bank
categories are suggestions. Accepted categories and owned-account transfers
require explicit review files; a transfer preview does not confirm anything.
Transfer scores represent evidence, not calibrated probabilities.

After preparing review JSON using the examples in the
[README](../README.md), apply it with:

```sh
fin bank categories apply categories.json
fin transfers review transfers.json
fin bank project
fin bank categories history
fin transfers history
```

The filenames above are files you create, not files generated by the commands.
Confirm a transfer only when both movements belong to your accounts. Registered
IBANs improve the evidence used to rank candidates. Review changes are audited;
regenerate the journal after changing settings or decisions.

## Exporting and querying accounting

```sh
fin journal export > my-finances.journal
hledger -f my-finances.journal balance
hledger -f my-finances.journal balance expenses -M
hledger -f my-finances.journal register equity:unclassified
hledger -f my-finances.journal register expenses
```

The hledger CLI is optional and installed separately. The exported journal uses
exact decimal amounts and includes stored accounting entries from all imported
banks. It does not infer missing opening balances. Editing the journal does not
update SQLite; regenerate it from the database after making changes through
`fin`. `fin journal export` writes to standard output, whereas bank sync publishes
the journal file automatically.

Monthly expense reports become useful after categorizing movements. Account
totals represent the records you supplied; opening balances and complete coverage
are needed before treating them as actual bank balances. Financial aggregation
belongs to hledger; `fin` tracks import evidence, account identity and review state.

## Library and dependency changes

This build uses Base, decimal, Ptime and Bos, with no Core or Bigdecimal
dependency. Saved configuration remains compatible; databases use schema 12 and
are rebuilt from the original statements (see above).

OCaml callers use `Quantity.t` (a `Decimal.t` alias) and `Date.t` (a `Ptime.date`
alias). Use the `Quantity` functions for exact accounting sums and products:
ordinary decimal arithmetic follows its configured precision and rounding rules.
Dates remain calendar dates, and Wise booking days retain their supplied local
date. Bos handles high-level file operations; import file-identity checks and
private configuration permissions remain in place.

## Command reference

| Command | Purpose |
| --- | --- |
| `fin init` | Create the database, with first-import guidance |
| `fin status` | Import coverage and outstanding review work |
| `fin paths` | Resolved storage paths |
| `fin accounts --help` | Account listing, config and history |
| `fin bank --help` | Imports, reconciliation and category review |
| `fin transfers --help` | Candidates, preview and explicit decisions |
| `fin journal --help` | hledger journal export |
| `fin backup` | Copy the database and account configuration |
| `fin delete` | Delete all fin state, after confirmation |
| `fin db --help` | Lower-level database and source-event commands |

For detailed options, put `--help` after the specific command. Argument errors
exit with status 124; operation failures exit nonzero and print a diagnostic.
Check the per-file results after an import failure before retrying.
