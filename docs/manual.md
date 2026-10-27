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
account settings in SQLite. It does not ask you to enter accounts. Accounts are discovered from imported records. `fin db init` provides
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
history and provider reconciliation commands still output JSON. Account snapshot export remains
JSON, and journal export remains hledger text.

## Storage and backups

The default database is `~/.local/share/fin/fin.sqlite3`. `XDG_DATA_HOME`
overrides the base directory when set to an absolute path. `fin paths` reports
only `data_dir` and `database`. There is no fin configuration directory or
implicit account settings file.

SQLite holds everything fin knows: the original statement contents, movements,
accounting entries, account names, registered IBANs, card settlement links and
review history. `fin accounts edit` and sync's IBAN prompts update that database.
JSON account snapshots are optional exports to a filename you choose; they are
read only when you explicitly run `fin accounts import FILE`.

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
database (safe even while `fin` is running), including all account settings.
To restore, run `fin delete`, then copy the backup's `fin.sqlite3` to the database
path it prints.
Keep your original bank exports too.

`fin delete` lists the database, SQLite sidecars, generated journal and any pending
account edit draft, then asks you to type `yes`. Your statement folders and
exported account snapshots are never touched. In a script, `fin delete --yes`
skips the question; without a terminal and without `--yes` it refuses.
`--database FILE` selects which database and associated files to delete.

This experimental release accepts schema version 13; it does not migrate older
experimental databases, and refuses them without changing the file. After
installing a release with a new schema, save an account snapshot as well as a
full backup, then delete and rebuild:

```sh
fin accounts export -o ~/fin-accounts.json
fin backup ~/fin-backups
fin delete
fin init
fin bank sync boursobank ~/Statements/boursobank
fin bank sync wise ~/Statements/wise
fin bank sync bunq ~/Statements/bunq
fin bank sync revolut ~/Statements/revolut --account-id main
```

After syncing all banks, restore account settings with
`fin accounts import ~/fin-accounts.json`. The snapshot refers to already
discovered source accounts; it cannot create them. A full SQLite backup is for
restoring a database of the same schema version; an account snapshot is for
restoring settings into a rebuilt database.
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

ZIP exports can be passed directly, including Wise's CAMT.053 downloads:

```sh
fin bank sync wise ~/statements/wise
# Or name a ZIP file explicitly:
fin bank sync wise ~/statements/wise/statement.zip
```

Files ending in `.zip` (case insensitive) are opened in memory with Zipc; nothing
is extracted onto disk. Each member goes through the selected bank's usual
import and companion matching. History identifies it as `archive.zip/member.xml`.
The member's original bytes are archived in SQLite in the same way as an ordinary
input file. Replaying a ZIP, or later importing its extracted files, does not
create duplicate movements.

The default 64 MiB limit applies both to the ZIP file and to the total expanded
size, with a maximum of 1024 members per archive. `--max-file-mib` changes the
byte limit. Members are processed in name order; hidden and partial paths are
ignored. Unsafe paths, corrupt data, encrypted members, unsupported compression
and nested ZIPs are reported as read failures. Stored and deflate compression
are supported. Valid members commit independently if another member fails.
An explicitly named ZIP containing no recognized bank documents fails the
command, just like an unsupported file; unrelated members alongside valid
statements are reported as unsupported without failing the command.

For Revolut, choose a stable account label with `--account-id`, since its CSV does
not identify the account; use separate labels and folders for different Revolut
accounts. If you omit it on a terminal, sync asks for the label; scripts must
supply it. Labels accept letters, digits, underscores and hyphens. Use the same
label every time. Revolut exports one file per currency: sync them together under
one label, and each currency becomes its own account. Reverted transactions, which
never moved money, are skipped. Sync scans the folder recursively, archives the inputs,
imports movements, projects accounting entries and publishes a combined
`bank.journal` beside the selected database. `--journal FILE` chooses another
output location. Syncing each bank into the same database produces one combined
view of your finances.

On a terminal, sync asks for an IBAN when it first discovers an account whose
statements do not supply one. It asks once for a group of currency accounts with
the same bank and account identifier, and registers the answer on all missing
accounts in that group. Invalid IBANs are explained and can be entered again.
Enter skips; syncing those accounts again does not repeat the question. Use
`fin accounts edit` to fill it later. Cards never prompt, and scripts never prompt
for IBANs. Answers are audited in SQLite. Sync reads and updates account settings
only in the database. Exported snapshots are never read or rewritten during sync.

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

## Account settings and snapshots

```sh
fin accounts list
fin accounts edit
```

`fin accounts edit` opens your accounts in your editor (`$VISUAL`, then `$EDITOR`,
then `vi`) as JSON: set ledger account names, register IBANs and associate a credit
card with its settlement account, then save and quit. The edits are applied at
once, atomically and audited, in SQLite. No settings file is maintained.
If something is invalid, such as an IBAN with wrong check
digits, nothing is applied: in a terminal you can edit again, otherwise the draft
is kept beside the database as `fin.sqlite3.edit`, and the next `fin accounts edit`
continues from it. A successfully applied or unchanged draft is removed.

Statements from Wise and Bunq include the account's IBAN; for BoursoBank and
Revolut, supply it when sync asks or register it with `fin accounts edit`. A
credit card does not need its own IBAN. IBANs may use spaces or lower case; `fin` checks them against the country's registered
length and structure and the check digits, and refuses invalid ones. Keep the
account identity fields unchanged so settings refer to the correct imported
account.

To save or restore account settings separately from a full database backup:

```sh
fin accounts export -o ~/fin-accounts.json
fin accounts import ~/fin-accounts.json --dry-run
fin accounts import ~/fin-accounts.json
fin accounts history
```

Export writes a version 1 JSON snapshot to the chosen file, or standard output
when `-o` is omitted. Import requires an explicit filename and applies the settings
atomically with an audit trail; `--dry-run` previews without saving changes.
Editing or importing a snapshot never makes it an active configuration file.
Later bank imports use only the settings saved in SQLite.

Omitted accounts and settings are kept; explicit `null` clears supported optional
settings. IBANs inferred from primary records cannot be overridden or cleared.
See the [account settings reference](../README.md#manage-bank-accounts) for the
JSON field definitions.

Existing version 1 account JSON files still import with `fin accounts import FILE`.
The old `accounts apply` command, optional `accounts edit CONFIG` argument and
`bank sync --accounts` option have been removed. Old files are left untouched;
there is no default account JSON path.

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
dependency. Version 1 account snapshots remain compatible; databases use schema 13 and
are rebuilt from the original statements (see above).

OCaml callers use `Quantity.t` (a `Decimal.t` alias) and `Date.t` (a `Ptime.date`
alias). Use the `Quantity` functions for exact accounting sums and products:
ordinary decimal arithmetic follows its configured precision and rounding rules.
Dates remain calendar dates, and Wise booking days retain their supplied local
date. Bos handles high-level file operations; import file-identity checks and
private snapshot and draft permissions remain in place.

`Source_event.create` takes `~occurred_at:Ptime.t`, and its occurrence constructor
is `At` instead of `At_ms`. Instants in the OCaml API use `Ptime.t`, including
source occurrences, Wise booking times, statement instants and audit times. Calendar dates remain `Ptime.date`;
statement metadata uses `Document.Date` or `Document.Instant`. Offset-free Revolut
CSV clocks remain explicitly named local source text, not UTC instants.

SQLite stores instants as canonical UTC RFC 3339 text with 12 fractional digits.
`ORDER BY occurred_at` and ordinary comparisons/indexes preserve the complete
chronological order. SQLite's date functions are not used for fine-grained
instant comparisons. Bank audit times reflect the clock precision SQLite provides,
padded to the canonical representation, without claiming extra clock resolution.

`fin db events` now returns `occurred_at` (RFC 3339 or null) instead of
`occurred_at_ms`; `occurred_on` is unchanged. The low-level `fin db ingest --at-ms`
input remains an explicit Unix-millisecond boundary and converts exactly to Ptime.
Statement report timestamps are normalized to UTC; calendar-date boundaries remain
dates. Wise movement IDs, raw payloads, local booking dates and journal output
remain unchanged. Schema 12 databases must be rebuilt using the storage instructions
above; no automatic migration runs.

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
| `fin backup` | Copy the database, including account settings |
| `fin delete` | Delete all fin state, after confirmation |
| `fin db --help` | Lower-level database and source-event commands |

For detailed options, put `--help` after the specific command. Argument errors
exit with status 124; operation failures exit nonzero and print a diagnostic.
Check the per-file results after an import failure before retrying.
