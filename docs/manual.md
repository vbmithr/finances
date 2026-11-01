# fin user manual

`fin` imports bank statements into a local SQLite database, lets you review
accounts and transactions, and produces an hledger journal. It currently supports
BoursoBank, Wise, Bunq and Revolut exports. It does not connect to banks or fetch
statements for you.

For a practical first month of bookkeeping, follow
[Personal accounting with fin and hledger](personal-accounting.md). This manual
is the command and storage reference.

## Getting started

Building from a Git checkout requires the pinned Notty submodule. Run
`git submodule update --init --recursive` after cloning or updating the checkout.

Confirmation prompts use `[Y/n]`: press Enter or enter `y` to accept, or `n`
to decline. This also applies to deletion and restore prompts.

Run `fin` for a short welcome, or `fin --help` for the command reference. Neither
command creates a database. Every command supports `--help`, including nested
commands such as `fin import --help`. The installed package also provides
`man fin` and subcommand man pages.

`tx` and `txn` are shortcuts for `transaction`; `acc` is a shortcut for
`account`; `inst` for `institution`; and `cat` for the top-level `category`
catalog. Under `transaction`, `cat` also abbreviates `category` for transaction
review commands. For example, `fin cat list` lists the catalog, and
`fin tx cat review EVENT-ID` reviews a transaction. Help displays canonical names.

From a development checkout, `make run ARGS='tx list'` runs the current code
through Dune without installing. It uses the normal Fin database unless you pass
`--database PATH`. On Arch, `make install-fast` builds an incremental development
package and installs it with pacman; it skips tests. See the [README](../README.md)
for the clean, tested package workflow.

Start with:

```sh
fin init
```

`fin init` creates the database and shows the next step. Imports and review
commands fail with "fin is not initialized" until a database exists, so a
mistyped `--database` path cannot start a second, empty database. `fin restore`
can also create a database from a backup. Repeating `fin init` preserves existing
records and account settings in SQLite. It does not ask you to enter accounts. Accounts are discovered from imported records. `fin db init` provides
quiet initialization for scripts, and `fin paths` shows where everything lives.

## Checking progress

```sh
fin status
fin account list
fin transaction list
fin transaction review
fin transfer candidates
```

`fin status` shows discovered banks and accounts, per-account observed date
ranges, movement counts, unclassified movements, unreviewed transfer candidate
pairs, and the latest inbox import attempt (including failures). Candidate pairs can overlap: their count is not a
count of distinct transfers. The latest attempt is not a summary of all unresolved
import failures; use `fin institution history` to inspect earlier attempts.

Dates describe the earliest and latest observed records, not continuous or
complete statement coverage. Status does not calculate bank balances or spending.
Use hledger for financial reports after exporting the journal.

Status, account listing, transaction review and transfer candidates show
readable text by default. Add `--json` for machine-readable output:

```sh
fin status --json
fin account list --json
fin transaction review --json > bank-review.json
fin transfer candidates --json > transfer-candidates.json
```

Transfer candidates JSON includes ranking and evidence; `--all` also includes
reviewed and excluded pairs. Institution import
history is readable too, with `--json` retaining its existing JSON layout. Other
history and provider reconciliation commands still output JSON. Account snapshot export remains
JSON, and journal export remains hledger text.

JSON uses Jsont codecs. Monetary quantities remain exact decimal strings.
Integer IDs in the range −9007199254740991 to 9007199254740991 are numbers;
larger IDs are decimal strings. Preserve these strings when preparing category
or transfer review files: unsafe numeric IDs and fractional IDs are rejected.
Repeated object fields are rejected, including in account snapshots and review
batches. JSON whitespace and error wording may differ from earlier versions;
the field layouts and account patch semantics are unchanged.

## Storage and backups

The default database is `~/.local/share/fin/fin.sqlite3`. `XDG_DATA_HOME`
overrides the base directory when set to an absolute path. `fin paths` reports
only `data_dir` and `database`. There is no fin configuration directory or
implicit account settings file.

SQLite holds everything fin knows: the original statement contents, movements,
accounting entries, account names, registered IBANs, card settlement links and
review history. `fin account edit` and the import command's IBAN prompts update
that database.
JSON account snapshots are optional exports to a filename you choose; they are
read only when you explicitly run `fin account import FILE`.

`bank.journal`, beside the database, is not state: every `fin import` rewrites
it from the database for hledger. Statement folders can live anywhere you
choose; fin only reads them.

Use `--database FILE` (or `-d FILE`) on each database command to work with an
alternative database:

```sh
fin init --database ~/fin-experiment.sqlite3
fin account list --database ~/fin-experiment.sqlite3
```

Save a SQLite snapshot without knowing where the database lives:

```sh
fin backup ~/fin-backups
```

`fin backup [DIR]` creates `fin-backup-YYYYMMDDTHHMMSSZ` in `DIR` (default: the
current directory), creating missing parent directories with mode 0700. The new
backup folder is readable only by you and holds a consistent copy of the
database (safe even while `fin` is running), including all account settings.
The snapshot includes original statement contents, movements, accounting entries,
settings and review history. Keep your original bank exports too.

Restore the complete workspace from a backup folder or its SQLite file:

```sh
fin restore ~/fin-backups/fin-backup-20261028T120000Z
fin restore ~/fin-backups/fin-backup-20261028T120000Z/fin.sqlite3 \
  --database ~/restored-fin/fin.sqlite3
```

`fin restore BACKUP` opens the source read-only and checks its schema, integrity
and foreign keys before copying. It creates a missing destination privately,
without requiring `fin init`. If fin state already exists at the destination,
it asks you to type `yes`; scripts must pass `--yes` to replace it. SQLite
replaces the database in one transaction and manages any WAL sidecars. Invalid
backups and failed copies preserve the existing database. Source and destination
must be different files. The source backup is kept unchanged.

After success, restore removes the generated `bank.journal` and any pending
account edit draft, so neither can carry changes from the previous database.
Use `fin journal export` to render the restored entries. Original statement
files and exported settings snapshots are kept.

`fin delete` lists the database, SQLite sidecars, generated journal and any pending
account edit draft, then asks `Delete these files? [y/N]`; only `y` or `yes`
confirms, and Enter declines. Your statement folders and
exported account snapshots are never touched. In a script, `fin delete --yes`
skips the question; without a terminal and without `--yes` it refuses.
`--database FILE` selects which database and associated files to delete.

### Rebuilding after a schema change

This experimental release accepts schema version 31; it does not migrate older
databases and refuses them without changing the file. If upgrading from schema 21
with customized institution typing profiles, use the old executable to save each
profile with `fin institution typing show INSTITUTION > profile.json` before
rebuilding. Recreate those customizations as rules after reimport. Before upgrading to a new
schema, use the **old fin executable** to export account settings and save a
full backup. Save the transaction list and review histories as reference when
you have decisions to recreate:

```sh
fin account export -o ~/fin-accounts.json
fin cat export > ~/fin-categories-portable.json
fin backup ~/fin-backups
fin transaction list --json > ~/fin-transactions-before-rebuild.json
fin transaction category history > ~/fin-categories-before-rebuild.json
fin transfer history > ~/fin-transfers-before-rebuild.json
```

Run `fin cat export` with a version that supports it; older releases do not have
this command. The file contains personal source identifiers, category decisions,
and category rules, so keep it private alongside your backups.

Also save `fin settlement history`, `fin rule list --json`,
`fin tx type history --json` and `fin reversal history` when those commands are
available in the old executable.

Then run `fin delete` with the old executable.

Keep the old executable if you may need to inspect or restore the old backup;
the new executable cannot open or restore a database from a different schema.
After installing the new version, create a database and import your original
statements. Replace these example paths and account labels with your own:

```sh
fin init
fin import boursobank ~/Statements/boursobank
fin import wise ~/Statements/wise
fin import bunq ~/Statements/bunq
fin import revolut ~/Statements/revolut --account-id main
```

After importing all banks, restore account settings with
`fin account import ~/fin-accounts.json`. The snapshot refers to already
discovered source accounts; it cannot create them. A full SQLite backup is for
restoring a database of the same schema version; an account snapshot is for
restoring settings into a rebuilt database.
Generic CSV institutions must be added again and files reimported with their
original institution and account labels; configure their CSV profiles again.
Account snapshots do not include rules or review decisions. To restore an export
made by `fin cat export`, after reimporting statements run:

```sh
fin cat restore ~/fin-categories-portable.json
fin cat restore ~/fin-categories-portable.json --apply
```

The first command previews changes. The second previews again and asks for
confirmation; `--yes` skips that prompt. The export covers the category catalog,
personal category rules, and current category decisions (including explicit resets),
not transfer, settlement, reversal or type decisions, other rules, or audit history.
An identical current category is left unchanged. Different existing decisions or
rules, missing or ambiguous identities, and changed source evidence block the entire
restore. It identifies movements by institution, source account, kind and source ID,
then verifies occurrence and SHA-256 hashes of the original payload and source fields;
it does not reuse database event IDs or guess a match. If source identities or
encoding changed, keep the original export, remove the affected decisions from a
copy, preview that copy again, and recreate those decisions manually. Duplicate
indistinguishable source rows are also reported as ambiguous. The snapshot does
not replace a database backup. After restoring decisions, project
and export the journal again.

Other review histories must still be recreated using saved reference exports. Their
event IDs are database-specific; do not blindly reuse old IDs.

## Importing statements

Use `fin import INSTITUTION PATH...` to read exports. This replaces the former
`fin bank sync` command; update existing scripts to use `fin import`.
Run `fin institution list` to list built-in and user-added institutions.
For BoursoBank, `bourso` and `boursorama` are accepted aliases for `boursobank`;
all three import into the same institution.
Add an institution with `fin institution add NAME`; optionally supply a sample
CSV to configure its layout without importing it: `fin institution add NAME SAMPLE.csv`.
Remove an unused user-added institution and its saved layout with
`fin institution delete NAME` (or `--yes` for scripts). Delete its accounts first
if it has imported movements. Built-in institutions cannot be deleted.
Then import a file or directory with `fin import NAME PATH`. A user-added
institution can import OFX 2.x XML statements, including all accounts present in
the file, or configure a generic account or card CSV layout. For example:

```sh
fin institution add mybank
fin import mybank ~/Statements/mybank/statement.ofx
```

Generic OFX uses the same parser and source transaction IDs as the BoursoBank
OFX importer. It does not automatically use BoursoBank rules or CSV enrichment.
A generic primary CSV and OFX cannot be mixed for one institution: both create
movements and could duplicate them. Instead, configure a CSV as an OFX companion
as described below; companion rows enrich movements without creating new ones.

Generic CSVs do not identify their account, so fin asks for an account label for each file unless
`--account-id` is supplied. Only use the same label for files from the same account or card balance.
Without a companion profile, generic directory imports process primary CSV files
in name order or import OFX/ZIP files; mixing these sources is refused. With a
companion profile, a directory can contain both OFX and companion CSV files.
Earlier files remain imported if a later file fails. For a generic CSV, you can also use `fin import FILE.csv`;
fin prompts for its provider and account details when needed.

### Attach an institution definition

An institution definition is a versioned JSON file for a primary CSV, an OFX
source with a CSV companion, a CAMT.053 source with a clock-based CSV companion,
a running-balance CSV, or the supported MT940/CSV profile. Validate a definition and preview a sample before installing it:

```sh
fin institution definition show wise > institution.json
$EDITOR institution.json
fin institution definition check institution.json sample.xml
fin institution add mybank
fin institution definition set mybank institution.json
fin institution list
fin import mybank ~/Statements/mybank
```

Change the institution and any provider-specific settings in the template
before attaching it. The institution name in the file must match the selected
institution. When adapting a bundled template for another institution, give
its rules distinct names as well. `set` previews the rules to add and asks for
confirmation; `--yes` accepts the preview in scripts. Existing rules with the
same names are retained, including rules belonging to other institutions. For a primary CSV, `fin import` requires `--account-id`
(or prompts for the label on a terminal). `check` accepts `--account-id` for its
preview and defaults to `preview`; it prints up to five movements or companion
rows and does not open or change the database. Invalid samples report the first
parse error. `show` accepts all four built-in institutions and installed
definitions.

For example, a primary CSV definition can be written as:

```json
{
  "version": 1,
  "kind": "csv",
  "csv": {
    "institution": "mycard",
    "aliases": [],
    "profile": {
      "separator": ",",
      "header": ["Date", "Merchant", "Amount"],
      "date": "Date",
      "date_format": "iso",
      "amount_format": "dot",
      "description": "Merchant",
      "amount": "Amount",
      "currency": "EUR",
      "account_type": "prepaid-card",
      "invert_amount": true,
      "skip_column": null,
      "skip_value": null
    }
  }
}
```

This profile tracks one fixed currency; positive amounts in this example are
purchases and are inverted to negative movements. Definitions can include default source-interpretation rules; these become normal
editable rules in `fin rule list`. For example, add this top-level field to a
definition:

```json
"rules": [
  {"name": "mycard-purchase", "pattern": "^CARD ", "action": "type", "target": "card"}
]
```

Supported actions are `type`, `card-reference` and `withdrawal-reversal`. A `type`
rule may also name a `field` to test (see "What a type rule matches"), and its
`target` may contain `{value}`, which takes the text the rule matched, so that
`wise:{value}` keeps a provider code as a provider-specific type. Rules may specify
`priority`, `case_sensitive`, `account_type` and `match_type` where applicable. Use
`fin rule add` for personal accounting rules. The CAMT interpreter supports the
policies exposed by the Wise-shaped profile rather than every bank's CAMT format;
likewise, the MT940 profile currently covers the observed Bunq-style structure,
not every bank's MT940 variant.

The bundled Wise CSV companion maps its description role to `Merchant`, rather
than the constructed `Description` column. When `Merchant` is empty, the companion
description is empty. Fin chooses one effective description: the matched, nonempty
`Merchant` value when companion rows agree, otherwise the primary CAMT description.
Original source descriptions and CSV fields remain available as evidence.

Fin saves the validated definition in its database, so later edits or removal of
the JSON file do not change existing imports. To update it, run `definition set`
again. Rule-only changes can be saved after import without changing existing
transactions; existing rules with the same names are not overwritten. Changes to
the source interpretation require removing the institution's imported evidence
first, so previously stored records cannot be silently reinterpreted. Source identities, overlap and reconciliation follow the chosen importer:
primary CSV uses day inventories, OFX uses its source IDs, and CAMT uses the
configured reference and clock policy. Backups include installed definitions.

### Configure a CSV companion

If an institution supplies both OFX and a CSV with additional transaction
fields, configure the CSV as a companion before importing it. A companion does
not create transactions. For example, use the bundled BoursoBank layout as a
starting point for a user-added institution:

```sh
fin institution companion show boursobank > companion.json
$EDITOR companion.json
fin institution companion set mybank companion.json
fin import mybank ~/Statements/mybank
fin statement companions --institution mybank --review
```

Replace the template's `name`, `header`, separator and column positions with
those of your CSV, and set `currency` to the file's ISO currency, such as
`"EUR"`. Positions are **zero-based**. `account_id`, `booked_on`, `amount`
and `description` are required positions; optional metadata positions may be
`null`. Positions distinguish repeated header names. `date_format` is `iso`,
`day-first` or `month-first`; `amount_format` and `balance_format` are `dot`
or `comma`. The header must match the profile. Boolean settings control header
and cell trimming and blank-row skipping. Profile changes are previewed and
require confirmation; `--yes` accepts them for scripts.

Rows match only a unique OFX transaction in the same institution and account,
on booking date, exact signed amount, currency and whitespace-normalized
original description. Ambiguous rows and conflicting metadata remain visible
for review. CSV bank categories are retained as source metadata; they are not
automatic accounting classifications. Companion rows may arrive before OFX.

Each imported CSV retains the layout used to read it. Changing the profile
applies to later files and does not reinterpret existing observations. Reimporting the same file under a different profile is rejected, including
when its stored source context differs.
`fin institution companion clear mybank` removes an unused profile; once
companion rows exist, fin keeps the profile available to display and match them.
A user-added institution with primary generic CSV movements cannot be switched
to companion mode until those accounts are removed.

Keep a separate inbox for each bank. Supply the formats below:

| Bank | Primary transaction source | Optional companion |
| --- | --- | --- |
| BoursoBank | OFX 2.x XML | BoursoBank CSV |
| Wise | CAMT.053 XML | Wise CSV |
| Bunq | MT940 | Bunq seven-column CSV |
| Revolut | Supported ten-column Current-account CSV (its `Type` column sets the movement type) | None |

Companion CSV files enrich the primary records; they do not create a second
transaction stream. Currencies must be ISO 4217 alphabetic codes such as `EUR`;
a file using any other code, or a lowercase one, is rejected and appears in
`fin institution history`. Other CSV layouts can be configured as described below. Excel, PDF and QIF
are not supported imports.

For example, put your BoursoBank exports in `~/Statements/boursobank`, then run:

```sh
fin import boursobank ~/Statements/boursobank
fin account list
fin transaction review
```

Use the corresponding institution for your other inboxes:

```sh
fin import wise ~/Statements/wise
fin import bunq ~/Statements/bunq
fin import revolut ~/Statements/revolut --account-id main
```

For known bank formats, the first argument names the institution: `boursobank`,
`bunq`, `revolut` or `wise`. This is required for formats such as OFX, CAMT
and MT940 that may be shared by several banks. The arguments after it are files or directories, any number of
them. Directories are searched recursively, skipping hidden and partially
downloaded files; a file you name is always processed, and fails the command if
no bank recognizes it. A file that clearly comes from another bank is rejected
and appears in `fin institution history`.

ZIP exports can be passed directly, including Wise's CAMT.053 downloads:

```sh
fin import wise ~/statements/wise
# Or name a ZIP file explicitly:
fin import wise ~/statements/wise/statement.zip
```

Wise MT940 exports are not supported; use Wise CAMT.053 and CSV. A directory
may contain other formats: unsupported files are reported rather than identified
as another bank solely from their filename extension.

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
accounts. If you omit it on a terminal, import asks for the label; scripts must
supply it. Labels accept letters, digits, underscores and hyphens. Use the same
label every time. Revolut exports one file per currency: import them together under
one label, and each currency becomes its own account. Reverted transactions, which
never moved money, are skipped. Import scans the folder recursively, archives the inputs,
imports movements, projects accounting entries and publishes a combined
`bank.journal` beside the selected database. `--journal FILE` chooses another
output location. Importing each bank into the same database produces one combined
view of your finances.

On a terminal, import asks for an IBAN when it first discovers an account whose
statements do not supply one. It asks once for a group of currency accounts with
the same bank and account identifier, and registers the answer on all missing
accounts in that group. Invalid IBANs are explained and can be entered again.
Enter skips; importing those accounts again does not repeat the question. Use
`fin account edit` to fill it later. Cards never prompt, and scripts never prompt
for IBANs. Answers are audited in SQLite. Import reads and updates account settings
only in the database. Exported snapshots are never read or rewritten during import.

You can add more exports and repeat the command. Unchanged documents and known
transactions are recognized. Conflicting evidence is reported for review.
Successful files remain imported even if another file fails. Inspect the summary
and history when import reports an error:

```sh
fin institution history
fin statement list
fin statement companions --review
```

`fin statement list` compares each bank statement's stated balances with the
movements it lists. `fin statement companions` lists the enrichment exports (the
BoursoBank, Wise and Bunq CSV files) with their fields and match state;
`--review` keeps the rows that still need attention. Both accept
`--institution` to show one bank. `fin statement match` recomputes the matches;
`fin import` already does this.

`fin institution history` is a log of file import attempts, not a list of transactions.
It shows the bank, timestamp, filename, outcome, new/existing movement counts and
any diagnostic. Attempts appear newest first; an unchanged file can appear more
than once because each attempt is recorded. Use `fin institution history --limit 10` for
the ten latest attempts, or `fin institution history --json` for machine-readable output.
Use `fin transaction list` to browse individual movements instead.

See [Bank import and review reference](../README.md#folder-workflow) for
additional detail, or use `fin import --help` for current options.


### Configure another CSV

For a CSV that describes an account or card, initialize fin and run:

```sh
fin import statement.csv --provider my-provider --account-id main
```

The first import must run in a terminal. Fin detects a comma or semicolon
separator from the header when unambiguous, and asks if it cannot decide. It
then proposes date, description and account amount columns when it recognizes
them. Accept the proposal or choose columns yourself. For questions with
numbered choices, enter the number; empty and invalid choices are retried. It
also asks for an account type (current, savings, prepaid card or credit card)
and whether outgoing amounts are positive. ISO dates and dot-decimal amounts
are recognized directly; for other supported dates and comma-decimal amounts,
fin shows examples and asks which format to use. The account currency is fixed
for the file. If there is a summary row, specify its label;
fin skips it only when its date is empty. Review the preview and confirm to import.
The preview shows each CSV amount beside the signed account movement; other
CSV fields, such as local currency and exchange rate, retain their source signs.
Generic CSV directory imports review each file separately. Raw fields are retained
with the source evidence, including original currency amounts and exchange rates.
The mapped account amount, not the local amount, is posted to the ledger.

The provider name identifies the institution; fin prompts for a nonempty name
when omitted on a terminal and retries invalid answers. The layout is saved with the accepted document in the database
and reused for later files with the same header and provider. Supply the same `--account-id` for the same
account, and different labels for separate account or card balances. A known layout can be
imported without a terminal. Reimports are safe: repeated identical rows are
preserved as distinct occurrences, and a changed or partial overlapping day is
rejected for review. Layouts with the same header but different settings are
ambiguous and require separate handling. Fin does not infer transfers between
cards or from card top-ups.

For generic imports recorded under the former `csv` institution, delete the
old account and import its files under the provider name. See account deletion
below; reviews and account settings associated with the deleted account are lost.

Current and savings accounts default to `assets:bank:PROVIDER:TYPE:ACCOUNT`.
Fin can record an optional IBAN for them. A generic CSV profile still covers
one account and one fixed currency. Supported dates are ISO `YYYY-MM-DD`,
day-first `DD/MM/YYYY` or `DD-MM-YYYY`, and month-first `MM/DD/YYYY` or
`MM-DD-YYYY`. Amounts use a dot decimal mark without grouping, or a comma
with optional space grouping. Separate debit/credit columns, multiple currencies
and other date or number formats are not yet supported.

Prepaid card balances are assets. Select `prepaid-card` for a separately funded
card and `credit-card` for a card paid later. New generic prepaid cards default
to `assets:cards:PROVIDER:ACCOUNT` and credit cards to
`liabilities:cards:PROVIDER:ACCOUNT`. Account settings can map either to another
asset or liability account.

## Delete an account

`fin account list` shows a short ID alongside each account. Use that ID to
inspect what deletion will remove, then delete it (replace `ACCOUNT_ID` below
with the value shown):

```sh
fin account delete ACCOUNT_ID --dry-run
fin account delete ACCOUNT_ID
```

Deletion removes the account's transactions, projected entries, classifications,
settings, exclusive source documents and their import history. Statement files
outside fin remain untouched. The command asks for confirmation; scripts can use
`--yes`. The ID is derived from the provider and source account identity, so it
is stable across runs and reimports of the same identity. Fin refuses deletion if
two accounts have the same short ID. Deletion is atomic and refuses shared documents, companion
references, transfer decisions and settlement references from another account.
The default generated `bank.journal`, if present, is refreshed; regenerate any
custom journal separately. Make a backup first if the removed records may be needed.

## Account settings and snapshots

```sh
fin account list
fin account edit
```

`fin account edit` opens your accounts in your editor (`$VISUAL`, then `$EDITOR`,
then `vi`) as JSON: set ledger account names, register IBANs and associate a credit
card with its settlement account, then save and quit. The edits are applied at
once, atomically and audited, in SQLite. No settings file is maintained.
If something is invalid, such as an IBAN with wrong check
digits, nothing is applied: in a terminal you can edit again, otherwise the draft
is kept beside the database as `fin.sqlite3.edit`, and the next `fin account edit`
continues from it. A successfully applied or unchanged draft is removed.

Statements from Wise and Bunq include the account's IBAN; for BoursoBank and
Revolut, supply it when import asks or register it with `fin account edit`. A
credit card does not need its own IBAN. IBANs may use spaces or lower case; `fin` checks them against the country's registered
length and structure and the check digits, and refuses invalid ones. Keep the
account identity fields unchanged so settings refer to the correct imported
account.

To save or restore account settings separately from a full database backup:

```sh
fin account export -o ~/fin-accounts.json
fin account import ~/fin-accounts.json --dry-run
fin account import ~/fin-accounts.json
fin account history
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

Existing version 1 account JSON files still import with `fin account import FILE`.
The old `accounts apply` command, optional `accounts edit CONFIG` argument and
`bank sync --accounts` option have been removed. Old files are left untouched;
there is no default account JSON path.

## Reviewing transactions and transfers

For a credit card paid from a checking account, start with:

```sh
fin settlement candidates
fin settlement review
```

For the experimental full-screen review, run `fin settlement review --tui`.
Use the up and down arrows (or `j` and `k`) to choose a suggestion, Enter to
review it, and `y` to apply. Press `n` or Escape to return to the list; `q` or
Ctrl-C exits without applying a pending proposal. Resize the terminal if it is
too small to show all proposed changes. The full-screen mode requires a terminal
and cannot be combined with `--yes`.

For BoursoBank, fin recognizes its deferred-card debit description and proposes
a match only when the complete masked card identifier uniquely matches a card
at the same institution and bank, in the same currency. It excludes suggestions
when an opposite payment on that card is already present within seven days, or
the card is already linked to a different checking account. These are suggestions,
not automatic accounting decisions. Select a transaction by its event ID. If
needed, fin proposes mapping the card to a liability account and linking it to
the checking account alongside the settlement. One final confirmation applies
the entire proposal atomically; declining or interrupting before confirmation
leaves both the setup and settlement unchanged. Only the selected settlement is
recorded; similar suggestions remain for review.

For other institutions, or where no suggestion is available, use
`fin account list` to find the IDs and link the accounts explicitly:

```sh
fin account link-card CARD-ID CHECKING-ID
```

This previews the relationship and a liability account when needed. It requires
confirmation (`[Y/n]`, with Enter accepting); `--yes` accepts the proposal in a script. Alternatively, `fin
account edit` lets you map the card to a `liabilities:...` account and set its
`settlement_account` to the checking account. For example, the following is a
fictional pair of entries
in the editor's `accounts` array:

```json
{
  "institution": "boursobank",
  "bank_id": "example-bank",
  "account_id": "checking-example",
  "account_type": "CHECKINGS",
  "ledger_account": "assets:bank:checking"
}
```

```json
{
  "institution": "boursobank",
  "bank_id": "example-bank",
  "account_id": "card-example",
  "account_type": "CREDITCRD",
  "ledger_account": "liabilities:cards:boursobank",
  "settlement_account": {
    "institution": "boursobank",
    "bank_id": "example-bank",
    "account_id": "checking-example",
    "account_type": "CHECKINGS"
  }
}
```

Use your existing entries and copy the checking account's four identity fields
exactly into the card's `settlement_account`; keep the other accounts and source
identity fields unchanged. Save and exit the editor. Categorize individual card
purchases as expenses, and treat the monthly repayment as settlement, so it is
not counted as spending a second time.

Find the bank debit's event ID with `fin transaction list`, then replace `123`
with that ID:

```sh
fin settlement review 123
fin settlement history
fin settlement undo 123
```

Review shows the bank movement, linked card and proposed liability repayment;
type `yes` to confirm. If several cards link to the same checking account, use
`--card ID` with the ID from `fin account list`. The movement must be an
unclassified, fee-free debit in the card's currency. Fin does not check that the
debit equals a particular set of purchases; check the statement yourself. If the
card statement already contains the other side of the payment, use transfer
review instead. A confirmed settlement cannot also be categorized or confirmed
as a transfer. Undo it before correcting that decision, changing the card's
ledger account or deleting either account. Review and undo accept `--yes` for
scripts; automatic card setup still requires a terminal. `fin settlement history`
reports both actions as JSON. Export a fresh
journal after review changes, for example `fin journal export > my-finances.journal`.

```sh
fin transaction review
fin transfer candidates
```

Unclassified movements initially use `equity:unclassified`. Imported bank
categories are suggestions. To classify or re-review a movement interactively, find its event
ID in `fin transaction list`, then replace `123` below with that ID:

```sh
fin transaction category review 123
```

To assign or change categories without the interactive batch review, put the
account first, followed by one or more transaction event IDs:

```sh
fin tx cat set expenses:food:restaurants 123 456
```

Use `equity:unclassified` as the account to return the movement to review. The
command needs no reason or confirmation and applies all IDs atomically: if one
cannot be changed, none are changed. Confirmed transfers, settlements and
reversals must be undone first. Regenerate the journal after changing a decision.

Fin maintains a personal catalog of income and expense account names. Start with
`fin cat init` to review suggested categories, select all or numbered entries,
and confirm. Repeating the command skips existing names. Add your own with
`fin cat add expenses:food:groceries`; `fin cat list` lists the catalog,
including unused entries, and `--json` outputs an array. Remove an unused name
with `fin cat delete expenses:food:groceries`. Change any transaction assignments
or rules using that exact category first. Deletion retains historical decisions
and does not remove subcategories. Category decisions also add their account to
the catalog. The initializer requires a terminal.

Choose a catalog category by number or enter an `expenses:...` or `income:...`
account. Fin suggests unclassified exact matches followed by unclassified fuzzy
matches above the similarity cutoff, across institutions. Results are numbered and marked `exact` or `fuzzy` (with a
similarity score). Enter comma-separated numbers and ascending ranges such as
`1-5,7,8-19` to select or deselect each row; selected rows are marked `x`.
Overlapping ranges are rejected. Suggestions exclude transactions with existing
categories. Use `search` to find and explicitly select transactions in other
categories to change them; their existing categories are displayed. Transactions
already in the chosen category are excluded from search too. Confirmed
transfers, settlements and reversals cannot be selected.

Enter `search` to search effective descriptions by a case-insensitive substring.
Search can be repeated and selections carry over; `fuzzy` returns to the original
exact and fuzzy results. `all` selects all selectable rows in the current list,
including fuzzy and previously categorized matches, and proceeds. It retains prior selections. `none`
clears the selection and proceeds; `done` proceeds with the current selection.
Fuzzy scores are not probabilities. Matching uses character-trigram overlap,
folds ASCII case and treats punctuation as spaces; it is not accent-insensitive,
and short names under four bytes have no fuzzy suggestions.

The seed is included if its category will change. If it already has the chosen
category, it is left untouched. Check the proposed batch and confirm to apply it;
`q`, EOF, or declining confirmation cancels. This command requires a terminal.
It checks that every selected category is unchanged since review before saving,
so a concurrent decision cannot be overwritten. The separate `search` command uses substring matching; it does not search every
raw source column.


Confirmed same-currency transfers export as one journal transaction with direct
postings to both bank accounts, retaining both source event IDs and any fee postings.
Fin uses the earlier booking date as the transaction date and attaches the later
booking date to its bank and fee postings. Between those dates, date-filtered reports
reflect the two bank balances without a separate in-transit asset; the books may
therefore appear temporarily unbalanced by date. Fin keeps both source movements
and review history in SQLite. Regenerate the journal after confirming or undoing.

`fin transfer candidates` shows possible pairs, their evidence and ranking;
scores are not calibrated probabilities. Run `fin transfer review` to inspect
each pair and choose `c` to confirm, `r` to reject, or `s` to skip that pair. `a` stages
all supported pairs and finishes selection, leaving other candidates for later review;
`d` finishes selection. Fin shows the pending pairs and asks once before applying
the batch. Confirm only if both accounts are yours and the movements represent
the same transfer. `q`, EOF, Ctrl-C, or declining the final confirmation leaves
the batch unchanged. A conflicting or stale decision refuses the whole batch.
For scripted batches, supply a JSON review file as shown below.

For a failed ATM withdrawal followed by its full refund, review a reversal pair:

```sh
fin reversal candidates
fin reversal review ORIGINAL-ID REVERSAL-ID
fin reversal history
fin reversal undo ORIGINAL-ID REVERSAL-ID
```

Candidates require two unreviewed `withdrawal` movements on the same account,
with a negative original and a positive movement whose source label indicates a
reversal, equal opposite amounts in the same currency, no fees,
and a reversal booked no earlier than the original. Matching amounts are only
evidence: check the descriptions and both event IDs before confirming. Confirmation
puts both counter-postings in a shared `assets:reversals:pair-...` clearing
account. The balance clears when both movements are included. Undo restores both
to review. `--yes` skips confirmation on `review` and `undo`. This workflow covers
full ATM withdrawal reversals; partial reversals and fees need separate work.
BoursoBank `ANNUL RETRAIT DAB` movements have type `withdrawal` and recorded
reversal evidence. Types follow the rules at once; refresh source evidence with `fin tx refresh-evidence --institution boursobank`.

To undo a confirmed transfer, use its two event IDs (shown by
`fin transfer candidates --all` or `fin transfer history`):

```sh
fin transfer undo FIRST-EVENT-ID SECOND-EVENT-ID
```

The command previews both movements and asks for confirmation; `--yes` skips
the prompt. Undo returns both movements to unclassified, records a rejection
in the audit history, and hides that pair from default candidates. You can
then classify either movement separately. Regenerate the journal afterward.

After preparing review JSON using the examples in the
[README](../README.md), apply it with:

```sh
fin transaction category apply categories.json
fin transfer review transfers.json
fin journal project
fin transaction category history
fin transfer history
```

After categorizing a transaction, preview similar unclassified movements with
its event ID (replace `123` with a reviewed transaction ID):

```sh
fin transaction category similar 123
fin transaction category similar 123 --json
```

The preview searches across institutions, comparing their effective descriptions.
Matching
ignores ASCII case and whitespace; it is not fuzzy matching. The preview shows
unclassified matches and flags matches categorized differently. It never applies
a category; use interactive review or an explicit decisions file to apply changes.
Confirmed transfers are excluded. Check each match: the same name does not always
mean the same purchase purpose.

The filenames above are files you create, not files generated by the commands.
Confirm a transfer only when both movements belong to your accounts. Registered
IBANs improve the evidence used to rank candidates. Review changes are audited;
regenerate the journal after changing settings or decisions.

## Transaction status

`fin tx status` summarizes imported bank movements by accounting decision and
shows the percentage classified. Classified means a category, confirmed transfer,
settlement or reversal; categorized counts income and expense categories alone.
Both percentages use all imported bank movements as the denominator. An empty
database shows `n/a` rather than a percentage. `fin tx status --json` emits counts
and numeric percentages, with `null` percentages when there are no movements.
The category breakdown lists exact assigned accounts without rolling up parent
categories. The movement-type breakdown includes manual type overrides and
`unknown` for movements without a type. Both show counts, not amounts. Transfers
and reversals are counted per movement, not per pair. The top-level
`fin status` separately summarizes account coverage and import progress.

## List transactions

`fin transaction list` lists all imported movements, including reviewed ones,
newest first, with the institution and account ID for each movement. The
displayed event IDs can be used in review decisions; account IDs come from
`fin account list`.

The table shows the movement type and the counter-account used in the exported
journal. `-` means the movement remains unclassified (`equity:unclassified`).
Confirmed transfers show the other bank account; reversals show their clearing
account. On a terminal, the table opens
in `$PAGER` (default `less -FRX`); an empty `PAGER` disables paging. Redirected
output and `--json` go directly to stdout.

```sh
fin transaction list
fin transaction list --from 2026-01-01 --to 2026-01-31
fin transaction list --search coffee --limit 20
fin tx list --type interest
fin tx list --type card-settlement --decision settlement
fin tx list --type unknown --decision unclassified
fin transaction list --order asc
fin transaction list --json
```

Dates are inclusive booking dates. Use `--account ID` for one account or
`--institution NAME` for a provider; `--search` matches the effective description
using ASCII case-insensitive comparison. `--order asc`
lists oldest first; `--order desc` is the default.
`--type` matches the displayed movement type, including provider-specific values;
`--type unknown` selects movements without a type. `--decision` filters by the
kind of accounting decision: `unclassified`, `category`, `transfer`, `settlement`
or `reversal`. It does not filter by counter-account. Filters combine, and
are applied before sorting and `--limit`.
Sorting uses the source event's date or timestamp in UTC; date-only events sort
before timed events on the same date, with event IDs breaking ties. Sorting
precedes `--limit`, which must be positive. A matched Wise CSV can provide the
effective description; missing or conflicting values fall back to the primary
description. JSON contains one effective `description`, `movement_type`, and
`detail_sources` companion row IDs, which can be inspected with
`fin statement companions`. Provider-specific types are prefixed with the provider
name. A reported transfer type does not confirm a transfer between your accounts.

To list types and correct an individual movement's type:

```sh
fin tx type list
fin tx type show EVENT-ID
fin tx type set EVENT-ID fee
fin tx type set EVENT-ID bonus
fin tx type history
fin tx type reset EVENT-ID
```

`type list` shows common types and provider-specific types observed in imported
data; use one of those values with `type set`. `unknown` is a list filter, not a
type. Manual types are overrides: they do not change the source movement or its
accounting classification, survive reimports and rule changes, and take
precedence over derived types. `type show` and transaction-list
JSON expose the imported type and any override. `type reset` removes the override
and returns to the imported type. The append-only history records changes;
`fin tx type history --json` is available for scripts.
On import, fin determines BoursoBank movement types from explicit leading OFX
description words: `PAIEMENT CARTE` and `AVOIR` (card refunds), `CARTE` on a card account, `VIR` or
`VIREMENT`, `PRLV` or `PRELEVEMENT`, `RETRAIT` and `ANNUL RETRAIT DAB`. Other descriptions remain
untyped, except `INTERETS`, `INTÉRÊTS`, `*INTER.BRUTS` and `*INT DEB`
interest descriptions; `REM CHQ` cheque deposits; `CHQ.` issued cheques;
card commissions, fees and fee refunds (`*CION CB`, `*FRAIS CB`,
`*RETROC *FRAIS CB`); and `Relevé différé Carte …` card settlements (including
the malformed `Relev{ diff{r{ Carte …` form found in some exports). These
map to `interest`, `deposit`, `cheque`, `fee` and `card-settlement`, respectively.
Fee refunds retain their positive amount. The original description remains available for
inspection. An interest type is source information, not an income classification.
The settlement type likewise describes the statement; confirming a settlement
is a separate accounting decision.

### Automatic rules at import

BoursoBank's typing defaults are ordinary automatic rules. Inspect them and
any user rules with `fin rule list` (or `--json`). Add your own rules and
choose whether they apply automatically to new imports:

```sh
fin rule add cafes --institution boursobank --description 'CAFE' \
  --set-type card --category expenses:food --automatic
fin rule mode cafes disabled
fin rule mode cafes review
fin rule mode cafes automatic
```

Rules are ordered by priority (lower first), then name. Set the order on
creation with `--priority`; the first matching type action and first matching
category action win independently. Automatic typing is determined before
automatic categories. Automatic rules match the main description, or the field the
rule names with `--field`, and can constrain the source account type with
`--account-type`. Rules with a type action match `--match-type` against the type
the parser reported (usually none); category-only rules match it against the
resulting imported type.
Manual overrides and reviewed accounting decisions remain authoritative.
Settlement rules require review and cannot run automatically.

Replace a rule with `fin rule add NAME ... --replace`, supplying its complete
new conditions, action, mode and priority. Adding, changing, disabling or deleting a
type rule retypes existing transactions at once, and the type history records the
rule. Category rules affect future imports only. Use `fin rule preview NAME` and `fin rule apply
NAME` to review and apply decisions to existing transactions. Reimporting the
same source does not rerun automatic rules. The database and backups contain
rules; the bundled defaults are installed for a fresh database from each bundled
institution's definition, one file per institution:
[BoursoBank](../institutions/boursobank.json), [Wise](../institutions/wise.json),
[Bunq](../institutions/bunq.json) and [Revolut](../institutions/revolut.json).
`fin institution definition show INSTITUTION` prints the same file.
`fin institution show INSTITUTION` summarizes one in plain text: its kind, what fin
reads from each source column by role, the columns it does not read, what rules
can match (type rules test the main description, or any field a rule names, such as
a column of the CSV), its default rules and, when a database exists, how many
movements are imported by type. It also works for installed definitions.

There is one kind of type rule. A bank's own type code is just a column: Wise's
definition maps its `Transaction Details Type` column and, ahead of it, the prefix
of the `TransferWise ID` column (`FEE-` to `fee`, `BALANCE_CASHBACK-` to `bonus`);
Revolut's maps its `Type` column (Card Payment and Card Refund to `card`, Deposit to
`deposit`, Transfer to `transfer`, ATM to `withdrawal`, Charge to `fee`). Both end
with a catch-all that keeps any other code as a provider-specific type
(`wise:{value}`, `revolut:{value}`). The first rule that matches, in priority order,
decides. Types are derived from the rules, so editing a rule retypes imported movements at
once. The bundled rules are
installed into a new database; an existing database needs them added with `fin rule
add`, or rebuilding.

#### See everything stored about a transaction

`fin tx show EVENT-ID` prints what fin knows about one transaction in plain text: its
type, category and confirmed transfer; the source event (institution, account,
kind, source identifier, occurrence and retrieval time); where it was imported from
(the stored document with its hash and import time, every file it was read from,
and the statement with its period and balances); the source's own fields as stored
(the payload, indented); and each matched companion row with every column under its
header name, the match state and policy, the display name and tags it gave, and the
value the type rules matched. It changes nothing. Companion rows include the
personal columns of the file, such as a cardholder's name, so be careful when
pasting the output. `fin rule explain EVENT-ID` shows how the type was decided.

#### What a type rule matches

A type rule tests one *field* of the transaction. Without `--field` it tests the
`description`, which is the main text the source's adapter declares: the OFX name
for BoursoBank, the payment description for Bunq (not the counterparty's name,
which comes first in the list), and the single description for Wise and Revolut.
`--field` chooses another:

- `text`: any of the transaction's descriptions;
- a named text the source carries: `name` and `memo` (OFX), `name` and
  `counterparty` (Bunq), `reference` (the Wise CAMT bank transaction code, such as
  `FEE-CARD-…`);
- any column of a CSV primary, by its header exactly as it appears in the file:
  Revolut's `Type`, `Product`, `State` or `Balance`, or a column of a bank you added
  yourself, such as `Merchant ID`. Fin stores every column of each row beside the
  transaction, and `fin tx show` lists them under "Source columns".

A rule matches when any value under the field matches. A field that a transaction
does not have matches nothing, and `fin rule explain` says so. A rule that names a
field must be automatic (`--automatic`), because reviewed rules work from stored
transactions, which do not keep fields. An automatic rule applies to what is already
imported as soon as it is saved; use `fin rule test` to preview it.

```sh
fin rule test wise-ref-fee --institution wise --field reference \
  --description '^FEE-' --set-type fee
fin rule add wise-ref-fee --institution wise --field reference \
  --description '^FEE-' --set-type fee --automatic
```

On a Wise CAMT statement alone, with no CSV, that types every fee line. Fields of
the tag-based formats are limited to the named ones above, because only those are
exposed so far. `fin rule gaps` groups by the main description, like the rules.

#### Explain a movement's type

`fin rule explain EVENT-ID` (the event ID is in `fin tx list`) traces how one
movement got its type, in the order fin applies the sources: a manual override, then the
type rules over the transaction's fields, matched companion columns counting as
fields, the first rule that matches deciding. Each stage shows the rule that decides and the text it matched, the later rules
that also match but lose to it, and a count of rules that did not match (`--all`
lists them with the reason, such as a different account type). It also shows any recorded category. Category rules are not traced; use `fin rule preview`.

#### Find untyped movements and test a rule before saving it

`fin rule gaps` lists movements that have no type, grouped by how their first
description starts (the first one or two words, stopping at a word with a digit),
largest group first. Each group shows its count, dates, debits and credits, an
example description and a starting expression. Use `--institution` and `--limit`
(default 20), or `--json`.

```sh
fin rule gaps --institution boursobank
fin rule test cafes '^CAFE(?:\s|$)' --institution boursobank --set-type card
fin rule add cafes '^CAFE(?:\s|$)' --institution boursobank --set-type card --automatic
```

`fin rule test NAME [REGEX]` evaluates a draft type rule as if it were saved as an
automatic rule, against every stored movement of its institution, and changes
nothing. It accepts the type-rule options of `fin rule add` (`--description`,
`--set-type`, `--institution`, `--case-sensitive`, `--match-type`, `--account-type`
and `--priority`), and requires `--set-type`: category rules are personal and are
not tested here. A saved rule with the same name is replaced in the test, so
testing an edited rule shows what the edit changes. The report counts the matches
as newly typed, type changed, already this type, decided by an earlier rule
(named, so you can see what shadows the draft) or covered by a manual type
override, and gives the number of movements without a type before and after. It
lists up to `--limit` examples (default 10) per outcome, or prints everything with
`--json`. Only automatic rules take part, because only they type movements on
import.

Types are never refreshed by hand: fin recomputes them after every import, rule
change and institution-definition change, and records each change in `fin tx type
history`. Event IDs, accounting classifications and review history are untouched.
A type rule that names a column also applies to the rows of a matched companion
file, tested on that file's own columns:

```sh
fin rule add wise-card --institution wise --field 'Transaction Details Type' \
  --description '^CARD$' --set-type card --case-sensitive --automatic
```

Name the column as it appears in the file's header; the rule matches when any cell
under that header matches. For Wise the `TransferWise ID` prefix separates fee lines
and cashback from the bank code `CARD`. Wise ships those rules as `wise-id-fee` and
`wise-id-cashback`; add your own the same way, for example to type card orders:

```sh
fin rule add wise-id-card-order --institution wise --field 'TransferWise ID' \
  --description '^CARD_ORDER_CHECKOUT-' --set-type fee --case-sensitive --automatic \
  --priority=-2000
```

The first rule that matches, in priority order, decides, so a rule with a lower
priority number outranks the others. A column that the file does not have matches
nothing, and a row that no rule decides has no type of its own. The value the
deciding rule matched is what `fin rule explain` shows for the companion file, and
`fin rule list` shows the field of each rule. A type that contains `{value}` keeps
the matched text as a provider-specific type. Lower priorities run first; new rules
default to 1000, so give a rule a negative priority to outrank the bundled ones. The
A row of a companion
file that is not matched yet affects nothing until its transaction arrives.
Manual type overrides and accounting decisions are preserved. Use
`fin transaction review` for unclassified items and hledger for accounting
reports and running balances.

### Settlement and reversal evidence rules

Automatic evidence rules record source clues at import without confirming an
accounting decision. BoursoBank's built-in rules extract the card reference
from deferred-card debits and identify withdrawal-reversal wording. Inspect
them with `fin rule list`; they can be disabled or replaced like other rules.

A user-added institution can define its own rules:

```sh
fin rule add-evidence card-id card-reference '^CARD PAYMENT (.+)$' \
  --institution mybank
fin rule add-evidence atm-refund withdrawal-reversal '^ATM REVERSAL ' \
  --institution mybank
fin tx refresh-evidence --institution mybank
```

The `card-reference` expression must contain a capture group; fin records group
1 from the first trimmed source description when it is the only description.
A withdrawal-reversal expression matches the first trimmed description. Rules
can also restrict `--account-type` and `--match-type`; lower `--priority`
values run first. Evidence `--match-type` tests the stored imported type, not a
manual type override. `--case-sensitive` changes the default case-insensitive
matching. Multiple matching reversal rules still produce one hint, while the
first matching card-reference rule supplies the reference.

A hint is only evidence: settlement and reversal candidates still need their
normal account, amount and date checks and explicit confirmation. Changing,
disabling or replacing a rule affects future imports. `fin tx refresh-evidence`
previews changes to stored clues and asks before applying them; it does not
change types, classifications or reviewed decisions. Transaction-list JSON exposes the recorded clues
as `source_card_reference` and `source_withdrawal_reversal`.

## Transaction rules

Rules let you reuse type and classification decisions. Review-mode rules
require an explicit preview and application; automatic rules apply on import. Conditions can match an institution, a description expression, an
effective movement type, or a combination; all supplied conditions must match.
The effective type includes a manual override when one exists.
Create a rule, inspect its matches, then apply it explicitly:

```sh
fin rule add cafes 'coffee|café' expenses:food --institution boursobank
fin rule preview cafes
fin rule apply cafes
fin rule list
```

To draft a reusable personal rule from an already categorized transaction:

```sh
fin rule from-category 123 cafes --contains 'Coffee Shop'
fin rule from-category 123 cafes --contains 'Coffee Shop' --save
fin rule preview cafes
fin rule apply cafes
```

Replace the ID, name and text with your own. The text must appear in the example's
effective description and is treated literally, not as a regular expression. Omit
`--contains` to use the whole description, or use `--institution NAME` to restrict
scope. The draft displays matches without changing anything; `--save` asks before
saving a review-mode rule, and `--yes` skips that confirmation. Saving a rule does
not classify transactions or enable automatic application. Use `fin rule mode`
to opt into automatic behavior, noting import-time rules see the primary source
description. Export personal category rules with `fin cat export`.

For Wise balance cashback, propose a type and an income account together:

```sh
fin rule add wise-cashback --institution wise --description '^Cashback$' \
  --set-type interest --category income:interest:wise
fin rule preview wise-cashback
fin rule apply wise-cashback
```

To categorize movements already typed as interest, regardless of description:

```sh
fin rule add interest-income --match-type interest --category income:interest
fin rule preview interest-income
fin rule apply interest-income
```

An action can set a type, a category, or both; `--set-type` and `--match-type`
accept the common or observed provider-specific types from `fin tx type list`.
An institution, description or type condition is required. When both a type and
category are proposed, each is assessed separately. Existing manual type overrides
and reviewed accounting decisions are not overwritten: the preview explains any
skipped action, while another eligible action on the same transaction can still
be applied. A type already matching the proposal does not create a redundant
override. Use `fin tx type reset` or the relevant accounting review workflow to
change an earlier decision explicitly.
Use `--match-type unknown` to select movements without an effective type;
`unknown` cannot be used as a `--set-type` value.

The expression uses Perl-style regular-expression syntax and is searched in
the effective description for review-mode rules. Automatic rules at import use the
primary description available at import time. Matching ignores ASCII
letter case by default; pass `--case-sensitive` when creating the rule to
change that. `--institution` restricts it to the exact institution name; omit
it to search all institutions. Quote expressions to protect them from the shell.
Regex metacharacters must be escaped to match them literally: for example,
`'4810\*{8}2573'` matches the masked card number `4810********2573`.

For a deferred credit card, first link the card to its settlement account with
`fin account link-card`, then use its ID from `fin account list`:

```sh
fin rule add-settlement card-payment '^Relevé différé Carte' CARD-ID --institution boursobank
fin rule preview card-payment
fin rule apply card-payment
```

The preview shows current type and classification, type provenance, proposed
actions and skipped actions. JSON includes `apply_type` and `apply_account` for
each match. Previously reviewed decisions are not overwritten. Settlement matches must be debits from the
linked account in the card's currency, with a configured card liability; possible
matching card-side payments are skipped to avoid recording a settlement twice.
Application previews all eligible matches, asks for confirmation, and records
the batch atomically. If the rule or eligibility changes while you review it,
the batch is refused. Review-mode rules do not run automatically on import:
preview and apply them again for new transactions. Automatic category rules apply
on import. The decision history records the rule and
expression in accounting and type history. Use `fin rule rename OLD-NAME NEW-NAME` to rename a rule; existing
decisions retain the name recorded when they were made. Use `fin rule delete NAME` to remove a rule without undoing its
decisions. `fin rule list --json` and `fin rule preview NAME --json` provide
machine-readable output; `--yes` skips the confirmation for apply and delete.
Regenerate the journal after applying decisions.

## Exporting and querying accounting

Run hledger against a freshly generated journal without saving a file:

```sh
fin ledger balance expenses -Y
fin ledger balance expenses -M --tree
fin ledger register equity:unclassified
```

Arguments after `ledger` are passed to hledger. To select a different Fin
database, put `--database PATH` immediately after `ledger`, before the hledger
command. `fin ledger --help` shows Fin's help; `fin ledger balance --help` asks
hledger for help. The wrapper streams the journal on standard input, so it is
intended for reports rather than commands that need interactive input or edit a
journal. Edits to a generated journal cannot update the Fin database. Hledger's
output and exit status are passed through.

You can also export a journal explicitly:

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
`fin`. `fin journal export` writes to standard output, whereas import publishes
the journal file automatically.

Monthly expense reports become useful after categorizing movements. Account
totals represent the records you supplied; opening balances and complete coverage
are needed before treating them as actual bank balances. Financial aggregation
belongs to hledger; `fin` tracks import evidence, account identity and review state.

## Library and dependency changes

This build uses Base, decimal, Ptime and Bos, with no Core or Bigdecimal
dependency. Version 1 account snapshots remain compatible; databases use schema 31 and
are rebuilt from the original statements (see above).

OCaml callers use `Quantity.t` (a `Decimal.t` alias) and `Date.t` (a `Ptime.date`
alias). Use the `Quantity` functions for exact accounting sums and products:
ordinary decimal arithmetic follows its configured precision and rounding rules.
Dates remain calendar dates, and Wise booking days retain their supplied local
date. Bos handles high-level file operations; import file-identity checks and
private snapshot and draft permissions remain in place.

`Source_event.create` takes `~occurred_at:Ptime.t`, and its occurrence constructor
is `At` instead of `At_ms`. Instants in the OCaml API use `Ptime.t`, including
source occurrences, Wise booking times, statement instants and audit times.
Calendar dates remain `Ptime.date`;
statement metadata uses `Temporal.Date` or `Temporal.Instant`. Revolut CSV start
and completion clocks are assumed UTC and exposed as `started_at` and `completed_at`
of type `Ptime.t`. This assumption does not depend on your system timezone.
Wise CSV also exposes
`booked_at : Ptime.t` under an explicit UTC assumption, while preserving its
source-local matching with CAMT (whose actual instant can carry another offset).
BoursoBank OFX balance `as_of` values use `Ptime.t`; missing OFX timezone means
GMT and missing time means midnight according to the format. Invalid OFX balance
clocks or unsupported offsets are rejected. Bunq and BoursoBank
CSV booking/value dates remain `Ptime.date`, and invalid OFX `DTUSER` metadata
remains raw `user_date_source`.
Original source text and source-ID encoding remain unchanged. Revolut booking
dates and source-event occurrences remain the completion calendar date.

SQLite stores instants as canonical UTC RFC 3339 text with 12 fractional digits.
`ORDER BY occurred_at` and ordinary comparisons/indexes preserve the complete
chronological order. SQLite's date functions are not used for fine-grained
instant comparisons. Bank audit times reflect the clock precision SQLite provides,
padded to the canonical representation, without claiming extra clock resolution.

`fin db events` now returns `occurred_at` (RFC 3339 or null) instead of
`occurred_at_ms`; `occurred_on` is unchanged. The low-level `fin db ingest --at-ms`
input remains an explicit Unix-millisecond boundary and converts exactly to Ptime.
Wise companion inspection now formats its assumed-UTC `booked_at` as canonical
RFC 3339. Statement report timestamps are normalized to UTC; calendar-date boundaries remain
dates. Wise movement IDs, raw payloads, local booking dates and journal output
remain unchanged. Schema 12 databases must be rebuilt using the storage instructions
above; no automatic migration runs.

## Command reference

| Command | Purpose |
| --- | --- |
| `fin init` | Create the database, with first-import guidance |
| `fin import INSTITUTION PATH...` | Import statement files and publish the journal |
| `fin status` | Import coverage and outstanding review work |
| `fin paths` | Resolved storage paths |
| `fin account --help` | Account listing, config and history |
| `fin institution --help` | Institution list and generic import setup |
| `fin transaction --help` | Imported transaction listing |
| `fin statement --help` | Statement reconciliation and companion files |
| `fin reversal --help` | Withdrawal reversal candidates and review |
| `fin transaction --help` | Transaction listing, review and categories |
| `fin transfer --help` | Candidates, preview and explicit decisions |
| `fin journal --help` | hledger journal export |
| `fin backup` | Copy the database, including account settings |
| `fin restore BACKUP` | Restore a complete database snapshot |
| `fin delete` | Delete all fin state, after confirmation |
| `fin db --help` | Lower-level database and source-event commands |

For detailed options, put `--help` after the specific command. Argument errors
exit with status 124; operation failures exit nonzero and print a diagnostic.
Check the per-file results after an import failure before retrying.
