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
`account`; `inst` is a shortcut for `institution`. For example, `fin tx list` and `fin acc list` use the same commands
as their full names. Help displays the canonical command name.

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
account edit draft, then asks you to type `yes`. Your statement folders and
exported account snapshots are never touched. In a script, `fin delete --yes`
skips the question; without a terminal and without `--yes` it refuses.
`--database FILE` selects which database and associated files to delete.

### Rebuilding after a schema change

This experimental release accepts schema version 21; it does not migrate older
databases and refuses them without changing the file. Before upgrading to a new
schema, use the **old fin executable** to export account settings and save a
full backup. Save the transaction list and review histories as reference when
you have decisions to recreate:

```sh
fin account export -o ~/fin-accounts.json
fin backup ~/fin-backups
fin transaction list --json > ~/fin-transactions-before-rebuild.json
fin transaction category history > ~/fin-categories-before-rebuild.json
fin transfer history > ~/fin-transfers-before-rebuild.json
```

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
Rules, type overrides and review decisions are not included in the account
snapshot and must be recreated. Their event IDs are database-specific, so
use the saved transaction list and histories to identify the new transactions
and recreate decisions; do not blindly reuse old IDs. A backup preserves the old
decisions, but cannot transfer them to the new schema.

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
Then import a file or directory with `fin import NAME PATH`. Generic CSVs do
not identify their account, so fin asks for an account label for each file unless
`--account-id` is supplied. Only use the same label for files from the same card.
Directory imports process CSV files in name order; earlier files remain imported
if a later file fails. For a generic card CSV, you can also use `fin import FILE.csv`;
fin prompts for its provider and account details when needed.

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


### Configure another card CSV

For a CSV that describes card transactions, initialize fin and run:

```sh
fin import card.csv --provider my-provider --account-id my-card
```

The first import must run in a terminal. Fin detects a comma or semicolon
separator from the header when unambiguous, and asks if it cannot decide. It
then proposes date, description and account amount columns when it recognizes
them. Accept the proposal or choose columns yourself. For questions with
numbered choices, enter the number; empty and invalid choices are retried. It
also explains the account type and asks whether purchases have positive amounts. Dates
must be ISO `YYYY-MM-DD`, amounts must use a dot decimal mark, and the account
currency is fixed for the file. If there is a summary row, specify its label;
fin skips it only when its date is empty. Review the preview and confirm to import.
The preview shows each CSV amount beside the signed account movement; other
CSV fields, such as local currency and exchange rate, retain their source signs.
Generic CSV directory imports review each file separately. Raw fields are retained
with the source evidence, including original currency amounts and exchange rates.
The mapped account amount, not the local amount, is posted to the ledger.

The provider name identifies the institution; fin prompts for a nonempty name
when omitted on a terminal and retries invalid answers. The layout is saved with the accepted document in the database
and reused for later files with the same header and provider. Supply the same `--account-id` for the same
card, and different labels for separate card balances. A known layout can be
imported without a terminal. Reimports are safe: repeated identical rows are
preserved as distinct occurrences, and a changed or partial overlapping day is
rejected for review. Layouts with the same header but different settings are
ambiguous and require separate handling. Fin does not infer transfers between
cards or from card top-ups.

For generic imports recorded under the former `csv` institution, delete the
old account and import its files under the provider name. See account deletion
below; reviews and account settings associated with the deleted account are lost.

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
categories are suggestions. To classify a movement interactively, find its event
ID in `fin transaction list`, then replace `123` below with that ID:

```sh
fin transaction category review 123
```

Choose a previously used category by number or enter an `expenses:...` or
`income:...` account. The command shows matching unclassified movements and lets
you select all, none, or individual numbered matches. It also shows conflicting
reviewed classifications. Check the proposed batch and type `yes` to apply it;
enter `q` at a selection prompt or decline confirmation to cancel. This command
requires a terminal. It checks that all selected movements are still unclassified
before saving the batch, so a concurrent decision cannot be overwritten.

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
BoursoBank `ANNUL RETRAIT DAB` movements have type `withdrawal`; refresh
older types with `fin tx refresh-types --institution boursobank`.

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

The preview matches within the same institution, using the merchant for card
movements when available, or otherwise the ordered descriptions. The two kinds
of match do not mix. It ignores ASCII case and whitespace. It shows other
unclassified movements and separately flags
matches categorized differently. It never applies a category; use interactive
review or an explicit decisions file to apply changes. Confirmed transfers are excluded.

The filenames above are files you create, not files generated by the commands.
Confirm a transfer only when both movements belong to your accounts. Registered
IBANs improve the evidence used to rank candidates. Review changes are audited;
regenerate the journal after changing settings or decisions.

## List transactions

`fin transaction list` lists all imported movements, including reviewed ones,
newest first, with the institution and account ID for each movement. The
displayed event IDs can be used in review decisions; account IDs come from
`fin account list`.

The table shows the source movement type separately from its accounting
classification, including confirmed settlements. On a terminal, the table opens
in `$PAGER` (default `less -FRX`); an empty `PAGER` disables paging. Redirected
output and `--json` go directly to stdout.

```sh
fin transaction list
fin transaction list --from 2026-01-01 --to 2026-01-31
fin transaction list --search coffee --limit 20
fin tx list --type interest
fin tx list --type card-settlement --classification settlement
fin tx list --type unknown --classification unclassified
fin transaction list --order asc
fin transaction list --json
```

Dates are inclusive booking dates. Use `--account ID` for one account or
`--institution NAME` for a provider; `--search` matches the display name or
original description using ASCII case-insensitive comparison. `--order asc`
lists oldest first; `--order desc` is the default.
`--type` matches the displayed movement type, including provider-specific values;
`--type unknown` selects movements without a type. `--classification` accepts
`unclassified`, `category`, `transfer`, `settlement` or `reversal`. Filters combine, and
are applied before sorting and `--limit`.
Sorting uses the source event's date or timestamp in UTC; date-only events sort
before timed events on the same date, with event IDs breaking ties. Sorting
precedes `--limit`, which must be positive. When a uniquely matched Wise CSV
provides a merchant or movement type, the list shows those fields; otherwise it
falls back to the source description. Conflicting values are omitted. JSON
includes the original `description`, optional `display_name` and `movement_type`,
and `detail_sources` companion row IDs, which can be inspected with
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
accounting classification, survive reimports and `refresh-types`, and take
precedence over imported and companion types. `type show` and transaction-list
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

### Configuring import-time typing

Each institution has a source typing profile. BoursoBank starts with the
built-in rules described above; a user-added CSV institution starts with an
empty profile. Inspect and edit a profile as JSON, then install it:

```sh
fin institution typing show boursobank > boursobank-types.json
$EDITOR boursobank-types.json
fin institution typing set boursobank boursobank-types.json
fin tx refresh-types --institution boursobank
```

The version 1 profile names its institution and contains an ordered `rules`
array. Each rule has a `description` regular expression, `account_type` (an
exact source account type, or `null` for any account), `case_sensitive` (a
boolean), and `type`. The expression matches the first trimmed source
statement description; the first matching rule wins. Use a type from
`fin tx type list` or a provider-specific type; `unknown` is not a type.
The file replaces the whole profile, so retain any default rules you need.
The supplied BoursoBank defaults are in
[`banks/source_type_defaults.json`](../banks/source_type_defaults.json).

Setting a profile previews it and asks for confirmation (`--yes` accepts it).
It affects subsequent imports. Existing movements change only when you run and
confirm `refresh-types` as above. Manual type overrides and accounting decisions
are preserved. `fin institution typing reset boursobank` previews and restores
the bundled defaults; for a user-added institution it restores an empty profile.
Deleting a user-added institution also deletes its profile. Profiles are stored
in the database and included in database backups.

These source rules are separate from reviewed transaction rules: they assign
source types at import and do not categorize transactions. Wise's type codes
from matched companion CSV rows are still handled by its adapter and are not
changed by this profile or by `refresh-types`.

Refresh existing types from stored source evidence after importer improvements:

```sh
fin transaction refresh-types --institution boursobank
```

The command previews changed stored types and asks for confirmation; `--yes`
applies the preview without prompting. Omit `--institution` for all institutions.
It does not change event IDs, transactions, accounting classifications or review
history. It rechecks the preview and applies the type changes atomically. Types
from matched companion files are separate and are not changed by this command.
Use `fin transaction review` for unclassified items and hledger
for accounting reports and running balances.

## Transaction rules

Rules let you reuse type and classification decisions without silently changing
imported data. Conditions can match an institution, a description expression, an
effective movement type, or a combination; all supplied conditions must match.
The effective type includes a manual override when one exists.
Create a rule, inspect its matches, then apply it explicitly:

```sh
fin rule add cafes 'coffee|café' expenses:food --institution boursobank
fin rule preview cafes
fin rule apply cafes
fin rule list
```

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
the display name and each original description field. Matching ignores ASCII
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
the batch is refused. Rules do not run automatically on import: preview and apply
them again for new transactions. The decision history records the rule and
expression in accounting and type history. Use `fin rule rename OLD-NAME NEW-NAME` to rename a rule; existing
decisions retain the name recorded when they were made. Use `fin rule delete NAME` to remove a rule without undoing its
decisions. `fin rule list --json` and `fin rule preview NAME --json` provide
machine-readable output; `--yes` skips the confirmation for apply and delete.
Regenerate the journal after applying decisions.

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
`fin`. `fin journal export` writes to standard output, whereas import publishes
the journal file automatically.

Monthly expense reports become useful after categorizing movements. Account
totals represent the records you supplied; opening balances and complete coverage
are needed before treating them as actual bank balances. Financial aggregation
belongs to hledger; `fin` tracks import evidence, account identity and review state.

## Library and dependency changes

This build uses Base, decimal, Ptime and Bos, with no Core or Bigdecimal
dependency. Version 1 account snapshots remain compatible; databases use schema 21 and
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
| `fin institution --help` | Institution list and generic CSV setup |
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
