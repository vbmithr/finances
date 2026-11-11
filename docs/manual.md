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

A command that changes data shows what it will change and then asks. The prompt is
`[Y/n]` (Enter accepts) for changes you can redo, and `[y/N]` (Enter declines) for
commands that delete data or replace state, such as `fin delete` and `fin restore`.
`--yes` skips the question in scripts; without a terminal and without `--yes`, the
command prints what it would change and fails. Answering `n` is the way to preview.

Run `fin` for a short welcome, or `fin --help` for the command reference. Neither
command creates a database. Every command supports `--help`, including nested
commands such as `fin import --help`. The installed package also provides
`man fin` and subcommand man pages.

`op` is a shortcut for `operation`; `acc` is a shortcut for
`account`; `inst` for `institution`; and `cat` for the top-level `category`
catalog. Under `operation`, `cat` also abbreviates `category` for review
commands. For example, `fin cat list` lists the catalog, and
`fin operation category review OPERATION-ID` reviews a movement. Help displays canonical names.

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
fin operation list
fin operation review
fin transfer candidates
```

`fin status` shows discovered banks and accounts, per-account observed date
ranges, movement counts, operations that have no bank account (such as CDC
rows, per institution, with how many are unresolved), unclassified movements, unreviewed transfer candidate
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
fin operation review --json > bank-review.json
fin transfer candidates --json > transfer-candidates.json
```

Transfer candidates JSON includes ranking and evidence; `--all` also includes
reviewed and other unavailable pairs. Unreviewed pairs involving a categorized
movement are excluded. Institution import
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
review history. `fin account edit` and `fin account iban` update that database.

`fin backup DIR` saves two files in a new dated folder: `fin.sqlite3`, a copy of the
database, and `decisions.json`, everything you decided. `fin restore FOLDER` puts the copy
back; it only works for the same schema version. `decisions.json` is for rebuilding, and is
what survives a schema change.

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
settings and review history. Keep your original bank exports too. Backup does
not check the schema version, so it also works on a database this version of `fin`
cannot open; take one before rebuilding after a schema change.

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

fin does not migrate older databases: it refuses them without changing the file. To rebuild,
save your decisions with the **old fin executable**, then start again from your statements
with the new one.

```sh
fin decisions export ~/fin-decisions.json   # old executable; fin backup also writes this
fin backup ~/fin-backups                    # keep a copy to go back to
fin delete
```

Keep the old executable if you may need to go back; the new one cannot open a database of
another schema. After installing the new version:

```sh
fin init
fin import --all ~/Statements
fin decisions restore ~/fin-decisions.json
```

`--all` imports each folder of `~/Statements` named after an institution or an alias (see
[Importing statements](#importing-statements)); `fin import NAME FOLDER` imports one. Replace
these paths with your own. User-added institutions and their records definitions must be added again.

### What a decisions file holds

`fin decisions export FILE` saves everything you decided, as one private JSON file:

- account settings: ledger names, IBANs and card settlement links
- the category catalog and your rules
- category assignments, including explicit resets, manual type overrides, and the accounts
  and commodities you resolved for movements
- card settlements, reversals, exchanges and transfers

Statements are not in it: they come back from `fin import`. Neither are rejected transfer
candidates, audit history or independently created operations; `fin backup` keeps those.

`fin decisions restore FILE` applies all of this in the right order, as one transaction. It
shows what would change and asks `[Y/n]`, and answering `n` leaves everything as it was, which
makes it a preview. `--yes` skips the question for scripts; without a terminal the command fails
unless you pass it. Restoring twice changes nothing.

Each decision finds its movement by institution, source account, kind and source ID, then
checks the occurrence and hashes of the original source. Database IDs are not reused and no
match is guessed. A decision that matches nothing, an ambiguous or changed source, a conflict
with something you already decided, or a missing settlement card is a problem: the restore
stops, lists it, and changes nothing. If that happens, remove the affected decisions from a
copy of the file and recreate them by hand.

Each section of the file is optional, so a copy with only `"accounts"` or `"transfers"` restores
only that. The file contains personal source identifiers: keep it private. After restoring,
export the journal again.

## Importing statements

Import an institution's statements by naming it: `fin import wise ~/Statements/wise`. If you
keep one folder per institution, each named after the institution or an alias
(`fin institution list` shows both, for example `bourso` for BoursoBank), import them all at
once:

```sh
fin import --all ~/Statements
```

Each folder is imported as `fin import NAME FOLDER` would, in name order, with a summary per
institution. Every name is checked first: a folder that is not an institution or alias, or a
file directly in `~/Statements`, stops the import before anything is read, so nothing is
skipped silently. Hidden folders are ignored. Folders must be real directories, not
symbolic links, as for any import.

### Inspect CDC CSV exports

CDC is a bundled institution definition, `institutions/cdc.toml`, not code: it lists
the expected CSV header, the columns and what each reported transaction kind moves.
Name it `cdc`.

To see what fin reads from an export without a database, convert it:

```sh
fin convert cdc wallet.csv card.csv
```

The command imports and posts nothing. It prints records, keeping each CSV row's
fields, and refuses bad timestamps and amounts. A kind with no known movements gets
no movements. Symbols are scoped to the provider: fin does not claim that a symbol
identifies a crypto asset or network. Native amount columns are valuations; they
do not create movements. A single movement does not establish the counterparty,
wallet, category or fee. The output contains personal transaction details and
should be kept private.

To retain an export and its proposed operations in the database, import it like
any other venue — there is no separate preview step:

```sh
fin import cdc wallet.csv
fin db sources --institution cdc
fin db event operations EVENT-ID --json
```

CDC imports go through the same pipeline as banks: files and directories
are scanned recursively (including inside ZIP archives), each recognized file is
imported in sorted order with the usual per-file and summary outcome lines, and the
journal is published. `fin db sources --institution cdc --json` includes the
original fields and source event IDs. The complete CSV is archived in the database.
Reimporting the identical file is reported as unchanged. Fin also accepts a
complete records conversion of a single archived source (or the original source
after its records conversion) when the records file names exactly that one source
and its source digest, complete event set, and stored evidence agree. Other files containing already imported operation rows, and
files containing indistinguishable identical rows, are rejected as ambiguous.
Review those exports before importing them. A file with only the
header is a valid statement of no activity: it imports with `inserted=0`. A reimport that
finds a stored
operation differing from the importer's interpretation (movement roles,
commodities, quantities or source links) is refused without changing the stored
decisions.

Imported legs are proposals, with unresolved account and commodity identities;
unknown rows may have no proposed legs. Only explicitly resolved and confirmed
two-movement exchanges can currently be projected to hledger; other imported
operations are not posted and no opening balances are established. Transaction
decision exports do not contain the original CSV evidence: use a full database
backup or keep the original exports. Treat backups and JSON output as private.

A crypto exchange or a block explorer reports symbols fin does not trust by name: a
ticker scoped to the exchange, or a token's contract scoped to its network. Say once
what each one is; it applies to every movement reporting it:

```sh
fin asset list                        # every reported symbol, with the name its source states
fin asset map cdc BTC BTC             # an exchange's ticker
fin asset map polygon 0x3c49 USDC     # a token contract, by its first characters
fin asset ignore polygon 0x5273       # spam: kept as evidence, never counted
fin asset adopt CowSwap.json          # map the contracts a token list names
fin asset ignore-unmapped polygon     # then ignore what is left, usually spam
```

An ignored symbol's operations leave `fin operation list`; `--ignored` lists them again.
`fin asset ignore-unmapped [PROVIDER]` previews every symbol still unmapped and ignores
them after one confirmation; assumed symbols, such as an exchange's tickers, are not
unmapped and are left alone. `fin asset unmap` undoes either.

A token list, as CoW Swap, Uniswap or Coingecko publish them (tokenlists.org), says which
contract on which chain is which token. `fin asset adopt LIST.json` matches your
unmapped contracts on the networks whose definitions give their chain, shows what the
list says of each, and maps them all after one confirmation; the history records the
list's name and version. It never changes what you mapped or ignored, and skips a list
symbol that cannot be an asset, such as `USDC.e`. A contract the list does not name is
not thereby spam: lists are short, so ignore spam yourself.

Most symbols need no decision. Until you map one, `fin asset list` shows what it is
assumed to be: an exchange's ticker is the asset of that name (XLM on cdc and on Kraken
is the same Stellar), an ISO currency code is that currency, and a unit an institution
defines for itself, such as Kraken's fee credits, is what its definition says (`KFEE`).
Map or ignore one when the assumption is wrong, such as a crypto ticker that is also a
currency code (Mantle's `MNT`): `fin asset map kraken MNT MANTLE`. Token contracts are
never assumed: map them yourself or with `fin asset adopt`.

The code is an ISO currency code, which is fiat, or an asset's symbol, which is also
its name in the journal. An asset is one asset whichever network holds it: USDC on
Ethereum and on Polygon is `USDC`, and which network a movement was on stays in its
source fields. Map a token contract only once you have checked it, such as on its
issuer's site: anyone can name a token USDC. `fin asset unmap` removes a mapping;
neither changes while confirmed transfers or exchanges rely on it.

For a recognized two-movement exchange, inspect its roles with
`fin operation show ID`, then assign an account to each movement. For example, using
the roles `from` and `to`:

```sh
fin operation resolve 123 from assets:exchange:eur
fin operation resolve 123 to assets:exchange:btc
fin operation exchange confirm 123
fin journal export > journal.hledger
```

Confirm only after checking the source, both accounts and
quantities. Confirmation previews the two movements and two `equity:trading`
postings, then asks for approval. `--yes` skips the prompt. Use
`fin operation exchange undo ID` before changing a confirmed resolution.

The exchange workflow currently requires one source record, exactly two
opposite-sign movements in distinct commodities, and both movements explicitly
resolved. It does not infer fees, opening balances, cost basis or capital gains.
Unknown records with no proposed movements and bank-side forex still require
further review tooling. `fin decisions export` saves movement resolutions and confirmations.

Use `fin import INSTITUTION PATH...` to read exports. This replaces the former
`fin bank sync` command; update existing scripts to use `fin import`.
Run `fin institution list` to list built-in and user-added institutions with their
type: `bank`, `crypto exchange` or `wallet` (brokers are not modelled yet). Every
definition file, including the bundled ones, declares it with `"type": "bank"`,
`"type": "crypto-exchange"` or `"type": "wallet"`; it defaults to `bank` when omitted.
A wallet may name its EVM chain with `chain_id` in `records`, as Ethereum and Polygon
do, or a non-EVM network with `network`, a nonempty string. A wallet may also leave
its network unspecified. Banks and exchanges cannot specify either field, and a wallet
cannot specify both. `fin institution show`, without an
argument, gives an overview of every institution: type, origin, how many movements
and operations are imported, how many operations are still unresolved, and the
first and last observed dates.
For BoursoBank, `bourso` and `boursorama` are accepted aliases for `boursobank`;
all three import into the same institution.
Add an institution with `fin institution add NAME`. Attach a definition to import its CSV files.
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
Configure a CSV companion as described below, or attach a records definition for
a primary CSV. Earlier files remain imported if a later file fails.

### Attach an institution definition

An institution definition is a versioned TOML file that describes an institution,
its statement formats and its default rules. Each `[[statements]]` section names a
format; a CSV section references a converter JSON file or an experimental Lua
2.5 script, relative to the TOML file. For example:

```toml
version = 1
institution = "mybank"
type = "bank"

[[statements]]
format = "csv"
definition = "mybank-csv.json"

[rules.mybank-card]
pattern = '^CARD '
action = "type"
target = "card"
```

Copy a bundled TOML file and its referenced files from `institutions/` to use as a
starting point, then adjust the institution name, converter and rule names. Validate
a definition and preview a sample before saving it:

```sh
fin institution definition check mybank.toml sample.csv
fin institution add mybank
fin institution definition set mybank mybank.toml
fin institution list
fin import mybank ~/Statements/mybank
```

To use a Lua converter, replace the `definition` line with
`script = "mybank.lua"`. The script declares its institution, CSV header,
account and converter metadata, and returns records from each row; see the
[Lua converter guide](lua-csv-converters.md) for the contract and bundled
examples. The Lua integration is experimental: it supports static or row-derived
accounts, exact headers, stated balances, observations and statement claims.
Lua observation settings, including journal tags, live in `config.observation`;
the host supplies the selected account and source fields for records.
An institution cannot mix JSON and Lua CSV converters yet. File-wide checks, row skipping, general decimal arithmetic and some observation
matching settings are not yet supported. Lua numbers are floating point; use
`decimal`, `decimal_comma`, `subtract`, `negate` and `sign` for exact quantities.
Only run scripts you trust: they can loop indefinitely.

A CAMT statement section may also specify `script = "mybank.lua"`. Its
`interpret_camt` function receives parsed entries and chooses their reference and
group; the same file can also implement the CSV converter. When using a script,
omit the profile's `reference_field`,
`reference_normalization` and `operation_group` settings; the script supplies
those decisions. See
[Lua interpretation for structured statements](lua-statement-interpretation.md)
for the callback and its fields. The bundled Wise CAMT profile remains unchanged.

The institution name in the TOML and each CSV converter must match the selected
institution. `set` previews the rules to add and asks for confirmation; `--yes`
accepts the preview in scripts. Existing rules with the same names are retained,
including rules belonging to other institutions. `check` prints up to five movements, an operation count,
or companion rows and does not open or change the database. Invalid samples report
the first parse error. `show` prints the resolved definition as JSON, including its
CSV layouts, for any built-in or installed institution. The previous self-contained
JSON definition format remains accepted by `check` and `set`.

For a walkthrough of CSV converter definitions, see the
[CSV converter guide](csv-converter.md); the [field reference](records-format.md#csv-definitions)
lists the available settings. Definitions can
include default source-interpretation rules; these become normal editable rules in
`fin rule list`.

Supported actions are `type`, `card-reference` and `withdrawal-reversal`. A `type`
rule may also name a `field` to test (see "What a type rule matches"), and its
`target` may contain `{value}`, which takes the text the rule matched, so that
`wise:{value}` keeps a provider code as a provider-specific type. Rules may specify
`priority`, `case_sensitive`, `account_type` and `match_type` where applicable. Use
`fin rule add` for personal accounting rules. The CAMT reader supports the settings
exposed by the Wise-shaped profile rather than every bank's CAMT format; likewise,
the MT940 reader covers the observed Bunq-style structure, not every bank's MT940
variant; the [MT940 grammar](mt940-grammar.md) describes the supported syntax.
CAMT and MT940 statement profiles inherit their institution
and profile version from the enclosing TOML definition. Legacy event identifiers
default from the institution name, and the Bunq profile no longer requires CSV
companion settings. The Bunq MT940 entry pattern is no longer configurable;
previously saved definitions using the bundled pattern remain readable. A saved
definition with a different custom pattern is rejected and needs a supported
statement converter.

The bundled Wise Lua script names `Merchant` as its observation `journal_description`, rather than
the constructed `Description` column. Fin chooses one effective description: the matched,
nonempty `Merchant` value when CSV rows agree, otherwise the CAMT description. Original
source descriptions and CSV fields remain available as evidence.

Fin saves the validated definition in its database, so later edits or removal of
the TOML or referenced JSON or Lua files do not change existing imports. To update it, run `definition set`
again. Rule-only changes can be saved after import without changing existing
transactions; existing rules with the same names are not overwritten. Changes to
the source interpretation require removing the institution's imported evidence
first, so previously stored records cannot be silently reinterpreted. Source identities, overlap and reconciliation follow the chosen importer:
primary CSV uses day inventories, OFX uses its source IDs, and CAMT uses the
configured reference and clock policy. Backups include installed definitions.

### Import Kraken

Kraken is bundled, for spot ledger exports of either era:

```sh
fin import kraken ~/kraken/ledgers.zip
```

Each row of the ledger is a movement, on one account per asset. A trade's rows read as one operation,
such as `-500 EUR → -2 FEE → +0.01 BTC`, with its fee credit as a supporting row. Euros and other listed
currencies are real currencies, which transfers with your banks can pair on; other assets stay as
Kraken's symbols, translated from codes like `XXBT`, until you resolve them. A row with no `txid` is left
out: Kraken lists a deposit or withdrawal again without one before it settles. Anything but spot `currency`
rows is refused, and each file's balances must add up. Trade exports are not read yet.

### Configure a CSV companion

If an institution supplies both OFX and a CSV with additional transaction
fields, configure the CSV as a companion before importing it. A companion does
not create transactions. For example, start from the bundled example layout for a
user-added institution:

```sh
fin institution companion example > companion.json
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
`iso-datetime`, `day-first` or `month-first`; `amount_format` and `balance_format` are `dot`
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
fin operation review
```

Use the corresponding institution for your other inboxes:

```sh
fin import wise ~/Statements/wise
fin import bunq ~/Statements/bunq
fin import revolut ~/Statements/revolut
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

For Revolut no label is needed: a person has one personal account, with a balance per currency.

Import never asks for an IBAN. Some statements do not state their account's IBAN;
transfers to and from such an account are then matched by amount and date only.
`fin account list` says which accounts lack one, and `fin transfer candidates` shows
the IBAN another bank's row gives as the counterparty, which is usually it. Set it
with `fin account iban ID IBAN`, once for all the currencies of one account, or
remove it with `--clear`; changes are audited in SQLite. A card account has no
IBAN, and statements that state one take precedence. Import reads and updates
account settings only in the database. Exported snapshots are never read or
rewritten during import.

You can add more exports and repeat the command. Unchanged documents and known
transactions are recognized. Conflicting evidence is reported for review.
Successful files remain imported even if another file fails. Inspect the summary
and history when import reports an error:

```sh
fin institution history
fin statement list
fin statement totals
fin statement companions --review
```

`fin statement list` compares each bank statement's stated balances with the
movements it lists. `fin statement totals` compares source-stated totals with
the records they name. It reports the difference as movements minus the stated
total; `reconciled` and `difference` are null if a linked observation or a
record without a movement in the stated commodity prevents comparison.
`fin statement companions` lists the enrichment exports (the BoursoBank, Wise
and Bunq CSV files) with their fields and match state; `--review` keeps the rows
that still need attention. These commands accept `--institution` to show one bank. `fin statement match` recomputes the matches;
`fin import` already does this.

`fin institution history` is a log of file import attempts, not a list of transactions.
It shows the bank, timestamp, filename, outcome, new/existing movement counts and
any diagnostic. Attempts appear newest first; an unchanged file can appear more
than once because each attempt is recorded. Use `fin institution history --limit 10` for
the ten latest attempts, or `fin institution history --json` for machine-readable output.
Use `fin operation list` to browse individual movements instead.

See [Bank import and review reference](../README.md#folder-workflow) for
additional detail, or use `fin import --help` for current options.


### Configure another CSV

For a EUR account named `main` and a file with columns `Date`, `Description`
and `Amount`, save this converter as `mybank-csv.json`:

```json
{
  "format": "fin-csv-converter",
  "version": 1,
  "institution": "mybank",
  "institution_type": "bank",
  "header": [
    "Date",
    "Description",
    "Amount"
  ],
  "account": {
    "id": "main",
    "type": "current",
    "currency": {
      "code": "EUR"
    }
  },
  "occurred": {
    "column": "Date",
    "layout": "iso-date"
  },
  "cases": [
    {
      "movements": [
        {
          "role": "primary",
          "quantity": {
            "column": "Amount"
          },
          "commodity": {
            "code": "EUR"
          }
        }
      ]
    }
  ],
  "descriptions": [
    "Description"
  ]
}
```

Create `mybank.toml` alongside it:

```toml
version = 1
institution = "mybank"
type = "bank"

[[statements]]
format = "csv"
definition = "mybank-csv.json"
```

This assumes signed amounts and one account. Adapt the layout to the actual
statement; see the [CSV definition reference](records-format.md#csv-definitions).
Then check and attach it before importing:

```sh
fin institution add mybank
fin institution definition check mybank.toml statement.csv
fin institution definition set mybank mybank.toml
fin import mybank statement.csv
```

The old interactive generic CSV importer and `kind: csv` institution definitions
are no longer supported.

### Print a statement as records

`fin convert` prints statement files as records, in JSON Lines. It stores nothing and needs
no database. It reads Wise CAMT, BoursoBank OFX, bunq MT940, and the CSV files of Wise, Revolut,
CDC, FDM, BoursoBank and bunq:

```sh
fin convert wise statement.xml | grep CARD-1
fin convert wise ~/statements/wise -o wise.jsonl.zst
```

`-o` writes a file and prints a summary, and a name ending in `.zst` compresses it with
zstd. Fin reads plain and compressed files alike, and concatenated files too.

A CSV definition can be a [JSON layout](records-format.md#csv-definitions) or an
experimental Lua script:

```sh
fin convert --definition mine.json statement.csv
fin convert --definition mine.lua statement.csv
```

A directory is walked recursively in name order, leaving out hidden and partial files and
symbolic links, and a `.zip` file is opened in memory. In either, every statement is
converted, and anything the converter does not recognize, such as Wise's MT940 files, is
named on standard error and skipped. A corrupt or encrypted member, a nested archive, a
statement that fails to parse, or finding no statement at all stops the command.

A row that several files state appears once. If they state it differently, or any file
cannot be read, nothing is printed.

A CAMT file gives one counted record per entry, and a CSV gives observations describing
those entries. A CSV with no rows, as Wise exports for a balance with no activity, gives a
document with no records. Keep the output private: it holds your statement's names and references.

### Import through records

Every bundled institution is imported through records, directly from its files or from a records
file, plain or compressed. Both give the same result:

```sh
fin import fdm ~/statements/fdm
fin import fdm records.jsonl.zst
```

FDM exports one card per file, and each card gets its own account named after the file:
`~/statements/fdm/1118.csv` is imported into `assets:cards:fdm:1118`. Name the files after
the last four digits of the cards and a batch import, such as `fin import --all`, needs
nothing more. `--account-id ID` overrides the file name for the files it imports:

```sh
fin import fdm ~/statements/fdm
fin import fdm statement.csv --account-id 2233
```

`fin convert` names the account the same way. FDM purchase rows are treated as
outflows whether the export states their amounts as positive or negative; refund
rows are inflows, and adjustment signs are preserved. Fin retains `Balance after`
in the original source fields, but does not validate it as a consecutive balance:
some FDM exports omit balance changes between rows. If you previously imported an
FDM file whose signed amounts were inverted, back up your database, remove the
affected account, and reimport it to correct its movements and journal. Account
removal also removes its review decisions. FDM's spent and deposited totals are
currently each linked to all records in the file, so their reported differences
are not a comparison against only spending or only deposits.

Observations in a records file are paired with its counted records, and balances are stored:
`fin statement balances` lists them as JSON. Source-stated totals are retained
and compared with linked records by `fin statement totals`. A statement without
opening and closing balances, such as BoursoBank's OFX, is not listed as a
balance reconciliation.

### Check a records file

A converter turns one institution's statement into a records file, in the format
described in [records-format.md](records-format.md). Validate its output while you write
one:

```sh
fin records check records.jsonl
```

A valid file prints one summary line per institution. Anything else prints where the file is wrong, such
as `records.jsonl: line 4: occurred: invalid date or instant: "2025-02-31"`, and exits
with status 124. The command needs no database and stores nothing; fin cannot import
records files yet.

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

`fin account list` lists your accounts with their IDs. `fin account show ID` shows one:
its details, the cards that belong to it (see [Cards](#cards)), the card accounts it pays,
and how many of its movements are unclassified, categorized, transfers, settlements or
reversals. Add `--json` for the same as one object.

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

`fin decisions export` saves account settings with your other decisions, and
`fin account history` lists what changed. Settings live in SQLite; a file is never an active
configuration, and later imports use only what is saved in the database. In the file, omitted
accounts and settings are kept and an explicit `null` clears an optional setting. IBANs inferred
from primary records cannot be overridden or cleared. See the
[account settings reference](../README.md#manage-bank-accounts) for the fields.

## Reviewing transactions and transfers

For a credit card paid from a checking account, start with:

```sh
fin settlement candidates
fin settlement review
fin settlement review --all
```

`--all` records every listed settlement at one confirmation, setting up each card once. Add
`--card ID` to take one card. `--yes` works once a card is set up, as setting one up needs a terminal.

For the experimental full-screen review, run `fin settlement review --tui`.
Use the up and down arrows (or `j` and `k`) to choose a suggestion, Enter to
review it, and `y` to apply. Press `n` or Escape to return to the list; `q` or
Ctrl-C exits without applying a pending proposal. Resize the terminal if it is
too small to show all proposed changes. The full-screen mode requires a terminal
and cannot be combined with `--yes`.

For BoursoBank, fin recognizes its deferred-card debit description and proposes
a match only when the complete masked card number belongs to exactly one card
account at the same institution and bank, in the same currency. A card account's
own number is one of its cards; see [Cards](#cards) for a replaced card's. It excludes suggestions
when an opposite payment on that card is already present within seven days, or
the card is already linked to a different checking account. These are suggestions,
not automatic accounting decisions. Select a transaction by its event ID. If
needed, fin proposes mapping the card to a liability account and linking it to
the checking account alongside the settlement. One final confirmation applies
the entire proposal atomically; declining or interrupting before confirmation
leaves both the setup and settlement unchanged. Only the selected settlement is
recorded; similar suggestions remain for review.

### Cards

A bank can replace a card, when it expires or is lost, or add one, without changing
the card account behind it. The new card has a new number, so the settlement debits
of the old one name a number that is not the account's: fin cannot match them, and
they stay out of `fin settlement candidates`.

```sh
fin card list
```

lists every card your statements name: the card account it belongs to, or
`unassigned`; how fin knows (`statement` for an account's own number, `assigned` for
one you recorded); the bank rows naming it and their dates; how many are settled or
undecided; and from when the account has purchases of its own. Purchases name the
card account, not the card, so fin cannot tell which of two cards on one account
bought what.

If you know an unassigned card belonged to a card account, record it:

```sh
fin card assign CARD ACCOUNT-ID
```

`CARD` is the number as `fin card list` prints it, or a unique ending such as its
last digits; `ACCOUNT-ID` is the card account's ID from `fin account list`. fin shows
the card, the account and the rows naming the card, asks before recording, then says
how many of its debits `fin settlement candidates` now offers. `fin card unassign
CARD` removes an assignment that no recorded settlement uses. Assignments are in
`fin decisions export` and come back with `fin decisions restore`.

Settling a card paid before its account's statements begin leaves the account with
payments and no purchases behind them; `fin card list` warns about it. Export the
account's older statements from the bank if you can; otherwise categorize those
payments as spending instead of settling them.

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

Find the bank debit in `fin operation list`, then use `fin operation show ID` to
find its source event ID and replace `123`
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
fin operation review
fin transfer candidates
```

Unclassified movements initially use `equity:unclassified`. Imported bank
categories are suggestions. To classify or re-review a movement interactively,
find its operation ID in `fin operation list` and replace `123` below with it:

```sh
fin operation category review 123
```

To assign or change categories without the interactive batch review, put the
account first, followed by one or more operation IDs:

```sh
fin category add expenses:food:restaurants
fin operation category set expenses:food:restaurants 123 456
```

The category must already be in your catalog, for `set` and `apply` alike, so that a typo cannot
create a new one; add it with `fin category add`. Only `equity:unclassified`, which returns a movement to review, needs no entry.

To assign the transactions selected by any `fin operation list` filters, pipe its JSON
output into `set` using `-` in place of an operation ID:

```sh
fin operation list --type withdrawal --decision unclassified --json |
  fin operation category set expenses:cash-withdrawals -
```

With `-`, fin shows how many transactions would change, what they are categorized as now and a few
examples, then asks `[y/N]` on the terminal. `--yes` skips this for scripts; without a terminal the
command fails unless you pass it. Operation IDs you type are not asked about.

To put back the last batch of category changes:

```sh
fin operation category undo
fin operation category undo CHANGE-ID   # the batch holding this change in the history
```

It shows what would change and asks `[y/N]`; `--yes` is for scripts. Each transaction returns to its
earlier category and reason, or to having no decision. Changes within a second of each other count as
one batch. An older batch is refused if any of its transactions changed since,
and undoing an undo redoes it. Decisions on transfers and settlements are not undone this way.

Both commands must use the same database. You can combine `-` with explicit
operation IDs. An empty list makes no changes. Like `set` with explicit IDs, this
replaces existing categories; include `--decision unclassified` if you only
want to assign unclassified transactions. Invalid input or any invalid ID
rejects the whole batch.

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

For a fast, one-at-a-time workflow, run:

```sh
fin operation category queue
fin operation category queue --type card
fin operation category queue --type card --type withdrawal
fin operation category queue --type card --institution boursobank
fin operation category queue --key f=expenses:food:restaurants --key r=expenses:home:rent
```

Use `--type` to include only movements of a given effective type; repeat it to
include multiple types. This includes manual type overrides and provider-specific
types, and `--type unknown` selects movements without a type. The count in the
queue covers the filtered selection. `--institution` takes one provider the same way.

The full-screen queue shows unclassified transactions, with incoming amounts in
green and outgoing amounts in red (zero is uncolored). Set `NO_COLOR` to a
nonempty value to disable these colors. A displayed letter assigns
a category and advances. By default it also selects all other
unclassified transactions with the same effective description in the current
queue, across institutions. Matching ignores ASCII case and repeated whitespace;
empty descriptions are never matched. The screen shows the number of other exact
matches. Press `x` to toggle this batching for the session; `u` undoes the entire
last batch. A `--type` or `--institution` filter limits the batch to the filtered queue, and skipped
transactions are not included. `s` skips a transaction until the next session. `/` searches all categories: type a substring, use Up and Down, and
press Enter to assign the highlighted category. `b` browses the hierarchy from
`expenses` and `income`: use Up and Down to select, Enter or Right to open a
branch or assign a leaf, and Left or Backspace to go up. Space assigns a selected
category even when it has children. A `+` marks a branch; intermediate groups
that are not catalog categories cannot be assigned. `a` adds a category one segment at a time and
returns to the same transaction: each level lists what exists, typing filters it, and `+ new` adds
what you typed. `Enter` or `Right` goes deeper, `:` does too while typing a whole path, `Left` goes
back, and `= add` makes the level you are on a category. The category then comes first in the
menu with its key, ready to assign, using capitals and digits once lowercase letters run out. `u` undoes the last staged assignment.
Assignments and newly added categories remain in memory until `q` saves them
atomically and quits. Ctrl-C or terminal disconnection discards staged changes.
If a transaction changes elsewhere during the session, saving is refused and the
queue remains open; no staged changes are applied. Undo the affected batch or
press Ctrl-C to discard and start again.

Fin assigns available letters automatically for the session. Repeat `--key LETTER=ACCOUNT` to choose shortcuts for existing categories; lowercase `a`, `b`,
`q`, `s`, `u`, and `x` are reserved. Bindings are stable during the session, and
explicit `--key` options can be reused on later invocations. Categories without
a displayed shortcut remain available through search or the hierarchy. A terminal
large enough to show the full transaction and selected category is required.

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


Some rows are one activity because the institution says so. A Wise conversion has a
debit, a credit in the other currency and usually a `FEE-` row, linked by their
references, so `fin operation list` shows them as one row and `fin operation show`
accepts any of their IDs. A conversion shows only the rows imported so far. A row in
a confirmed transfer stays out of the group. These rows still post separately in the
journal.

Confirmed same-currency transfers export as one journal transaction with direct
postings to both bank accounts, retaining both source event IDs and any fee postings.
Fin uses the earlier booking date as the transaction date and attaches the later
booking date to its bank and fee postings. Between those dates, date-filtered reports
reflect the two bank balances without a separate in-transit asset; the books may
therefore appear temporarily unbalanced by date. Fin keeps both source movements
and review history in SQLite. Regenerate the journal after confirming or undoing.

For a transfer involving a wallet, resolve its movement first, then link the two
single-movement operations:

```sh
fin transfer link FIRST-OPERATION-ID SECOND-OPERATION-ID
fin transfer unlink OPERATION-ID
```

Use `--first-role` or `--second-role` if a movement's role is not `primary`.
The movements must have equal and opposite quantities of the same resolved
commodity, different accounts, and one source each. A bank movement must be
unclassified and have no fee. Bank-to-bank pairs use `fin transfer review` and
`fin transfer undo`. `--yes` skips the confirmation prompt.

`fin decisions export` saves confirmed transfers. Restoring matches each primary movement by
source identity and refuses missing, ambiguous or changed evidence. Rejected candidates and audit
history are not saved; `fin backup` keeps those.

`fin transfer unmatched` lists each unclassified transfer, deposit or withdrawal that has no candidate,
and why: the opposite amount is already decided or was ruled out, the same amount is up to 30 days
away, an amount differs by what may be a fee, or nothing among your imported statements is a partner.
The last is usually a payment to or from someone else, or an account you have not imported. Add
`--institution NAME` to take one provider, or `--json`.

`fin transfer candidates` shows possible pairs of unclassified movements, their
evidence and ranking. Categorized movements are excluded until returned to
`equity:unclassified`; previously reviewed pairs remain visible with `--all` and
in transfer history. Candidates are shown by descending score, with event IDs
breaking ties; interactive review follows the same order. Scores are not calibrated
probabilities, and a higher score does not guarantee that a pair is a transfer.
The `--all` output appends unranked unavailable or reviewed pairs. Run `fin transfer review` to inspect
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
reversal evidence. Types follow the rules at once; refresh source evidence with `fin operation refresh-evidence --institution boursobank`.

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
fin operation category apply categories.json
fin transfer review transfers.json
fin journal project
fin operation category history
fin transfer history
```

After categorizing a transaction, preview similar unclassified movements with
its event ID (replace `123` with a reviewed transaction ID):

```sh
fin operation category similar 123
fin operation category similar 123 --json
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

`fin operation status` summarizes imported bank movements by accounting decision and
shows the percentage classified. Classified means a category, confirmed transfer,
settlement or reversal; categorized counts income and expense categories alone.
Both percentages use all imported bank movements as the denominator. An empty
database shows `n/a` rather than a percentage. `fin operation status --json` emits counts
and numeric percentages, with `null` percentages when there are no movements.
The category breakdown lists exact assigned accounts without rolling up parent
categories. The movement-type breakdown includes manual type overrides and
`unknown` for movements without a type. Both show counts, not amounts. Transfers
and reversals are counted per movement, not per pair. The top-level
`fin status` separately summarizes account coverage and import progress.

## List operations

`fin operation list` lists one row per stored operation, including reviewed
ones, newest first. Each row has an operation ID, date, amounts, institution,
account, type, counter-account, description and state. The `AMOUNTS` column
shows each movement's amount and commodity together. Use `fin operation show ID`
for individual movements and source event IDs used by transfer and settlement commands.
The table shows calendar dates; timestamps use their UTC date. The detail view
and JSON retain exact source times, and default sorting uses the full occurrence.
Account IDs
come from `fin account list` and are `-` when no bank account is mapped. The
`ID` column is the database-local operation ID; it may change after a
rebuild. `STATE` marks operations that cannot be posted yet: `unresolved` when
a movement has no account, `unreviewed` when the operation has no bank review
row at all, and `needs interpretation` when no movement was proposed. For
CDC rows with no proposed movement, the amounts column shows
the original source fields and mark the amount `reported`; these fields are not
account movements. JSON exposes `reported_amount` and `reported_currency`
separately from `amount` and `currency`.

CDC movements can be inspected with `fin operation show` and resolved;
confirmed two-movement exchanges are supported, while general categorization
is not yet available. Date filters use each operation's occurrence date in UTC
for timestamps.

Confirmed bank transfers appear as one operation with both movements; account
and institution filters match either movement. Undoing a transfer separates
the original operations again.

Use `fin db event operations EVENT-ID` to inspect the operation linked to an
imported bank movement or CDC source; add `--json` for commodity
identities and source references. Confirmed transfers show both movements and
source events together. Undoing a transfer exposes the original operations
again. An account shown as not assigned in the operation can still have a
bank-to-ledger mapping, shown by `fin account list`; the operation does not
duplicate that mapping yet.

The table shows the movement type and the counter-account used in the exported
journal. `-` means the movement remains unclassified (`equity:unclassified`).
Confirmed transfers show the other bank account; reversals show their clearing
account. On a terminal, the table opens
in `$PAGER` (default `less -FRX`); an empty `PAGER` disables paging. Redirected
output and `--json` go directly to stdout.

```sh
fin operation list
fin operation list --from 2026-01-01 --to 2026-01-31
fin operation list --search coffee --limit 20
fin operation list --type interest
fin operation list --category expenses:food:restaurants
fin operation list --type card-settlement --decision settlement
fin operation list --type unknown --decision unclassified
fin operation list --asc
fin operation list --order-by currency,amount --asc
fin operation list --order-by institution,date --desc
fin operation list --institution cdc
fin operation list --json
```

Dates are inclusive booking dates. Use `--account ID` for one account or
`--institution NAME` for a provider; `--search` matches the operation description
using ASCII case-insensitive comparison. `--type`, `--decision` and
`--category` select an operation when any of its movements matches, so a
CDC exchange is listed under either of its movement types. `--type`
matches the displayed movement type, including provider-specific values;
`--type unknown` selects movements without a type. `--decision` filters by
accounting decision kind, not by
counter-account. `--category ACCOUNT` (also spelled `--counter-account`) matches
the exact counter-account, including `equity:unclassified` or a transfer account;
it does not include subcategories. `--from` and `--to` compare a movement's
booking date, so an operation with no bank movement (such as a CDC row)
is excluded by any date filter. Filters combine and are applied before sorting
and `--limit`.

Without `--order-by`, sorting uses the source event date or timestamp in UTC,
newest first. Date-only events sort before timed events on the same date. Use
`--asc` or `--desc` to choose a direction; descending is the default. To sort by
one or more table columns, pass a comma-separated list to `--order-by`. Supported
names are `date` (booking date), `amount`, `currency`, `institution`, `account`,
`type`, `counter-account`, `description`, and `event`. The aliases `booked_on`,
`account-id` / `account_id`, `movement_type`, `counter_account`, and `event-id` /
`event_id` are also accepted. Text sorting is case-sensitive; missing types and
unclassified counter-accounts sort as `-`. The selected direction applies to all
columns, with operation ID as the final tie-breaker. Sorting precedes `--limit`.
Amounts are compared exactly, but not converted between currencies; use
`--order-by currency,amount` to group them by currency. Rows without a bank
movement sort last on bank columns, since they have no booking date, amount or
type.

SQLite applies sorting, filters and the limit before Fin loads the displayed
review data. Its decimal collation compares stored amount text numerically.
A matched Wise CSV can provide the
effective description; missing or conflicting values fall back to the primary
description. JSON contains one effective `description`, `movement_type`, and
`detail_sources` companion row IDs, which can be inspected with
`fin statement companions`. Provider-specific types are prefixed with the provider
name. A reported transfer type does not confirm a transfer between your accounts.

To list types and correct an individual movement's type:

```sh
fin operation type list
fin operation type show OPERATION-ID
fin operation type set fee OPERATION-ID
fin operation type set bonus OPERATION-ID OTHER-OPERATION-ID
fin operation type history
fin operation type reset OPERATION-ID
```

`type list` shows common types and provider-specific types observed in imported
data; use one of those values with `type set`. `unknown` is a list filter, not a
type. `type set` applies all provided IDs atomically; if an ID is invalid, none
are changed. For a multi-movement operation, `type show`, `set` and `reset`
require `--source-event EVENT-ID` to select the side shown in `fin operation
show ID`; `set` accepts one operation ID when this option is used. Category
and rule commands refuse multi-movement operations. Manual types are overrides: they do not change the source movement or its
accounting classification, survive reimports and rule changes, and take
precedence over derived types. `type show` and transaction-list
JSON expose the imported type and any override. `type reset` removes the override
and returns to the imported type. The append-only history records changes;
`fin operation type history --json` is available for scripts.
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
[BoursoBank](../institutions/boursobank.toml), [Wise](../institutions/wise.toml),
[Bunq](../institutions/bunq.toml) and [Revolut](../institutions/revolut.toml).
`fin institution definition show INSTITUTION` prints the resolved definition as JSON.
`fin institution show INSTITUTION` summarizes one in plain text: its kind, what fin
reads from each source column by role, the columns it does not read, what rules
can match (type rules test the main description, or any field a rule names, such as
a column of the CSV), its default rules and, when a database exists, how many
movements are imported by type. It also works for installed definitions and for
movement-CSV institutions such as `cdc`, where it shows the columns read, how each
reported kind becomes an operation, and the imported operations.

There is one kind of type rule. A bank's own type code is just a column: Wise's
definition maps its `Transaction Details Type` column and, ahead of it, the prefix
of the `TransferWise ID` column (`FEE-` to `fee`, `BALANCE_CASHBACK-` to `interest`);
Revolut's maps its `Type` column (Card Payment and Card Refund to `card`, Deposit to
`deposit`, Transfer to `transfer`, ATM to `withdrawal`, Charge to `fee`). Both end
with a catch-all that keeps any other code as a provider-specific type
(`wise:{value}`, `revolut:{value}`). The first rule that matches, in priority order,
decides. Types are derived from the rules, so editing a rule retypes imported movements at
once. The bundled rules are
installed into a new database; an existing database needs them added with `fin rule
add`, or rebuilding.

#### See everything stored about a transaction

`fin operation show OPERATION-ID` prints what fin knows about one operation in plain text: its
type, category and confirmed transfer; the source event (institution, account,
kind, source identifier, occurrence and retrieval time); where it was imported from
(the stored document with its hash and import time, every file it was read from,
and the statement with its period and balances); the source's own fields as stored
(the payload, indented); and each matched companion row with every column under its
header name, the match state and policy, the display name and tags it gave, and the
value the type rules matched. It changes nothing. Companion rows include the
personal columns of the file, such as a cardholder's name, so be careful when
pasting the output. `fin rule explain OPERATION-ID` shows how the type was decided.
The output identifies its source event for use with `fin db event operations`.
A source event that has no bank movement, such as a
CDC row, has no review detail; `fin operation show` then prints the
operation's movements instead, marking any movement whose account is not
assigned yet.

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
  transaction, and `fin operation show` lists them under "Source columns".

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

`fin rule explain OPERATION-ID` traces how one
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
change and institution-definition change, and records each change in `fin operation type
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
`wise-id-cashback`. The bundled cashback rule assigns `interest` to balance
cashback. An existing database retains its installed rules; to update the old
`bonus` rule, run:

```sh
fin rule add wise-id-cashback --institution wise --field 'TransferWise ID' \
  --description '^BALANCE_CASHBACK-' --set-type interest --case-sensitive \
  --automatic --priority=-99 --replace
```

Fin recomputes imported types after the replacement; manual type overrides and
accounting categories remain unchanged. Add your own rules in the same way, for
example to type card orders:

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
`fin operation review` for unclassified items and hledger for accounting
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
fin operation refresh-evidence --institution mybank
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
disabling or replacing a rule affects future imports. `fin operation refresh-evidence`
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
fin rule preview cafes
fin rule apply cafes
```

Replace the ID, name and text with your own. The text must appear in the example's
effective description and is treated literally, not as a regular expression. Omit
`--contains` to use the whole description, or use `--institution NAME` to restrict
scope. The draft displays matches without changing anything; fin then asks before
saving a review-mode rule, and `--yes` skips that confirmation. Saving a rule does
not classify transactions or enable automatic application. Use `fin rule mode`
to opt into automatic behavior, noting import-time rules see the primary source
description. `fin decisions export` saves stored rules.

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
accept the common or observed provider-specific types from `fin operation type list`.
An institution, description or type condition is required. When both a type and
category are proposed, each is assessed separately. Existing manual type overrides
and reviewed accounting decisions are not overwritten: the preview explains any
skipped action, while another eligible action on the same transaction can still
be applied. A type already matching the proposal does not create a redundant
override. Use `fin operation type reset` or the relevant accounting review workflow to
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

To replace existing categories on matching transactions, first inspect the same
selection that `--force` will apply, then run:

```sh
fin rule preview cafes --force
fin rule apply cafes --force
```

This replaces different categories, including manual ones. Transactions already
in the target category are skipped. Confirmed transfers, settlements and
reversals remain protected, and `--force` does not replace manual type overrides.
Confirmation is still required unless `--yes` is also given.

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
dependency. Version 1 account snapshots remain compatible; databases use schema 33 and
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
statement metadata uses `Temporal.Date` or `Temporal.Instant`. Revolut CSV records
use the completion calendar date, or the start date for a reverted row; the source
fields retain both clocks without assuming a timezone.
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
| `fin operation --help` | Imported transaction listing |
| `fin statement --help` | Statement reconciliation, stated balances and totals, and companion files |
| `fin convert INSTITUTION PATH...` | Print statement files, directories and ZIPs as records; `--definition` for any other CSV |
| `fin records check FILE` | Validate a statement converter's output |
| `fin reversal --help` | Withdrawal reversal candidates and review |
| `fin operation --help` | Transaction listing, review and categories |
| `fin transfer --help` | Candidates, preview and explicit decisions |
| `fin journal --help` | hledger journal export |
| `fin backup` | Copy the database, including account settings |
| `fin restore BACKUP` | Restore a complete database snapshot |
| `fin delete` | Delete all fin state, after confirmation |
| `fin db --help` | Lower-level database and source-event commands |

For detailed options, put `--help` after the specific command. Argument errors
exit with status 124; operation failures exit nonzero and print a diagnostic.
Check the per-file results after an import failure before retrying.
