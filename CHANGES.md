# Changes

Entries go at the top, under Unreleased, in the same commit as the change. Say
what a user of `fin` must know: new or changed commands and options, imports that
are now accepted or rejected, output format changes, and any **Action required**,
such as rebuilding the database after a schema change. Internal changes get a
short line of their own.

## Unreleased

### 2026-10-30

- Make BoursoBank import-time source typing configurable, with bundled defaults
  and `fin institution typing show`, `set` and `reset`. User-added institutions
  can define profiles too. Changes apply to new imports; preview and confirm
  `fin tx refresh-types` to update existing source types. Manual overrides and
  accounting decisions remain intact. Wise companion type mappings remain in
  its adapter. **Action required:** schema 21 requires rebuilding the database.
  Before upgrading, use the old executable to back up and export account settings;
  after reimport, inspect and recreate review decisions and rules.

- Extend previewed rules with optional effective-type conditions and movement
  type actions, independently or alongside category decisions. Preserve reviewed
  overrides and classifications, and record type decisions in audited history.
  **Action required:** schema 20 requires rebuilding the database; use the old
  executable to back up and export settings before upgrading, then inspect and
  recreate review decisions and rules.

- Add `bonus` and `fin tx type list`, `show`, `set`, `reset` and `history`.
  Manual type overrides are audited, preserve imported evidence and accounting
  classifications, and survive reimports and type refreshes. **Action required:**
  schema 20 requires rebuilding the database; back up and export account settings
  with the old executable, then inspect and recreate review decisions.

- Replace the copied Notty sources with a pinned Git submodule tracking a fork
  with its upstream history and OCaml compatibility fix. **Action required:**
  initialize submodules before building from a Git checkout.

- Align the pre-commit formatting check with Dune's vendored-source exclusion.

- Type BoursoBank `AVOIR` card refunds as `card` and ATM withdrawal reversals
  as `withdrawal`, retaining their positive amounts. Reversal suggestions use
  the provider's source wording as evidence; refresh stored types with
  `fin tx refresh-types`.

- Add full ATM withdrawal reversal candidates, review, undo and audit history.
  BoursoBank `ANNUL RETRAIT DAB` movements receive a `withdrawal` type.
  **Action required:** schema 20 requires rebuilding the database; back up and
  export settings with the old executable before upgrading. Review decisions
  need to be inspected and recreated after reimport; see the manual.

- Add `fee` and `cheque` movement types and recognize BoursoBank card fees,
  fee refunds, issued cheques, cheque deposits, additional interest labels and
  malformed deferred-card labels found in exports.
  Run `fin transaction refresh-types` to update previously imported movements.

- Add `fin transaction list --type` and `--classification` filters; use
  `--type unknown` to find transactions without a movement type.

- Record BoursoBank deferred-card statements as `card-settlement` movement types.
  Add `fin transaction refresh-types` to preview and refresh existing types from
  stored source evidence without deleting transactions or review decisions; decline
  the confirmation to preview without applying changes.

- Add `tx` and `txn` aliases for `transaction`, `acc` for `account`, and `inst`
  for `institution`.

### 2026-10-29

- Add `fin transfer undo FIRST-EVENT-ID SECOND-EVENT-ID` to preview and undo a
  confirmed transfer, retaining audit history and returning both movements to review.

- In transfer review, selecting all supported pairs now goes directly to the
  batch preview; remaining candidates are left for later review.

- Show each transfer pair before asking for a choice, and distinguish choices
  for the current pair from actions on the pending batch.

- Stage transfer review decisions in a guided batch, including a shortcut for
  supported pairs and a single final confirmation. Cancellation leaves no changes.

- Combine transfer ranking and candidates in `fin transfer candidates`, with
  `--all` for reviewed and excluded pairs. `fin transfer review` can now confirm
  a pair interactively using event IDs; JSON batches remain supported. **Action
  required:** replace `fin transfer preview` and update scripts for the new
  candidates JSON structure.

- Rename `fin transfers` to `fin transfer` for consistency with other command
  groups. **Action required:** update scripts using the old command name.

- Add an `interest` movement type and recognize BoursoBank interest descriptions
  at import. Existing stored types can now be refreshed with
  `fin transaction refresh-types`.

- Show accounting classifications, including card settlements, in transaction
  listings and page interactive tables. Avoid reloading all movements for each
  settlement in a rule batch.

- Replace the `fin bank` command group with `fin transaction review`,
  `fin transaction category`, `fin institution history`, `fin statement list`,
  `fin statement companions`, `fin statement match`, and `fin journal project`.
  **Action required:** update scripts that use the old command paths.

- Add `fin rule rename OLD-NAME NEW-NAME`, preserving existing decision history.

- Explain literal regex metacharacters in rule help and invalid-expression errors.

- Use `[Y/n]` confirmations throughout the CLI; Enter accepts the displayed
  proposal and `n` declines it, including for deletion and restore.

- Add reusable `fin rule` regular-expression rules for income and expense
  categories and linked-card settlements. Preview matches and explicitly confirm
  each atomic batch; importing transactions never applies rules automatically.

- Add `fin account link-card` and guided settlement discovery for unambiguous
  BoursoBank deferred-card debits. Card setup and each settlement require review;
  matching card-side payments suppress single-movement suggestions. The guided
  review selects by event ID and applies setup and the selected settlement in a
  single transaction after confirmation; interruption before confirmation leaves
  both unchanged.

- Add an experimental `fin settlement review --tui` full-screen queue using
  vendored Notty. It shows pending changes before confirmation, and Ctrl-C or
  quitting leaves unapplied changes untouched.

- Add `fin settlement review EVENT-ID` to confirm a single bank debit against
  its linked card liability, plus `settlement undo` and audited history. Document
  the credit-card setup and settlement workflow with an account settings example.

- Add `fin institution list`, `add` and `delete`, and allow named generic
  institutions to import CSV files or directories, reviewing account labels per
  file. Update welcome and initialization guidance to explain institutions. The
  former `fin bank list` has been removed. Deletion is limited to user-added
  institutions without imported transactions.

  **Action required:** schema 20 requires a database rebuild; export settings and
  back up with the old executable first. Recreate generic institutions and CSV
  layouts after rebuilding, and inspect review decisions before reapplying them.

- Add `fin bank list` to show built-in importers and their aliases.

- Show separate bank and generic CSV forms in `fin import --help`. **Action
  required:** replace `fin import csv FILE` with `fin import FILE`; directories
  require an explicit bank name.

- Accept registered institution aliases in `fin import`, including `bourso` and
  `boursorama` for BoursoBank.

- Store primary movement types at import. Show BoursoBank types derived from
  explicit OFX description prefixes, including `PAIEMENT CARTE` on checking
  accounts, without treating a reported transfer as a confirmed owned-account
  transfer.

  **Action required:** schema 14 requires rebuilding the database from statements.
  Use the old executable to export account settings and back up the database before
  upgrading. Review decisions
  are tied to event IDs and must be inspected before reapplying them; see the manual.

- Identify Bunq MT940 exports by their contents instead of treating every
  `.mt940` file as Bunq. Wise MT940 files are reported as unsupported.

- Show merchant names and movement types from uniquely matched Wise CSV rows in
  transaction listings and review. Similarity review groups card purchases by
  merchant when available. After rebuilding the database for schema 14, run
  `fin bank match` if companion matches are not current.

- Add `fin transaction list --order asc|desc`, sorting by source event date or
  timestamp before applying the limit; descending remains the default.

- Add interactive `fin bank categories review EVENT-ID` to classify a movement
  and selected matching movements together after an explicit preview and
  confirmation. Concurrent classifications are protected from overwrites.

- Add `fin bank categories similar EVENT-ID` to preview unclassified matches
  and conflicting reviewed classifications without applying decisions.

- Record the category-and-tags model, first review workflow and requirement to
  vendor Notty if it is selected.

### 2026-10-28

- Document category design, similarity-based review, reusable rules and a
  possible Notty terminal interface.

- Show the institution in the readable transaction list.

- Keep the `fin` welcome and initialization guidance current with transaction
  listing and check all top-level commands appear in the welcome.

- Add `fin transaction list` with account, provider, date and description
  filters, a result limit and JSON output.

- Show numbered choices in interactive CSV setup and retry invalid selections.

- Retry empty or invalid generic CSV provider names without suggesting a real
  provider in the prompt.

- Show source amounts alongside signed account movements in the CSV preview.

- Fix Arch package man-page generation after renaming `fin account`, and cover
  every top-level command with a generated man page.

- Rename the `fin accounts` command group to `fin account`. **Action required:**
  update scripts and aliases to use the singular command.

- Show stable account IDs and add `fin account delete ID`, with preview,
  confirmation and checks for shared evidence and relationships; deleted
  development imports can be reimported.

- Allow asset and liability ledger mappings; new generic prepaid and credit cards
  use card-specific asset and liability account defaults.

- Add interactive generic card CSV import with suggested mappings, separator
  detection, provider-scoped identities, saved layouts, preview and account-scoped
  overlap checks. **Action required:** use `--provider NAME` for noninteractive
  generic imports; delete and reimport legacy `csv` accounts under their provider.

- Add a synthetic, CPU-timed `fin status` performance regression test.

- Share companion CSV wiring and CSV-only adapter construction in
  `Adapter_support`, completing the migration of BoursoBank, Bunq, Wise and
  Revolut CSV handling to `Csv_reader` and `Csv_schema`.
- Move Revolut CSV row decoding onto `Csv_schema`, preserving reverted-record
  handling, per-currency running-balance checks and duplicate occurrences.
- Move Wise CSV row decoding onto `Csv_schema` with UTC clock decoding and
  explicit cross-field date, direction, reference and fee/FX checks.
- Introduce `Csv_schema` with typed cell decoders, compiled column selectors and
  row/column diagnostics, and move BoursoBank and Bunq CSV row decoding onto it.
  Normalized values, raw companion fields and matching policies are unchanged.
- Share CSV reader, UTF-8 and header handling across the four bank formats.
  Each format retains its existing delimiter and header normalization rules;
  raw fields, identities and matching policies are unchanged.
- Record a design for shared CSV decoding and typed format profiles, including
  primary/companion roles, semantic hooks, staged migration and compatibility
  checks. Documentation only; CSV import behavior is unchanged.
- Add a personal-accounting guide for fin and hledger: choose a first period,
  import statements, review transfers and categories, produce spending reports,
  add verified opening balances and closing assertions, and establish a monthly
  routine. Explain current accounting limits and backing up manual journal files
  alongside SQLite snapshots.
- Rename statement ingestion to `fin import BANK PATH...`, with the same options,
  prompts, import rules and journal output. **Action required:** replace
  `fin bank sync` in scripts and commands; the old command has been removed.
- Add `fin restore BACKUP`, accepting a backup folder or SQLite file. It checks
  schema 13, SQLite integrity and foreign keys before replacing the destination
  through SQLite's transactional copy API. Replacing existing state asks for
  confirmation; scripts can use `--yes`. A missing database is created privately.
  Successful restoration clears its stale generated journal and account edit
  draft, retaining original statements and exported settings snapshots. Backup
  output now provides the restore command. No schema change is required.
- Restore status and account-listing performance after the Jsont migration:
  compute account identifiers once per account and use a 256-byte scratch
  buffer per JSON encode instead of repeatedly allocating 64 KiB. Larger
  outputs flush in chunks; nested encodes keep independent buffers. Output
  and schema are unchanged; no database rebuild is needed. On 4,370 movements,
  median warm status falls from 220 ms to 43 ms, matching the pre-Jsont build;
  JSON account listing falls from 187 ms to 20 ms.
- Replace Yojson and manual JSON-tree conversions with typed `Jsont.t` codecs
  for account settings, review decisions, bank payloads, stored lists and CLI
  reports. Domain validation and account patch semantics remain in force;
  source identities, payload layouts and schema 13 stay compatible.
- JSON integer IDs outside the exact interoperable range
  (−9007199254740991 to 9007199254740991) are encoded as decimal strings.
  Review files accept those strings; unsafe numeric IDs and fractional IDs are
  rejected. **Action required:** scripts handling such large IDs must accept
  strings and write them as strings in review files. No database rebuild is
  needed.
- Library users can use the exported codecs directly; account references now
  expose `reference_jsont` instead of JSON-tree conversion helpers.

### 2026-10-27

- Unify event occurrences and statement times as `Temporal.t` (`Date` or
  `Instant`), with shared `to_string`/`of_string_exn` conversion. This replaces
  `Source_event.occurrence`, `Document.time` and the document time conversion
  functions. Stored text, schema 13 and CLI output are unchanged.

- Status and account listings now ask SQLite for movement counts and per-account
  date ranges; status also queries the unclassified count. Only candidate legs
  and representative account movements are decoded. Account discovery reuses
  the existing source read. No schema
  change or database rebuild is needed.

- Speed up `fin status` by sharing loaded movements, batching review metadata
  queries and preparing known-account IBAN evidence once per candidate search.
  Review and transfer candidate listings also use batched metadata. Output and
  schema are unchanged; no database rebuild is needed.

- Wise CSV `booked_at` is now `Ptime.t` under a UTC assumption, replacing
  `booked_local`. Matching still uses the source-local CAMT clock and milliseconds;
  inspection displays the assumed-UTC time as canonical RFC 3339. Raw fields stay
  unchanged.
- OFX balance `as_of` is now `Ptime.t`. The lossless OFX reader adds `instant` for
  explicit typed conversion using OFX's midnight/GMT defaults and numeric offsets.
  Invalid balance clocks or unsupported timezone offsets are rejected. BoursoBank
  DTUSER remains raw evidence as `user_date_source`. Date-only
  values, schema 13, movement identities and journal output are unchanged.

- Revolut start and completion clocks are assumed UTC. `Revolut_csv.transaction`
  exposes `started_at` and `completed_at` as `Ptime.t` instead of local strings.
  Timestamp validation and ordering use Ptime. Payloads, source IDs, booking dates
  and schema 13 remain unchanged; no database rebuild is needed for this change.

- `fin backup DIR` creates missing parent directories privately, so a first backup
  to a new directory succeeds. Existing backup folders are never overwritten.

**Action required:** schema 13 refuses earlier databases unchanged. Export account
settings with the previous binary, back up, then delete/init/reimport statements
and restore the account snapshot. See the manual's storage instructions.
`fin db events` replaces `occurred_at_ms` with RFC 3339 `occurred_at`; statement
report instants and audit times now use canonical UTC text with 12 fractional digits.

- Instants use `Ptime.t` throughout the OCaml model, including source events, Wise
  booking and statement times, and import/review audit timestamps. Rel codecs store
  them as UTC text, preserving all picoseconds and indexed chronological sorting.
- Statement boundaries distinguish dates from instants. Raw bank timestamp text,
  Wise local booking days, source IDs and payloads remain unchanged. Offset-free
  Revolut clocks remain local source text. The `--at-ms` ingest input converts
  exactly at the CLI boundary, including valid pre-epoch dates.

- Internal: replace the OFX bank functor with ordinary functions. Parsing takes
  an explicit institution; probing and decoding need no bank parameters. Account
  identities, imports and journal output are unchanged.

**Action required:** use `fin accounts import FILE` instead of `accounts apply`;
there is no default account file. Existing version 1 JSON files still import.
`accounts edit CONFIG` and `bank sync --accounts FILE` have been removed: edit
settings in SQLite or explicitly import a snapshot after discovering the accounts.

- SQLite is the only account settings store. `fin paths` reports only `data_dir`
  and `database`; `config_dir` and `accounts_config` are gone. Init no longer
  advertises a config path. Edits and sync IBAN answers update only SQLite.
- Account exports are optional snapshots at a path you choose. They are never
  loaded or rewritten automatically. Backup copies the complete SQLite database,
  including account settings; delete removes database state and pending editor
  drafts, leaving exported snapshots and any old config files untouched.
- Sync no longer reports mapping or settlement changes, since it imports no
  account settings file. Schema 12 and the version 1 snapshot format are unchanged.

- `fin bank sync` imports ZIP exports automatically using Zipc, including Wise
  CAMT.053 downloads. Members appear in history as `archive.zip/member`; nothing
  is extracted onto disk. The read limit also bounds total expanded bytes, and
  archives have a 1024-member limit. Corrupt, encrypted and nested archive members
  are reported without preventing valid members from importing. Schema 12 and
  movement identities are unchanged.

- Fix transfer evidence when an IBAN is registered on another currency of the
  same bank account: that sibling no longer hides a genuine transfer. Different
  accounts and explicit IBAN contradictions still conflict (issue #1).
- On a terminal, bank sync asks for a missing account label and for the IBAN of
  newly imported accounts whose statements omit it. Currency accounts sharing a
  bank identifier are asked together. Enter skips an IBAN; replay does not ask
  again. Answers use the audited account settings and update an existing config.
  Scripts still require `--account-id` and never prompt for IBANs.

**Action required:** schema 12 refuses schema 11 databases. Back up, delete and
rebuild: `fin backup`, `fin delete`, `fin init`, then sync each bank's statements
again.

- Revolut exports with reverted transactions import: reverted rows, which Revolut
  lists with no completion date and no balance, are skipped instead of rejecting
  the whole file. Pending rows still reject it.
- `fin accounts edit` opens the accounts in your editor and applies the edits on
  save: ledger names, IBANs and card settlement links. Invalid edits apply nothing
  and can be edited again; the result is saved as the account configuration.
- Movements are stored normalized when they are imported (new `movements` table),
  and every read uses that table instead of decoding payloads again. Transfer
  candidates are a SQL self-join on currency and principal instead of a comparison
  of every pair of movements in OCaml: on 1,955 movements, `fin status` takes
  0.09 s instead of 0.62 s, `fin transfers candidates` 0.05 s instead of 0.57 s
  and `fin transfers preview` 0.09 s instead of 1.14 s, with identical output. An
  event whose payload does not decode is now rejected when it is imported.

**Action required:** `fin bank sync` takes the bank as its first argument:
`fin bank sync boursobank ~/statements/bourso` instead of
`fin bank sync ~/statements/bourso --institution boursobank`. `--institution` is
gone from `sync`, and so is its `boursobank` default.

- `fin accounts apply` without a configuration file says that none exists, that
  it is optional, and how to create one, instead of "No such file or directory".
  The manual's rebuild steps mark it as needed only with a configuration.
- `fin bank sync BANK PATH...` accepts any number of files and directories.
  Directories are searched recursively as before; a file named explicitly is always
  processed, even if hidden, and fails the command if no bank recognizes it. A file
  reached twice is imported once. `--help` describes both arguments.

**Action required:** schema 11 refuses databases created by earlier versions, without
changing them. Move the old database aside and rebuild it from your statement
folders (see "Storage and backups" in the manual).

- `make install` builds the Arch package and installs it with `sudo pacman -U`;
  on systems without pacman it stops and points to `make package`.
- `fin backup [DIR]` writes a consistent copy of the database, and the account
  configuration if any, to a new private `fin-backup-YYYYMMDDTHHMMSSZ` folder.
  `fin delete` lists all fin state (database, `bank.journal`, account
  configuration) and deletes it once you type `yes`; `--yes` skips the question.
  Neither needs a path. The unsupported-schema error points to them.
- `fin init` is the only command that creates a database. Every other command,
  imports included, fails with "fin is not initialized" until it has run, so a
  mistyped `--database` no longer starts an empty database. `fin status --json`
  loses its `initialized` field, now always true.
- `fin accounts apply` names every configured account that has no imported
  statements yet, with its bank, type and identifier, and says to sync its
  statement folder first. Before, it repeated "unknown source account" once per
  account without naming any.
- Currencies must be ISO 4217 alphabetic codes such as `EUR`. A file using any
  other code, such as `BTC`, or a lowercase one, is rejected and listed in
  `fin bank history`; before, any three-letter token was accepted. The kernel holds
  currencies as `Iso4217.t` from the `iso4217` library (31ae952).
- IBANs are validated against the SWIFT IBAN Registry (Release 103, 89 countries):
  country code, length, BBAN structure and check digits. A stored IBAN that no
  longer validates is an error. Error messages never include the account number.
  New library `finances.iban` (b4d468a).
- Schema 11: every bank is stored in the same generic tables (statements, companion
  rows and matches, day inventories, import contexts) instead of 18 bank-specific
  ones. There is no migration (c762e29).
- Bank commands are the same for every bank: `fin bank statements`,
  `fin bank companions` and `fin bank match` replace `bank boursobank import/csv`,
  `bank wise status` and `bank bunq status`. Import formats are named by their
  adapter (38e88c8).
- Banks are adapters behind one registry. Each adapter parses its files into a
  bank-neutral document and decodes its stored movements; the kernel names no bank.
  An import under an unknown institution is rejected and recorded, and
  `fin bank sync --help` lists the known ones (c9b08c8, 38e88c8).
- New library `finances.ofx`: a standalone, lossless OFX 2.x reader that BoursoBank
  is built on (17a752b).
- `fin init`, a first-run welcome and a user manual (`docs/manual.md`). `fin status`
  shows import coverage and review progress. Accounts, movements, transfer previews
  and `fin bank history` print readable text by default; the previous JSON is
  behind `--json` (ae68766, a3e1931).
- Internal: Core, Core_unix, Bigdecimal and ppx_jane replaced by Base, decimal,
  Ptime and Bos; quantities stay exact through local decimal contexts. The schema is
  defined with typed Rel tables. Journal output, JSON reports and source identities
  are unchanged (10975b5, 0075e37).
- Internal: `finances.opam` is generated from `dune-project`; pinned dependencies
  live in `finances.opam.template` (22a6482).

### 2026-10-26

- The command is `fin`. The database and account configuration default to the XDG
  data and config directories; `fin paths` shows them and `--database` overrides
  the database (1940c88).
- Arch Linux package with man pages, built with `make package`. Commands are grouped
  under `db`, `accounts`, `bank`, `transfers` and `journal`; the flat per-bank
  commands are removed. One schema baseline replaces the historical migrations,
  and older experimental databases are refused unchanged (c175682).
- Accounts: names, IBANs and card settlement links are managed through versioned
  configuration and `fin accounts`, with audited changes and dry runs (a4baedc).
- Transfers between your own accounts: same-currency candidate pairs, ranked by IBAN
  evidence, with explicit confirm and reject decisions that can be undone. Nothing
  is confirmed automatically (57119e2, a4baedc).
- Revolut imports: completed Current-account CSV records, with an account label you
  choose, exact net amounts and separate fee postings (57119e2).
- Bunq imports: reconciled MT940 statements, with the seven-column CSV as a
  companion (ec984f3).
- Reviewed categories for bank movements, with audited corrections (ec984f3).
- Inbox imports for several banks, with archived evidence, an import attempt
  history and account mappings. Wise CAMT.053 statements with the Wise CSV as a
  companion, and BoursoBank CSV enrichment of OFX imports (be4f64e).
- `fin journal export` writes a reproducible hledger journal; unclassified movements
  post against `equity:unclassified` (c120569).

### 2026-10-25

- Initial version: a SQLite store of immutable source evidence and balanced
  postings, and atomic OFX bank statement imports with replay protection and
  archived original documents (64b7ef4).
