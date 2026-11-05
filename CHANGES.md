# Changes

Entries go at the top, under Unreleased, in the same commit as the change. Say
what a user of `fin` must know: new or changed commands and options, imports that
are now accepted or rejected, output format changes, and any **Action required**,
such as rebuilding the database after a schema change. Internal changes get a
short line of their own.

## Unreleased

- The full-screen category queue and settlement review start at the top left instead of centering
  each line, so that shortcuts and categories line up.
- `fin operation category queue --institution NAME` reviews one provider's movements.
- `fin settlement review --all` records every settlement `fin settlement candidates` lists at one
  confirmation, setting up each card once.
- Speed up limited `fin operation list` pages by reviewing only selected
  movements and avoiding an unnecessary scan of operation IDs. Filters,
  ordering and grouping remain unchanged.

- Compare amounts in `fin operation list --order-by amount` with SQLite's
  decimal collation, including values beyond 40 decimal places. SQLite's
  decimal extension is compiled into Fin and initialized on each connection.
  **Action required:** builds need the unpublished Rel extension API commit;
  pin a checkout as described in the README. No database rebuild is needed.

- `fin operation list` filters, sorts and limits in SQL, and reads only the rows it returns: on about
  5,300 operations the full list takes half the time, and `--limit` now saves work (0.37 s to 0.10 s).
  Its output is unchanged. Commands that read reviews share the same query.
- Institution definitions have one kind for bundled and adapted sources: `records`, which holds a
  statement format (`camt`, `mt940` or `ofx`), a CSV layout (`csv`), or both. **Action required:** a
  definition file of kind `camt-csv`, `mt940-csv`, `running-csv`, `ofx-csv` or `movement-csv` is no
  longer accepted; start again from `fin institution definition show NAME`. Kind `csv`, which
  `fin institution add` writes, stays. No rebuild is needed for bundled institutions. A database
  that saved a definition of an old kind stops with an error naming its institution.
- CSV layouts gain `require`: conditions every remaining row must meet, or the file is refused.
  Revolut refuses a `State` other than `COMPLETED` or `REVERTED`, and Wise a `Transaction Type` other
  than `DEBIT` or `CREDIT`. A layout's columns must appear in its `header`.
- **Action required:** `fin import cdc --inspect`, with its `--summary` and `--json`, is gone. Use
  `fin convert cdc FILE`, which also needs no database. The count of identical rows went with it.
- `fin institution companion show` reads only what you saved; `fin institution companion example`
  prints a layout to start from. `fin institution show` lists a CSV layout's columns by role, and those it checks.
- The records format now checks running balances, for every source and whoever wrote the file: in a
  section with one source, each account's `balance_after` must equal the one before plus the row's
  movement, listed oldest or newest first. Merged sections are not checked, as exports can have gaps
  between them. This is new for Wise and any records file. A negative `fee` is refused from converters
  too, not only when a file is read.
- CSV layouts can state more about how a file's columns relate, and the bundled ones do, as their old
  parsers did: `require` conditions gain `matches`, `not_before`, `prefix_of`, the signs `not-positive`
  and `not-negative`, and `when`. Revolut refuses a completion before its start, a reverted row with a
  balance or a completion, a completed row without them, and another `Product`. Wise refuses a date that
  disagrees with its clock, a clock with no milliseconds, a `DEBIT` above zero, a `CREDIT` below zero
  and a missing `TransferWise ID`. bunq refuses an amount that is not comma with two decimals, and an
  account with a space.
- Every CSV layout: an unreadable balance, rate or valuation is refused, where it used to count as
  absent. Under a decimal comma, a dot in a number is refused. An `iso-date` cell with anything after
  the date, and an `iso-datetime` cell with no valid clock, are refused.
- Restoring a decision on a row imported through records now compares what identifies the row: its
  account, ID, date, movements, fee and group. A converter that later keeps more detail, or cleans
  a description, no longer blocks restoring decisions. Sources with no row IDs keep their check that
  identical rows cannot be told apart.
- **Action required:** rows imported through records have new identities, so exports from before
  the move no longer restore. The tool that translated them went with the old CDC adapter, and is
  in the history at 47c0043, with the steps in that version's manual.
- `fin import wise` now goes through records, as every bundled institution does. Its journal is
  unchanged, except that `wise-reported-fees` keeps the value as stated (`0.00`, not `0`). A file
  that states one row twice is refused, where it used to leave the row ambiguous.
- `fin import boursobank` now goes through records. Its journal is unchanged, except that 19 rows the
  adapter left ambiguous on real files now pair and gain their value date and tags. A CSV row whose
  description disagrees with the only candidate on its date and quantity stays unmatched, as before.
- `fin import bunq` now goes through records. Its journal is unchanged. Its CSV states EUR by
  definition, so a statement in another currency is left unmatched instead of being ambiguous.
- `fin import revolut` now goes through records and no longer takes `--account-id`: a person has
  one personal account, named `personal` in the ledger, as in
  `assets:bank:revolut:revolut:current-EUR:personal`. **Action required:** remove `--account-id` from
  your scripts, and map any account settings you saved under `main` to `personal` when you rebuild.
  A second account, such as a company's, can be imported as a records file naming it. Rows are stored
  with their date rather than an assumed UTC time, as Revolut's clock states no offset; the clock
  stays in their fields.
  Reverted rows, which the adapter dropped, are kept as operations with no movements and their
  amount as reported, so `fin operation list` now shows them; they post nothing to the journal.
- `fin convert` says on standard error how many rows a layout's `only` left out.
- `fin import cdc` now goes through records. Its operations and movements are unchanged, but their
  source events have a new kind, so rebuild with the schema 34 rebuild below. The message for two
  identical rows in one file is now `repeated id: identical rows cannot be told apart`.
- New `fin statement balances` lists the balances imported sources stated. Records files now
  import with their observations paired and their balances stored. **Action required:** schema 34
  requires a database rebuild: export your decisions, delete the database and import again, as for
  schema 33 below.
- `fin import fdm` imports a new institution through records, from its CSV or from a records
  file. It is the first to do so; the others keep their adapters until their records give the
  same result.
- Internal: `bin/main.ml` is split into one module per command group. No user-visible
  change.
- `fin convert` also reads BoursoBank OFX, bunq MT940 and the CSV files of Revolut, CDC,
  FDM, BoursoBank and bunq, and
  `--definition FILE` reads any other CSV from a file saying which column means what.
- New `fin convert wise PATH...` prints Wise CAMT and CSV files as records, without a
  database and without storing anything. `-o FILE` writes a file instead, compressed if
  its name ends in `.zst`. A row stated by several files appears once; if they disagree,
  nothing is printed. A statement with no entries adds no records and is not an error.
  A `.zip` file or a directory is opened and every statement inside is converted; other
  files are named on standard error and skipped.
- Building fin now needs zstd: `opam install conf-zstd` rebuilds `bytesrw` with it, and
  the Arch package depends on `zstd`.
- New `fin records check FILE` validates a records file, the JSON Lines format in
  `docs/records-format.md`, plain or compressed with zstd. It needs no database and stores
  nothing. fin cannot import records files yet.
- Errors from JSON input now name the place and nothing else, such as
  `movements[0].quantity: Expected string but found number`, instead of a trace of
  every enclosing value with terminal escape codes. This applies to every JSON file fin
  reads.
- Internal: the records format (`docs/records-format.md`, a JSON Schema and examples)
  for statements converted outside fin's adapters.
- Wise rows the bank states belong together now read as one operation: a conversion's
  debit, its credit in the other currency and its `FEE-` row appear as one row in
  `fin operation list`, and `fin operation show` works from any of their IDs. They
  group in either import order, and from CAMT alone. The rule is data, an optional
  `operation_group` in a CAMT definition. A row in a confirmed transfer stays out of
  its group. The journal is unchanged: each row still posts on its own.
- Internal: one module, `Operation_groups`, now decides which stored operations read
  as one; the operation list and operation lookups both use it. No user-visible
  change. A database where an operation is named by two transfer links now reports
  an error instead of hiding movements; fin does not create that state.
- One confirmation convention everywhere: a command that changes data shows what
  it will change, then asks `[Y/n]`, or `[y/N]` when it deletes data or replaces
  state (`fin delete`, `fin restore`, deleting an institution, rule, account or
  companion profile). `--yes` skips the question; without a terminal and without
  `--yes` the command prints what it would change and fails. Answering `n` is the
  way to preview. **Action required:** `fin operation restore`, `fin category
  restore`, `fin rule restore`, `fin transfer restore` and `fin operation exchange
  restore` no longer take `--apply`, and `fin rule from-category` no longer takes
  `--save`; drop those flags from scripts and keep `--yes`. Confirming an
  exchange or transfer and undoing a transfer now default to yes. A restore with
  nothing to change skips the question and prints its summary with zero counts.
- `fin operation export` and `fin operation restore` now say what they cover: the
  decisions on imported operations (categories, manual types, resolved accounts),
  never the operations themselves. Help text and manual only; behavior is unchanged.
- Institutions have a type, `bank` or `crypto exchange`, shown by `fin institution
  list` and `fin institution show`. Institution definitions and movement-CSV
  profiles declare it as `"type"` (`bank` or `crypto-exchange`, default `bank`);
  the bundled definitions state it explicitly.
- `fin institution show` without an argument gives an overview of every
  institution (type, origin, movements, operations, unresolved, first and last
  dates), and `fin institution show cdc` now describes the CDC profile instead of
  reporting an unknown institution.
- `fin status` reports operations that have no bank account, such as CDC rows, per
  institution; `fin status --json` has a new `operations` list. Before, CDC was
  missing from status. `fin institution list` lines now include the type, for
  example `wise (bank, built-in)`.
- Internal: the 9,500-line `lib/store.ml` is now the `finances.store` library
  (`store/`), split by domain into `Handle`, `Setup`, `Sources`, `Import`,
  `Ledger`, `Review`, `Restore`, `Transfers`, `Accounts`, `Explain` and
  `Operations`, each with its own interface; there is no `Store` facade.
  `Db_schema`, `Transfer_ranking` and `Category_similarity` moved with it.
  Library users `open Finances_store` and call the modules directly (for
  example `Review.bank_review`); no user-visible change.
- Reimporting a CDC file now checks each already-stored row's operation
  (occurrence, movement roles, commodities, quantities and source links) against
  the importer's interpretation and refuses a discrepancy with an error, leaving
  stored decisions untouched. A CSV with only the header is a statement of no activity,
  and imports with `inserted=0`.
- `fin import cdc` now imports through the same pipeline as
  every other venue: the bundled movement-CSV profile registers as a registry
  adapter, so files, directories and ZIP archives are scanned in one run with the
  standard per-file outcomes and summary, and the journal is published. Source
  events and proposed operations are stored exactly as before; ambiguous overlaps
  (identical rows in one file, or rows already imported from another file) are
  still rejected. **Action required:** `fin import` no longer previews movement
  CSV imports and does not take `--apply` or `--yes`; `fin import cdc FILE`
  imports immediately, like bank statements. `fin import cdc --inspect
  [--summary] [--json]` remains the read-only, database-free view.

- Add `fin transfer link` and `unlink` for reviewed transfers involving a
  resolved wallet movement. Portable transfer snapshots version 2 also preserve
  these links; version 1 bank snapshots remain readable. Restore movement
  resolutions and account mappings before transfer links.

- Add movement account and commodity resolution and explicit exchange review.
  Confirmed two-movement exchanges project to hledger using `equity:trading`,
  with explicit user-chosen crypto symbols. Operation exports version 5 include
  movement resolutions; confirmed exchanges use a separate portable export.
  **Action required:** older executables cannot read version 5 snapshots; keep
  your previous exports. Schema 33 requires a rebuild as noted below.

- Persist confirmed bank transfers as links between operation movements, with
  membership constraints and atomic synchronization with bank review and undo.
  **Action required:** schema 33 requires a database rebuild. Before upgrading,
  export accounts, categories, rules, transaction decisions and confirmed
  transfers, and keep a full database backup.

- Persist a shared operation-transfer link for each confirmed bank transfer,
  using the two stable movement identities. Operation list, detail grouping
  and portable transfer export now consume that representation. The bank
  decision and journal projection remain in place during the transition.

- The operation list displays UTC calendar dates consistently; exact source
  times remain in operation details and JSON and continue to determine sorting.

- Add `fin transfer export` and `fin transfer restore` for confirmed
  transfers. The portable links identify both primary movements using source
  identities and evidence hashes. Restore previews and applies atomically;
  rejected pairs and review history remain available only in a full backup.

- Category set/review, type set/show/reset, and rule explain/from-category now
  take operation IDs from `fin operation list`. Piped category input uses the
  `operation_id` JSON field. **Action required:** update scripts that passed
  source event IDs. For a multi-movement operation, type commands require
  `--source-event EVENT-ID`; other single-movement commands refuse ambiguity.

- Simplify `fin operation list` to one row per operation, with an `ID` and one
  `AMOUNTS` column. Source event IDs appear in `fin operation show ID`.
  **Action required:** replace use of the
  removed `--movements` option and update scripts parsing the text table.

- Add `fin operation restore --institution NAME` to restore decisions for an
  imported institution while leaving other institutions' decisions for later.
  The preview reports how many decisions were skipped; conflicts and missing
  records within the selected institution still block the atomic restore.

- CDC operations without interpreted movements show the source's
  reported amount and currency, marked as reported, and the state `needs
  interpretation`. JSON exposes `reported_amount` and `reported_currency`;
  these fields do not imply an accounting movement.

- `fin operation show` now accepts the operation ID shown in the `ID`
  column instead of a source event ID. **Action required:** update scripts and
  use `fin db event operations EVENT-ID` to inspect a source event.

- `fin operation list` now reads stored operations instead of the bank
  movements table: a bank movement is a one-movement operation and keeps its
  previous columns and a CDC row appears as one operation. Confirmed
  bank transfers appear as one operation with two movements. The `AMOUNTS`
  column shows movements together. The `ID` and `STATE` columns show the
  database-local operation ID and whether the operation is `unresolved` (a
  movement has no account) or `unreviewed`. Date filters use
  the operation's occurrence date. `--type`,
  `--decision` and `--category` now match when any movement of the operation
  matches; `--search` matches the operation description; sorting always happens
  in OCaml with the operation ID as the final tie-breaker. `--json` rows gain
  `operation_id`, `occurred`, `movements` and `movement_rows`; bank fields keep
  their names and are `null` on operations without a bank movement. `fin
  operation show` now prints the operation ID carrying the event, and shows the
  operation's movements for a source event that has no bank movement (such as a
  CDC row) instead of failing.
  **Action required:** scripts that parse the `fin operation list` JSON must
  tolerate `null` bank fields and the new keys; scripts that relied on event ID
  ordering as the final tie-breaker should sort explicitly with
  `--order-by event`.
- Rename the `fin transaction` command group to `fin operation` (beta breaking
  change): `fin operation list`, `fin operation show`, `fin operation review`,
  `fin operation category ...`, `fin operation type ...`, `fin operation
  status`, `fin operation export` and `fin operation restore` replace their
  `fin tx` / `fin transaction` spellings; `op` is the only shortcut for
  `operation`, and `tx` / `txn` / `transaction` no longer work. Source-record
  inspection moves to the `db` group: `fin db sources [--institution NAME]
  [--json]` replaces `fin transaction sources`, and `fin db event operations
  EVENT-ID` replaces `fin tx operation EVENT-ID`. Help text, the manual and
  README examples use the new names.
  **Action required:** update scripts and muscle memory from `fin tx ...` /
  `fin transaction ...` to `fin operation ...` (or `fin op ...`).
- CDC is now a bundled JSON profile (`institutions/cdc.json`:
  header, columns and per-kind rules) read by a generic movement-CSV interpreter,
  like the banks. `fin import` selects it through the institution profile, not
  by name. New kinds are profile entries, not code. Stored data is unchanged.
- `fin import cdc` accepts several CSV files and directories (searched
  recursively), like the bank importers.
- `fin backup` no longer requires the current schema, so a database that
  needs a rebuild can still be backed up first.
- Fold the `fin crypto` group into the general commands (beta breaking change):
  `fin import cdc FILE` previews, `--apply` stores and `--inspect`
  (with `--summary`, `--json`) reads without a database;
  `fin transaction sources [--institution NAME] [--json]` lists stored source
  records of any institution. Source JSON now includes `venue` and `kind`.
  **Action required:** replace `fin crypto inspect-cdc`, `import-cdc` and
  `list` in scripts.
- Add `fin import cdc` to preview and store CDC CSV evidence and
  proposed movements, and `fin transaction sources --institution cdc` to browse the imported sources.
  Identical document replay is safe; ambiguous overlapping or identical rows
  are rejected. Proposals do not create journal entries or verified balances.
  A full database backup is required to preserve these imports.

- Add persisted operations with source-linked, stable movement identities and
  exact commodity quantities; imported bank events get one operation each.
  Confirmed transfers group both operations on read, and undo restores the two
  original operation identities without rewriting source movements.
  Inspect the result with `fin db event operations EVENT-ID` (or `--json`).
  Existing bank reviews and exports retain event-ID compatibility; category and
  type decisions and their histories are keyed by the primary movement.
  Portable transaction snapshots do not yet include independent operations.

- Transaction decision exports use explicit movement roles. Restore continues to
  read versions 1–4; bank category decisions still require the `primary` role.

- Define an operation model linking source references to multiple proposed
  balance movements; use it in CDC inspection and evidence import.

- Add `fin import cdc --inspect --summary` to count transaction kinds and flag
  identical source rows for review without deduplicating them.

- Add an explicit core commodity identity for fiat and provider-scoped reported
  symbols, and a read-only `fin import cdc --inspect` CSV interpretation. Unknown
  patterns remain in review; no accounting entries or balances are created.

- Accept `YYYY-MM-DD HH:MM:SS` in generic CSV date columns; validate the clock,
  use its calendar date for booking and retain the original value as evidence.

- Replace `fin tx cat export/restore` with `fin tx export/restore` and include
  manual type overrides alongside category decisions. Restore previews conflicts
  and applies all decisions atomically; older category snapshots remain readable.
  **Action required:** update scripts to use the new command names.

- Change `fin tx type set` to accept the type first and one or more event IDs,
  applying the batch atomically. **Action required:** update scripts using the
  former `EVENT-ID TYPE` order or parsing the former success message.

- Add `--force` to rule preview and application to replace existing categories
  on matches, retaining protections for other accounting decisions.

- Add `fin tx list --category ACCOUNT` (also `--counter-account`) to filter
  transactions by their exact counter-account.

- Let `fin tx cat set ACCOUNT -` read a filtered `fin tx list --json` from
  standard input, and apply the selected transaction IDs atomically.

- Show transfer candidates and interactive review in descending score order,
  with event IDs breaking ties.

- Exclude categorized transactions from unreviewed transfer candidates and
  transfer review; resetting a category makes a movement eligible again.

- Add `fin tx list --order-by COLUMN,...` and `--asc` / `--desc` for
  multi-column sorting, with exact amount comparisons. **Action required:**
  replace `--order asc` and `--order desc` with `--asc` and `--desc`.

- Change `fin tx cat set` to take the account first, followed by one or more
  event IDs, and apply the batch atomically. **Action required:** update scripts
  using the former `EVENT-ID ACCOUNT` argument order.

- Stage category queue assignments and new categories until `q` saves them
  atomically; Ctrl-C discards the staged changes.

- Classify unclassified exact-description matches in the current category queue
  together, with a visible count, a toggle and whole-batch undo.

- Allow `fin tx cat queue --type TYPE` to restrict the queue to one or more
  effective movement types, including `unknown`.

- Show incoming queue amounts in green and outgoing amounts in red, with more
  space between transaction details and category controls. `NO_COLOR` disables
  the amount colors.

- Type Wise `BALANCE_CASHBACK-` movements as `interest` instead of `bonus`.
  **Action required:** existing databases retain their stored rule; replace
  `wise-id-cashback` as described in the manual to retype those movements.

- Separate portable snapshots into `fin cat export/restore` for the catalog,
  `fin tx export/restore` for transaction decisions, and `fin rule export/restore`
  for stored rules. Assignment and rule restore create missing categories
  atomically. Older combined exports can be restored section by section.
  **Action required:** update backup scripts to save all three new exports;
  `fin cat export` now contains only category names.

- Add `fin tx cat queue` for keyboard-driven classification with shortcuts,
  hierarchical category browsing, search, category creation, skip and undo.

- Add `fin cat delete ACCOUNT` to remove a catalog category that is not used by
  current transaction assignments or category rules.

- Speed up journal export by batching posting reads, selecting only the movement
  fields and matched companion data needed for export, and reusing that data
  for descriptions.

- Add `fin ledger` to run hledger reports against a freshly generated journal.

- Add `fin tx status` to summarize transaction decisions and show classified and
  categorized percentages and category and movement-type breakdowns, with JSON
  output for scripts.

- Exclude previously categorized transactions from automatic category-review
  suggestions; explicit search can still find categories that need correction.

- Hide transactions already assigned to the chosen category in interactive
  category-review matches and search. A seed already in that category is left
  untouched rather than creating a redundant decision.

- Match portable category decisions by stable source identity, occurrence, payload
  and source-field hashes; missing, conflicting or ambiguous records block the
  atomic batch. Portable exports omit transfer decisions and review history.
  `make run` no longer echoes its command into redirected JSON output.
- Add `fin rule from-category` to preview a literal description rule based on a
  categorized example and optionally save it as a review-mode rule.

- Remove the 20-result cap from fuzzy category review, showing every match above
  the existing similarity threshold.

- Make `all` in category review select every selectable displayed match, including
  fuzzy and previously categorized transactions, before the final confirmation.

- Add `make run ARGS='...'` for incremental checkout execution, and
  `make package-fast` / `make install-fast` for cached development packages.
  Fast packages skip tests; the existing package and install targets remain clean
  tested builds. Package scratch defaults to `~/.cache/codex/scratch/finances-arch`.

- Allow `fin tx cat review` to change an existing category and explicitly select
  categorized similar movements. Changes are confirmed together and applied only
  if every selected decision still matches the reviewed snapshot.

- Add `fin tx cat set ACCOUNT EVENT-ID...` to assign, change or reset categories
  without preparing a JSON file or entering a reason.

- Export confirmed transfers as one direct bank-to-bank transaction, retaining
  both source event IDs, per-leg booking dates and fee postings. Transaction lists
  show the other bank account as counter-account. SQLite still keeps internal
  per-movement clearing projections for review and undo. **Action required:**
  regenerate exported journals and update reports or scripts that relied on
  `assets:transfers:pair-*` accounts. Date-filtered reports between different
  booking dates have no in-transit asset and may appear temporarily unbalanced.

- Show the actual counter-account in `fin tx list`, with `-` for unclassified
  movements. **Action required:** the decision-kind filter is now `--decision`
  (formerly `--classification`); transaction and review JSON use `decision`
  instead of `classification`. Update scripts accordingly.

- Show fuzzy suggestions immediately alongside exact matches in category review.
  Select or deselect multiple rows using comma-separated numbers and ranges;
  `all` selects all selectable displayed matches.

- Clarify the category-review prompt by separating number selection instructions
  from the commands users can enter.

- Use one effective transaction description in review, listings, similarity and
  bank journal exports. A consistent nonempty Wise companion `Merchant` wins;
  otherwise use the primary source's main description. Original sources remain
  available for inspection. Review-mode rule matching uses that description.
  **Action required:** transaction/review JSON now has one `description` and no
  `display_name`; update scripts and regenerate exported journals. Automatic
  import-time rules still use the primary description available at import.

- Make category review results easier to scan by showing a source-provided display
  name when available, instead of repeating it alongside the transaction description.
  Both texts remain searchable.

- Rank approximate category-review matches across institutions by
  character-trigram similarity; users select results and confirm the batch.

- Add interactive substring search during category review, with selections that
  can be toggled across searches. Existing classifications appear as context;
  explicitly selected categories can be changed after confirmation.

- Search across institutions for similar category-review transactions. A card
  descriptions can match across providers, and interactive candidates show their
  institution. Conflicting classifications remain visible.

- Map the Wise CSV companion description to `Merchant` instead of `Description`.
  The original CAMT and CSV fields are retained as source evidence. **Action required:** existing databases retain their installed definition;
  source interpretation changes require removing Wise imported evidence and installing
  the updated definition before reimporting. Preserve and recreate affected reviews
  and rules as needed.

- Move category catalog commands to `fin category` (`fin cat`); transaction
  category review commands remain under `fin transaction category` (`fin tx cat`).

- Add a personal category catalog with `fin cat add` and an interactive
  `fin cat init` for selecting optional starter categories. Category decisions
  add their account to the catalog; `list` now includes unused categories.
  **Action required:** schema 31 adds the catalog. Rebuild the database from
  statements and recreate rules and review decisions; reapplied decisions populate
  the catalog. Existing databases are not migrated.

- Add `cat` as a shortcut for top-level `category` and for `transaction category`.

- Add `fin category list` to list income and expense
  accounts, with `--json` for scripts.

### 2026-10-31

- CAMT+CSV institution definitions no longer take `payee` or `details_type` in
  `columns`; nothing read them, since a rule can match any column by header with
  `--field`. **Action required:** remove both keys from a user-written definition,
  which is otherwise rejected. `fin institution show` no longer lists them.

- Types are derived. A movement's type is recomputed from the type rules whenever
  they could change: after an import, a companion match, and any rule or institution
  definition change. A manual override still wins. Editing a rule therefore retypes
  imported movements at once, and each change is recorded in `fin tx type history`
  with the rule that caused it. Matched companion columns now count as fields of the
  movement, so one rule form covers both. **Action required:** schema 30 drops
  `companion_reported_types` and lets `movements.movement_type` be updated (and
  nothing else); rebuild the database from your inboxes. Removed: `fin tx
  refresh-types`, `fin tx refresh-companion-types`, `Store.preview_*` and
  `Store.refresh_*` for types; new `Store.retype`. `fin rule explain` has two stages,
  the manual override and the rules, and `fin tx show` no longer lists a separate
  companion type.

- One kind of type rule. `reported-type` and `reported-other` are gone: a bank's own
  code is an ordinary column, so a rule names the column with `--field` and the first
  matching rule in priority order decides, whether the column belongs to the
  transaction's own source or to a matched companion file. Wise's rules now test
  `Transaction Details Type` and `TransferWise ID`, Revolut's test `Type`, and the
  catch-alls keep an unrecognised code as `wise:{value}` or `revolut:{value}`, where
  `{value}` takes the text the rule matched. Removed: `fin rule add-reported-type`
  (use `fin rule add --field COLUMN --automatic`), `Movement.reported_code` and the
  companion annotation's `reported_type`, and the Wise rule for the `UNKNOWN` code,
  which now falls into the catch-all as `wise:UNKNOWN`. `fin tx
  refresh-reported-types` is now `fin tx refresh-companion-types`. Bundled rules keep
  their own names and priorities when installed. `fin rule explain` shows two stages
  after the override, the rules over the transaction's fields and the companion
  file's types. The definition format drops the two actions and accepts `field` on
  `type` rules. **Action required:** schema 29 changes the kinds the rules table
  accepts, so rebuild the database from your statements, then recreate your rules and
  review decisions; rules you added with `add-reported-type` become `fin rule add
  --field COLUMN --automatic`, and definitions you wrote with the old actions must use
  `type`. OCaml consumers: `Registry.create` takes `?type_rules`, `Source_typing.decide`
  and `matched_value` return the matched value, and `Reported_type_defaults` is
  replaced by `Type_default`.

- Keep every column of a CSV primary beside its transaction, so type rules can match
  any of them with `--field COLUMN`, named by the file's header: Revolut's `Type`,
  `Product`, `State` and `Balance`, and any column of a bank you added yourself.
  `fin tx show` lists them under "Source columns", `fin institution show` says
  rules can match them, and the stored transaction now remembers which of its texts
  is the main one, so `fin rule gaps` groups Bunq by the payment text as the rules
  do. The columns live in a new `source_event_fields` table; payloads and source
  identities are unchanged. The named field `type` that Revolut exposed briefly is
  now the column `Type`. **Action required:** schema 28 adds that table and a
  `main_description` column, so rebuild the database from your statements, then
  recreate your rules and review decisions. OCaml consumers: `Source_event.create`
  and `create_dated` take `?fields` and a trailing `()`; `Source_event.t` and
  `Movement.t` carry `fields`.

- Let automatic type rules match a named field of the primary source:
  `fin rule add` and `fin rule test` take `--field FIELD`. `description` is now the
  main text the adapter declares instead of the first text, `text` matches any
  description, and sources expose named fields: OFX `name` and `memo`, Bunq `name`
  and `counterparty`, the Wise CAMT `reference`, and Revolut `type`. On a Wise CAMT
  statement alone, a rule on `reference` types 187 fee lines and 29 cashback rows.
  `fin rule explain` shows the field and value each rule tested. Bunq is the one
  behaviour change: its automatic rules now see the payment description, which was
  the second text, instead of the counterparty's name; it has no bundled rules.
  `fin rule gaps` still groups by the first text. OCaml consumers: `Movement.t` has
  `fields` and `main_description`, `Source_typing.rule` a `field`, and
  `Movement.field_values` resolves a name.

- Ship Wise rules on the `TransferWise ID` column and for the `MONEY_ADDED` code:
  `FEE-` to `fee`, `BALANCE_CASHBACK-` to `bonus`, and `MONEY_ADDED` to `deposit`. On
  the real archives they type 187 fee lines, which the bank code had typed as
  `card`, `conversion` or `transfer`, 29 cashback rows that were untyped, and 2
  top-ups. They are installed into a new database, which the schema 27 rebuild
  provides. `fin institution show` now names the column a rule matches.

- Add `fin tx show EVENT-ID`: everything stored about a transaction. Its type,
  category and confirmed transfer, the source event and payload fields, the stored
  document and every file it was imported from, the statement, and each matched
  companion row with every column under its header name, the match policy and
  what its type rules matched. Read-only.

- (Superseded by the single rule form above.) Let reported-type rules match any column
  of a companion file: `fin rule
  add-reported-type NAME INSTITUTION REGEX TYPE --field COLUMN`, with COLUMN named as
  in the file's header. On a Wise archive, `--field 'TransferWise ID'` with `^FEE-`
  and `^BALANCE_CASHBACK-` types the 187 fee lines and 29 cashback rows that the
  bank code `CARD` or `UNKNOWN` left wrong or untyped. Every companion now exposes
  its raw columns by header name, `fin tx refresh-reported-types` recomputes every
  companion row from the rules (inserting and removing as needed), the definition
  rule format gains an optional `field`, and `fin rule list` shows it.
  **Action required:** schema 27 adds a `field` column to the rules table, so
  rebuild the database from your statements, then recreate your rules and review
  decisions. OCaml consumers: `Classification_rule.create` takes `?field` and a
  trailing `()`, and `Companion.t` has a `fields` function.

- Add `fin rule explain EVENT-ID` to trace how a movement got its type: manual
  override, the bank's code, description rules and the companion file's code, with
  the deciding rule, the rules that lose to it and whether the saved type is stale.
  Read-only; `--all` lists the rules that did not match.

- Add `fin institution show INSTITUTION`: kind, what fin reads from each source
  column, columns it does not read, what rules can match, default rules and
  imported movements by type. Read-only; it reads stored rows without decoding
  payloads, so it takes about 5 ms on 4,000 movements.

- `fin delete` now asks `[y/N]`: only `y` or `yes` confirms, and Enter declines.
  Other confirmations are unchanged.

- Type Revolut movements from the `Type` column of the CSV: Card Payment and Card
  Refund become `card`, Deposit `deposit`, Transfer `transfer`, ATM `withdrawal`
  and Charge `fee`; other codes are kept as `revolut:CODE`. The mapping is ordinary
  rules in `institutions/revolut.json`, editable with `fin rule`, and
  applies to a primary CSV as well as to companion rows. A movement carries the
  bank's own code (`Movement.reported_code`), and description rules see the type
  it gives. **Action required:** the bundled rules are installed into a new
  database only. For an existing database, add them with `fin rule
  add-reported-type` (see the manual) or rebuild it, then run `fin transaction
  refresh-types --institution revolut`. The schema is unchanged.

- Add `fin rule gaps` to list movements without a type, grouped by how their
  descriptions start with a suggested expression, and `fin rule test` to evaluate
  a draft type rule against stored movements without saving it. Both are
  read-only. The CLI and database are unchanged otherwise. Internal: `Source_typing`
  exposes `matches`; the store gains `test_type_rule` and `description_gaps`.

- Ship each bundled institution as one definition file (`institutions/boursobank.json`,
  `wise.json`, `bunq.json`, `revolut.json`) holding its source profile and default
  rules, replacing nine preset, profile and default-rule files. `fin institution
  definition show` prints the same files; the default rules of a new database are
  unchanged. **Action required:** OCaml consumers must replace
  `Institution_preset.all`/`find`, `Camt_institution_preset.all`,
  `Camt_profile.find`, `Csv_clock_profile.find`, `Csv_running_profile.all`/`find`
  and `Mt940_presets` with `Builtin.definitions` and its accessors. The CLI and
  database are unchanged.

- Express the MT940 adapter as ordinary profile-bound functions, using the
  shared registry adapter constructor. Its import behavior is unchanged.

- Move MT940 source and transaction handling into `finances.mt940`; replace
  profile functors with runtime values and retain the CSV companion in the
  institution library. **Action required:** OCaml consumers must link the new
  library and update MT940 API references. CLI behavior and schema are unchanged.

- Rename the internal import workflow and file-scanning modules from `Sync` and
  `Inbox` to `Workflow` and `Files`. **Action required:** OCaml consumers of
  `finances.import` must update these module and report-field references; the CLI
  and database are unchanged.

- Rename the import workflow and institution directories and OCaml libraries to
  `import` and `institutions`; move shared statement sources under `formats/`.
  **Action required:** OCaml consumers must update references to
  `finances.import` and `finances.institutions`. CLI behavior and the database
  schema are unchanged.

- Replace dedicated Bunq and Revolut modules with bundled profiles and shared
  MT940/CSV and running-balance CSV import components. Preserve transaction
  identities, source evidence, reconciliation and companion matching.

- Add versioned runtime institution definitions for primary CSV, OFX with CSV
  companions, and supported CAMT/CSV profiles. `fin institution definition
  show`, `check` and `set` expose templates, preview samples and attach profiles
  to existing institutions. Default source rules are previewed and added to the
  unified rule list without overwriting existing rules. **Action required:**
  schema 26 requires rebuilding the database. Back up and export account settings with the old executable, then
  reimport statements and recreate rules and review decisions.

### 2026-10-30

- Compose Wise from bundled CAMT and CSV profiles and shared importers, and move
  reported type-code mappings into the unified rules. Add `fin rule
  add-reported-type` and previewed `fin tx refresh-reported-types`; existing
  overrides and accounting decisions survive a refresh. **Action required:**
  schema 25 requires rebuilding the database. Use the previous executable to
  back up and export account settings, then reimport statements and recreate
  rules and review decisions.

- Extract a shared CAMT.053 XML reader, profile-driven source interpreter,
  configurable clock-based CSV decoder and reference/clock matcher. Wise import
  behavior and identities are preserved;
  importing other institutions’ CAMT files is not yet exposed in the CLI.

- Compose BoursoBank from a data-driven preset, the generic OFX importer and
  CSV companion engine. Preserve aliases, source identities, matching policy
  and existing evidence behavior while removing the dedicated adapter.

- Move BoursoBank card-settlement and withdrawal-reversal hints into automatic
  evidence rules, and add `fin rule add-evidence` and `fin tx refresh-evidence`.
  Store clues at import; refreshing existing clues previews and confirms changes
  without altering reviewed decisions. **Action required:** schema 24 requires
  rebuilding the database; back up and export settings with the old executable,
  then reimport statements and recreate review decisions and rules.

- Add configurable CSV companions for user-added OFX institutions, with exact
  account, date, amount, currency and description matching. Store the selected
  layout with each document; ambiguous rows and metadata conflicts remain for
  review. BoursoBank uses the same layout and matching engine. **Action required:**
  schema 23 requires rebuilding the database. Back up and export settings with
  the old executable, then reimport statements and recreate review decisions.

- Extend generic CSV setup to current and savings accounts, with explicit
  outgoing-amount signs, optional IBANs and bank-account ledger defaults. Saved
  profiles cover one account and fixed currency; setup also supports day-first
  and month-first dates and comma-decimal amounts, with explicit format choices.
  Existing saved profiles retain their original ISO and dot-decimal defaults.

- Allow user-added institutions to import OFX 2.x using the shared parser and
  stable source IDs. `fin institution list` labels them as generic rather than
  CSV-only. To avoid duplicate movements, fin refuses mixing generic primary CSV
  and OFX sources for the same institution; configured CSV companions can
  enrich OFX without creating a second transaction stream.

- Unify automatic source typing and reviewed transaction rules. BoursoBank
  defaults now appear in `fin rule list`; `fin rule add --automatic` creates
  import-time type and category rules, and `fin rule mode` changes application
  mode. Existing movements change only through previewed operations. Settlement
  rules continue to require review. **Action required:** schema 22 requires a
  rebuild. Back up and export settings using the old executable, then reimport
  statements and recreate rules and review decisions. Before upgrading from
  schema 21, export customized typing profiles with the old executable; the
  former institution typing commands and profiles have been removed.

- (Superseded by the unified rules above.) Make BoursoBank import-time source typing configurable, with bundled defaults
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
