# Plan

Where `fin` stands and what comes next. Update this file in the commit that changes
its status: tick off finished work, add discovered work, and refresh the "Where we
are" figures when they move. History belongs in `CHANGES.md`; designs stay in the
other documents, which this one links to rather than repeats.

Last updated: 2026-11-01.

## Direction

Fin brings personal financial evidence together for two connected views:

- **Accounting:** what is owned or owed and where money came from or went. Bank
  movements, exchange balances, transfers, fees and realized results belong here.
- **Positions:** current investment or derivatives exposure and its performance,
  including instrument, size, direction, entry and mark prices, unrealized P&L,
  funding, observation time and source.

The views may use the same source events, but they answer different questions. A
perpetual position is exposure, not necessarily ownership of the underlying asset.
Do not count notional exposure as an asset balance or include unrealized P&L twice.
Keep source evidence and uncertainty visible; reconcile derived results against
provider observations before treating them as authoritative.

## Where we are

### Code

- Import orchestration lives in `import/workflow.ml`, file and archive scanning
  in `import/files.ml`, institution definitions and adapters in `institutions/`,
  and reusable statement sources in `formats/statement/`. MT940 has its own
  `formats/mt940/` directory and `finances.mt940` library. The workflow and
  institution libraries are `finances.import` and `finances.institutions`.
- Wise uses a matched, nonempty and consistent CSV `Merchant` as the effective
  transaction description, falling back to CAMT; source evidence remains intact.
- All four banks import: BoursoBank (OFX, CSV companion), Wise (CAMT.053, CSV
  companion), Bunq (MT940, CSV companion) and Revolut (CSV). All use bundled
  profiles and shared import components; none has a dedicated bank module.
  User-added institutions can import OFX or configure a generic CSV layout.
- Storage is schema 31: generic tables for every bank, plus each movement stored
  normalized at import, so reads are SQL queries instead of payload decoding. No
  migrations; older databases are refused unchanged.
- Event occurrences and statement boundaries share `Temporal.t` (`Date` or
  `Instant`) and its text codec; calendar dates never imply a time or timezone.
- Instants use `Ptime.t`, with Rel codecs preserving picoseconds as fixed-width UTC
  text. Date-only statement boundaries remain dates; original source clocks and
  Wise identity encodings stay unchanged. Revolut start/completion clocks are
  interpreted as UTC Ptime instants while preserving original payload bytes.
  Wise CSV clocks assume UTC and retain source-local CAMT matching; OFX balance
  instants use the format's timezone defaults. Only raw source evidence and invalid
  DTUSER placeholders remain textual.
- Status uses SQL counts and account date ranges, decoding only candidate legs
  and representative account movements. Account discovery reuses source rows.
  Review metadata is loaded in batches; candidate searches prepare known-account
  IBAN evidence once.
  No data is cached across commands or later account/review edits.
  On 4,370 movements, warm status measured about 45–56 ms in a later run,
  after an earlier 230 ms measurement under slower CPU conditions. A
  frame-pointer profile attributes the largest share to SQLite transfer-pair
  searches. A synthetic 4,400-movement status test guards CPU time; investigate
  and reduce transfer matching cost before tightening its budget.
- JSON uses typed `Jsont.t` descriptions throughout configuration, review files,
  bank payloads, storage and CLI reports; hand-built JSON trees and Yojson are
  removed. Codecs preserve exact quantities and account patch semantics.
  Encoding uses small per-call buffers; status and account listings compute
  each source account identifier once per account rather than per movement.
- IBANs and currencies are validated types (`Iban.t`, `Iso4217.t`).
- Confirmed same-currency transfers export as direct bank-to-bank journal entries,
  with separate posting dates and both source IDs; SQLite retains per-movement
  projections for review and undo.
- Movements project to balanced entries against `equity:unclassified`, unless a
  reviewed category or confirmed transfer says otherwise. `fin journal export`
  writes a reproducible hledger journal.
- Statement ingestion is `fin import BANK PATH...` or a configured generic CSV.
  `fin backup` saves one
  consistent SQLite snapshot; `fin restore BACKUP` validates and restores it,
  with overwrite confirmation and automatic removal of stale generated files.
- OCaml test suites and 93 CLI cases, plus status performance and package
  man-page checks. Selected import changes have also been compared by
  rebuilding a personal database and diffing reports and source payloads.
- Issue #1 is implemented locally and awaits publication.
- The installed Arch package is `finances 0.1.0-23`; the tree uses schema 31.

### Personal data

A prior database rebuilt from `personal/*/inbox` contained:

| Bank | Account | Movements | Covered |
| --- | --- | --- | --- |
| BoursoBank | checking | 20 | 2026-09-01 to 2026-09-28 |
| BoursoBank | card (settles to checking) | 1 | 2026-09-07 |
| Bunq | current EUR | 154 | 2026-01-01 to 2026-10-16 |
| Revolut | current EUR (`main`) | 35 | 2026-04-13 to 2026-10-20 |
| Wise | balance EUR | 538 | 2025-01-07 to 2025-12-29 |

- That snapshot had 748 movements, all unclassified, and no category or transfer
  decisions. Categorization has since begun; these figures do not describe the
  current database.
- 4 transfer candidate pairs, none confirmed.
- The Wise 2025 and Bunq 2026 statements reconcile exactly (difference 0).
  BoursoBank and Revolut have no statement balances to reconcile against.
- Companion rows: all matched except 8 BoursoBank CSV rows, ambiguous because the
  OFX has identical movements.
- An earlier live schema 13 database contained 4,427 movements across 12
  accounts, all unclassified, with 123 transfer candidate pairs. Those figures are
  historical, not current status.

## Next actions

1. Continue importing the main spending accounts for a useful period, reviewing
   categories and transfers, and backing up the resulting decisions. Verify a
   representative hledger report and identify remaining coverage and balance gaps.
2. Choose one crypto venue and a useful first read-only view of reported balances
   and open positions. Inspect available source data and record its semantics before
   implementing an importer or calculation.
3. Record the card-balance transfer once its date and amount are known. Publish
   `main` when ready.

## Workstreams

### Generic institutions and reusable import formats

The chosen model is **generic format support plus institution configuration**.
Readers handle the structure of CSV, OFX, CAMT or MT940; institution settings
supply the interpretation the format does not establish, such as columns, signs,
account and transaction identities, matching policies and import-time rules.
Fin previews the result, preserves source evidence and leaves uncertain meanings
for review. The configuration is saved and reused for subsequent imports.

An institution represents the real-world provider; a definition describes how
fin imports and interprets its statements. The user creates the institution,
then attaches or configures its import definition from a sample or a file. Fin
validates and previews the definition before saving it. Built-in institutions
start with bundled definitions that users can inspect and adjust. Definitions
are saved configuration for the existing workflow, not institutions themselves
or a competing setup path.

Improve the generic readers and configuration using real institution exports.
Keep fin's existing internal representation; do not introduce a public interchange
format or external converter framework until a concrete case demonstrates the
need. A format or policy that cannot yet be expressed may still require custom
implementation.

The goal is to add an institution through a user-supplied definition, without
writing OCaml or rebuilding fin. Versioned definitions now cover primary CSV,
OFX with a CSV companion, Wise-shaped CAMT with a CSV companion, a running-balance
CSV, and an MT940/CSV profile. Fin stores attached definitions in the database and
uses the same representation for the bundled institutions. Not every bank's format or policy is
configurable yet.

- [x] Load validated definitions at runtime, retain them in the database, and
  expose bundled definitions as starting points. Preserve importer identity and
  overlap policies across reimports and process restarts.
- [x] Validate definitions and preview sample statements without importing. The
  preview shows a few movements or companion rows and reports parse errors.
- [x] Test a primary CSV definition against the two card exports: 57
  movements imported, and replay was unchanged.
- [x] Attach a validated definition to an existing institution, preview its
  default source rules and add them to the unified rule set without overwriting
  existing rules. Permit rule-only updates and refuse source interpretation
  changes while imported evidence depends on the previous definition.
- [ ] Integrate sample-based setup and saved definitions into one configuration
  model without losing per-file account prompts for generic CSV directories.
  Support changing source interpretation while preserving prior document profiles.
- [ ] Extend sample diagnostics to identify all rejected rows where possible,
  and test a previously unsupported institution. Generalize multi-account and
  multi-currency CSV, identifiers, signs and companion matching as needed.
- [ ] Reassess scripting when a concrete provider needs behavior that cannot
  reasonably be expressed in a definition. A small native adapter remains an
  option for genuinely unusual formats.

- [x] Allow a user-added institution to import supported OFX 2.x without an
  institution-specific adapter; preserve account identity and FITID deduplication.
- [x] Extend single-account generic CSV profiles to current and savings accounts,
  including signed amounts, optional IBANs and distinct bank ledger defaults.
- [x] Support ISO, day-first and month-first dates and dot/comma decimal amounts,
  asking about ambiguous formats and preserving old saved profile defaults.
- [ ] Extend generic CSV profiles to multiple currencies and accounts, debit/credit
  columns, additional date and number formats, identifiers and pending or reversed states.
  Suggest settings from samples and preview them.
- [x] Generalize exact-match CSV companions and configurable layouts; move the
  BoursoBank layout and matching onto the shared engine. Store layouts with
  documents, and require a fixed currency for generic companions.
- [ ] Guide companion layout setup interactively, support currency columns and
  additional matching strategies, and add a workflow to remove companion evidence.
- [x] Express settlement-card references and withdrawal-reversal evidence through
  import-time rules, with previewed refresh and existing review checks.
- [x] Compose the BoursoBank preset from generic OFX, companion configuration
  and default rules; remove its dedicated adapter after equivalence tests.
- [x] Extract a shared CAMT.053 XML reader, profile-driven source interpreter,
  configurable clock-based CSV layout and reference/clock matcher; compose
  Wise from these shared components while preserving its identities and results.
- [x] Compose the Wise preset from shared CAMT and CSV components and move its
  reported type mappings into unified import-time rules with previewed refresh.
- [ ] Extend configurable CAMT format policies beyond the Wise-shaped profile. Keep
  explicit source identity, overlap and correction policies.

### Statement observations and matching

- [x] Compare the available Wise CAMT, MT940 and CSV archives; record matching
  evidence and ambiguities in [statement observations](statement-observations.md).
- [x] Evaluate a source-neutral observation model with auditable transaction
  links. Deferred: formats of one account are requested together for the same
  period, so primary/companion remains the model. See
  [observation model](observation-model.md) and the revised decisions in
  [reconciliation design](reconciliation-design.md).
- [ ] Allow several companions per primary institution source, when a bank needs
  it. Wise no longer does: it uses CAMT and a CSV companion.
- [ ] Improve companion matching: institution-scoped reference normalization
  (the Wise suffix case), a mutual-unique candidate rule with a one-day date
  tolerance, and a way to resolve the 8 ambiguous BoursoBank CSV rows. Report
  disagreeing fields as conflicts.
- [x] Decide whether Wise MT940 is worth parsing: no. It adds only a SWIFT type
  code derivable from the reference prefix and direction; see
  [statement observations](statement-observations.md#outcome-wise-mt940-is-not-parsed).
  Wise keeps CAMT as primary and CSV as companion.

### Rule editor and shared definitions (`docs/classification.md`)

- [x] Document the existing rules and sketch a rule editor and the sharing of
  interpretation rules through institution definitions.
- [x] `fin rule test` for a draft type rule: matches, newly typed and changed
  movements, rules that shadow it and manual overrides; read-only (2026-10-31).
  Conflicts with reviewed decisions and near misses are not reported yet.
- [ ] Simpler rule model ([proposal](classification.md#a-simpler-rule-model-proposal)):
  one rule form (field, expression, type or category), named fields, derived
  types. Every column of every source is a field named as the source names it.
  Decide the open questions there, then stage it: companion fields (step 1, done),
  primary fields (2a done: named fields from the payload, `description` as the
  adapter's main text, `text` for any, `--field` on automatic type rules; 2b done for
  CSV: the `source_event_fields` side table and the stored main-text index; tag-format
  flattening deferred), one rule form (step 3, done 2026-10-31: `reported-type` and
  `reported-other` removed, a bank's code is a column, `{value}` templates), then
  derived types without the refresh commands (step 4, done 2026-10-31, schema 30) and
  dropping the unused `payee` and `details_type` definition columns (step 5, done).
- [x] Wise rules shipped in `wise.json` (2026-10-31), on the `TransferWise ID`
  column rather than the description: `FEE-` to `fee` (187 rows), `BALANCE_CASHBACK-`
  to `bonus` (29) and the `MONEY_ADDED` code to `deposit` (2). They are ordinary
  column rules, so they move to the single rule form with the others.
- [x] `fin tx show EVENT-ID` (2026-10-31): source, files, payload fields, statement,
  companion rows by column, decisions.
- [x] `fin rule explain EVENT-ID` (2026-10-31): type trace by stage, stale-type
  warning. Category rules are not traced yet; `--json` is not implemented.
- [x] `fin institution show INSTITUTION` (2026-10-31): fields read and unread, what
  rules can match, default rules, imported types.
- [x] `fin rule gaps`: movements with no type, grouped by the first words of their
  description, with a suggested expression (2026-10-31). The grouping key is a
  heuristic to tune on real data.
- [x] Revolut typing from its CSV `Type` column (2026-10-31): six values plus a
  catch-all, as reported-type rules in `institutions/revolut.json`. A movement
  carries the bank's own code and `Source_typing` applies reported rules as the
  base type before description rules. Existing databases need the rules added
  (the bundled rules are installed only into a new database), which is the
  provenance and update work below.
- [x] Express each bundled institution as one institution definition file
  (`institutions/{boursobank,wise,bunq,revolut}.json`), replacing nine files of
  presets, profiles and default rules. Verified 2026-10-31: `definition show`
  output and the 19 default rules of a fresh database are byte-identical to
  before, and a registry test pins the canonical form, rule naming and unique
  rule names.
- [ ] Add metadata and `examples` to definitions; `fin institution definition
  check` runs the examples and warns about personal-looking text. A dune test
  checks every bundled definition.
- [ ] `fin rule export` writes selected interpretation rules as a definition
  fragment, refuses personal actions and never writes database descriptions.
- [ ] Contribution recipe in the manual: gaps, test, add, export, synthetic
  examples, pull request.
- [ ] Rule provenance and updates for existing databases (definition, version,
  origin), and a priority convention for bundled rules.
- [ ] Decide on amount and direction conditions from real cases.
- [ ] Terminal rule editor on the same engine.

### 1. Bank adapters (`docs/bank-adapters.md`)

- [x] Baseline, `iban` library, kernel types and registry, schema 11, import behind
  the adapters (steps 1 to 5).
- [x] Currencies as `Iso4217.t`.
- [x] Extract the shared `camt.053` document reader and its current statement,
  balance and entry views; use a profile-driven source interpreter for Wise.
- [x] Move Bunq and Revolut onto shared MT940/CSV and running-balance CSV
  components and bundled profiles, preserving identities and observed behavior.
- [x] Extract the profile-driven MT940 source and transaction codec into the
  `finances.mt940` library, using runtime profile values rather than functors.
  The adapter uses ordinary functions and a shared registry constructor.
- [ ] Broaden generic CAMT and MT940 parsing beyond the supported profile shapes;
  broader CAMT support may need a `Bic.t` (ISO 9362).
- [ ] Step 7: an external adapter example and a conformance suite every adapter
  runs.
- [ ] Open questions: partial export days blocking later exports under
  `Day_inventory`; whether consecutive statements must chain; whether Wise CSV fees
  ever become posted fees.

### 2. Usable books (`docs/bank-workflow.md` milestones)

The import side is done; turning movements into accounting is not.

- [x] Folder ingestion and account mapping.
- [x] Use the singular `fin transfer` command group.
- [x] Add short aliases for transaction, account and institution commands.
- [x] Show transaction counter-accounts and filter listings by type and accounting decision.
- [x] List common and observed provider types and audit manual type overrides.
- [x] Match rules on effective type and preview type and category actions together.
- [x] Move BoursoBank source typing to editable per-institution profiles, with
  previewed changes and explicit refresh of existing movements.
- [x] Replace source typing profiles and reviewed transaction rules with one
  ordered rule set, automatic defaults and previewed historical changes.
- [ ] Extend automatic rules to settlements after designing safe import-time
  card matching and reversal handling.
- [x] Move Wise companion type-code mappings into unified automatic rules,
  preserving companion matching and explicit refresh semantics.
- [x] Undo confirmed transfer pairs by event ID, with an audited confirmation.
- [x] Combine transfer evidence and candidates, and review pairs interactively;
  show each pair and explain which choices act on it or the batch. Selecting all
  supported pairs proceeds directly to the batch preview.
- [x] Organize review, statement evidence, import history and projection commands
  under transaction, statement, institution and journal, respectively.
- [x] Credit-card setup previews its link and accepts a default-yes confirmation.
- [x] Recognize BoursoBank interest movements at import without classifying income.
- [x] Recognize BoursoBank cheque and fee descriptions, including fee refunds.
- [x] Review full ATM withdrawal reversal pairs with audit and undo.
- [x] Use signed card and withdrawal types for refunds and reversals, with
  provider-specific source evidence for withdrawal reversal suggestions.
- [ ] Support partial reversals and fees after designing their accounting.
- [x] Recognize deferred-card settlement types and refresh stored movement types
  with a preview and confirmation, without removing accounting decisions.
- [x] Transaction listings show accounting classification and use a terminal pager;
  settlement rule batches reuse loaded data across decisions.
- [x] CSV enrichment, with ambiguous matches visible.
- [x] Find similar transactions across institutions during category review.
- [x] Search descriptions and select transactions across searches during category review.
- [x] Set or correct an individual category directly; re-review categorized batches.
- [x] Offer uncapped ranked fuzzy suggestions during category review; evaluate on real variations.
- [x] Export and safely restore category decisions and rules across database rebuilds;
  preview literal category rules from reviewed examples.
- [x] Maintain a personal category catalog with `fin cat add`, `list`, `delete`
  and interactive `init`.
- [ ] [Classification and review](classification.md): rules (regular expressions
  with automatic, review and disabled modes, priorities, type and category
  actions), previews and interactive review exist. User tags, richer similarity,
  rule provenance and shareable institution definitions, a rule-authoring workflow and a possible
  vendored Notty interface remain to be designed and implemented.
- [ ] Transfer matching: the review workflow exists; the 4 candidates await
  decisions. FX and split transfers are not handled.
- [ ] Reconciliation and reporting: Wise and Bunq statements reconcile; opening
  balances are not inferred or verified, BoursoBank balances are not reconciled, and
  there is no correction workflow for changed source evidence.

### 3. Smaller gaps

- [x] Provide incremental checkout execution and a cached Arch development package
  path, keeping the clean tested package workflow.
- [x] Generate Arch package man pages for every top-level command and check the
  package list against the CLI in the test suite.

- [x] [Shared CSV decoding and format profiles](csv-imports.md): extract typed
  row descriptions and common reading/diagnostics, retaining primary/companion
  roles, identities and matching policies. `Csv_reader`, `Csv_schema`, all four
  bank CSV schemas and `Adapter_support` companion/profile helpers are in place.
- [x] Keep the welcome in sync with top-level commands and show transaction
  listing in the setup guidance.
- [x] List imported transactions with institution and account IDs, provider, date
  and description filters, and JSON output; source-field provenance remains later work.
- [x] Initial generic card CSV import: suggested mappings, separator detection,
  interactive setup, preview and a saved
  layout with account-scoped identities and conservative day overlap checks.
- [ ] Implement the [provider, account and format model](account-identities.md),
  including meaningful ledger names and account-scoped deletion.
- [x] Delete account-scoped development imports by their displayed stable ID
  after preview; reimport source
  files under their provider instead of migrating legacy `csv` identities.
- [x] Use numbered choices for interactive CSV setup and retry invalid answers.
- [x] Require a valid provider name and retry empty or invalid terminal input.
- [x] Show CSV amounts and source details alongside signed account movements
  in the generic CSV preview.
- [ ] Extend generic CSV setup beyond one fixed-currency account and a single
  signed amount column; support pending states and more flexible layout selection.
- [x] Allow asset and liability ledger mappings; new generic prepaid and credit
  cards default to their respective card hierarchies.
- [x] [Personal accounting guide](personal-accounting.md): a first-month workflow
  from imports and classification to hledger reports, opening balances,
  reconciliation and ongoing backups.
- [x] Backup creates missing destination directories privately, including nested
  paths, so the documented first-backup command succeeds.

- [x] SQLite is the only account settings store. No config directory or implicit
  file remains; snapshot export/import is explicit, editor drafts are temporary,
  and backup includes all settings in the database.

- [x] ZIP statement exports import directly through Zipc, with per-member history,
  idempotent replay, bounded expansion and no filesystem extraction.

- [x] Issue #1: shared-IBAN currency siblings do not contradict transfers; import
  asks for missing labels and new account IBANs on a terminal and saves answers
  in SQLite.

- [x] `fin account edit` sets IBANs, names and card links without handling the
  configuration file by hand.
- [x] `fin backup` and `fin delete` save and remove all fin state without
  knowing where it lives; `fin init` creates a fresh database.
- [x] `fin restore BACKUP` restores a whole SQLite snapshot without manual deletion
  or copying, validates it before replacement and asks before overwriting state.
- [x] Statement imports use the top-level `fin import` command instead of `bank sync`.
- [ ] `bank.journal` is rewritten only by `fin import`, so it is stale after
  `fin account import FILE` or a review decision until the next import. Every command
  that changes accounting should rewrite it, or the file should go in favour of
  `fin journal export`.
- [x] `fin account import FILE` names the configured accounts that have no imported
  statements, instead of repeating "unknown source account".
- [x] `fin import BANK PATH...` accepts named bank files or directories; a
  generic CSV can also be passed directly and configured interactively.
- [x] `fin account import FILE` requires a snapshot filename and offers a dry run;
  there is no default file to confuse with active settings.
- [ ] `fin paths` prints only JSON, like `statement list` and `statement companions`.
- [ ] `fin statement list` and `fin statement companions` print only JSON, unlike the
  other commands, which print text and take `--json`.
- [ ] The 8 ambiguous BoursoBank CSV rows need a way to be resolved.

### Positions and crypto

Fin has a linear perpetual instrument and position observation model, including
unrealized P&L in the quote commodity. It does not yet fetch Paradex data, persist
position observations, derive positions from fills, or calculate funding.

- [ ] Build a first read-only view for one venue using reported balances and open
  positions. Include the instrument, size, direction, entry and mark prices,
  unrealized P&L, observation time and source; distinguish missing data from zero.
- [ ] Bring crypto deposits, withdrawals and fees into the accounting workflow and
  review transfers between banks, wallets and exchanges.
- [ ] Import fills, funding and other necessary events, derive positions, and
  reconcile them against provider-reported observations. Surface discrepancies and
  verify venue-specific contract and settlement semantics.
- [ ] Build a consolidated view only when account coverage, balances and valuation
  dates and sources are understood. Avoid counting collateral, notional exposure or
  unrealized P&L more than once.

### Later scope

A background inbox watcher, Excel and QIF parsing remain later work; until those
formats are supported, the manual tells users to export a supported format instead.

## Suggested order

1. Make the existing accounting workflow dependable for a representative period:
   import main spending accounts, categorize useful groups, review transfers,
   preserve decisions, and inspect a report and remaining reconciliation gaps.
2. In parallel with that ongoing work, choose a venue and deliver the first
   read-only balance and position view. Do not wait for all historical bank
   transactions to be categorized.
3. Connect crypto cash movements to the books, then derive and reconcile positions
   from fills and funding against provider observations.
4. Add a consolidated view with explicit coverage, price sources and valuation
   times. Prioritize adapters and other infrastructure when an actual source needs
   them; defer ML classification until review experience shows where it helps.
