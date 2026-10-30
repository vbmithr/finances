# Plan

Where `fin` stands and what comes next. Update this file in the commit that changes
its status: tick off finished work, add discovered work, and refresh the "Where we
are" figures when they move. History belongs in `CHANGES.md`; designs stay in the
other documents, which this one links to rather than repeats.

Last updated: 2026-10-30.

## Where we are

### Code

- All four banks import: BoursoBank (OFX, CSV companion), Wise (CAMT.053, CSV
  companion), Bunq (MT940, CSV companion) and Revolut (CSV). BoursoBank is a
  data-driven preset; the others use adapters behind the registry. User-added
  institutions can import OFX or configure a generic single-account CSV layout.
- Storage is schema 24: generic tables for every bank, plus each movement stored
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
- Movements project to balanced entries against `equity:unclassified`, unless a
  reviewed category or confirmed transfer says otherwise. `fin journal export`
  writes a reproducible hledger journal.
- Statement ingestion is `fin import BANK PATH...` or a configured generic CSV.
  `fin backup` saves one
  consistent SQLite snapshot; `fin restore BACKUP` validates and restores it,
  with overwrite confirmation and automatic removal of stale generated files.
- 216 tests (151 OCaml cases and 65 CLI cases), plus status performance and
  package man-page checks. Selected import changes have also been compared by
  rebuilding a personal database and diffing reports and source payloads.
- Issue #1 is implemented locally and awaits publication.
- The installed Arch package is `finances 0.1.0-23`; the tree uses schema 24.

### Personal data

A prior database rebuilt from `personal/*/inbox` contained:

| Bank | Account | Movements | Covered |
| --- | --- | --- | --- |
| BoursoBank | checking | 20 | 2026-09-01 to 2026-09-28 |
| BoursoBank | card (settles to checking) | 1 | 2026-09-07 |
| Bunq | current EUR | 154 | 2026-01-01 to 2026-10-16 |
| Revolut | current EUR (`main`) | 35 | 2026-04-13 to 2026-10-20 |
| Wise | balance EUR | 538 | 2025-01-07 to 2025-12-29 |

- 748 movements, all 748 unclassified. No categories or transfer decisions exist,
  in the rebuilt database or in the live one.
- 4 transfer candidate pairs, none confirmed.
- The Wise 2025 and Bunq 2026 statements reconcile exactly (difference 0).
  BoursoBank and Revolut have no statement balances to reconcile against.
- Companion rows: all matched except 8 BoursoBank CSV rows, ambiguous because the
  OFX has identical movements.
- The live schema 13 database currently contains 4,427 movements across 12
  accounts, all unclassified, and reports 123 transfer candidate pairs. The
  57 card movements were reimported under their named provider.

## Next actions

1. Record the card-balance transfer once its date and amount are known.
2. Begin categorizing movements and reviewing transfer candidates.
3. Push `main` when ready; it is public on GitHub.

## Workstreams

### Generic institutions and reusable import formats

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
- [ ] Generalize the remaining Wise companion type mapping and other reusable
  format policies. Keep explicit source identity, overlap and correction policies.

### 1. Bank adapters (`docs/bank-adapters.md`)

- [x] Baseline, `iban` library, kernel types and registry, schema 11, import behind
  the adapters (steps 1 to 5).
- [x] Currencies as `Iso4217.t`.
- [ ] Step 6: `mt940` format library under Bunq and `camt` under Wise, like `ofx`
  under BoursoBank. `camt` will need a `Bic.t` (ISO 9362).
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
- [x] Filter transaction listings by movement type and accounting classification.
- [x] List common and observed provider types and audit manual type overrides.
- [x] Match rules on effective type and preview type and category actions together.
- [x] Move BoursoBank source typing to editable per-institution profiles, with
  previewed changes and explicit refresh of existing movements.
- [x] Replace source typing profiles and reviewed transaction rules with one
  ordered rule set, automatic defaults and previewed historical changes.
- [ ] Extend automatic rules to settlements after designing safe import-time
  card matching and reversal handling.
- [ ] Move Wise companion type-code mappings into configurable source typing,
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
- [ ] [Classification and review](classification.md): interactive review now
  classifies a movement and selected normalized card-merchant or description matches in one
  confirmed batch. Saved rules match descriptions or effective types and preview
  type, category or card settlement decisions explicitly. User tags,
  richer similarity and a richer review interface
  remain to be designed and implemented. An experimental
  settlement queue now uses a pinned Notty fork submodule; evaluate it and extend
  it to classification.
- [ ] Transfer matching: the review workflow exists; the 4 candidates await
  decisions. FX and split transfers are not handled.
- [ ] Reconciliation and reporting: Wise and Bunq statements reconcile; opening
  balances are not inferred or verified, BoursoBank balances are not reconciled, and
  there is no correction workflow for changed source evidence.

### 3. Smaller gaps

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

### 4. Later scope (README "Current scope")

Not started: fetching Paradex data, positions from fills, persisted position
observations, funding, a background inbox watcher. Excel and QIF parsing are listed
as later work in `docs/bank-workflow.md`; until then the manual tells users to export
a supported format instead.

## Suggested order

1. Install the current build and publish the change when ready (next actions
   1 and 2).
2. Categorization rules (workstream 2): the reason to keep books, and what makes the
   748 movements mean anything.
3. Decide the 4 transfer candidates, then opening balances and reconciliation.
4. Adapter steps 6 and 7, when a new bank or format needs them. They are internal and
   change no user-visible behaviour.
