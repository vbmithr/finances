# Plan

Where `fin` stands and what comes next. Update this file in the commit that changes
its status: tick off finished work, add discovered work, and refresh the "Where we
are" figures when they move. History belongs in `CHANGES.md`; designs stay in the
other documents, which this one links to rather than repeats.

Last updated: 2026-10-27.

## Where we are

### Code

- All four banks import: BoursoBank (OFX, CSV companion), Wise (CAMT.053, CSV
  companion), Bunq (MT940, CSV companion) and Revolut (CSV). Each is an adapter behind
  one registry; the kernel names no bank.
- Storage is schema 12: generic tables for every bank, plus each movement stored
  normalized at import, so reads are SQL queries instead of payload decoding. No
  migrations; older databases are refused unchanged.
- IBANs and currencies are validated types (`Iban.t`, `Iso4217.t`).
- Movements project to balanced entries against `equity:unclassified`, unless a
  reviewed category or confirmed transfer says otherwise. `fin journal export`
  writes a reproducible hledger journal.
- 110 tests. Each change is verified by rebuilding the personal database from its
  inboxes with the previous and the new build and diffing every report.
- `main` is 25 commits ahead of `origin/main`.
- The installed Arch package is `finances 0.1.0-14` (schema 11); the tree is at
  package revision 15 (schema 12).

### Personal data

From a database rebuilt from `personal/*/inbox`:

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
- The live database (`~/.local/share/fin/fin.sqlite3`) is schema 10. Nothing is
  lost by rebuilding it, since it holds no review decisions.

## Next actions

1. **Install the current build and rebuild the database** (`make install`, then
   the rebuild steps in the manual under "Storage and backups").
2. **Push `main`** when ready; it is public on GitHub.
3. **Fill the coverage gaps** above: Wise for 2026, BoursoBank before September
   2026, Revolut before April 2026, if those accounts were in use.

## Workstreams

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
- [x] CSV enrichment, with ambiguous matches visible.
- [ ] Accounting rules and review: manual reviewed categories exist, but nothing is
  categorized yet and there are no versioned categorization rules. This is the
  largest gap: every movement still posts to `equity:unclassified`.
- [ ] Transfer matching: the review workflow exists; the 4 candidates await
  decisions. FX and split transfers are not handled.
- [ ] Reconciliation and reporting: Wise and Bunq statements reconcile; opening
  balances are not inferred or verified, BoursoBank balances are not reconciled, and
  there is no correction workflow for changed source evidence.

### 3. Smaller gaps

- [x] `fin accounts edit` sets IBANs, names and card links without handling the
  configuration file by hand.
- [x] `fin backup` and `fin delete` save and remove all fin state without
  knowing where it lives; only `fin init` creates a database.
- [ ] `bank.journal` is rewritten only by `fin bank sync`, so it is stale after
  `fin accounts apply` or a review decision until the next sync. Every command
  that changes accounting should rewrite it, or the file should go in favour of
  `fin journal export`.
- [x] `fin accounts apply` names the configured accounts that have no imported
  statements, instead of repeating "unknown source account".
- [x] `fin bank sync BANK PATH...`: the bank is a required argument, and the paths
  are any number of files or directories, described in `--help`.
- [x] `fin accounts apply` without a configuration file says that none exists,
  that it is optional, and how to create one with `fin accounts export -o`.
- [ ] `fin paths` prints only JSON, like `bank statements` and `bank companions`.
- [ ] `fin bank statements` and `fin bank companions` print only JSON, unlike the
  other commands, which print text and take `--json`.
- [ ] The 8 ambiguous BoursoBank CSV rows need a way to be resolved.

### 4. Later scope (README "Current scope")

Not started: fetching Paradex data, positions from fills, persisted position
observations, funding, a background inbox watcher. Excel and QIF parsing are listed
as later work in `docs/bank-workflow.md`; until then the manual tells users to export
a supported format instead.

## Suggested order

1. Install and rebuild (next actions 1 and 2): there is no other way to use the
   current code on real data.
2. Categorization rules (workstream 2): the reason to keep books, and what makes the
   748 movements mean anything.
3. Decide the 4 transfer candidates, then opening balances and reconciliation.
4. Adapter steps 6 and 7, when a new bank or format needs them. They are internal and
   change no user-visible behaviour.
