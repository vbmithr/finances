# Changes

Entries go at the top, under Unreleased, in the same commit as the change. Say
what a user of `fin` must know: new or changed commands and options, imports that
are now accepted or rejected, output format changes, and any **Action required**,
such as rebuilding the database after a schema change. Internal changes get a
short line of their own.

## Unreleased

### 2026-10-27

**Action required:** `fin bank sync` takes the bank as its first argument:
`fin bank sync boursobank ~/statements/bourso` instead of
`fin bank sync ~/statements/bourso --institution boursobank`. `--institution` is
gone from `sync`, and so is its `boursobank` default.

- `fin bank sync BANK PATH...` accepts any number of files and directories.
  Directories are searched recursively as before; a file named explicitly is always
  processed, even if hidden, and fails the command if no bank recognizes it. A file
  reached twice is imported once. `--help` describes both arguments.

**Action required:** schema 11 refuses databases created by earlier versions, without
changing them. Move the old database aside and rebuild it from your statement
folders (see "Storage and backups" in the manual).

- `make install` builds the Arch package and installs it with `sudo pacman -U`;
  on systems without pacman it stops and points to `make package`.
- `fin db reset` moves the database and its `bank.journal` aside under a
  timestamped name, so starting over needs no paths; the account configuration is
  kept. The unsupported-schema error now says to run it.
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
