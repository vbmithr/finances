# Shared CSV decoding and format profiles

Status: shared engine and interactive single-account CSV import implemented, 2026-10-30. All four bank CSV formats decode rows through
`Csv_reader` and `Csv_schema`, and `Adapter_support` shares companion wiring and
CSV adapter construction.

## Goal

Make an ordinary new CSV export layout a small typed format description, rather
than another hand-written parser and adapter module. Share reading, column
decoding, validation and diagnostics, while retaining explicit policies for
movement identity, overlap and companion matching.

The intended model is one engine, a profile per export layout, and small semantic
hooks where a bank needs them. A profile still describes the format: arbitrary
CSV cannot reliably supply its own account identity, amount interpretation or
accounting meaning.

Interactive setup saves a limited current-account, savings-account or card CSV
profile with imported evidence. User-added OFX institutions can also store an
editable JSON companion layout; each imported companion document retains its
layout, and exact matching includes a required fixed currency.

## What exists today

The project uses the `Csv` library for CSV syntax, with `Csv_reader` and
`Csv_schema` above it. `Csv_reader` shares BOM removal, UTF-8 checks, reader
options and header checks; `Csv_schema` adds typed cells, compiled column
selectors, blank-row policies and row/column diagnostics. All four bank CSV
formats decode rows through `Csv_schema`, and `Adapter_support` shares companion
wiring and CSV-only adapter construction.

The existing formats have different roles:

| Format | Role | Semantics that must remain explicit |
| --- | --- | --- |
| BoursoBank CSV | Enrich OFX movements | Exact institution/account/date/amount/description matching and metadata conflicts |
| Bunq CSV | Enrich MT940 movements | Repeated-transaction occurrence inventories and currency ambiguity |
| Wise CSV | Enrich CAMT movements | Normalized references, local-clock matching and fee/FX metadata conflicts |
| Revolut CSV | Create movements | Account labels, transaction state, fee arithmetic, running balances and duplicate occurrences |

A shared engine must not turn the first three into another movement stream.
Their companion observations may arrive before the primary source and remain
unmatched until evidence is available.

## Shared decoding layer

Introduce a typed description such as `'row Csv_schema.t`, inspired by Jsont's
typed descriptions. It should decode rows into the existing OCaml record types,
replacing positional list matching and repeated array extraction. CSV writing is
not required for this import refactor.

The shared layer should provide:

- CSV dialect options, UTF-8 validation and optional BOM handling, using the
  existing CSV library rather than another quoting/parser implementation.
- Header validation and compilation of column selectors once per document.
- Typed cells: text, required text, optional values, exact quantities, currencies,
  calendar dates and clocks with an explicit interpretation.
- Row construction and validation, plus a document-validation callback.
- Diagnostics identifying the format, data row and column; distinguish malformed
  CSV from a field conversion or a document-level inconsistency.
- A raw-row envelope retaining the original field strings and data-row number
  alongside the typed value.

Keep row numbering compatible with today's persisted companion rows: the first
data record is row 1, excluding the header. A CSV record containing embedded
newlines is still one record, not several data rows.

Trimming and blank-row policy belong in the profile. BoursoBank and Wise currently
trim headers and skip blank data rows; Bunq and Revolut have different acceptance
rules. A blanket cleanup pass would change those rules and could alter evidence.
Preserve raw cells even when the typed decoder normalizes their contents.

### Column selectors

Support positions with expected header names, and named selectors when unique.
Never silently choose one column when a requested name is duplicated. The
underlying library's name lookup alone is insufficient for all current exports.

BoursoBank illustrates why positions remain useful:

| Zero-based position | Expected header | Meaning | Decoder |
| --- | --- | --- | --- |
| 0 | `Date Opération` | Booking date | ISO calendar date |
| 6 | `Solde` | Movement amount | French decimal notation |
| 8 | `Numéro Compte` | Source account identifier | Required text |
| 10 | `Solde` | Reported balance | Optional dot-decimal quantity |

The two `Solde` columns have different meanings and number formats. An explicit
occurrence selector could also represent them, but name-only lookup cannot.
The first migration should preserve exact expected header layouts, rather than
silently accepting reordered or additional columns.

### Scalar decoding

Reuse `Quantity`, `Date`, Ptime, `Iso4217` and `Iban` where appropriate. Quantities
must stay exact; do not route them through floats. Decimal signs, grouping
characters, precision requirements and decimal marks must be specified by each
format, not guessed from individual values.

Distinguish calendar dates from instants. For clocks without an offset, the
profile must select an explicit interpretation. Preserve Wise's existing UTC
representation and source-local matching behavior, rather than treating those
two uses of time as interchangeable.

## Format profiles and policies

A format profile combines:

| Part | Responsibility |
| --- | --- |
| Identification | Institution and stable format identifier, reader dialect, header recognition |
| Row schema | Typed fields, normalization, optional values and row validation |
| Role | Primary movement source or companion observations |
| Context | Required account label and other explicit import hints |
| Document validation | Ordering, running balances and consistency across records |
| Integration | Conversion to the existing `Document.t`, or companion annotation/description |
| Identity and matching | An explicit existing policy or a small format-specific hook |

Use ordinary typed functions for semantic checks. Avoid growing a general
expression language inside the profile format.

Recognition and validation remain distinct. A recognizable but malformed export
should report its actual parse error, rather than disappear as unsupported.
Keep the bank selected explicitly through `fin import BANK PATH...`; recognizing
a CSV header does not establish account ownership.

### Policies that remain meaningful code

The abstraction should make these rules easier to see, not hide them:

- Revolut skips only the specifically supported reverted-record shape. Unknown
  states or malformed completed records are errors, not rows to drop.
- Revolut movements use amount minus fee, with separately posted fees and
  per-currency running-balance/order checks. The first reported balance does not
  independently establish an opening balance.
- Repeated identical transactions retain occurrence identities. Hashing each row
  and deduplicating equal hashes would lose legitimate repeated movements.
- The kernel's `Source_ids` and `Day_inventory` overlap policies stay explicit;
  sharing a CSV reader does not choose an overlap policy for a new format.
- BoursoBank's exact matching, Bunq's occurrence matching and Wise's reference/
  clock matching retain their current ambiguity and conflict rules. Common
  indexing helpers can follow if the same matching operation is genuinely shared.
- Companion annotation remains enrichment. For example, a reported Wise fee must
  not become an additional posted fee merely because the cell decoder is shared.

Retain bank-specific hooks when needed. The success criterion is eliminating
repeated mechanics, not eliminating every function associated with a bank.

## Fit with the existing architecture

Start shared modules inside `finances.banks`, alongside `Adapter_support`.
They can use the core's validated types and remain pure, with no SQLite, hledger
or filesystem dependency. Separate a standalone format library later only if
the boundary proves useful.

Keep `Adapter.S`, `Document.t` and `Companion.t` as the integration boundaries:

- A primary profile produces events and, where supported, statement observations.
- A companion profile produces numbered raw-field observations and supplies the
  existing annotation, description and matching operations.
- SQLite archiving, transactions, import history, overlap enforcement and
  projection remain in the kernel. File and ZIP traversal remain in `bank/`.

Today `Adapter.S` selects overlap policy and account-label requirements for the
institution, not separately for each format. Initial profiles must respect those
contracts. Changing their granularity is separate work, not an implicit effect
of extracting the CSV decoder.

Existing adapters can initially delegate their CSV parsing to profiles while
continuing to handle XML or MT940 normally. For a straightforward CSV-only
institution, a shared adapter constructor should eventually supply the required
`Adapter.S` wrapper from profile values. Adding a layout should not require
copying another full module just to satisfy the registry interface.

Keep stored format IDs, companion policy IDs, canonical payload codecs and source
identities unchanged during this extraction. The profile's type description is
not automatically a new persisted payload format.

## Migration sequence

1. Capture the current acceptance rules and raw-field/row-number behavior in
   fixtures, including malformed inputs and duplicate transactions.
2. Extract the common reader and header handling. The current parsers delegate
   to `Csv_reader` without changing their public interfaces or normalized output.
3. Introduce typed cells and row construction. Move BoursoBank and Bunq layouts
   into descriptions; leave their matching policies in place.
4. Move Wise's layout into a description, keeping cross-field date, direction,
   reference and fee/FX checks explicit.
5. Move Revolut's layout, using its existing movement constructor and document
   validation. Preserve reverted-record handling, balances and occurrences.
6. Factor repeated companion glue and add a generic adapter constructor where
   the migrated profiles demonstrate a useful common contract.
7. Demonstrate that a further simple layout can be added as a profile without
   another copy of the reading and column-conversion machinery.

Each step should be reviewable independently. Do not replace all parsers and
identity policies in one change.

## Acceptance criteria

- Existing valid inputs produce identical typed values, raw companion fields,
  row numbers, source identities and canonical payloads.
- Reimports and overlapping exports preserve duplicate-occurrence behavior and
  never create a second movement stream from companions.
- Matching, ambiguity and conflict outcomes remain unchanged.
- Current malformed, ambiguous and inconsistent inputs remain rejected or
  explicitly queued for review. Error messages gain useful field context without
  silently broadening acceptance.
- Cover BOMs, quoted separators/newlines, invalid UTF-8, duplicate header names,
  blank rows, localized decimals, invalid dates, unknown currencies, reverted
  rows, repeated transactions and running-balance failures.
- Compare every report and stored payload against the previous build on rebuilt
  personal databases, and check performance on realistic imports and reads.
- The common layer removes repeated parsing code; new profiles do not need
  manual field indexing scattered through their implementation.

No schema change or database rebuild should be needed if these representations
remain identical. Any intentional change to identity or persisted interpretation
is separate work under the project's schema/version policy.

## Alternatives and later decisions

Small reader helpers alone are a useful first extraction, but leave most typed
column-conversion boilerplate in place. The intended end state includes typed row
descriptions, not just a shared `Csv.of_string` wrapper.

hledger's [CSV rules](https://hledger.org/hledger.html#csv) offer a useful reference
for declarative mappings and conditional handling. Their output is an hledger
journal. This design instead targets fin's existing evidence, movement and
companion contracts; keeping that boundary also preserves the kernel's
independence from hledger.

Defer the external configuration syntax, public package extraction, permissive
header evolution and a universal matching language. First establish the typed
OCaml interface against the four current layouts and their actual policies.
