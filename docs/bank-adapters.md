# Bank adapters and format libraries

Status: the registry and generic schema are implemented. BoursoBank, Wise, Bunq
and Revolut use data-driven presets and shared import components. Broader format
coverage and external adapter conformance work remain.

## Rules

These are invariants, not goals. Tests enforce them (see Enforcement).

1. **No bank-specific schema.** Adding, changing or removing a bank never adds
   DDL. Every bank uses the same kernel tables; facts that do not fit go into
   the source event payload or the companion row fields, never into a new table
   or column.
2. **Adapters never touch SQLite.** An adapter is pure: bytes and an explicit
   context in, a normalized document out; stored events in, normalized
   movements out. The kernel owns archiving, transactions, duplicate detection,
   overlap policy, reconciliation arithmetic, review decisions and projection.
3. **Formats are not institutions.** OFX, CAMT and MT940 are standards with
   their own libraries. An adapter is an institution that combines one or more
   format parsers with that bank's quirks and identity rules.
4. **Identities are deterministic.** The same document always yields the same
   events, so reimports and overlapping exports never duplicate movements. The
   software is in development: changing an identity rule bumps the schema
   version, and databases are rebuilt from the original statements. There are
   no migrations.
5. **Adapters decode their own events.** There is no generic payload codec for
   existing banks. Dispatch is by `(venue, kind)`, because one kind
   (`bank-transaction`, OFX) is shared by every OFX institution.

## Layers

```
format libraries     iban   camt   mt940   ofx         (no finances dependency)
        |
bank adapters        boursobank  wise  bunq  revolut    (no Rel/SQLite dependency)
        |
kernel               Adapter types, Registry, Store, projection, review
        |
CLI                  builds the registry, passes it to Store and import
```

Dune dependencies enforce the arrows. Store receives a registry value and does
not name any adapter. Only the CLI (or a custom `fin` built with external
adapters) lists which adapters are linked.

## Format libraries

Each library is a faithful, typed reader for one standard. It knows the
standard, not banks and not accounting.

- Depends only on Base, decimal, Ptime, xmlm or re as needed. It must not depend
  on `finances`, so it can be extracted into its own repository like
  `ocaml-hledger` once stable.
- Is lossless where the standard is loose: it keeps raw strings for fields
  banks commonly abuse (OFX `DTUSER`, MT940 `:86:`), and exposes typed views
  beside them. It never repairs data silently.
- Rejects what is not in the standard, with precise errors. Bank-specific
  tolerance lives in the adapter, applied to the raw field.
- Uses exact decimals and calendar dates distinct from instants.
- Produces no identities, ledger accounts or source IDs.
- Is tested against real captured files (anonymized fixtures) as well as the
  specification, since bank exports diverge from it.

Start in this repository under `formats/` as separate dune libraries, each with
its own public name. Extract once an interface has survived two banks. Check
names against opam before publishing.

### `iban`: ISO 13616 (implemented)

`formats/iban`, public name `finances.iban`. The kernel uses `Iban.t` for every
IBAN it holds; text appears only in SQLite and JSON.

ISO 13616-1:2020 defines the IBAN structure. Check digits use ISO/IEC 7064
MOD 97-10. SWIFT, as registration authority under ISO 13616-2, publishes the
IBAN Registry with each country's length and BBAN layout.

`Account_identity.normalize_iban` already implements the generic part: the
length bounds, the `CCdd` prefix and MOD 97-10. The library adds:

- An abstract `Iban.t` that can only be built validated; electronic and
  printed (groups of four) renderings.
- Per-country length and BBAN structure, bank and branch identifier positions
  and SEPA membership, from a table generated from the SWIFT registry PDF by
  `tools/registry_of_pdf.py` (Release 103, Sep 2026; 89 countries). The script
  rejects any block whose structure, lengths or example disagree. Unknown
  country codes are rejected.
- Rejection of check digits `00`, `01` and `99`. They can pass MOD 97-10 but
  are never issued.
- Country code, check digits and BBAN accessors, plus, where the registry
  defines them, the bank and branch identifiers.
- Not yet: `Bic.t` (ISO 9362), which CAMT will need.

This also tightens `inferred_iban`. Today any checksum-valid 15–34 character
account identifier counts as IBAN evidence; with the registry, the country
length must match too.

### `camt`: ISO 20022 cash management

The `finances.camt` library reads `camt.053` XML independently of an
institution, retains the XML tree, namespaces and attributes, and exposes
statement, account, balance and entry views. Its current views cover the fields
needed by the existing importer; other XML remains available in the tree. It
accepts CAMT.053 namespace versions, while Wise's configured source profile
requires `.001.10`. Original document bytes are stored separately.

`Camt_source` interprets those views using a typed profile for the servicer,
reference field, reference normalization, account and source identity. It also
checks booked entries and statement reconciliation. The shipped profile covers
Wise; this is not yet a general-purpose CLI importer for arbitrary CAMT exports.
The CAMT/CSV adapter and its clock-based CSV companion decoder are shared; the
Wise column names and conflict fields come from a validated bundled profile. The
current CSV decoder assumes comma-separated, day-first millisecond timestamps,
and signed dot-decimal amounts. Wise's reported type codes use bundled automatic
rules stored in the same rule table as other source rules. Each CSV observation
retains its resolved type at import; explicit refresh previews changes. Further
work includes broader user-configurable ISO 20022 fields and reconciliation
policies, and CAMT.052/054. A runtime definition can configure this existing
CAMT/CSV profile shape without rebuilding fin.

### `mt940`: SWIFT MT940/MT942

The shared MT940 source currently supports the observed Bunq-style statement
shape and a profile for institution identity, framing, entry expression, memo
markers and its CSV companion. It preserves dated occurrence identities and
checks opening plus movements equals closing. Other MT940 variants, including
other field forms and memo structures, still need format support; this is not a
complete MT940/MT942 library.

### `ofx`: OFX 2.x (implemented)

`formats/ofx`, public name `finances.ofx`, depending on Base, Ptime, Re and
xmlm. It returns statements, accounts, balances and transactions with values as
written and each STMTTRN tree; `calendar_date` reads OFX dates without moving
them across time zones. `instant` provides a Ptime view of OFX datetimes, applying
midnight and GMT defaults and explicit decimal-hour offsets per section 3.2.8 of
the [OFX specification](https://www.financialdataexchange.org/common/Uploaded%20files/OFX%20files/OFX%202.2.pdf).
`Ofx_source` exposes balance observation instants as Ptime and applies:
exact quantities, the `DTUSER` exclusion, correction refusal and the canonical
payload, whose bytes are unchanged (an inbox replay re-imports every stored
movement as existing).


XML OFX 2.x bank statement message sets: STMTRS, STMTTRN, LEDGERBAL/AVAILBAL,
status codes and the date syntax with optional time and timezone. Correction
records (CORRECTFITID, CORRECTACTION) are parsed and surfaced, not rejected by
the library; the adapter decides. Credit-card statements (CCSTMTRS) and OFX 1.x
SGML can follow. The canonical transaction rendering used for BoursoBank
payloads stays byte-identical (rule 4); it moves to the adapter if the library's
tree differs.

### CSV is not a format library

Bank CSV exports share no standard. Each CSV layout stays in its adapter. The
kernel's adapter toolkit offers helpers for the common mechanics: BOM handling,
separator, exact header match, decimal comma and validated UTF-8.

## Adapter contract

The signature lives in `core/adapter.ml`:

```ocaml
module type S = sig
  val institution : string          (* the venue of every event it produced *)
  val aliases : string list         (* names in other banks' descriptions *)
  val kinds : string list           (* event kinds it decodes *)
  val decode : Source_event.t -> Movement.t Or_error.t
  val formats : string list         (* recorded in import history *)
  val probe : filename:string -> string -> string option
  val takes_account_label : bool    (* exports that do not name their account *)
  val parse : context:context -> format:string -> string -> Document.t Or_error.t
  val overlap : overlap             (* Source_ids | Day_inventory *)
  val companions : Companion.t list
end
```

Automatic evidence rules record card references and reversal clues at import.
Review workflows consume those stored clues and still require confirmation.

- The kernel probes every registered adapter. A file is parsed by the requested
  institution's adapter when exactly one of its formats recognizes it; a file
  only another bank recognizes is rejected with that institution's name; a file
  no bank recognizes is unsupported. A recognizable but malformed file is still
  recognized, so parsing explains what is wrong.
- `context` carries explicit user hints (Revolut's account label). An adapter
  returns the hints that are part of the evidence in `Document.context`; the
  kernel stores them per document, so the same bytes cannot later mean another
  account.
- `Document.t` holds statements with the events they list, other events,
  companion rows (row number and raw fields) and context. It never contains
  ledger accounts or categories. The kernel checks that every event carries the
  adapter's institution and one of its kinds.
- `overlap` selects the kernel's duplicate policy: `Source_ids` (OFX FITID,
  Wise references) or `Day_inventory` (Bunq, Revolut: per account and day,
  sorted payloads must agree across documents).
- A `Companion.t` matches rows of one format against the institution's own
  events, annotates matched rows for the journal (a value date and named tags)
  and describes rows for `fin statement companions`. Storage, audit, journal emission
  and listing are the kernel's.

`Ofx_bank` shares OFX decoding, probing and adapter defaults. Parsing takes the
institution explicitly: `Ofx_bank.parse ~institution contents`. User-added
institutions can use a generic OFX adapter. BoursoBank composes that importer,
its CSV companion and aliases from `banks/institution_presets.json`; source
evidence and typing come from ordinary default rules. A user-added OFX
institution can configure an exact-match CSV companion; its layout is stored
with each document. Generic
primary CSV remains a separate source and cannot be mixed with OFX.

### Normalized movement

`Movement.t` replaces `Bank_transaction.t` everywhere in the kernel:
projection, transfers, ranking, IBAN evidence and the journal. It carries
enough for the kernel never to inspect a payload:

- `account`: bank ID, account ID, account type, currency (`Iso4217.t`).
- `own_iban`: `Iban.t option`, plus whether an IBAN is applicable at all.
  This replaces the `CREDITCRD` special case in Store.
- `booked_on`, `value_on` (date2 in the journal; today only Wise and Bunq).
- Typed bank timestamps use Ptime. Wise CSV clocks assume UTC but match CAMT by
  source-local millisecond clock; original raw fields remain evidence. OFX balance
  times follow OFX's timezone defaults. Invalid DTUSER stays raw metadata.
- `amount` (signed, net change of the account) and `fee`, non-negative and
  posted separately. The projection invariant stays: bank `+amount`, counter
  `-(amount + fee)`, fees `+fee`.
- `counterparty`: optional name and `Iban.t`. This replaces the Bunq-only
  `counterparty_iban`.
- `reference`: optional bank or end-to-end reference, used for transfer ranking.
- `descriptions`: ordered free texts.
- `aliases`: institution names as they appear in other banks' descriptions.
  This replaces the BoursoBank/Boursorama case in `Transfer_ranking`.

## Generic schema

The kernel tables stay as they are: source events, import documents and their
links, import attempts, journal entries and postings, account mappings,
identities and settlements, categories, transfer decisions, and their histories.
Bank-specific tables are replaced by:

| Table | Purpose | Replaces |
| --- | --- | --- |
| `document_contexts(document_id PK, context_json)` | Hints an import used, immutable | `revolut_documents` |
| `statements(id, document_id, ordinal, institution, source_account, currency, reference?, period_start, period_end, opening, opening_as_of, closing, closing_as_of)` | Bank-stated balances and period, immutable | `wise_statements`, `bunq_statements` |
| `statement_events(statement_id, event_id)` | Movements a statement includes | `wise_statement_events`; Bunq's implicit document links |
| `day_inventories(institution, source_account, booked_on, signature, document_id)` | Overlap policy `Day_inventory` | `bunq_day_inventories`, `revolut_day_inventories` |
| `companion_rows(id, document_id, row_number, institution, format, fields_json)` | Raw companion observations, immutable | `bank_csv_rows`, `wise_csv_rows`, `bunq_csv_rows` |
| `companion_matches(row_id PK, event_id?, state, reason, candidates_json, policy)` | Current match | three `*_csv_matches` |
| `companion_match_changes(...)` | Append-only match audit | three `*_csv_match_changes` |

Notes:

- Event occurrences and statement period and balance times use `Temporal.t`: `Date of Date.t`
  (Bunq) or `Instant of Ptime.t` (Wise). Rel codecs serialize them to calendar-date
  or canonical UTC text. Original offset-bearing text remains in archived source
  documents; local booking dates and raw matching clocks stay separate.
- `bank_import_attempts.format` loses its CHECK list. The format is validated
  against the registry when written. Stored values are kept.
- `companion_rows` stores fields only. BoursoBank's typed columns (category,
  balance, pointage…) are recomputed by the adapter's `of_fields`, as Wise and
  Bunq already do. On replay, differing stored fields are an error for every
  bank; today only BoursoBank checks.
- Bunq statements gain explicit `statement_events`, so reconciliation is one
  kernel function: opening + sum of linked movements − closing.

## No migration

Schema changes bump the version; `Store` refuses any other version without
touching the file, and the database is rebuilt by importing the archived inboxes
and importing the account settings snapshot again. Schema 11 replaced the 18
bank-specific tables this way. Verification compares reports from a database
rebuilt with the previous build against one rebuilt with the new build.

## Implementation sequence

1. **Baseline.** Snapshot the personal database (21 OFX, 154 Bunq, 35
   Revolut, 538 Wise events): event set, journal, reconciliations, companion
   matches, transfer candidates. Every later step diffs against it.
2. **`iban` library.** Done: `Account_identity`, account snapshots and Store
   use `Iban.t`; reports on the personal database are unchanged.
3. **Kernel types and registry.** Done for reading: `finances.core` holds
   `Movement.t`, `Adapter.S` and `Registry`; the banks are a separate library
   without SQLite; projection, transfers, ranking, IBAN and card rules, and
   journal value dates use `Movement.t` from the registry; imports under an
   unregistered institution are rejected. The adapter signature has only the
   read side (`decode`, `kinds`, `aliases`); `probe`/`parse`, overlap policy and
   companions are added with steps 4 and 5. Reports and a full inbox replay on
   the personal database are unchanged.
4. **Schema 11.** Done (schema 12 added `movements`, and schema 13 typed timestamps): the generic tables above, with Store's import, matching
   and reconciliation code written over them and one `statement_reconciliations`
   for every bank. Rebuilding the personal database gives the same events,
   journal entries, reconciliations and match states; only companion row IDs in
   journal tags are renumbered.
5. **Import behind the adapters.** Done: probing, parsing, overlap policy and
   companions are adapter members; Store, the journal exporter and the CLI name
   no bank, and the kernel library does not depend on `finances.banks`. The
   per-bank commands became `fin statement list`, `fin statement companions` and
   `fin statement match`; `Bank_import.format` is the adapter's format text. A
   rebuild from the personal inboxes gives the same events, entries, match
   states and reconciliations; journal provenance is one `companion-row-ids`
   tag.
6. **Format libraries for the remaining banks:** the `camt.053` reader is shared;
   extract `mt940` under Bunq and complete generic CAMT import.
7. **External adapter example and conformance suite,** written against the
   finished interface.

## Enforcement

- `schema_test`: the set of tables equals the kernel list. No table or column
  name contains a registered institution or format name.
- Dune: format libraries cannot depend on `finances`; adapters cannot depend on
  `rel` or `rel.sqlite3`; the store library does not depend on adapters.
- Registry construction fails on duplicate institutions, duplicate
  `(institution, kind)` or duplicate `(institution, format)`.
- Conformance suite run by every adapter: deterministic parsing, exact amounts
  and fees, date and timezone boundaries, overlapping exports, repeated
  identical payments, malformed inputs, replay without new events, decode of
  every produced event, and a stable journal.

## Current identity rules

Changing any of these requires a schema version bump and a rebuild:

- Revolut source IDs hash a JSON object that includes `"format":"revolut-v1"`.
  The tag is therefore part of identity.
- OFX events of every institution share kind `bank-transaction` and store
  account identity, currency and canonical STMTTRN XML; `DTUSER` is omitted.
- Bunq identity is the SHA-256 of currency plus the normalized movement key,
  plus an occurrence ordinal. Wise identity is reference, nanosecond instant and
  direction.

## Open questions

- Partial days: an export that covers its own export date blocks every later
  export of that day under `Day_inventory`. Options are excluding the export
  day, or a correction workflow that supersedes an inventory with an audit row.
- Statement continuity: whether consecutive statements of one account should
  chain (closing = next opening) as a kernel check.
- Wise fees: the observed CAMT has no `Chrgs`. Whether the CSV fee column should
  ever become a posted fee, or stay a journal tag.

## Transaction rules

Fresh databases install bundled BoursoBank defaults into the same ordered rule
table as user rules. Store applies automatic type and category actions to new
movements; changing a rule does not rewrite existing movements. Type refresh and
rule application preview historical changes. Wise companion type-code defaults
are in the same rule table, and their import-time snapshots have a separate
previewed refresh.
