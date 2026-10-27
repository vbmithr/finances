# Bank adapters and format libraries

Status: design, not implemented. Today Store dispatches explicitly to four
built-in banks, and 18 SQLite tables are specific to one bank. This document
fixes the target: banks become adapters behind one interface, standard file
formats become independent libraries, and the schema has no bank-specific
tables.

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
CLI                  builds the registry, passes it to Store and sync
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

A reader for `camt.053` (statement), later `camt.052` (intraday report) and
`camt.054` (notification), across their namespace versions (`.001.02` to
current). Wise's parser accepts only `camt.053.001.10` today.

Typed model of the subset banks use: `Stmt` (Id, ElctrncSeqNb, FrToDt, Acct
with IBAN/Othr, Ccy, Svcr), `Bal` (type codes OPBD/CLBD/ITBD/CLAV…, amount,
CdtDbtInd, date or datetime), `Ntry` (Amt, CdtDbtInd, RvslInd, Sts, BookgDt,
ValDt, AcctSvcrRef, BkTxCd domain and proprietary, AddtlNtryInf, Chrgs) and
`NtryDtls/TxDtls` (Refs including EndToEndId, RltdPties debtor/creditor names
and accounts, RmtInf structured and unstructured). Reconciliation checks
(opening + entries = closing, entry counts) are offered as functions, not
forced during parsing.

Wise keeps in its adapter: the servicer check, which field is its reference
(`BkTxCd/Prtry/Cd`), cashback reference normalization and identity rules.
Counterparty IBANs and charges become available to the movement model.

### `mt940`: SWIFT MT940/MT942

The field grammar: `:20:`, `:21:`, `:25:`, `:28C:`, `:60F:/:60M:`, `:61:`,
`:86:`, `:62F:/:62M:`, `:64:`, `:65:`; and the `:61:` subfields (value date,
optional entry date, D/C/RC/RD, funds code, amount, type code, customer
reference, `//` bank reference, supplementary details). Entry-date year
inference is a library function with the rule documented.

`:86:` is free text whose structure varies by bank. The library returns it raw,
plus optional parsers for common structured layouts (`/TAG/value` sequences,
`?nn` subfields). Bunq's `/IBAN/…/NAME/…/REMI/…` decoding, the `NONREF`
handling and the `BUNQ` header check stay in the Bunq adapter.

### `ofx`: OFX 2.x (implemented)

`formats/ofx`, public name `finances.ofx`, depending on Base, Ptime, Re and
xmlm. It returns statements, accounts, balances and transactions with values as
written and each STMTTRN tree; `calendar_date` reads OFX dates without moving
them across time zones. BoursoBank's `Bank_ofx` keeps the bank's rules on top:
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
  and describes rows for `fin bank companions`. Storage, audit, journal emission
  and listing are the kernel's.

`Ofx_bank.Make (struct let institution = … let aliases = … end)` is a complete
adapter for any OFX bank. BoursoBank is that plus its CSV companion.

### Normalized movement

`Movement.t` replaces `Bank_transaction.t` everywhere in the kernel:
projection, transfers, ranking, IBAN evidence and the journal. It carries
enough for the kernel never to inspect a payload:

- `account`: bank ID, account ID, account type, currency.
- `own_iban`: `Iban.t option`, plus whether an IBAN is applicable at all.
  This replaces the `CREDITCRD` special case in Store.
- `booked_on`, `value_on` (date2 in the journal; today only Wise and Bunq).
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

- Statement period and balance dates are the bank's own text: a calendar date
  (Bunq) or a timestamp with its offset (Wise). Nothing reads them as instants,
  and converting to milliseconds would lose the offset.
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
touching the file, and the database is rebuilt by syncing the archived inboxes
and applying the account configuration again. Schema 11 replaced the 18
bank-specific tables this way. Verification compares reports from a database
rebuilt with the previous build against one rebuilt with the new build.

## Implementation sequence

1. **Baseline.** Snapshot the personal database (21 OFX, 154 Bunq, 35
   Revolut, 538 Wise events): event set, journal, reconciliations, companion
   matches, transfer candidates. Every later step diffs against it.
2. **`iban` library.** Done: `Account_identity`, account configuration and Store
   use `Iban.t`; reports on the personal database are unchanged.
3. **Kernel types and registry.** Done for reading: `finances.core` holds
   `Movement.t`, `Adapter.S` and `Registry`; the banks are a separate library
   without SQLite; projection, transfers, ranking, IBAN and card rules, and
   journal value dates use `Movement.t` from the registry; imports under an
   unregistered institution are rejected. The adapter signature has only the
   read side (`decode`, `kinds`, `aliases`); `probe`/`parse`, overlap policy and
   companions are added with steps 4 and 5. Reports and a full inbox replay on
   the personal database are unchanged.
4. **Schema 11.** Done: the generic tables above, with Store's import, matching
   and reconciliation code written over them and one `statement_reconciliations`
   for every bank. Rebuilding the personal database gives the same events,
   journal entries, reconciliations and match states; only companion row IDs in
   journal tags are renumbered.
5. **Import behind the adapters.** Done: probing, parsing, overlap policy and
   companions are adapter members; Store, the journal exporter and the CLI name
   no bank, and the kernel library does not depend on `finances.banks`. The
   per-bank commands became `fin bank statements`, `fin bank companions` and
   `fin bank match`; `Bank_import.format` is the adapter's format text. A
   rebuild from the personal inboxes gives the same events, entries, match
   states and reconciliations; journal provenance is one `companion-row-ids`
   tag.
6. **Format libraries for the remaining banks:** `mt940` under Bunq and `camt`
   under Wise, like `ofx` under BoursoBank.
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
