# Records format

Status: version 1 draft, 2026-11-04. The schema and examples exist; fin does not read
the format yet.

A records file is what one institution's statement says, as JSON Lines: every entry of
the statement becomes one record. A **converter** reads a statement and prints the file; fin
validates and stores it, then matches what can be matched across all stored records.
Categories, rules and the journal also stay in fin.

There are two kinds of converter, with the same output and the same checks:

- **Built in.** Institutions fin ships with keep working directly: `fin import wise
  FILE` runs the built-in converter, then imports its records. `fin convert wise FILE`
  prints those records without importing them.
- **Your own.** For any other institution, write a program in any language, needing no
  fin code, and import what it prints: `fin import records.json`.

These commands are the plan; none exists yet.

- Schema: [`formats/records/fin-records.schema.json`](../formats/records/fin-records.schema.json)
- Examples: [`formats/records/examples`](../formats/records/examples). `valid/` is
  accepted, `invalid/` has a line failing the schema, and in `semantic/` every line passes
  the schema but fin refuses the file. A test compares fin and the schema on mutations of
  these examples. The schema describes one line, because a schema cannot describe JSON Lines.

It is evidence, not accounting: a record states what happened to one account and never
names a counterparty account, a category or a decision.

## File

A records file is [JSON Lines](https://jsonlines.org): one JSON object per line, each with
a `kind`. Name it `.jsonl`, or `.jsonl.zst` when compressed with zstd. Fin reads both,
telling them apart by the file's first bytes.

```
{"kind":"header","format":"fin-records","version":1,"institution":"wise","identity":"source"}
{"kind":"source","name":"export.csv","sha256":"..."}
{"kind":"account","id":"balance-EUR","currency":"EUR"}
{"kind":"record","id":"CARD-1/...","account":"balance-EUR","occurred":"2025-01-02","movements":[...]}
{"kind":"end","records":1}
```

A file is one or more **sections**, each from a `header` line to an `end` line. Between
them, lines come in any order, so a converter can print a row as soon as it reads it.
Files can be concatenated, compressed ones included: `cat a.jsonl b.jsonl` is a file.
Sections of one institution and identity read as one document. What two of them both state
counts once, and it is an error if they state it differently, apart from the `statements`
of a record, which add up.

The `end` line counts the section's records, because a file cut between two lines is
otherwise valid JSON Lines. A file with no `end` line is refused.

| Kind | Members |
|---|---|
| `header` | `format` (`fin-records`), `version` (`1`), `institution` (lower case; records of one institution can group together), `identity` (`source` or `inventory`, see below), optional `converter` (name and version, for diagnosis) |
| `source` | `name` and `sha256` of a statement file read; a section has at least one |
| `account` | An account the records use |
| `statement` | Stated period and balances |
| `record` | One source row |
| `end` | `records`, how many `record` lines the section has |

Records keep source order. An empty statement gives a section with no records, not an
error, and with no accounts when the source names none. A statement with no records is a
quiet period.

An account has an `id`, stable across exports, and optionally `bank`, `number`, `type`
and `currency`. `iban` gives its IBAN, and `no_iban` says it has none, such as a card;
with neither, a registered IBAN may supply it.

A statement has `id`, `account`, `currency`, `period_start`, `period_end`, and `opening`
and `closing`, each a `quantity` with `as_of`. Fin reports a statement whose records do
not reconcile; it does not refuse it, because exports can be partial.

## Record

The members outside `details` and `statements` are the **fixed part**. Reimporting a record
whose fixed part differs is refused. Overlapping exports add to `statements`: Wise names
each export's statement differently, so one row can be listed by several.

| Member | Meaning |
|---|---|
| `id` | Identity, unique per account, see below |
| `account` | An `accounts` id |
| `statements` | The `statement` ids that list this row, if any |
| `occurred` | A date, or an instant with an offset, as stated; a date asserts no time of day |
| `value_on` | Value date |
| `movements` | Balance effects; empty when the converter cannot interpret the row |
| `fee` | Non-negative, on a one-movement record: part of its balance effect, posted apart |
| `group` | Key shared by rows the institution states are one activity |
| `supporting` | This row only supports its group, such as a fee row, and never represents it |
| `observation` | This row describes a row stated elsewhere, see below; it is never counted |

A movement has a `role` unique in its record (`primary`, or `from` and `to`), a
`quantity` and a `commodity`. Quantities are decimal strings, never numbers. A symbol
that does not identify an asset is `reported`, scoped to its provider; only ISO 4217
codes are `fiat`.

`details` is **enrichment**. A later import or a paired observation may add to it but may
not change it, so a record gains data without changing identity.

| Member | Meaning |
|---|---|
| `descriptions` | Free texts, main one first |
| `type` | A movement type, if the source states one |
| `counterparty` | `name` and `iban` as stated; evidence for matching, not an account |
| `references` | `servicer`, `end_to_end`, `payment`, `hash` |
| `exchange` | `source`, `target`, `rate`, `target_quantity`, as stated |
| `reported` | An amount stated but not interpreted, with its symbol |
| `fields` | Raw values as `[name, value]`, in source order; rules match these |

## Observations

Some institutions report one payment in two files, such as an OFX statement and a CSV
export with more text. A converter marks the rows of the secondary one `observation`. Each states its one movement, for matching, and its `details`.

Fin pairs an observation with a counted record of the institution: by
`references.servicer` when both have it, otherwise by account, date and quantity. A
unique pair adds the observation's `details` to the record, in either import order. A
pair that is ambiguous or conflicts stays visible for review; it is neither merged nor
counted. An observation with no pair is kept, and it is reported as unmatched.

The Wise converter will mark its CSV rows this way. The CSV names a currency and no account,
so these rows use an account `balance-EUR` that holds no counted record, and they pair by
reference. That reference alone can be shared by a payment and its refund, so pairing also
compares quantity and date. The CSV clock has no offset, so a row's `occurred` is its date
and the clock stays in `fields`.

## Matching

Fin matches three ways, using stored evidence only:

| What | Example | Decided by |
|---|---|---|
| A row and its observation | OFX row and its CSV row | automatic if unique, else review |
| Rows of one activity | Wise rows with one `group` | the institution, through `group` |
| The two sides of a transfer | bank to bank | you, from scored candidates |

## Rules for converters

- **IDs must not depend on the download.** The same row must get the same `id` from any
  export covering it. Use the institution's identifier where it has a stable one. If it
  shares one between rows of one account, such as a payment and its refund, add what
  tells them apart. Never use a row number in a file.
- **`identity`.** Use `source` when IDs identify rows. Use `inventory` when the source has
  none, so identical rows cannot be told apart: then the rows of an account and day must
  agree across files, and a partial or changed day is refused for review. IDs can then be
  derived from the day and position.
- **`group` only for what the institution itself states**, through its own identifiers.
  Never infer it from amounts and dates. Two rows of an institution group when they have
  the same `group`, across files and in any import order.
- **One fact, one place.** Wise repeats a fee on the conversion row and as its own row:
  the row becomes a record, and the repeated figure stays in `fields`.
- **Do not pair rows.** Fin pairs across files, and it sees every import. Keep every entry
  of the statement, including ones that look duplicated. A converter may read several
  files into one section, with a `source` line for each; a row two files both state is
  one record.
- **Report, do not repair.** Pass values as stated, and put what cannot be interpreted in
  `reported` and `fields` with no movements.

## What fin checks beyond the schema

Shape is not enough, so fin also refuses: a line outside a section, a `header` before the
previous section's `end`, a file with no `end` line, an `end` that miscounts, a section
with no `source`, an `id` repeated for an account, an account or
statement listed twice, a reference to an unknown account or statement, a record naming
another account's statement or one statement twice, a role repeated in a record, a `fee` on a record without
exactly one movement, an observation without exactly one movement, a `fiat` code that is
not ISO 4217, and an impossible date.

## From CAMT

The concepts come from CAMT.053 where they fit, so converting CAMT loses little:

| CAMT | Records |
|---|---|
| `Stmt`, `Bal` (`OPBD`, `CLBD`) | `statements`, `opening`, `closing` |
| `Ntry`, `BookgDt`, `ValDt` | `records`, `occurred`, `value_on` |
| `Refs/AcctSvcrRef`, `EndToEndId`, `RmtInf` | `references`, `descriptions` |
| `CcyXchg` | `exchange` |
| `Chrgs` | `fee`, or a `supporting` row |
| `RltdPties` | `counterparty` |

Left out: message headers, status codes and batch structure. An unbooked entry is not a
record.
