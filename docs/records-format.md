# Records format

Status: version 1 draft, 2026-11-04. `fin records check FILE` validates a file and `fin convert`
prints one. Fin imports records for FDM; other institutions still import through their adapters.

A records file is what one institution's document says, as JSON Lines: every entry becomes
one record. The document may be a statement, or only a list of movements with no stated
period or balances. It then has no `statement` line, and says nothing about being complete. A **converter** reads a statement and prints the file; fin
validates and stores it, then matches what can be matched across all stored records.
Categories, rules and the journal also stay in fin.

There are two kinds of converter, with the same output and the same checks:

- **Built in.** Institutions fin ships with keep working directly: `fin import wise
  FILE` runs the built-in converter, then imports its records. `fin convert wise FILE`
  prints those records without importing them.
- **A CSV definition.** A CSV-only institution needs no code, only a file saying which column
  means what: `fin convert --definition mine.json FILE`, see [below](#csv-definitions).
- **Your own.** For anything else, write a program in any language, needing no fin code, and
  import what it prints: `fin import records.jsonl`.

`fin convert` opens directories and ZIP files, and reads the CSV layouts of the institutions
below, Wise CAMT, BoursoBank OFX and bunq MT940. Importing records, including through `fin import`, is the
plan.

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
of a record, which add up, and what one says of an account and another does not. A source with
row IDs and one without make separate sections.

The `end` line counts the section's records, because a file cut between two lines is
otherwise valid JSON Lines. A file with no `end` line is refused.

| Kind | Members |
|---|---|
| `header` | `format` (`fin-records`), `version` (`1`), `institution` (lower case; records of one institution can group together), `identity` (`source` or `inventory`, see below), optional `converter` (name and version, for diagnosis) |
| `source` | `name` and `sha256` of a statement file read; a section has at least one |
| `account` | An account the records use |
| `statement` | Rows the source says belong together, with their period or total if stated |
| `balance` | A balance the source states at a point in time |
| `record` | One source row |
| `end` | `records`, how many `record` lines the section has |

Records keep source order. An empty statement gives a section with no records, not an
error, and with no accounts when the source names none. A statement with no records is a
quiet period.

An account has an `id`, stable across exports, and optionally `bank`, `number`, `type`
and `currency`. `iban` gives its IBAN, and `no_iban` says it has none, such as a card;
with neither, a registered IBAN may supply it.

A statement has `id` and `account`, and optionally `period_start` with `period_end`, and
`total`, what its rows add up to with signs as in the movements, as a `quantity` and a
`commodity`. A source may state neither. Rows list it in `statements`. Fin reports a
statement that does not reconcile; it does not refuse it, because exports can be partial.

A balance has `account`, `quantity`, `commodity`, `as_of` and a `type`: `opening`, `closing`,
`available`, or `day_end` for one balance per day whichever rows it follows. It stands alone,
so a source with no statement can state one, and overlapping exports state it once.

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
| `movements` | Balance effects; empty when the source says the row had none, or the converter cannot interpret it, with the amount in `reported` |
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
| `exchange` | `source`, `target`, `rate`, and a signed `target_quantity`, as stated |
| `reported` | An amount stated but not interpreted, with its symbol |
| `balance_after` | The account's balance after this row, if the source displays one |
| `valuations` | What the source says this row is worth, as `quantity` and `commodity`, as stated |
| `fields` | Raw values as `[name, value]`, in source order; rules match these |

A displayed running balance is evidence, so it is kept as stated, never computed. It is in
the commodity of the record's one movement, and sits in `details` so that it reaches the
record when it comes from an observation, as Wise's does. A balance that does not follow
each row is a `balance` line instead.

## Observations

Some institutions report one payment in two files, such as an OFX statement and a CSV
export with more text. A converter marks the rows of the secondary one `observation`. Each states its one movement, for matching, and its `details`.

Fin pairs an observation with a counted record of the institution: by
`references.servicer` when both have it, otherwise by account, date and quantity. A
unique pair adds the observation's `details` to the record, in either import order. A
pair that is ambiguous or conflicts stays visible for review; it is neither merged nor
counted. An observation with no pair is kept, and it is reported as unmatched.

The Wise converter marks its CSV rows this way. The CSV names a currency and no account,
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
with no `source`, a balance of an unknown account, an `id` repeated for an account, an account or
statement listed twice, a reference to an unknown account or statement, a record naming
another account's statement or one statement twice, a role repeated in a record, a `fee` or
`balance_after` on a record without exactly one movement, an observation without exactly one movement, a `fiat` code that is
not ISO 4217, and an impossible date.

## Coverage

Audited on 2026-11-04 against real files of each institution, by structure and counts. A
source file is often no statement at all, only a list of movements.

| Source | What it states | In the format |
|---|---|---|
| Wise CAMT | Period, opening and closing, entries with instants | `statement`, `balance`, `record` |
| Wise CSV | Row list, running balance after each row, exchange columns | observations with `balance_after` and `exchange` |
| BoursoBank OFX | Period, one available balance, entries with `FITID` | `statement` with a period, an `available` balance |
| BoursoBank CSV | Row list with two columns both named `Solde`, bank category | the second `Solde` as `day_end` balances; the category in `fields` |
| bunq MT940 | Opening and closing, entries with a value date, no row IDs | `balance`, `inventory` identity |
| bunq CSV | Row list with date and value date, no IDs, identical rows | observations, `inventory` identity |
| Revolut CSV | Row list per currency, running balance, `REVERTED` rows | `balance_after`; a reverted row has no movements and a `reported` amount |
| CDC CSV | Row list of up to two legs, stated fiat values, some hashes | `valuations`, `references.hash`, a signed exchange target |
| FDM CSV | Card rows with local and USD amounts, a final total row | `exchange`, a `statement` with a `total` |

All of these have been converted from real files. bunq's opening balance plus its rows equals
its closing balance exactly.

Measured, so these are not guesses: Wise and Revolut running balances reconcile on every
consecutive pair, with Revolut's balance moving by `Amount` minus `Fee`. BoursoBank has
exactly one balance per value date on every account. It reconciles from day to day on most
days of two accounts and hardly ever on a third, apparently a card, so it is stated
evidence, not a running total. FDM's last row is the exact sum of its dated rows.

For pairing, reference and date do not always decide. bunq has no references: account, booking
date and quantity pair all 332 rows, 22 of which share a key with another row, against 113 on
the value date. Fin will need the descriptions or the source order for those.

## CSV definitions

A definition describes one CSV layout. It holds no logic beyond conditions on a cell, so it
stays readable; what it cannot say needs a converter of its own. The bundled ones are in
[`institutions/converters`](../institutions/converters). Fin refuses a definition with an
unknown member or a column not in its header.

```json
{
  "format": "fin-csv-converter",
  "version": 1,
  "institution": "mine",
  "institution_type": "bank",
  "header": ["When", "What", "Currency", "Text"],
  "account": { "id": "main-{Currency}", "currency": { "fiat": "Currency" } },
  "occurred": { "column": "When", "layout": "day-first" },
  "cases": [
    {
      "movements": [
        { "role": "primary", "quantity": { "column": "What" }, "commodity": { "fiat": "Currency" } }
      ]
    }
  ],
  "descriptions": ["Text"]
}
```

| Member | Meaning |
|---|---|
| `institution_type` | `bank` or `crypto-exchange` |
| `header` | The exact header; a file with another one is not this layout |
| `separator`, `decimal` | Default `,` and `dot`; spaces that group digits are ignored |
| `observation` | The rows describe rows stated elsewhere |
| `only` | Conditions a row must meet to be part of this source at all |
| `account` | `id`, where `{Column}` is that row's value, and `bank`, `type`, `currency` |
| `id` | `{"columns": [...]}` or `{"row_hash": true}`; absent means `inventory` identity |
| `occurred`, `value_on` | `column`, `fallback`, `layout` (`iso-date`, `iso-datetime`, `day-first`), and `utc` |
| `cases` | The first whose `when` conditions hold gives `movements` and `fee` |
| `reported` | `quantity` and `symbol` columns for a row no case takes |
| `total` | `when` conditions of a row stating what the others add up to, with its `quantity` and `commodity` |
| `balance_after`, `balances` | Running balance column, and `balance` lines to state from each row |
| `descriptions`, `type`, `counterparty_name`, `counterparty_iban`, `references`, `exchange`, `valuations` | Columns for those members of `details`; the first filled `counterparty_name` wins |

- A column name repeated in the header is `Solde#2` for the second one.
- A quantity is `column`, with optional `negate` and `minus` (another column).
- A commodity is `{"fiat": column}`, `{"code": "USD"}`, or `{"reported": column, "provider": name}`.
- A condition is `column` with `in`, `sign` (`positive`, `negative`) or `filled`.
- A clock with no stated offset gives a date, unless `utc` says the column is in UTC.

Every column stays in `fields`, as stated.

## From CAMT

The concepts come from CAMT.053 where they fit, so converting CAMT loses little:

| CAMT | Records |
|---|---|
| `Stmt`, `Bal` (`OPBD`, `CLBD`) | `statement`, `balance` lines of type `opening`, `closing` |
| `Ntry`, `BookgDt`, `ValDt` | `records`, `occurred`, `value_on` |
| `Refs/AcctSvcrRef`, `EndToEndId`, `RmtInf` | `references`, `descriptions` |
| `CcyXchg` | `exchange` |
| `Chrgs` | `fee`, or a `supporting` row |
| `RltdPties` | `counterparty` |

Left out: message headers, status codes and batch structure. An unbooked entry is not a
record.
