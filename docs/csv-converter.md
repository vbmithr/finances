# CSV converter definitions

A CSV converter can be a JSON configuration for Fin's generic CSV
converter or an experimental Lua 2.5 program. This guide covers the JSON
configuration; the [Lua converter guide](lua-csv-converters.md) explains the programming
alternative. A JSON definition tells the converter which columns to
read and what they mean. The
converter produces [records](records-format.md): Fin's common representation of
statements and their transactions.

The institution's TOML file names the converter definition or Lua script. The TOML file also
contains the institution's identity and default rules; the CSV definition
contains the instructions for one CSV layout. For example:

```toml
# mybank.toml
version = 1
institution = "mybank"
type = "bank"

[[statements]]
format = "csv"
definition = "mybank-csv.json"
```

The path to `mybank-csv.json` is relative to `mybank.toml`. An institution can
have more than one CSV statement section, each pointing to a different layout.
The converter does not execute the JSON as code: it interprets a fixed set of
fields and conditions implemented in Fin.

## Follow one row

Suppose `statement.csv` contains:

```csv
Date,Description,Amount
2026-01-02,Coffee,-3.50
```

This definition describes a single EUR account with signed amounts:

```json
{
  "format": "fin-csv-converter",
  "version": 1,
  "institution": "mybank",
  "institution_type": "bank",
  "header": ["Date", "Description", "Amount"],
  "account": {"id": "main", "currency": {"code": "EUR"}},
  "occurred": {"column": "Date", "layout": "iso-date"},
  "cases": [
    {
      "movements": [
        {
          "role": "primary",
          "quantity": {"column": "Amount"},
          "commodity": {"code": "EUR"}
        }
      ]
    }
  ],
  "descriptions": ["Description"]
}
```

Save it as `mybank-csv.json`. Read it from top to bottom:

- `format` and `version` identify the definition language. `institution` and
  `institution_type` identify the source; the institution name must also match
  the TOML file when the two are used together.
- `header` says which CSV this definition recognizes. The names, order and
  number of columns must match.
- `account` says which account the row belongs to. Here every row belongs to
  `main`, an account in EUR.
- `occurred` reads the row's date from `Date`, in `YYYY-MM-DD` format.
- `cases` says how to turn a row into movements. This definition has one case
  with no conditions, so it applies to every row. Its single primary movement
  takes its quantity from `Amount` and its currency from the fixed code `EUR`.
- `descriptions` copies the `Description` column into the record's details.

For the example row, the result describes a movement of **−3.50 EUR** on
`main`, dated 2026-01-02, with description `Coffee`. It also retains nonempty source
columns as fields. The result describes what the statement says; it does not
assign a spending category.

## Try it without importing

Run the converter directly, using the CSV JSON file rather than the TOML file:

```sh
fin convert --definition mybank-csv.json statement.csv -o records.jsonl
fin records check records.jsonl
```

`fin convert` does not need a database and does not import anything. It writes
records as JSON Lines; without `-o`, it prints them to standard output. Inspect
that output to see the account, date, movement and fields the converter produced.

To try the complete institution definition and its rules, use:

```sh
fin institution definition check mybank.toml statement.csv
```

This validates the TOML and referenced CSV definition and previews the sample;
it also does not import. To attach the definition and import, follow the
[manual's CSV setup](manual.md#configure-another-csv).

## Why are there cases?

The common example needs only one unconditional case. Other exports put
several kinds of activity in the same CSV. A `cases` entry can have a `when`
list of conditions, followed by the movements and optional fee to produce. Fin
uses the **first** case whose conditions all hold. A case without `when` is a
catch-all, so place it last.

For example, [Ethereum’s transaction layout](../institutions/csv/ethereum-1.json)
uses different cases for failed, incoming and outgoing transactions. Each case
interprets the relevant columns for that kind of row. `only` is separate: it filters rows out of
the source altogether. `require` checks assumptions about remaining rows and
rejects the file if they fail. If no case matches, the record has no movements;
`reported` can retain a reported quantity and symbol for such a row.

## Other settings you may encounter

A real layout may need a different `separator` or `decimal`, or `headers` for
several accepted versions of the header. `value_on` adds a value date alongside
`occurred`. An account ID can include a column, such as `{Currency}`, when one
file contains several accounts.

`id` controls how converted rows are identified. Without it, Fin uses an
inventory identity based on account, date and row position; a definition can
instead use source columns or a hash of the row. This choice affects how Fin
recognizes overlap between statements, so it should reflect what the source
actually guarantees. Balances can be described separately with `balance_after`
or `balances`; they are evidence for checking the movements.

## Reading the bundled definitions

[FDM](../institutions/lua/fdm.lua) now uses Lua. Its CSV has a `total` row:
that row states a total rather than another movement. Its script also records
the local currency, amount and exchange rate when available.

[BoursoBank](../institutions/csv/boursobank.json) has a different role. Its CSV
currently supplies **observations** to enrich movements from OFX, rather than
creating another set of movements. `observation: true` says so. The two `Solde`
columns mean different things; `Solde#2` selects the second. `journal_tags` selects information for the journal after a CSV observation is
paired with an OFX movement. `conflict_columns` names columns that exports of
the same CSV row must agree on. See [BoursoBank's institution file](../institutions/boursobank.toml)
for both statement sections.

[Revolut](../institutions/csv/revolut.json) illustrates row filters, checks and
fees. [Ethereum](../institutions/ethereum.toml) points to two CSV definitions:
one for transactions and one for token transfers. Those definitions also infer
the wallet address from the export and report tokens by contract.

These are useful examples of what the configuration language can express, but
not every definition is as simple as the one above. Cases, row filtering,
identity, balances and observations describe different aspects of importing;
understanding a complex definition can require following several of them at
once. The [field reference](records-format.md#csv-definitions) lists the
available members and their precise meanings.
