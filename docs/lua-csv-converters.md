# Experimental Lua CSV converters

A Lua CSV converter is a Lua 2.5 program that turns each CSV row into a
[record](records-format.md). Fin reads the CSV and validates the resulting
records. The institution-specific header, account and interpretation belong in
the script. The [CDC converter](../institutions/lua/cdc.lua) is a complete
example; [FDM](../institutions/lua/fdm.lua) emits a file-level total, and
[Bunq](../institutions/lua/bunq.lua) describes observations for several accounts;
[BoursoBank](../institutions/lua/boursobank.lua) also emits day-end balances,
[Revolut](../institutions/lua/revolut.lua) subtracts fees exactly, and
[Wise](../institutions/lua/wise.lua) enriches CAMT movements, and
[Kraken](../institutions/lua/kraken.lua) accepts two ledger layouts.

Refer to a script from an institution TOML file:

```toml
[[statements]]
format = "csv"
script = "converter.lua"
```

The path is relative to the TOML file. `fin institution definition check
mybank.toml statement.csv` validates the script and previews its result. Attach
it with `fin institution definition set mybank mybank.toml`. Fin saves the script
source in the database; later imports do not need the original file. Once
imported evidence depends on a definition, changing its script is treated as a
source change and is refused until that evidence is removed.

You can also test a script without an institution or database:

```sh
fin convert --definition converter.lua statement.csv -o records.jsonl
fin records check records.jsonl
```

## Write a converter

Suppose `statement.csv` contains:

```csv
Date,Description,Amount
2026-01-02,Coffee,-3.50
```

Save this complete program as `converter.lua`:

```lua
config = {
  institution = "mybank",
  identity = "source",
  converter = "mybank csv",
  version = "1",
  account = { id = "checking", currency = "EUR" },
  header = { "Date", "Description", "Amount" }
}

function convert(row, raw, context)
  local descriptions = array()
  if row["Description"] ~= "" then
    descriptions = { raw["Description"] }
  end
  return {
    kind = "record",
    id = context.row_hash,
    occurred = row["Date"],
    movements = {
      { role = "primary", quantity = decimal(row["Amount"]),
        commodity = { kind = "fiat", code = "EUR" } }
    },
    details = { descriptions = descriptions }
  }
end
```

Run the two `fin convert` and `fin records check` commands above. The record
has one movement of `-3.5 EUR`, on `checking`, dated 2026-01-02, with the
original description and source fields. It states what the CSV says; it does
not assign a spending category.

## Script contract

The global `config` table declares:

- `institution`: the institution name; it must match the TOML definition.
- `header`: the exact CSV header as a Lua array. Alternatively, `headers` is
  an array of exact headers for multiple versions of an export. Specify exactly
  one of these. Repeated names use the first occurrence by name; later
  occurrences use `#2`, `#3` and so on in `row` and `raw`. Ambiguous generated
  names are refused.
- `separator`: an optional single character; the default is a comma.
- `identity`: `source` or `inventory`, as described in the records format.
- `converter` and `version`: strings identifying the converter in the output.
- `account`: a static records-format account object, as in the example. For
  several accounts, omit it and define `account(row)` instead. It returns an
  account object for each row. Fin uses its `id` for the record and refuses
  conflicting account objects with the same ID.

`convert(row, raw, context)` receives tables of trimmed cells and original
cells, indexed by column name. `context.row_hash` is a hash of the entire row;
`context.fields` holds its nonempty original cells as records-format field pairs.
Return a records-format table with `kind = "record"`, an `id`, `occurred`,
`movements`, and any desired `details`. Fin supplies the selected account and,
unless provided explicitly, the original source fields. It also supplies the
header, source, account and end lines and validates the complete document. If a
record supplies an `account`, it must agree with the selected account.

A whole-row hash changes if *any* cell changes. Use it as the record ID only
when that is appropriate for the source. If the source has no stable row IDs,
`inventory_id(account, date)` assigns a position per account and date in the
file. For observations, `observation_id(account, date)` uses the same numbering
with the observation prefix. These helpers preserve identical rows; they do
not turn them into independently identifiable source transactions.

## Amounts, checks and Lua values

Lua numbers are floating point. Keep financial amounts as strings:

- `decimal(text)` parses and normalizes an exact decimal with a dot;
  `decimal_comma(text)` does the same with a comma and rejects dots.
- `subtract(left, right)` subtracts two exact dot-decimal strings.
- `date_of_datetime(text)` takes a `YYYY-MM-DD HH:MM:SS` clock and returns its
  calendar date, without assuming a timezone. The output date is validated as
  part of the records document.
- `day_first(text)` rearranges `DD-MM-YYYY` to `YYYY-MM-DD`; the records
  validator checks the resulting date.
- `starts_with(text, prefix)` tests a string prefix; `drop_prefix(text, prefix)`
  returns the remaining text, or `nil` if the prefix does not match.
- `normalize_hex(reference, description, prefix, label, length)` normalizes
  prefixed hexadecimal references by removing hyphens and lowercasing; it also
  prefixes a bare hexadecimal reference when its description matches `label`
  case-insensitively. It rejects a malformed prefixed reference.
- `sign(text)` returns `"positive"`, `"negative"`, `"zero"`, or `nil` if the
  dot-decimal input is invalid. `negate(text)` returns its exact negation.
- `matches(text, pattern)` tests a Perl-style regular expression and
  `valid_iban(text)` checks an IBAN; both return a truthy value or `nil`.
  `reject(message)` aborts the conversion and reports the source row.
- `utc_datetime(text)` turns an ISO date-time without an offset into a UTC
  records timestamp. Use it only when the source says the clock is UTC.

`array()` makes an empty array; nonempty sequential tables are treated as
arrays. `yes` and `no` are host-provided booleans for records fields. They are
userdata values: do not use `no` as a Lua conditional, because Lua treats it as
truthy. Lua's `nil` omits an object field. Returning `nil` instead of a record
is rejected; use `classify(row)` to skip a source row deliberately.

## Observations and accounts

Bunq has an account column and its CSV describes movements counted from MT940.
Its [script](../institutions/lua/bunq.lua) defines `account(row)`, declares `config.observation`, and uses
`observation_id` for its IDs. Fin marks every record from this source as an
observation; the script cannot override that setting for individual records.
`observation_id` allocates IDs within each account and date, preserving duplicate
rows. The ID alone does not match records: Fin matches observations with counted
records using the existing records adapter and its matching rules.

For journal enrichment, `config.observation.journal_tags` maps tag names to CSV field names.
Lua 2.5 cannot use arbitrary string keys in the same table-constructor syntax as
modern Lua, so Bunq creates the table and then assigns its hyphenated key:

```lua
config.observation = { journal_tags = {} }
config.observation.journal_tags["bunq-counterparty"] = "Counterparty"
```

## Stated balances

For a record row, `convert` may return a records-format balance table as its
second result, or `nil` if the row states no balance. The table has
`kind = "balance"`, `type`, `quantity`,
`commodity` and `as_of`; Fin supplies the selected account. As with records, an
explicit account must agree with the selected account. Repeated identical
balances are merged; conflicting balances are refused. BoursoBank uses this to
capture its second `Solde` column as a day-end balance.

`config.observation.journal_description` may name a CSV field to use as the
matched movement’s journal description. For observation sources,
`config.observation.conflict_columns` is an array of
CSV field names whose differences between matched exports require review.
Original field names and values, including repeated names, remain in the record's
source fields.

## Statement totals

A script can optionally define `classify(row)`, returning `"record"`,
`"statement"`, or `"skip"`. Fin classifies all nonempty rows first, calls
`convert` for the record rows in their original order, then calls it for the
statement rows. A skipped row is not interpreted or included in the records;
`fin convert` reports the number skipped. It runs classification in a separate
fresh Lua state to count skipped rows, then converts with another fresh state;
classification should be deterministic. Kraken skips rows without `txid`.
A statement row returns a records-format `statement` table rather than a record.
`context.source_hash_prefix` contains the first 16 hexadecimal characters of
the file's SHA-256 digest, which can be used in a file-specific statement ID.

If `config.link_statements = yes`, Fin links every emitted record to every
emitted statement. The record must not itself set `statements`. Without this
option, scripts can leave statements unlinked or select them per record. For a
static account, statement rows are retained even if there are no records. A
row-derived account cannot be selected for a statement-only file and its
statement rows are not converted.

FDM's converter treats purchases as outflows and refunds as inflows for both
positive-spending and signed exports. It preserves adjustment signs. Its
`Balance after` column remains in the source fields rather than being checked as
a continuous balance, because some exports omit balance-changing activity.

Fin stores FDM's spent and deposited totals exactly as stated, without guessing
which records they cover. `fin statement totals --institution fdm` lists the
claims with comparison unavailable. The original source file is also retained.

## Current limits

This is an experimental integration, currently used by CDC, FDM, Bunq,
BoursoBank, Revolut, Wise and Kraken. It supports multiple exact headers,
static or row-derived accounts, deliberate row skipping, records, balances,
observations and statement claims. It does not yet support arbitrary file-wide
checks, general decimal arithmetic, or all observation matching settings.
An institution cannot combine JSON and Lua CSV converters in one definition yet.
During conversion the Lua state persists across rows within one file. Only run
scripts you trust: the host does not expose the standard Lua IO library, but it
does not limit execution time or memory.

## What the interface reveals

The script makes provider decisions visible in ordinary conditionals and Fin
owns CSV parsing, exact quantities and records validation. CDC's branching and
Bunq's account selection fit that split well.

The API is still rough. `array()`, `yes` and `no` expose Lua 2.5
representation details, and decimal strings and special helpers make
arithmetic less natural. FDM's totals need a separate classification pass.
Movements use records-format tables directly.
A more cohesive file-level API remains worth exploring; this interface is
experimental.
