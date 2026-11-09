# Experimental Lua CSV converters

A Lua CSV converter is a Lua 2.5 program that turns each CSV row into a
[record](records-format.md). Fin reads the CSV and validates the resulting
records. The institution-specific header, account and interpretation belong in
the script. The [CDC converter](../institutions/lua/cdc.lua) is a complete
example.

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

## Script contract

The script defines a global `config` table with:

- `institution`: the institution name; it must match the TOML definition.
- `header`: the exact CSV header as a Lua array, with distinct names.
- `separator`: an optional single character; the default is a comma.
- `identity`: `source` or `inventory`, as described in the records format.
- `converter` and `version`: strings identifying the converter in the output.
- `account`: one static records-format account object, such as
  `{ id = "checking", bank = "mybank", currency = "EUR" }`.

It also defines `convert(row, raw, context)`. Fin passes tables of trimmed cells
and original cells, indexed by column name. `context.row_hash` is a stable hash
of the row, suitable as an ID when the source does not provide one;
`context.fields` holds nonempty source cells in records-format field pairs.
Return a records-format table with `kind = "record"`, an `id`, the account ID,
`occurred`, `movements`, and any desired `details`. Fin supplies the header,
source, account and end lines and validates the complete records document.

A simple record body looks like this, inside `convert`:

```lua
return {
  kind = "record",
  id = context.row_hash,
  account = config.account.id,
  occurred = row["Date"],
  movements = {
    { role = "primary", quantity = decimal(row["Amount"]),
      commodity = { kind = "fiat", code = "EUR" } }
  },
  details = { fields = context.fields }
}
```

`decimal(text)` parses and normalizes a decimal exactly; `sign(text)` returns
`"positive"`, `"negative"`, `"zero"`, or `nil` for an invalid quantity.
`utc_datetime(text)` converts an ISO date-time without an offset into a UTC
records timestamp. Lua numbers are floating point and are rejected in output
records. Use decimal strings and these helpers for amounts.

`array()` makes an empty array; nonempty sequential tables are treated as arrays.
`yes` and `no` are booleans for records fields. Lua's `nil` omits object fields.
Returning `nil` instead of a record is currently rejected; row skipping is not
yet supported.

## Current limits

This is an experimental integration, currently used by CDC. It supports one
static account, one exact header per script and row-based records. It does not
yet support duplicate headers, multiple accounts, file-wide checks and totals,
row skipping, observation enrichment, or arithmetic helpers for decimals.
An institution cannot combine JSON and Lua CSV converters in one definition yet.
The Lua program is run once per file and its state persists across rows. Only run
scripts you trust: the host does not expose the standard Lua IO library, but it
does not limit execution time or memory.
