# Lua interpretation for structured statements

An institution may use one Lua script for CSV and CAMT statements. Fin parses
both formats in OCaml. CSV rows and CAMT entries have different shapes, so the
script provides a separate entry point for each and can share ordinary Lua
functions between them. MT940 interpretation remains a design idea.

```toml
[[statements]]
format = "camt"
script = "lua/wise.lua"
[statements.profile]
namespace = "urn:iso:std:iso:20022:tech:xsd:camt.053.001.10"
servicer_contains = "wise"
account_type_prefix = "balance-"

[[statements]]
format = "csv"
script = "lua/wise.lua"
```

Paths are relative to the TOML file. Fin saves the script source in the
institution definition; each input file gets a fresh Lua state. A CAMT-only
script needs neither CSV configuration nor a CSV `convert` function. See the
[Lua CSV guide](lua-csv-converters.md) for the CSV interface.

The CAMT entry point is `interpret_camt(entry, statement)`. Entry fields are
`proprietary_code`, `account_servicer_reference`, `description`, `booked_at`,
`value_on`, `amount`, `currency`, and `direction` (`credit` or `debit`). The
statement supplies `id`, `account`, `account_type`, `currency`, `period_start`,
and `period_end`. Optional source fields are `nil`. Amounts are signed decimal
strings, and timestamps retain their source offsets.

Return a table containing a nonempty `reference`, and optionally a nonempty
`group` and `supporting = yes` or `no`. A supporting entry requires a group.
Unknown fields are rejected. The callback chooses and normalizes the reference
and supplies grouping. A profile with a script must omit `reference_field`,
`reference_normalization`, and `operation_group`; those settings are for profiles
without a script. Fin forms the record
identity from the interpreted reference and validates the result. The profile
still controls format recognition and account typing.

## Wise example

Wise's CSV script currently normalizes a reference with the `BALANCE_CASHBACK-`
prefix. Its CAMT profile expresses the same rule separately. The following
**fragment** illustrates the callback:

```lua
function wise_reference(reference, description)
  return normalize_hex(reference, description,
                       "BALANCE_CASHBACK-", "cashback", 32)
end

function interpret_camt(entry, statement)
  return {
    reference = wise_reference(entry.proprietary_code, entry.description)
  }
end

-- The CSV convert function can call the same function:
-- wise_reference(row["TransferWise ID"], row["Merchant"])
```

The CAMT callback must receive the **original** proprietary code, before the
current OCaml profile normalizes it. It also needs the source description used
for the cashback rule. In the current code, `Camt_source.make_transaction`
normalizes the reference before forming its source identity; moving this rule
requires changing that boundary, not normalizing an already-normalized value a
second time. The account-servicer reference should also be available where the
source provides it, so choosing between reference fields is an institution
decision.

The normalized reference currently contributes to the CAMT record ID together
with the booking instant and credit/debit direction. Fin must apply the script's
interpretation before calculating that ID, and preserve the existing ID for the
same Wise entry. It must also retain the original source file and fields rather
than replacing the evidence with the normalized value.

The current Wise profile also groups conversion and fee entries using reference
prefixes. The prototype moves that interpretation together with reference normalization;
its tests check that CSV observations still match the same CAMT entries.

## Bunq and MT940

A corresponding MT940 input could expose the parsed booking date, value date,
signed amount, currency and transaction text, together with the statement's
account and period. For example, a proposed `interpret_mt940(entry, statement)`
could identify the counterparty, name and remittance from the transaction text.
The script should see the original text as well as any fields the parser can
extract without institution-specific assumptions.

The generic MT940 reader now parses `:61:` independently of Bunq's profile;
the Bunq interpreter checks the parsed entry and uses profile markers to extract
`:86:` details. An MT940 experiment could give Lua those parsed fields and the
original memo, then compare its output with the current interpreter. The host
would still need to enforce the statement and identity invariants.

MT940 entries have no stable source ID in this workflow. Fin currently assigns a
position per booking date and account. The interpretation step must preserve
that inventory identity and the distinction between identical rows; a script
must not silently collapse them.

## Responsibilities and open decisions

- OCaml remains responsible for transport, syntax, exact decimal values, date
  parsing, format-level checks, document boundaries and records validation.
  Lua receives amounts as decimal strings and timestamps with their source
  offset preserved; it must not convert them through floating-point numbers.
- Lua may select and interpret institution-specific fields. The callback
  returns explicit interpreted values rather than mutating the parsed input;
  the host supplies the record envelope and verifies any required fields.
- The host must define which interpreted values can affect an ID, account,
  matching reference or operation group. They cannot be treated as harmless
  presentation metadata: changing them can change matching and stored evidence.
- CAMT statement periods and opening/closing balances, MT940 balances, and
  whether a record counts or is an observation remain explicit in records.
  A callback failure should identify the file and entry and reject the import
  atomically, as the current converters do.
- Decide which format settings are truly syntax/recognition settings and which
  belong in Lua. Namespace recognition may stay with the parser; reference-field
  choice, normalization and grouping are stronger candidates for interpretation.
  Do not remove a setting until a script can preserve its behavior.
- A saved definition must continue to carry the script used for later imports.
  Changes to identity-affecting interpretation need the existing source-change
  protection and reimport checks; historical evidence must not be silently
  reinterpreted.

## Prototype and remaining work

The test script [wise-shared-prototype.lua](../test/fixtures/wise-shared-prototype.lua)
shares reference normalization between CSV and CAMT. Tests compare its records,
matching and reimports with Wise's existing profile. The bundled Wise definition
still uses that profile; users can opt into a CAMT script in their own definitions.
Script source changes are protected by the same checks as other source changes.

The records importer persists interpreted grouping. The older CAMT transaction
payload codec does not preserve that grouping on a decode and should be revisited
before using it to round-trip interpreted transactions. MT940 interpretation
remains unexplored.
