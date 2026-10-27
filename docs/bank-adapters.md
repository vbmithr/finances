# Bank adapter interface proposal

This is a proposed extension interface, not a feature of the current CLI. Today,
bank parsers have their own types and Store dispatches explicitly to the built-in
banks. An external package cannot yet register another bank with `fin`.

## Direction

Use an OCaml module signature for pure bank parsers and an explicit registry for
discovery. The kernel owns file access, archiving, validation, transactions,
duplicate detection, review decisions and accounting projection. A plugin should
not need SQLite access or permission to decide accounting categories.

Start with plugins linked into a custom `fin` executable at build time. This lets
external authors publish ordinary OCaml libraries and gives us compiler-checked
interfaces without an OCaml runtime loading/ABI protocol. Dynamic loading can be
considered separately if installing plugins without rebuilding becomes necessary.

## Shared document model

A common signature only helps if its output is common too. Define validated
builders for an import document containing:

- Source accounts: bank identifiers, account identifiers, type and currency.
- Primary movements: stable source ID, booking date, optional timestamp/value
  date, signed net amount, separately recorded fees, description and raw evidence.
- Optional statement facts: opening and closing balances, currency, the bank's
  stated period and links to the movements included in that statement.
- Companion observations: metadata, suggested categories and matching references
  that enrich primary movements without creating a second transaction stream.

Quantities use Decimal values with the kernel's exact arithmetic policy. Calendar
dates use Ptime dates and remain distinct from instants. Raw document bytes are
archived independently of the normalized document. Completeness is never inferred
from the earliest and latest movement alone.

The kernel needs a common, versioned source-event codec for new adapters.
`Bank_transaction.of_event` currently recognizes specific built-in payloads;
adding a registry alone would leave an external bank's records unreadable by
review, transfer matching and accounting. Existing built-in IDs, account selectors
and payload encodings must remain stable during conversion.

## Parser contract

The intended signature is conceptually:

```ocaml
module type S = sig
  val institution : string
  val parser_version : string
  val formats : Format.t list

  val probe : filename:string -> contents:string -> Probe.t

  val parse
    :  context:Context.t
    -> format:Format.t
    -> filename:string
    -> contents:string
    -> Document.t Base.Or_error.t
end
```

`Format`, `Probe`, `Context` and `Document` above are proposed shared types. Format
identifiers should be namespaced strings or an abstract extensible identifier,
rather than another closed variant listing every bank. `Context` carries explicit
user-supplied hints such as Revolut's account label; it does not expose the DB.

`probe` recognizes formats, including recognizable but malformed inputs that
should produce a useful parse error. It must not infer account ownership or
silently choose among ambiguous formats. The registry validates unique bank and
format identifiers, selects within the explicitly requested institution, and
rejects ambiguous matches with diagnostics. Parsing must be deterministic for
the same input and context.

Parser versions describe implementation changes; they must not automatically
become part of transaction identity. A parser update must not turn an existing
movement into a new payment. Filenames and absolute inbox paths are not stable
movement identifiers either. When bank-provided IDs are absent, the adapter must
document its overlap and duplicate-occurrence policy.

## Registration and capabilities

Provide an explicit `Registry.register (module Adapter : S)` API and a constructor
for a CLI using a chosen registry. Built-in registration should be visible in one
place, rather than relying on module initialization order. A later bank/format
listing command can use the same registry and format descriptions.

Represent statement reconciliation and companion enrichment as optional data or
capabilities. Do not require no-op reconciliation functions from banks whose
exports provide no balances. Keep bank-specific normalization inside adapters;
keep reconciliation arithmetic and storage rules in the kernel.

## Implementation sequence

- Define the shared document builders and generic source-event codec first.
- Add the module signature, registry and an external-adapter example.
- Exercise the example through normal import, review, transfers and journal
  export; testing the parser alone is insufficient.
- Move built-in banks behind the interface one at a time. Preserve their source
  identities and compare replayed archives and journals with existing baselines.
- Standardize statement and companion persistence after accounting for their
  different coverage and matching rules. Specify any schema change explicitly.

A conformance suite should check deterministic parsing, exact amounts and fees,
dates and timezone boundaries, overlapping exports, repeated identical payments,
malformed inputs, replay without duplicate movements, and stable journal output.
External adapter packages should be able to run it against synthetic fixtures.
