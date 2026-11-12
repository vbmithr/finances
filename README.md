# Fin

Fin brings personal financial evidence together for accounting and investment
position tracking. It imports statements and other source records, keeps their
provenance, and helps review accounts, categories, and transfers. It can export
a journal for analysis with [hledger](https://hledger.org/).

Fin is alpha software. The accounting workflow is usable for supported sources;
position tracking and broader crypto reconciliation are still being developed.
Amounts are handled exactly, and source evidence and uncertainty remain visible.

## Get started

See the [user manual](docs/manual.md) for installation, importing, review, and
backup. For an end-to-end example, follow [Personal accounting with Fin and
hledger](docs/personal-accounting.md).

From a configured development checkout, `make build` builds Fin and `make test`
runs the tests. The [development and bank import reference](docs/bank-import-reference.md)
contains additional examples and package build instructions.

The [plan](docs/plan.md) describes current progress and next steps;
[CHANGES.md](CHANGES.md) records changes and required actions.

Fin is written in OCaml and licensed under the [ISC license](LICENSE.md).
