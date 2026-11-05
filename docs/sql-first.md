# Database work in SQL

Status: proposal, 2026-11-05. Needs one schema change, so one rebuild through `fin decisions`.

## Why

Fin keeps everything in SQLite, but most commands load whole tables into OCaml records and
filter, match and count there. Three performance bugs in a row came from this (2026-11-05):
`fin operation list` joined lists it read from JSON without an index, `fin status` scanned all
movements per operation, and `fin settlement review --all` re-read every review per settlement.
Each cost grew with the square of the data and passed any fixed time budget until it did not.

Rewriting `fin transfer unmatched` as one query showed the other side: the OCaml version was
about 140 lines of hash tables, the query is one statement, it runs in 0.22 s instead of 0.28 s on
real data, and it gave the same answers on all 697 rows, breaking one tie deterministically.

## The line

SQL does what a database is for:

- filtering, joining, sorting, limiting, counting and summing;
- matching rows against each other: transfer and settlement candidates, unmatched rows;
- deciding a row's current state: its decision, counter-account and effective description.

OCaml keeps what it is better at:

- reading source formats (CAMT, OFX, MT940, CSV) and the records format;
- checking a change before it is written, and enforcing invariants;
- prompts, the queue, tables and JSON;
- a rule only when it cannot be stated in SQL, and then stored, not recomputed on read.

A command should read with one or two queries returning what it shows. OCaml then formats.

## Facts a query cannot see today

Some logic lives in OCaml because the database does not hold what a query needs:

| Fact | Today | Proposal |
|---|---|---|
| An account's ledger name | `Bank_projection.ledger_account`, computed on read | stored in `bank_account_mappings` when an account first appears |
| A transfer's destination account | computed per read, passed in as a temporary table | a join on the stored ledger name |
| A CSV companion layout's display name | `annotate` in OCaml | stored on the matched companion row at import |
| Operation groups | `Operation_groups` in OCaml, passed as a temporary table | a stored `operation_group` column, written at import and on link |

Once those are stored, the read queries need no parameters from OCaml beyond filters.

## Queries in SQL files

Every query goes in its own file under `store/sql/`, such as `store/sql/unmatched_transfers.sql`,
embedded at build time the way bundled institutions already are (`institutions/dune` reads JSON
files into a generated module with `%{read:…}`), so no new dependency. `ppx_blob` would do the
same with a ppx. A file holds plain SQL with `?1`… parameters; the OCaml next to it declares the
row type and nothing else.

Two checks keep files honest:

- a test prepares every file against a fresh database, so a typo or a renamed column fails the
  test suite, not a user's command;
- `test/status_perf_test.py` already compares two sizes and fails a command whose cost grows
  faster than the data.

What files cannot do is share a fragment: SQLite has no include. That is what views are for.

## Views in the schema

The shared reads become views, defined once in the schema:

- `reviews`: each movement with its decision, counter-account and effective description, the
  query now assembled as a string in `Review.reviews_sql`;
- `operation_rows`: each operation's group, primary movement and display fields;
- `transfer_pairs`: exact opposite principals across accounts, already a query.

A command's file then reads from views, short enough to review. A view must not hide a scan
inside a loop; the scaling test covers that.

## Indexes

The same schema change adds the indexes the bugs above lacked:

- `operation_movements(operation_id)`, which every per-operation lookup scans today;
- `movements(currency, booked_on)`, for date windows such as the fee search;
- `companion_matches(event_id)`, `bank_transfer_memberships(event_id)` and
  `operation_transfer_memberships(movement_id)`, joined by event or movement today without one.

## Rel

Rel helps: it ties the schema and row types to OCaml, and a wrong column or type fails to compile,
which caught several of my mistakes. It hinders where SQL is assembled from OCaml strings and
where a twelve-column row is a twelve-argument function. Files and views remove the first; a
small helper reading rows by column name would shorten the second. Keep Rel for the schema, the
connection and transactions.

## Order

1. Schema change: stored ledger names, display names and groups; the indexes; the views. One
   rebuild, with `fin decisions export` before and `fin decisions restore` after.
2. Move existing queries to `store/sql/` unchanged, with the preparation test.
3. Rewrite commands one by one onto the views, each checked byte-for-byte against the previous
   build on a scratch rebuild of real statements, as done for `fin operation list` and `fin transfer
   unmatched`: settlement suggestions, `fin status`, transfer candidates, the category queue's
   selection, then `Review.bank_review`, which most commands still load whole.

## Not in scope

- Moving database engines. DuckDB stays parked; SQL files and views would carry over to it.
- An integrated runtime in the style of Rayforce, where the language and the data are one. It
  removes the boundary entirely, but means rewriting fin; SQL files and views get most of the
  benefit inside SQLite.
- Writes. Imports and decisions already write with SQL; their checks stay in OCaml.

## Open questions

- Whether `effective description` can be a plain view once display names are stored, or still needs
  the per-companion field lookup.
- Whether transfer candidates' IBAN evidence can be stated in SQL, or stays a check in OCaml on the
  candidates a query returns.
