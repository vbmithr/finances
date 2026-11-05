# Database work in SQL

Status: in progress, 2026-11-05. Schema 36 holds the stored facts; the views and the first query files
are in. See the plan for what is left.

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

| Fact | Before | Now |
|---|---|---|
| An account's ledger name | `Bank_projection.ledger_account`, computed on read | stored in `bank_account_mappings` when an account first appears |
| An account's short ID | a SHA-256 prefix computed in OCaml, passed in as a temporary table | stored next to the ledger name, as `public_id` |
| A transfer's destination account | computed per read, passed in as a temporary table | a join on the stored ledger name |
| A companion row's display name | `annotate` in OCaml, or a field lookup in the query | `companion_rows.display_name`, from `annotate` at import |
| Operation groups | `Operation_groups` in OCaml, passed as a temporary table | the `grouped_operations` view: a link pairs two operations and a stated group is a window over the records, with no transitivity to compute |

The read queries now take no parameters from OCaml beyond filters, as one JSON object.

## Queries in SQL files

Every query goes in its own file under `store/sql/`, such as `store/sql/unmatched_transfers.sql`.
A dune rule runs `store/gen/embed_sql.exe` over the directory and generates the `Sql` module, one
string per file, with no new dependency. A file holds plain SQL with `?1` parameters; the OCaml next
to it declares the row type and nothing else. Where a user picks the sort, the caller appends an
`ORDER BY` of view columns.

Two checks keep files honest:

- a test compiles every file against a fresh database (`Setup.check_queries`), so a typo or a renamed
  column fails the test suite, not a user's command;
- `test/status_perf_test.py` already compares two sizes and fails a command whose cost grows
  faster than the data.

What files cannot do is share a fragment: SQLite has no include. That is what views are for.

## Views

The shared reads are views, in `store/sql/views.sql`:

- `reviews`: each movement with its decision, counter-account and effective description;
- `transfer_sides`: a movement's confirmed transfer, with the other side's account;
- `linked_operations`, `stated_operations` and `grouped_operations`: which operations read as one;
- `operation_anchors` and `operation_rows`: each operation's group, primary movement and display
  fields;
- `group_reviews`: the review of every movement in a group, for filters that hold when any one does.

They are temporary views that each connection creates when it opens. A view holds no data, so a
change to one needs neither a schema version nor a rebuild, which a view in the schema would.

A command's file reads from views, short enough to review. A view must not hide a scan inside a
loop; the scaling test covers that, and lookups by event or operation inside a view are correlated
subqueries on an index.

## Indexes

Schema 35 added the indexes the bugs above lacked: `operation_movements(operation_id)`, which
every per-operation lookup scanned; `movements(currency, booked_on)`, for date windows such as the
fee search and `fin transfer unmatched`; and `companion_matches(event_id)`. The other two named here
before needed none: `bank_transfer_memberships` is keyed by event, and
`operation_transfer_memberships` by movement.

## Rel

Rel helps: it ties the schema and row types to OCaml, and a wrong column or type fails to compile,
which caught several of my mistakes. It hinders where SQL is assembled from OCaml strings and
where a twelve-column row is a twelve-argument function. Files and views remove the first; a
small helper reading rows by column name would shorten the second. Keep Rel for the schema, the
connection and transactions.

## Order

1. Done: schema change for stored ledger names, account IDs and display names, and the indexes;
   the views; the preparation test; reviews, `fin operation list` and `fin transfer unmatched` on
   files.
2. Move the remaining queries to `store/sql/` unchanged.
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

- Whether transfer candidates' IBAN evidence can be stated in SQL, or stays a check in OCaml on the
  candidates a query returns.
