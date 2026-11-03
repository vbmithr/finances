# `fin operation` over operations

Status: design agreed 2026-11-02; the command rename (2026-11-02) and step 1
(2026-11-02) are implemented, steps 2–4 are not. The operation and movement
model described in [operations.md](operations.md) is the general layer.

Step 1 as implemented: `fin operation list` calls `Store.operation_review`,
which reads `operations`, `operation_movements` and `operation_sources` and
attaches each movement's bank review row by its anchor event. Each row shows
the operation ID and amounts together; `fin operation show` displays movement
and source-event details. `--type`,
`--decision` and `--category` match when any movement matches, `--search` uses
the operation description, and sorting is done in OCaml with the operation ID as
the final tie-breaker. Date filters use the operation's occurrence date.
`fin operation show` accepts an operation ID. Bank decisions are keyed by the
primary movement; decisions for
arbitrary CDC movements are not implemented yet.

Confirmed bank transfers retain two records in the bank projection. The
operation list groups them on read and the detail view shows each side; undo
returns them to separate operations.

## Vocabulary

The model terms from [operations.md](operations.md) are authoritative:

- **Source event**: one provider record — evidence, identity, occurrence, raw fields.
- **Operation**: an interpretation of one or more source events.
- **Movement**: one balance effect of an operation — one account, one commodity,
  one exact signed quantity. Review decisions attach to movements.
- **Leg role** (`from`/`to`): a component of a movement's identity, and the
  shape names used by movement-CSV rules (`single-in`, `single-out`,
  `exchange`). Do not use "leg" for an operation's movements.

CLI mapping (decided and implemented 2026-11-02: only model words are used, old
names are dropped): the `fin transaction` group is renamed to **`fin operation`**
(`fin operation list`, `fin operation show`, review and category subcommands
move under it; `op` is the remaining shortcut), and the aliases `transaction`,
`tx` and `txn` are removed — the alias rewrite in `command_argv` loses those
cases. Source-event inspection stays on the `db` side: `fin db sources` lists
stored source records and `fin db event operations EVENT-ID` shows the
operations linked to one source event, because a group called `operation`
cannot contain a subcommand also called `operation`. "Transaction" is gone from
canonical command names, help text, the manual and README examples. This is a
user-visible breaking change, recorded with an **Action required** entry in
`CHANGES.md` (fin is in beta; clean design over backward compatibility).

## Target behavior

- **One view over operations.** A bank transaction is an operation with one
  movement; a CDC exchange is an operation with two. `fin operation
  list` shows both, filterable by institution,
  account or commodity, in addition to the existing date, search, type,
  classification and counter-account filters.
- **Display: one row per operation**, with its movements shown together, for
  example `-2 EUR → +0.2 CRO`. The detail view shows individual movements.
- **Review follows the same path.** Unresolved wallets, commodities and kinds
  appear in the review queue, not in a separate command. Category, type and
  review decisions link to movement identities, so they attach to crypto
  movements without a new mechanism.
- **The journal and hledger export read operations**, so reviewed exchanges and
  supported wallet transfers enter the books alongside bank movements.

## Order of work

1. **Make `fin operation list` and `fin operation show` read operations.**
   Done 2026-11-02. Multi-movement operations render as one row with the
   movements together; `fin operation show` provides details.
2. **Resolve wallets and commodities, then review them.** Account assignment and
   commodity resolution turn proposals into movements that can be reviewed;
   unknown kinds without proposed movements remain unposted and need a way to
   create movements. Resolution and exchange review are implemented.
3. **Project operations to the journal** and the hledger export.
   Confirmed exchanges and single-movement transfers are supported; general
   categorization and operations with fees or additional movements remain.
4. **Move the bank importer onto the same pipeline**, so there are no two paths.

## Transition policy

Keep the old bank tables (`movements`, `bank_categories`, transfer and
settlement decisions) as a projection during the transition, so categories,
rules and the export keep working while steps 1–3 land. The open question is how
far to take step 4: a full migration is a bigger change and a schema rebuild.
Schema 33 adds persisted movement-level transfer links while retaining the bank
decision and journal projection. It requires a database rebuild. Decide after
the general operation projection is proven against real data whether to drop
the remaining bank tables entirely or retain them as a projection.

## Display details to decide at implementation time

- Which existing filters apply to which movement (counter-account and
  classification are per-movement decisions; an operation row shows the primary
  movement's decision; the detail view should show each movement's own).
- Sorting: by operation date and ID by default.
- How proposed (unresolved wallet/commodity) operations render: visible but
  marked unresolved, excluded from journal projection.
