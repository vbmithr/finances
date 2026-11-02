# `fin tx` over operations

Status: design, agreed 2026-11-02. Not yet implemented. The operation and movement
model described in [operations.md](operations.md) is the general layer; `fin tx`
still reads only the bank `movements` table, so CDC operations never appear
in `fin tx list`. This document is the design for making the two meet.

Verified starting point: `fin tx list` calls `list_transactions` in `bin/main.ml`,
which reads `Store.bank_review`, which loads the `movements` table filtered by
`Registry.is_movement`. Operation movements live in `operations` /
`operation_movements` and are read only by `fin tx operation` and
`fin transaction sources`. Category, type and review decisions are already keyed by
movement identity (the primary movement's anchor event), so they can attach to
crypto movements unchanged.

## Vocabulary

The model terms from [operations.md](operations.md) are authoritative:

- **Source event**: one provider record — evidence, identity, occurrence, raw fields.
- **Operation**: an interpretation of one or more source events.
- **Movement**: one balance effect of an operation — one account, one commodity,
  one exact signed quantity. This is what `--movements` expands to and what
  review decisions attach to.
- **Leg role** (`from`/`to`): a component of a movement's identity, and the
  shape names used by movement-CSV rules (`single-in`, `single-out`,
  `exchange`). Do not use "leg" for an operation's movements.

CLI mapping (decided 2026-11-02: only model words are used, old names are
dropped): the `fin transaction` group is renamed to **`fin operation`**
(`fin operation list`, `fin operation show`, review and category subcommands
move under it), and the aliases `transaction`, `tx` and `txn` are removed
entirely — the alias rewrite in `command_argv` loses those cases. Source-event
inspection stays on the `db` side (`fin db events`); the current
`fin transaction operation EVENT-ID` (operations linked to one source event)
moves there as `fin db event operations EVENT-ID`, because a group called
`operation` cannot contain a subcommand also called `operation`. "Transaction"
disappears from canonical command names, help text, the manual and README
examples in the same change. This is a user-visible breaking change: ship it
with an **Action required** entry in `CHANGES.md` (fin is in beta; clean design
over backward compatibility). Until the rename lands, the old names still work
and this document refers to the current commands (`fin tx list`) when
describing verified behavior.

## Target behavior

- **One view over operations.** A bank transaction is an operation with one
  movement; a CDC exchange is an operation with two. `fin operation
  list` (renamed from `fin tx list`) shows both, filterable by institution,
  account or commodity, in addition to the existing date, search, type,
  classification and counter-account filters.
- **Display: one row per operation**, with its movements shown together, for
  example `-2 EUR → +0.2 CRO`. A `--movements` flag expands the list to
  individual movements. This is the agreed default; one row per movement remains
  available through `--movements` because decisions attach to movement
  identities.
- **Review follows the same path.** Unresolved wallets, commodities and kinds
  appear in the review queue, not in a separate command. Category, type and
  review decisions link to movement identities, so they attach to crypto
  movements without a new mechanism.
- **The journal and hledger export read operations**, so crypto enters the books.
  Today they read bank movements only.

## Order of work

1. **Make `fin operation list` and `fin operation show` (renamed from `fin tx
   list` / `fin tx show`) read operations.** Mostly read-only,
   so the risk is low. Bank rows keep their current shape (a one-movement
   operation looks exactly like today's bank movement row); multi-movement
   operations render as one row with the movements together, `--movements` to
   expand.
2. **Resolve wallets and commodities, then review them.** Account assignment and
   commodity resolution turn proposals into movements that can be reviewed;
   unknown kinds stay reviewable and unposted.
3. **Project operations to the journal** and the hledger export.
4. **Move the bank importer onto the same pipeline**, so there are no two paths.

## Transition policy

Keep the old bank tables (`movements`, `bank_categories`, transfer and
settlement decisions) as a projection during the transition, so categories,
rules and the export keep working while steps 1–3 land. The open question is how
far to take step 4: a full migration is a bigger change and a schema rebuild.
Fin is in beta and prefers a clean design, but the rebuild is deferred until
after step 3, when the operation projection is proven against real data. Decide
then whether to drop the bank tables entirely or keep them as a permanent
projection.

## Display details to decide at implementation time

- Which existing filters apply to which movement (counter-account and
  classification are per-movement decisions; an operation row shows the primary
  movement's decision, `--movements` shows each movement's own).
- Sorting: by operation date and ID by default; per-movement sort columns under
  `--movements`.
- How proposed (unresolved wallet/commodity) operations render: visible but
  marked unresolved, excluded from journal projection.
