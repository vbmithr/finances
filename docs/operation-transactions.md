# `fin operation` over operations

Status: design agreed 2026-11-02; the command rename (2026-11-02) and step 1
(2026-11-02) are implemented, steps 2–4 are not. Step 5 (grouping movements into
operations) is a proposal of 2026-11-03. The operation and movement
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
5. **Group movements into operations** by evidence strength, with one candidate
   finder for what must be inferred (proposal below). It does not depend on step
   4: bank operations already exist as operations with resolved movements.

## Transition policy

Keep the old bank tables (`movements`, `bank_categories`, transfer and
settlement decisions) as a projection during the transition, so categories,
rules and the export keep working while steps 1–3 land. The open question is how
far to take step 4: a full migration is a bigger change and a schema rebuild.
Schema 33 adds persisted movement-level transfer links while retaining the bank
decision and journal projection. It requires a database rebuild. Decide after
the general operation projection is proven against real data whether to drop
the remaining bank tables entirely or retain them as a projection.

## Grouping movements into operations (proposal)

Replaces the pairing proposal of 2026-11-03, which modelled a transfer or exchange
as a link with a kind between two operations.

### Problem

Bank transfers, wallet transfers, CDC exchanges and Wise conversions are handled by
different mechanisms, or by none. They are one question: which movements belong to
one economic event? A confirmed transfer today is two operations grouped on read,
a CDC exchange is one operation, and a Wise conversion is unrelated rows.

### Model

- **Every movement belongs to exactly one operation.** An operation has any number
  of movements. A movement exists independently of the grouping, with a stable
  identity (source identity and role), so regrouping never rewrites evidence.
- **One movement is a valid operation.** A card purchase, or one side of a transfer
  seen by one institution, is an operation with a counterparty not yet explained.
- **Grouping is decided by evidence strength:**

| Level | Evidence | Example | Effect |
|---|---|---|---|
| Same record | one source row states several movements | CDC exchange row | grouped on import |
| Source-stated | the institution's own identifier links its records | Wise `BALANCE-n` rows and `FEE-BALANCE-n` | grouped automatically, visible, can be split |
| Inferred | amounts, dates, IBAN, reference, shared hash | a transfer between two banks | proposed, then reviewed |

The institution boundary matters only through identifier scope: an institution's
reference proves a link among its own records and nothing about another
institution's. It does not by itself decide completeness. CDC's `card_top_up` and
`exchange_to_crypto_transfer` show one side within one institution, two accounts at
one bank each report only their own side, and a transfer can cross institutions and
commodities. A hash shared across institutions is strong evidence but stays
reviewed, in line with [operations.md](operations.md).

- **The shape is derived, never stored.** Set aside movements already explained
  individually, such as a fee with its own counter-account, and sum the rest by
  commodity. Nothing left is a transfer. Two commodities of opposite sign are an
  exchange. One commodity left is an external flow to classify, or a transfer whose
  other side is missing.

### Worked example: a Wise conversion

One real conversion, checked across the EUR and SGD exports, has three rows:

| `TransferWise ID` | Currency | Amount | `Exchange` columns |
|---|---|---|---|
| `FEE-BALANCE-n` | source | minus the fee | empty |
| `BALANCE-n` | source | minus the net | from, to, rate, target amount |
| `BALANCE-n` | target | plus the target amount | same values |

- The rows share the number `n`. The fee row's description also names `BALANCE-n`.
- Both `BALANCE-n` rows have the same timestamp. The fee row is a millisecond apart.
- The running balances show the account falling by net plus fee, across two rows.
- `Total fees` on the debit row repeats the fee. It is a cross-check, and posting
  from it would count the fee twice.
- The fee row is also typed `CONVERSION`, so that type does not identify the
  exchanging rows. A foreign-currency card row fills the `Exchange` columns without
  any second row, so those columns do not identify a conversion either. The
  `BALANCE-` prefix does, on the one export studied in
  [statement-observations.md](statement-observations.md).

Accounting: the net and target amounts post through `equity:trading`, as CDC
exchanges do; the fee posts to its own counter-account. All three movements stay
separate, each with its own source event.

### Source-stated grouping

These rules will belong to converters and reach fin as the `group` and `supporting`
members of [records](records-format.md). For Wise: group by `reference` after removing a
`FEE-` prefix, for `BALANCE-` references only. Other prefixes stay ungrouped until checked, because a reference
may also be shared by a payment and its refund.

The `reference` comes from CAMT and is stored on the source event, so this does not
need the CSV companion. `source_id` already includes the instant and direction, and
the source account differs per balance, so rows sharing a reference do not collide.

The group is a pure function of stored events, which makes it independent of import
order and needs no completion step. If a currency's export was never imported, the
group has one commodity and is reported as incomplete in `fin status` and the
operation list; it is not posted as an exchange.

### Inferred grouping: the candidate finder

Candidates are movements of any operation with resolved account and commodity and
no explanation yet, replacing the query over the bank `movements` table. A CDC
movement becomes eligible after `fin operation resolve`.

The evidence extends `Transfer_ranking`, and an exact maximum-weight matching still
decides which candidates are supported. Its weights and date window are hand-set
and are tuned on real data; scores are not probabilities. New evidence: `shared_hash`,
reading CDC's `Transaction Hash` through a new movement-CSV `reference` role.

A quantity difference is accepted only if a reported fee explains it, and the fee
posts explicitly. An unexplained difference is refused.

### Decisions and persistence

Stored decisions are only those a person made: **merge** these movements and
**split** this group, including a source-stated one. They are keyed on movement
identities, because operation IDs do not survive a rebuild, and they are
append-only and portable. Everything else is recomputed from stored events. The
schema 33 link table is the two-movement case of a merge and migrates into it.

A decision that conflicts with a new grouping is surfaced as a problem and never
silently overridden. Example: a category already saved on a conversion row, which
now belongs to an exchange.

### Commands

`fin operation candidates`, `fin operation merge`, and `fin operation split`.
`fin operation exchange confirm` remains the accounting review for any exchange,
one row or several, so CDC and Wise share one path. `fin transfer` and the exchange
export and restore are folded in as each step lands. Names are not final.

### Posting transfers

Decided 2026-11-03. Each side of a confirmed transfer posts on its own date against
a clearing account shared by the two sides, replacing the single entry on the
earlier date. Balances are then right on every date, and the clearing account shows
money in transit until the later side posts. Reversals already work this way. This
applies even when both dates match, so there is one rule. The journal gains one
account per transfer; its name is derived from movement identities, because row IDs
do not survive a rebuild. A side with no confirmed match stays in
`equity:unclassified`.

### Data to verify before building

- That the CAMT `reference` of a conversion matches the CSV ID on all three rows.
- That the pattern holds on other conversions; so far it is one fully checked
  conversion and one other credit row.
- Whether a conversion can have no fee row, or several rows per side.
- Whether CDC hashes appear on the counterparty side.
- Whether Revolut and bunq conversions have similar references.

### Steps

1. Derive operation membership from stored events and decisions, keeping today's
   transfer grouping and every journal result unchanged.
2. Post confirmed transfers through the clearing account, updating the journal
   tests and the manual. This is a visible journal change.
3. Add the source-stated rule for Wise, the incomplete state, and the conflict
   report for existing decisions on grouped rows.
4. Project a grouped Wise conversion through the exchange path, with the fee as its
   own movement.
5. Move candidates to operation movements, reproducing today's bank results on the
   existing fixtures first.
6. Merge and split decisions, migrating the existing links; schema bump and rebuild.
7. Read the CDC hash, then explained fee differences on inferred groups.

### Not covered

Partial fills, one payment split across several incoming rows when no source states
it, and value or cost basis across an exchange.

### Prior art

dledger posts balanced entries at import with a suspense counterparty, then voids
and re-posts matched pairs. Taken: one finder across all sources and a shared hash
as strong evidence. Not taken: rewriting entries, since fin regroups over unchanged
evidence, and automatic absorption of differences into a fee account. fin's
`equity:unclassified` plays the same role as its suspense accounts.

## Display details to decide at implementation time

- Which existing filters apply to which movement (counter-account and
  classification are per-movement decisions; an operation row shows the primary
  movement's decision; the detail view should show each movement's own).
- Sorting: by operation date and ID by default.
- How proposed (unresolved wallet/commodity) operations render: visible but
  marked unresolved, excluded from journal projection.
