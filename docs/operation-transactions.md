# `fin operation` over operations

Status: design agreed 2026-11-02; the command rename (2026-11-02) and step 1
(2026-11-02) are implemented, steps 2–4 are not. Step 5 (pairing movements into
transfers and exchanges) is a proposal of 2026-11-03. The operation and movement
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
5. **Pair movements across operations** into transfers and exchanges with one
   candidate finder and one review workflow (proposal below). It does not depend
   on step 4: bank operations already exist as operations with resolved movements.

## Transition policy

Keep the old bank tables (`movements`, `bank_categories`, transfer and
settlement decisions) as a projection during the transition, so categories,
rules and the export keep working while steps 1–3 land. The open question is how
far to take step 4: a full migration is a bigger change and a schema rebuild.
Schema 33 adds persisted movement-level transfer links while retaining the bank
decision and journal projection. It requires a database rebuild. Decide after
the general operation projection is proven against real data whether to drop
the remaining bank tables entirely or retain them as a projection.

## Pairing movements: one workflow for transfers and exchanges (proposal)

### Problem

Four mechanisms handle what is one kind of decision, "these movements are two
sides of the same economic event":

| Case | Source shape | Today |
|---|---|---|
| Bank to bank transfer | two bank rows, same commodity | `fin transfer candidates` and `review`, over the bank `movements` table |
| Bank to wallet or exchange transfer | one bank row, one resolved CDC movement | manual `fin transfer link` |
| CDC exchange | one row, two movements | `fin operation exchange confirm` |
| Wise currency conversion | two bank rows, two currencies | none: typed `conversion`, never paired |

A currency conversion at a bank and an exchange at a crypto venue are the same
event (a custodian swaps one commodity for another); a transfer is the same event
with one commodity moving between accounts. Only the commodity relation and the
projection differ.

### Model

A **pairing** is a decision over two movements, with a kind:

- **Transfer**: equal and opposite quantities of the *same* commodity, different
  accounts. Projection: direct postings between the two accounts (as today).
- **Exchange**: opposite signs, *different* commodities, same custodian
  (institution). Projection: the `equity:trading` postings fin already uses for
  CDC exchanges.

The two movements may belong to one operation (a CDC exchange row) or to two (two
Wise rows, two bank rows). A one-operation exchange is a pairing whose movements
already share an operation; confirming it uses the same command as any other
pairing, so `fin operation exchange confirm` is not a separate concept.

Pairing never changes source events or movements. A confirmed pairing is a
persisted link between two movement identities (generalizing the schema 33
movement-level transfer links with a kind), grouped on read as transfers are
today, and undone by deleting the link.

### Candidates

Input: movements of any operation that are resolved (account and commodity
known: bank movements via the account mapping, CDC movements after `fin operation
resolve`), carry no category or other decision, and are not already paired. The
candidate query moves from the bank `movements` table to operation movements.

Pair rules, before scoring:

- Transfer: same commodity, opposite sign, different accounts, booked within 7
  days, quantities equal. A tolerance for fees comes in a later step (below).
- Exchange: different commodities, opposite sign, same institution, booked
  within one day, and at least one side typed `conversion` or already part of one
  operation. Without the type requirement, any opposite-sign pair of different
  currencies in one institution would qualify, which invites false positives.

Evidence extends `Transfer_ranking` (codes and weights are tuned on real data,
not fixed here). Existing: `opposite_principal`, `booking_dates`,
`counterparty_iban`, `shared_reference`, `bank_name`, `no_competing_pair`. New:

- `shared_hash`: both sides carry the same on-chain or provider transaction hash.
  This is the strongest evidence for wallet transfers. CDC's `Transaction Hash`
  column is not read today; the movement-CSV profile gains a `reference` column
  role so it is stored as evidence (a profile format change).
- `same_custodian` and `type_conversion` for exchanges.
- `stated_counterpart`: the source itself states the other side. Wise's CSV rows
  carry `Exchange From`, `Exchange To`, `Exchange Rate` and `Exchange To Amount`;
  fin reads them today only to compare for conflicts. A row whose stated target
  currency and amount equal another row's currency and amount (same day, same
  institution) is strong evidence, and it needs no identifier. It applies only to
  rows whose `Transaction Details Type` is `CONVERSION`: a foreign-currency card
  payment fills the same columns for the merchant-side amount, and no second Wise
  row exists to pair with it.
- `fee_explained`: the quantity difference equals a fee the source reports.

Ranking is unchanged in kind: score each pair, then an exact maximum-weight
matching decides which candidates are supported (`Transfer_ranking.assignments`).
Scores are not probabilities.

### Fees

Transfers and exchanges with fees need a third movement or a fee posting. The
rule: a quantity difference is accepted only when evidence explains it
(`fee_explained`, or a reported fee movement on the same operation), and the fee
is posted explicitly to a fees account. Unexplained imbalances are refused, not
absorbed into a generic account with a warning. A fee spread hidden in the
exchange rate stays in the trading postings and is not separated out.

### Commands

One review workflow lists and decides both kinds, with a kind column in the
output and `--kind transfer|exchange` to filter:
`candidates`, `review` (interactive `c`/`r`/`s`, or a JSON decisions file),
`link OPERATION-ID OPERATION-ID` for a pair the finder did not propose, and
`undo`. Export and restore become one portable snapshot of pairings.

Command naming is open. Recommendation: one group, `fin operation pair`, replacing
the pairing commands in `fin transfer` and `fin operation exchange`. Under the
beta policy the old commands are removed, with an **Action required** note and
the portable snapshots migrated on restore. The alternative, keeping `fin
transfer` as the group and adding exchanges to it, reads wrongly for a currency
conversion.

### Data to verify before building

- Which Wise row of a conversion carries the `Exchange To` columns (one leg or
  both), and how exact `Exchange To Amount` is: a real foreign card row shows a
  target amount that differs from the amount in its own description and a rate
  rounded to five decimals, so treat it as approximate evidence, not as the
  quantity to post, and whether the two legs share a `TransferWise ID`. The ID is a record
  identity inside Wise and not a cross-institution key: the test fixture shows
  `CARD-1` on both a purchase and its reversal, so it groups related rows rather
  than naming one leg. Treat a shared ID as an optional hint (the existing
  `shared_reference` evidence already scores it), never as a requirement. Without
  either, date plus amounts is ambiguous when two conversions happen on one day,
  and such cases stay manual.
- Whether CDC deposits and withdrawals carry a hash that also appears on the
  counterparty side (another exchange, a wallet export, or a bank reference).
- Whether Revolut and bunq conversions have the same two-row shape.

### Steps

1. Candidates over operation movements for transfers only, reproducing today's
   bank results (the existing transfer fixtures must not change), then retire the
   bank-table query.
2. Add `kind` to the persisted link and migrate `link`, `unlink` and `undo` into
   the one workflow. Schema bump; the database is rebuilt, and snapshots restore
   through the version-aware importer.
3. Read the CDC hash into evidence (`reference` role) and add `shared_hash`.
4. Exchange candidates for bank conversions (Wise first, using
   `stated_counterpart`), confirm through the same projection as CDC exchanges.
5. Fee tolerance and explicit fee postings.
6. Optional: record the implied rate of each confirmed exchange as an
   observation, for valuing positions later.

### Not covered

Partial fills, three or more movements in one event, spread across several days,
and bank-side fees the source does not report. The bank importer migration
(step 4 above) is independent: it changes where bank operations come from, not
how they are paired.

### Prior art

dledger (a double-entry app with CEX adapters) pairs imported entries in one pass
across banks, exchanges and chains, scoring amount, date and a shared transaction
hash, and treats trades uniformly through `Equity:Trading` postings. Taken from
it: one cross-source finder, the hash as strong evidence, fee absorption (made
explicit here), implied rates. Not taken: voiding and re-posting entries (fin
links over unchanged evidence), suspense accounts (fin keeps movements
unresolved) and fixed thresholds.

## Display details to decide at implementation time

- Which existing filters apply to which movement (counter-account and
  classification are per-movement decisions; an operation row shows the primary
  movement's decision; the detail view should show each movement's own).
- Sorting: by operation date and ID by default.
- How proposed (unresolved wallet/commodity) operations render: visible but
  marked unresolved, excluded from journal projection.
