# Operations, movements and evidence

Status: implementation in progress, 2026-11-03. Schema 33 stores operations and
source-linked movements alongside the existing bank tables. Ordinary bank imports
also create one operation per source event. Operation reads group confirmed bank
transfers using their persistent review decision; undo exposes the original
operations again. Bank category and type decisions are keyed by their primary
movement, with event IDs retained as a compatibility reference. Journal projection
still uses the event-keyed bank model. CDC CSV evidence and proposed
operations can be stored without creating accounting entries.

## Distinct concepts

A **source event** preserves a provider's evidence, identity, occurrence and raw
fields. Multiple records may describe one activity; two identical records can
also describe two separate activities. A hash or a timestamp alone is not proof
of identity.

An **operation** is an interpretation of one or more source events. It groups
one or more **balance movements**, each with one account, one commodity and an
exact signed quantity. A movement may be proposed with an unknown account; it
cannot be treated as a reconciled account balance until the account is known.
Source links describe evidence for an operation and, where possible, the specific
movement it supports. They do not themselves establish ownership or equivalence.

An **accounting entry** explains confirmed effects using two or more balanced
postings. Movements and postings are related but distinct: an expense posting,
fee or trading posting need not be an observed balance movement. Accounting
must balance by full commodity identity, not just its display ticker. The
current kernel balances commodity strings; the new commodity identity is not yet
connected to that kernel or journal storage.

## Examples and constraints

- A card purchase has one observed account movement and an expense
  counter-posting; a fee may need its own posting.
- An own-account transfer can have two source events, one from each bank. Their
  movements are both evidence of one operation; exported postings must count
  each account effect once. A match is reviewed, never inferred merely because
  timestamps and amounts happen to agree.

The current bank bridge retains both underlying operations and movements when a
transfer is confirmed. Operation reads combine them under the first source
event's operation ID. This grouping uses a persisted movement-level link,
so undo restores the two original operation IDs without rewriting either source
or movement. It is not yet a general-purpose persisted operation merge.
The operation layer persists each confirmed bank pair as a link between two
movements. Both operation reads and `fin decisions export` consume the link.
The export saves stable references with occurrence and evidence hashes; restore
checks them before invoking the bank confirmation path. Bank confirmation and
undo update both representations atomically. Linking crypto accounts remains
to be implemented.
- A CDC conversion may have one source row with a negative movement in one
  commodity and a positive movement in another. If the wallets are not known,
  the movements remain proposals. The entry may use trading postings to balance
  each commodity.
- A cashback row may evidence an asset inflow. Its accounting classification,
  the receiving wallet and its fiat value must not be invented from the sign.
- A card top-up may show an outflow without a corresponding inflow. Preserve the
  missing leg and do not claim an internal transfer until supported by evidence.

## Commodity identity

Fiat has ISO currency identity; native network assets and contract tokens use
explicit network identity, and contract tokens may have a token ID. Network and
contract strings are treated exactly as supplied; chain-specific validation and
normalization are not yet implemented. A provider symbol is scoped to its provider and
is not automatically a canonical asset or chain contract. Resolution to a
canonical commodity needs explicit evidence and must preserve the original
symbol. A ticker can refer to different assets or networks. Commodity identity
and the account are independent: an account may hold multiple commodities.

## Persistence and migration

The existing bank `movements` table still uses the source event ID as its key.
Bank category and type decisions now reference the corresponding operation
movement, and retain a constrained event ID for existing queries. General
multi-movement review needs source-to-operation and source-to-movement links and
atomic review decisions. Matching and merge/split must preserve provenance and
refuse ambiguous identity. Source evidence is immutable; edits and review changes
are audited.

The movement identity uses a source's venue, account, kind
and source ID plus a semantic leg role such as `from` or `to`. That key is not
proof of an unchanged transaction: restore must also verify occurrence and source
evidence, and refuse ambiguous duplicate records. The role must be stable across
parser versions; a transient row position is not a portable identity. Linking a
second source must not silently replace the existing movement identity. These
version 5 portable decisions: bank reviews name the `primary` role; movement
resolutions record their actual role and original amount and commodity, and
verify the source occurrence and evidence. Versions 1–3 imply the primary role
for bank reviews. Persisted
movement anchors are unique per source event and role, and every anchor must be
among that movement's source references. Database IDs remain local to each rebuild.

Unknown rows with no proposed movements still need a way to create reviewed
movements without silently applying a decision to the wrong leg. Compare bank imports, reviews,
transfers, and hledger exports before and after the change. Fin does not migrate
databases automatically; document exports, rebuilding and restore for each schema.

## Reconciliation and projection

An operation can remain incomplete even when a provider row is syntactically
valid. Reconcile movements against independently reported account/commodity
balances and statement periods, including opening balances, before claiming
completeness. Overlapping exports need source-aware duplicate handling. Do not
sum unassigned wallet movements as if they came from one account or infer a fee
from a price difference. Unknown kinds remain reviewable and unposted.

CDC imports retain the original file and source fields, with provider-scoped
symbols and unassigned accounts. Replay of an identical document checks its
stored proposals; ambiguous overlapping documents and identical rows are
rejected. Two-movement exchanges can be resolved, confirmed and projected with
commodity-specific trading postings. Resolved single-movement operations can be
linked as transfers to a bank or another wallet when their quantities and
commodities match. Other non-bank operations remain unposted.
Realized
gains, cost basis, market prices and derivative positions are further questions;
an exchange rate inferred from a trade is not a current market valuation.
