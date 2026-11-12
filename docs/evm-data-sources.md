# EVM data sources for Fin

Research note, 2026-11-12. The immediate question is how to understand the
Ethereum and Polygon movements the user recalls making with CoW Swap. No wallet
address or transaction hash was sent to an external service for this research.
The CSVs establish individual source rows; they do not by themselves establish
which cross-chain rows are one order.

## What dledger does

The local `~/code/dledger` checkout has an Etherscan API pipeline in
`src/lib/handlers/pipeline.ts` and `src/lib/browser-etherscan.ts`. It fetches
normal and internal transactions, ERC-20 transfers, and ERC-721 and ERC-1155
transfers for an address. It groups the returned records by transaction hash
within a chain, saves the grouped response, and passes the group through a
registry of protocol handlers. It retains transaction metadata including the
hash, chain, sender, recipient, function, status and gas information. It also
supports a separate The Graph Token API path.

The DEX aggregator handler recognizes the CoW settlement contract as one of
several known addresses. Its match requires a normal transaction whose `to`
address is the known contract. Its generic fallback can still interpret token
flows, but a group without that normal transaction will not match this handler
on that condition. This is a limitation of the code path, not proof about any
particular transaction in our data. The cross-chain bridge handler classifies
individual transaction groups; the inspected code does not establish that it
links a CoW order to its destination-chain fulfillment.

There are also lessons in the implementation that Fin should evaluate rather
than assume correct: the generic handler skips failed transactions, which can
still have gas costs, and the The Graph conversion constructs native-transaction
records with zero gas when that source has no gas data. Absence of fee data
should stay unknown rather than become evidence of a zero fee.

## General on-chain evidence

[Etherscan's API](https://docs.etherscan.io/api-reference/endpoint/txlistinternal)
provides normal transactions, internal transactions and token transfer endpoints
by address. This is closer to the data dledger consumes than separate CSVs, but
requires provider access and coverage, pagination and rate limits to be checked
for each chain and plan. Provider responses are indexed observations, not proof
that every economic effect has been found.

Standard [EVM JSON-RPC](https://ethereum.org/developers/docs/apis/json-rpc/)
can fetch a transaction and its receipt by hash. The receipt gives status,
gas used, effective gas price and logs, while the transaction gives the input,
value, sender and recipient. ERC-20 transfer logs can identify token transfers;
contract calls and internal native transfers may require traces and protocol
interpretation. Receipt logs alone are not a complete wallet history, and
ordinary JSON-RPC does not provide a universal address-history endpoint. Check
which historical data and trace methods a particular endpoint actually serves.

[The Graph Token API](https://token-api.thegraph.com/) provides indexed EVM
transfer and balance endpoints. dledger uses it as an alternative data source.
It needs a separate check for network coverage, completeness, fee data, history
and current access terms before selecting it for Fin.

## CoW order evidence

The [CoW order book API](https://docs.cow.fi/cow-protocol/reference/apis/orderbook)
provides account orders, order details and trades; it also lists orders by a
**settlement** transaction hash. Do not assume the hash of an order-creation
transaction is a settlement hash. The API's actual response and identifiers
need checking for the source transaction type before relying on a join.

CoW documents that [cross-chain swaps](https://docs.cow.fi/cow-protocol/tutorials/cow-swap/swap-and-bridge)
can involve an off-chain order, a source-chain swap, a proxy and a separate
bridge step. Native-token orders can involve an on-chain creation transaction.
CoW Explorer presents the swap and bridge separately. This makes the order API
and its source data worth investigating, but does not identify the provider or
prove which of the user's rows are connected.

## Suggested first investigation

1. Choose one known CoW order and obtain its order identifier or export from the
   user's own CoW history. Keep wallet addresses and transaction hashes private.
2. In a read-only experiment, inspect the CoW order and trade records alongside
   the source-chain transaction and receipt and the destination-chain receipt.
   Check what identifiers, chain IDs, status, exact token amounts, gas, and
   execution timestamps are actually present. Identify the bridge provider only
   if the evidence says what it is.
3. Compare the observed wallet balance effects with Fin's existing CSV records.
   Record missing or conflicting evidence and any intermediate or proxy-owned
   flows separately; never count those as wallet movements merely because they
   occur in the same transaction.
4. Design a source adapter only after the experiment. Preserve raw responses,
   request provenance and interpretation versions; identify on-chain events by
   chain, transaction and log or trace position. Handle pagination, reorgs,
   incomplete history, fees and provider disagreement explicitly. Review
   cross-chain links before using them for accounting.

API queries reveal the requested address or transaction hash to the chosen
provider. A local node or saved user-provided responses can be used for a private
experiment; either way, the outcome should remain reproducible offline.
