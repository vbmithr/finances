# Reconciling statement observations

Fin should accept statement files as observations of transactions, regardless of
format or import order. An observation retains its source document and fields;
a transaction can have several linked observations. Statement boundaries,
balances and reconciliation remain properties of each statement. Conflicting
source values must remain visible rather than silently replacing one another.
The current implementation uses primary sources and companions, and remains the
plan (see [reconciliation design](reconciliation-design.md)). This document
records the matching evidence; the observation model it motivated is deferred in
[observation model](observation-model.md).

## Wise export comparison

On 2026-10-31, we compared the Wise CAMT.053, MT940 and CSV archives ending on
2026-10-29. The comparison used the original files in memory and a disposable
fin database, without changing the live database. These are observations about
these exports, not guarantees about future Wise statements or other banks.

- Each archive contains 2,003 transaction records across nine currencies; four
  of the currency statements have no movements. Each currency has one account in
  these archives, so currency was used to group candidate observations during
  this analysis. A real matcher must establish account identity independently.
- All 2,003 MT940 `:61:` fields contain `NONREF`, but the following continuation
  line contains a Wise reference. The CAMT reference is in its proprietary bank
  transaction code. Using currency, that reference and signed amount gives 1,972
  one-to-one candidate pairs; 31 MT940 entries have no exact reference candidate.
  Currency and reference alone leave 44 observations in non-unique groups.
- For the 1,972 reference-and-amount pairs, 1,956 have the same reported calendar
  date; the CAMT booking date is one day later for the other 16. These dates come
  from different source fields, so fin must retain both rather than force equality.
- Of the 31 reference mismatches, 29 have unique nearby amount-and-currency
  candidates whose CAMT description is cashback. The other two also have unique
  nearby amount-and-currency candidates. Those are candidate relationships, not
  proof that they are duplicates; their reference representations need further
  investigation before defining any normalization rule.
- Using only currency, date and signed amount, 1,895 MT940 entries have a unique
  CAMT candidate, 91 belong to 39 non-unique groups, and 17 have no same-day
  candidate. A unique candidate in this dataset alone does not establish identity.
- Eight of the nine MT940 account identifiers equal the CAMT account identifier.
  For GBP, MT940 supplies an eight-digit identifier that matches the end of the
  CAMT IBAN. A suffix match is evidence to inspect, not a generally safe account
  identity rule.
- The reported opening and closing balance amounts agree in all nine currencies.
  The CAMT local balance dates are one calendar day later than the corresponding
  MT940 dates in all nine. Keep the statement observations separate; do not
  assume equal balances imply identical statement boundaries.
- Fin's existing CSV-to-CAMT matcher matched 2,002 CSV rows and left one unmatched.
  That row has one same-day, same-amount, same-currency CAMT candidate with the
  same description and local clock to milliseconds, but the references differ:
  one is a suffix of the other. This is a candidate for investigation, not an
  instruction to match arbitrary reference suffixes automatically.

The current MT940 reader does not parse this Wise export: its framing, account
field, statement sequence field and entry layout differ from the supported
Bunq-style profile. Supporting the file requires extending the reader, not just
changing its recognizer.

## Direction

These goals describe the deferred observation model. Under the current plan the
matching evidence above feeds companion matching improvements.

- Represent every imported source record as immutable evidence, with a link to
  its document, account, original fields and any reliable source identifier.
  Keep distinct rows from the same statement distinct, including identical ones.
- Associate observations with a transaction using explicit, institution-scoped
  evidence and matching rules. Preserve source identifiers and source-specific
  dates. A match must be safe in both directions and under later imports, not
  merely unique in the current batch.
- When matching is ambiguous or evidence conflicts, retain the observations and
  expose candidates for review. Do not silently merge or create a duplicate just
  because an automatic match could not be established. Make corrections and
  manually confirmed links auditable and reversible.
- Keep statement membership and balance checks tied to their original documents.
  Merge transaction details only through traceable links, and define how
  conflicting descriptions, amounts, dates, types and fees are presented.
- Exercise both import orders, reimports, repeated identical payments, refunds,
  unmatched observations and conflicting evidence. First compare existing Wise
  CAMT and CSV behavior before enabling MT940 to add or link transactions.

## Outcome: Wise MT940 is not parsed

Checked 2026-10-31 against the same 2026-10-29 archives, field by field. The
MT940 entries hold a signed amount, currency, date, a typed reference on the
`:61:` continuation line, opening and closing balances, and an `/EXCH/` rate in
`:86:` on 237 entries. CAMT carries the amount, date, reference, balances and the
exchange rate (`CcyXchg`); the CSV carries all of these plus description,
merchant, payer, fees and running balance. The one MT940-only item is the SWIFT
type code (`FMSC`, `FCHG`, `FFEX`, `FTRF`, `FDDT`). It follows the reference
prefix (`FEE-` is `FCHG`, `BALANCE-` is `FFEX`, `DIRECT_DEBIT-` is `FDDT`);
`TRANSFER-` splits between `FTRF` (debits, 38) and `FMSC` (credits, 75), with 2
credits coded `FTRF`. Nothing is added that CAMT, CSV and the direction do not
already give.

Wise therefore uses CAMT as primary and CSV as companion. The 31 reference
mismatches and one-day date differences above were MT940-versus-CAMT effects and
do not arise without MT940. This is one export from one bank; Bunq's MT940 is its
primary and is unaffected.

