# MT940 grammar and Bunq interpretation

Fin parses common MT940 statement fields with Angstrom, then interprets Bunq's
export separately. This document describes the supported syntax, not a full
SWIFT network-message validator. The format reference is the
[SWIFT MT 940 message reference](https://www2.swift.com/knowledgecentre/rest/v1/publications/us9m_20190719/2.0/us9m_20190719.pdf?logDownload=true);
individual banks also publish their own usage conventions, for example
[Danske Bank's MT940 field specification](https://danskeci.com/-/media/pdf/danskeci-com/collection-service/collection-service/mt940.pdf?hash=2463BD39CA37DE67F69A0842A2BD03D9&rev=2f15973584ff48debbca63e67b0148bd).

## Supported syntax

The generic reader accepts UTF-8 text, LF or CRLF lines, and one or more
statements terminated by a line containing `-`. Lines before the first field
are treated as a preamble. A field begins with `:TAG:` at the start of a line;
subsequent lines are its continuation until the next field or terminator.
Tags use uppercase letters and digits. Fields, including unknown fields and
unassociated `:86:` fields, retain their text and order. A `:86:` immediately
after a `:61:` supplies its optional memo.

Each statement must have exactly one `:20:`, `:25:` and `:28:` or `:28C:`.
The reader accepts either `:60F:` or `:60M:` as the opening balance, and
`:62F:` or `:62M:` as the closing balance, with at most one of each; these
balances are optional in the syntax layer. Other fields remain available as
uninterpreted text.

The following notation describes the supported field values; brackets mean
optional, braces mean zero or more, and quoted characters are literal:

```text
balance       = direction, YYMMDD, currency, amount ;
entry         = YYMMDD, [ MMDD ], direction, [ funds-code ], amount,
                transaction-type, reference, [ "//", bank-reference ],
                [ newline, supplementary-text ] ;
direction     = "C" | "D" | "RC" | "RD" ;
amount        = digit, { digit }, ",", { digit } ;
transaction-type = ( "N" | "S" | "F" ), alphanumeric, alphanumeric,
                   alphanumeric ;
currency      = three-letter ISO 4217 code ;
funds-code    = uppercase-letter ;
```

The reader retains the reference, optional bank reference and supplementary
text, but does not enforce all SWIFT field lengths, character sets, message
ordering, or network validation rules. It interprets two-digit years as 2000
through 2099. An entry date without a year stays as month and day until the
institution interpreter resolves it. Amounts are exact decimal quantities;
reversal credit and reversal debit remain distinct directions.

This is a statement-export reader, not a complete parser for SWIFT envelopes,
MT942, arbitrary network blocks, multiple interleaved messages, or every bank's
extensions. The initial preamble can contain a bank header or a simple SWIFT
header, but full envelope validation and trailers are not implemented.

## Bunq

The Bunq adapter requires a booking date, an ordinary credit or debit,
no funds code, an `N` transaction type with three letters, and `NONREF` with no
bank reference or supplementary text in each `:61:`. It requires a single statement, a `:28:` field, final `:60F:` and `:62F:` balances,
and an accompanying `:86:` for each `:61:`. The account field must contain an
account and currency. The adapter checks the balance currencies, the statement
date interval, and that opening balance plus movements equals closing balance.
It uses the Bunq profile's markers to extract the counterparty, name and
remittance, resolves booking dates near the value date, and preserves Fin's
existing inventory identities and duplicate handling. It rejects unsupported
fields rather than silently dropping them.

These restrictions are Bunq import behavior, not general MT940 grammar. Other
banks would need an explicit interpretation and validation layer before their
statements could be imported. The generic reader by itself does not convert a
statement into Fin records or CAMT.
