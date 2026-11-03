# Import audit issues

Audit of commit `b2a5dac` (unified CDC import). Both issues were
reproduced with disposable databases.

## Reimport does not validate stored operation movements

The unified importer checks an existing source event's timestamp and payload,
but does not compare its stored operation and movements with the interpretation
derived from the CSV. The previous import path performed this comparison.

To reproduce:

1. Import a CDC CSV containing a two-movement exchange.
2. In a disposable database, change the stored `from` movement's quantity in
   `operation_movements` from `-2` to `-999` using SQLite.
3. Reimport the same CSV.

Fin reports the file as unchanged and leaves the incorrect quantity in place.
This reproduction deliberately alters the database to demonstrate the missing
check; a changed importer interpretation could also leave stored proposals out
of sync without such an alteration.

Reimport should compare the derived operation with the stored occurrence,
movement roles, quantities, commodities and source links. It should reject a
discrepancy with an actionable error, without changing the stored decisions or
silently replacing movements. Add tests for both unchanged replay and a
different stored interpretation.

## Header-only CSV is accepted as a successful import

The movement CSV adapter accepts an empty event list. A CDC CSV with
only the expected header is reported as imported with `inserted=0`; the previous
import path rejected a document with no transaction rows. This can make an empty
or incomplete export look like a successful import.

To reproduce, create a CSV containing only the CDC header and run
`fin import cdc FILE`. Fin currently reports `imported=1` and exits
successfully.

Reject the file with a clear no-transaction-rows error and add an import test
covering a header-only file.
