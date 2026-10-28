# Providers, accounts and imported formats

## Model

A provider is the institution that holds an account. An account identifies a
balance or obligation at that provider, independently of the file used to observe
it. Import formats describe documents: the same account may be observed through
more than one format and the same generic CSV importer may serve many providers.
A source-supplied bank or branch identifier is evidence useful for identifying
an account; it is not necessarily meaningful as a ledger hierarchy component.

A separately funded prepaid card is an asset account. A credit card balance can
be a liability. A transfer from a bank account to a prepaid card moves value
between assets. A transfer of a balance between replacement cards is a separate
accounting event; do not infer its amount or date from purchase exports.

Default ledger names should expose the account's meaning, for example
`assets:cards:examplecard:old`, and avoid repeating the provider or embedding import
formats. Preserve account names chosen by users. Allow suitable asset and
liability mappings and distinguish them from expense and income accounts.

## Existing generic CSV imports

Early generic imports stored `csv` as the provider. During development, users
may discard those accounts with `fin account delete` and import the source
files again using a named provider. Deletion is explicit and includes the
account's transactions and associated exclusive source records. Export account
settings or make a database backup first if they may be needed. Reviews attached
to the deleted transactions are also discarded.

Deletion refuses cases involving shared documents, companion references,
transfer decisions or settlement references from other accounts, because those
need separate handling. An unrecorded transfer between replacement cards must
still be recorded separately; deleting or reimporting cannot infer it.

## Implementation sequence

- Make generic imports use the selected provider rather than a file format as the
  institution, and keep the generic format on the imported document.
- Allow asset and liability account mappings and add meaningful defaults for new
  card accounts without silently changing existing mapped names.
- Allow account-scoped deletion with a dry run and conservative checks, so
  development imports can be discarded and reimported under the right provider.
- Update CLI help and the manual and verify their commands. Document deletion
  and reimport of development-era legacy accounts.
