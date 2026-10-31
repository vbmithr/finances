# Start personal accounting with fin and hledger

The first useful milestone is one month you can explain: the bank movements are
complete, transfers between your accounts are identified, and ordinary income
and spending have sensible categories. After that, add verified opening balances
so the books also agree with your bank balances.

This guide assumes basic double-entry accounting. It uses September 2026 and EUR
as examples; choose your own period and currencies. Amounts and event IDs below
are fictional. Commands were checked with the current fin CLI and hledger 1.99.5.
For individual options, see the [user manual](manual.md).

## Choose a manageable first month

Start with your day-to-day account and the account you most often transfer money
to. Pick a recent completed month for which you have complete statements. Bring
in the other end of transfers too: otherwise a payment to your own savings account
can look like unexplained spending.

If fin already contains older records, keep them. Focus reports on your chosen
month and review the corresponding event IDs. `fin transaction review` and `fin status`
cover the whole database; they currently have no month filter. Older unresolved
movements do not need to be finished before you can review a later month.

For this first pass, answer three questions:

- What income did I receive?
- What did I spend it on?
- Which movements just moved money between my own accounts?

Leave budgets, investment valuation and a complete net-worth statement for later.

## Keep evidence, decisions and reports in their proper places

fin holds imported evidence and reviewed decisions in SQLite. hledger reads a
journal exported from that database. Keep the generated journal separate from
any journal entries you maintain yourself:

| Item | Maintained by | Purpose |
| --- | --- | --- |
| Bank exports in `inbox/` | Your banks | Original evidence, retained unchanged |
| fin's SQLite database | fin | Imported records, account settings, review decisions and audit history |
| JSON files in `reviews/` | You | Instructions to apply to fin; the applied decisions live in SQLite |
| `ledger/bank.journal` | fin export | Generated bank accounting entries; regenerate after changes |
| `ledger/opening.journal` | You | Verified opening balances, added later in this guide |
| `ledger/checks.journal` | You | Closing-balance assertions |
| `ledger/main.journal` | You | Includes the files hledger should report on |

Set up a private working directory outside your statement inboxes:

```sh
umask 077
mkdir -p ~/fin-accounting
cd ~/fin-accounting
mkdir -p inbox/boursobank inbox/bunq inbox/revolut inbox/wise
mkdir -p ledger reviews reports backups
fin init
fin paths
fin backup backups
```

`fin init` preserves an existing database. The workspace above does not move that
database: `fin paths` shows its location. Run the remaining shell commands from
`~/fin-accounting`. Use `--database FILE` consistently if you choose another
database.

## Import statements and name your accounts

Copy your exports into the corresponding inboxes. Run only the commands for banks
whose files you have supplied:

```sh
fin import boursobank inbox/boursobank
fin import bunq inbox/bunq
fin import revolut inbox/revolut --account-id main
fin import wise inbox/wise
fin institution history --limit 20
fin status
fin account list
```

Use OFX XML for BoursoBank, MT940 for Bunq, supported Current-account CSV for
Revolut, and CAMT.053 XML or ZIP for Wise. Companion CSV files enrich primary
records for BoursoBank, Bunq and Wise; they are not another set of transactions.
Revolut's CSV is itself a primary source. Use the same Revolut account label on
every import. Separate Revolut accounts need separate labels and inboxes.

Inspect rejected or unreadable files in import history before trusting the
coverage. The first and last dates in `fin status` are observed movement dates;
they do not prove you have every statement between those dates.

Next, run:

```sh
fin account edit
```

Choose stable, readable ledger names. For example, use `assets:bank:daily` for
your day-to-day EUR account and `assets:bank:savings` for your reserve EUR account.
Keep separate ledger names for different accounts and currencies. Current fin
requires bank ledger names below `assets:bank:`.

Change `ledger_account`, and supply missing IBANs where appropriate. Keep the
source identity fields unchanged. Imported IBANs cannot be replaced. Registering
IBANs improves transfer evidence; it does not itself confirm transfers.

## Make an explicit hledger entry point

Export the current bank entries and create initially empty manual files:

```sh
fin journal export > ledger/bank.journal
touch ledger/opening.journal ledger/checks.journal
```

Create `ledger/main.journal` with:

```journal
account equity:opening-balances

include opening.journal
include bank.journal
include checks.journal
```

The include paths are relative to this file. The generated bank journal already
declares its accounts and currencies. The equity declaration is for the opening
entries you will add later. Keep this parent file: hledger's balance assertions
can see entries across [included files](https://hledger.org/hledger.html#assertions-and-multiple-files).

```sh
hledger --no-conf -f ledger/main.journal --strict check
```

All hledger examples specify the input file and disable configuration-file
defaults, so you know which books and report options you are using.

After changing account names, categories or transfers, export again. Only replace
`ledger/bank.journal`; keep your manual files. `fin import` also generates a
`bank.journal` beside the database by default, but this guide always refreshes the
workspace copy explicitly. Editing that generated copy does not update fin.

## Identify transfers before categorizing spending

```sh
fin transfer candidates
fin transfer candidates --json > reports/transfer-candidates.json
```

Check both account owners, amounts, dates and bank references. Candidates are
suggestions, and a high score does not establish ownership. A transfer to your
own savings account is neither an expense nor income.

Run `fin transfer review` to inspect pairs and stage decisions. Use `a` to stage
all supported pairs and finish selection, then check and confirm the proposed
batch. Other candidates remain for later review.
For a scripted batch, create `reviews/transfers-2026-09.json` with the actual IDs:

```json
[
  {
    "first_event_id": 103,
    "second_event_id": 104,
    "decision": "confirm",
    "reason": "Matched the EUR 300 withdrawal and receipt in my two accounts"
  }
]
```

```sh
fin transfer review reviews/transfers-2026-09.json
fin transfer history
fin journal export > ledger/bank.journal
```

Fin exports a confirmed pair as one journal transaction directly between the two
bank accounts. Both source event IDs remain in its metadata, and fees keep their
own postings. When booking dates differ, the later bank posting carries its own
hledger posting date. A date-filtered report between the two dates therefore
reflects the different bank balances without an in-transit asset; the books may
appear temporarily unbalanced by date.

Supported pairs have opposite equal principals, the same currency and booking
dates at most seven days apart. If a leg is missing, obtain its statement first.
Use `"decision": "reject"` for a false candidate or to undo a confirmation. An
already categorized leg must first be reset to `equity:unclassified` before it
can become a confirmed transfer.

## Use a small chart of income and expenses

Start with categories you would use to make a decision. A workable first chart is:

| Income | Expenses |
| --- | --- |
| `income:salary` | `expenses:housing` |
| `income:interest` | `expenses:groceries` |
| `income:other` | `expenses:eating-out` |
| | `expenses:transport` |
| | `expenses:health` |
| | `expenses:subscriptions` |
| | `expenses:leisure` |
| | `expenses:bank-fees` |
| | `expenses:other` |

Keep `other` for understood items without a useful finer category. Leave an
unexplained movement unclassified. You can refine the chart as useful patterns
emerge; there is no need to anticipate every merchant.

```sh
fin transaction review
fin transaction review --json > reports/bank-review.json
```

Review the amount, description, date and source account. Bank-supplied category
tags are suggestions, not applied decisions. Create a small batch in
`reviews/categories-2026-09.json`, replacing these illustrative IDs:

```json
[
  {"event_id": 101, "account": "expenses:groceries", "reason": "Weekly food shopping"},
  {"event_id": 102, "account": "income:salary", "reason": "September salary"}
]
```

```sh
fin transaction category apply reviews/categories-2026-09.json
fin transaction category history
fin journal export > ledger/bank.journal
hledger --no-conf -f ledger/main.journal --strict check
```

Categories change the counter-account; they preserve amounts and signs. A EUR 45
purchase becomes EUR -45 in the bank and EUR 45 in groceries. A refund of that
purchase belongs in the same expense account and reduces groceries. A salary
receipt increases the bank and credits `income:salary`.

Separately reported Revolut fees already post to `expenses:bank-fees`. Standalone
bank fee movements still need a category. Check the generated entry before adding
anything manually, so a fee is not counted twice.

An invalid batch applies nothing. Correct a classification by applying a new
decision for the same event ID, with a reason; use `equity:unclassified` to reopen
it for review. Identical decisions can be reapplied safely. Preserve event IDs
exactly, including decimal strings if fin reports them that way.

## Read your first spending report

Refresh the generated file, then inspect the month:

```sh
fin journal export > ledger/bank.journal
hledger --no-conf -f ledger/main.journal incomestatement --monthly \
  --begin 2026-09-01 --end 2026-10-01
hledger --no-conf -f ledger/main.journal balance expenses --tree --monthly \
  --begin 2026-09-01 --end 2026-10-01
hledger --no-conf -f ledger/main.journal register equity:unclassified \
  --begin 2026-09-01 --end 2026-10-01
```

hledger end dates are exclusive: October 1 includes all of September. The income
statement shows income and expenses with conventional signs. A raw `balance
income` report normally shows income credits as negative amounts.

The unclassified register matters even if its net balance is zero: unexplained
receipts and payments can cancel. Check individual movements, not just the sum.
Unclassified entries are outside the income statement, so unresolved items in
the month make its income or spending incomplete.

For a tiny example containing EUR 2,000 salary, EUR 45 groceries and a EUR 300
transfer to savings, the income statement should show EUR 2,000 income, EUR 45
expenses and EUR 1,955 surplus. The transfer should appear in neither total.
These are flows: no opening balances are needed to calculate them.

Keep currencies separate at first. Multiple currencies are reported separately;
an EUR/USD total needs an explicit valuation policy and suitable prices.

To trace a journal transaction back to an illustrative fin event:

```sh
hledger --no-conf -f ledger/bank.journal print 'code:^event-101$'
```

## Add opening balances before trusting account totals

fin imports movements but does not create opening-balance entries. A bank account
showing EUR -45 could mean only that you imported a EUR 45 purchase.

For each account, find a verified balance immediately before its earliest
imported movement. Use a start-of-day balance before that day's transactions,
or the opening balance of an earlier fully covered statement period. Opening
balances belong against equity, not income. This follows hledger's
[opening-balance convention](https://hledger.org/set-starting-balances.html).

For example, **only if the included history starts in September**, and the
verified opening amounts really are EUR 1,250 and EUR 4,000, put this in
`ledger/opening.journal`:

```journal
2026-09-01 Opening balances
    assets:bank:daily          1250.00 EUR
    assets:bank:savings        4000.00 EUR
    equity:opening-balances  -5250.00 EUR
```

If fin already contains January-to-August movements, use opening balances before
that history instead. `fin journal export` exports all stored entries, not just
your report month. Adding September opening balances on top of earlier imported
activity would double-count it. If you later import older history, revisit the
opening date and amount for the affected account.

```sh
hledger --no-conf -f ledger/main.journal --strict check
hledger --no-conf -f ledger/main.journal aregister assets:bank:daily \
  --end 2026-10-01
hledger --no-conf -f ledger/main.journal balancesheet --end 2026-10-01
hledger --no-conf -f ledger/main.journal balance assets:transfers --historical \
  --end 2026-10-01
```

Use the full prior history for balances; a report restricted to one month's
changes is not its closing balance. With the fictional openings and movements
above, daily closes at EUR 2,905 and savings at EUR 4,300. The balance sheet covers
the accounts you have included; it is not your complete net worth while other
assets or debts are absent.

The manual opening file affects hledger reports only. It does not change fin's
database, status or statement reconciliation.

## Reconcile, then keep a closing-balance check

```sh
fin statement list
fin statement companions --review
```

`fin statement list` checks the movements linked to each statement against its
stated balances. Wise and Bunq supply useful statement checks. Missing balance
observations mean there is nothing to check, not that a balance was verified.
Companion review is about matching enrichment rows; it is a different queue from
uncategorized movements.

Also compare each hledger closing balance with the bank's **booked** balance on
the same date. Available balances can include holds and differ. When they do not
agree, check missing periods, account names, opening balances, duplicates and
booking dates. Do not use an unexplained equity adjustment to hide a discrepancy.

Once an account agrees, append a zero-amount assertion to `ledger/checks.journal`.
For the fictional September example:

```journal
2026-09-30 Daily account closing balance
    assets:bank:daily       0.00 EUR = 2905.00 EUR

2026-09-30 Savings account closing balance
    assets:bank:savings     0.00 EUR = 4300.00 EUR
```

```sh
hledger --no-conf -f ledger/main.journal --strict check
```

The zeros add no movements. The assertions record balances you actually checked;
later changes to the history cause a failing check if they disturb those amounts.
Keep checks included after bank entries, so checks dated on the same day follow
that day's bank postings. See hledger's
[reconciliation workflow](https://hledger.org/reconciling.html) for more detail.

## Keep the difficult cases visible

Current fin supports one reviewed income/expense counter-account per movement and
explicit same-currency transfer pairs. It is not yet a general manual-entry UI.

| Case | First-pass treatment |
| --- | --- |
| Refund | Use the original expense category, preserving its sign |
| Transfer to another owned account | Confirm the matching imported legs |
| Card purchases and card settlement | Categorize purchases once; do not also categorize the whole card repayment as spending |
| Card settlement link in account settings | Link the card to checking, then review the bank debit with `fin settlement review EVENT_ID` |
| Cash withdrawal | Decide whether you will track cash; fin's category command cannot move the counter-posting to `assets:cash` |
| Loan principal, investments or money owed by someone else | These need asset/liability accounting; ordinary expense categories are insufficient |
| FX transfers, split purchases or transfers outside the supported date window | Record them as exceptions to resolve; do not force them into an unrelated category |

Some imported card accounts default to names under `assets:bank:`. Use
`fin account link-card CARD-ID CHECKING-ID` to link a credit card and propose a
liability account. For BoursoBank, `fin settlement review` can suggest the
relationship and the bank debit together. Confirm the bank debit after reviewing it;
the link alone does not create a posting. If the card statement contains a
matching payment movement, review the pair as a transfer instead. For unsupported
cases, leave a note with the event ID and keep them
visible as unresolved. Scope your first reports accordingly. hledger can model
these cases, but any manual extension must account for fin's existing postings
without recording the same bank movement twice. The [manual's card settlement
walkthrough](manual.md#reviewing-transactions-and-transfers) shows the account
settings and review commands.

## Make it a monthly routine

For each new statement period:

1. Download the primary statements and optional companions; retain the originals.
2. Run `fin backup backups`, then repeat the appropriate `fin import` commands.
3. Inspect import history and statement/companion checks; resolve coverage gaps.
4. Review owned-account transfers, then classify ordinary income and expenses.
5. Export `ledger/bank.journal` again and run hledger's strict check.
6. Review the period's unclassified movements and any transfer balances in transit.
7. Compare bank closing balances, then add verified assertions and recheck.
8. Read the income statement and expense breakdown; decide what to change next month.
9. Back up the database and your manual accounting files.

For a report you want to keep:

```sh
hledger --no-conf -f ledger/main.journal incomestatement --monthly \
  --begin 2026-09-01 --end 2026-10-01 --output-format csv \
  > reports/income-expenses-2026-09.csv
fin backup backups
```

fin's backup contains its SQLite state, including reviewed decisions. Back up
`ledger/main.journal`, `ledger/opening.journal`, `ledger/checks.journal`, review
notes and original exports separately with your usual private-file backup tool.
`fin restore BACKUP` restores SQLite; it does not restore those external files.
After restoration, regenerate `ledger/bank.journal` from the restored database
before using this workspace's reports.

Call the first month finished when its coverage is known, its classifications and
exceptions are understood, and the included account balances reconcile. Then
expand to another account or an earlier month using the same process.

## Further reading

- [fin user manual](manual.md): command, storage and recovery reference.
- [Account and review JSON reference](../README.md#manage-bank-accounts).
- [hledger quick start](https://hledger.org/5-minute-quick-start.html).
- [hledger manual](https://hledger.org/hledger.html): report periods, account types,
  journal includes and balance assertions.
