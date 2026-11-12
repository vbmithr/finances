#!/usr/bin/env python3
"""What a definition promises about a file is enforced, one check at a time.

Each case changes one thing in a file fin accepts and names the reason it must give.
"""
import csv
import io
import json
import os
import pathlib
import subprocess
import sys
import tempfile
import zipfile

FIN = os.path.abspath(sys.argv[1])
INSTITUTIONS = pathlib.Path(sys.argv[2])
FIXTURES = pathlib.Path(sys.argv[3])
ROOT = pathlib.Path(tempfile.mkdtemp(prefix="fin-csv-checks-"))
ENV = dict(os.environ, HOME=str(ROOT))
FAILED = []


def text(header, rows, separator=","):
    out = io.StringIO()
    writer = csv.writer(out, delimiter=separator, lineterminator="\n")
    writer.writerow(header)
    writer.writerows(rows)
    return out.getvalue()


def run(contents, *target):
    path = ROOT / "statement.csv"
    path.write_text(contents)
    return subprocess.run(
        [FIN, "convert", *target, str(path)], env=ENV, capture_output=True, text=True
    )


def accepts(label, contents, *target):
    result = run(contents, *target)
    if result.returncode != 0:
        FAILED.append(f"{label}: refused: {' '.join(result.stderr.split())[:300]}")


def refuses(label, reason, contents, *target):
    result = run(contents, *target)
    message = " ".join(result.stderr.split())
    if result.returncode == 0:
        FAILED.append(f"{label}: accepted")
    elif reason not in message:
        FAILED.append(f"{label}: refused for another reason: {message[:300]}")


def changed(rows, index, **columns):
    """The rows with some cells of one row replaced."""
    rows = [dict(row) for row in rows]
    rows[index].update({k.replace("_", " "): v for k, v in columns.items()})
    return rows


class Source:
    def __init__(self, name, header, rows, separator=","):
        self.name, self.header, self.rows, self.separator = name, header, rows, separator

    def contents(self, rows):
        return text(self.header, [[r[c] for c in self.header] for r in rows], self.separator)

    def good(self):
        accepts(f"{self.name} unchanged", self.contents(self.rows), self.name)

    def bad(self, label, reason, rows):
        refuses(f"{self.name} {label}", reason, self.contents(rows), self.name)


def from_fixture(name):
    with open(FIXTURES / f"{name}.csv", newline="") as source:
        rows = list(csv.reader(source))
    return Source(name, rows[0], [dict(zip(rows[0], row)) for row in rows[1:]])


# Revolut.
header = "Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance".split(",")
def revolut_row(*cells):
    return dict(zip(header, cells))
rows = [
    revolut_row("Deposit", "Current", "2026-01-01 09:00:00", "2026-01-01 09:00:01", "In", "150.00", "0.00", "EUR", "COMPLETED", "150.00"),
    revolut_row("Card Payment", "Current", "2026-01-02 09:00:00", "2026-01-03 23:00:00", "Shop", "-10.00", "1.00", "EUR", "COMPLETED", "139.00"),
    revolut_row("Card Payment", "Current", "2026-01-04 09:00:00", "", "Undone", "-5.00", "0.00", "EUR", "REVERTED", ""),
]
revolut = Source("revolut", header, rows)
revolut.good()
revolut.bad("balance", "states a balance of 140 after 150", changed(rows, 1, Balance="140.00"))
# Rows in either direction add up; a mix of the two does not.
third = revolut_row("Deposit", "Current", "2026-01-05 09:00:00", "2026-01-05 09:00:01", "In", "1.00", "0.00", "EUR", "COMPLETED", "140.00")
accepts("revolut newest first", revolut.contents([third, rows[1], rows[0]]), "revolut")
revolut.bad("order", "is out of order", [rows[1], third, rows[0]])
revolut.bad("completion before start", "precedes", changed(rows, 1, Completed_Date="2026-01-01 08:00:00"))
revolut.bad("negative fee", "a fee cannot be negative", changed(rows, 1, Fee="-1.00"))
revolut.bad("garbage fee", "unexpected Fee", changed(rows, 2, Fee="free"))
revolut.bad("garbage amount on a reverted row", "unexpected Amount", changed(rows, 2, Amount="x"))
revolut.bad("reverted with a balance", "unexpected Balance", changed(rows, 2, Balance="1.00"))
revolut.bad("reverted with a completion", "unexpected Completed Date", changed(rows, 2, Completed_Date="2026-01-04 09:00:00"))
revolut.bad("completed with no balance", "unexpected Balance", changed(rows, 1, Balance=""))
revolut.bad("completed with no completion", "unexpected Completed Date", changed(rows, 1, Completed_Date=""))
revolut.bad("completed with no type", "unexpected Type", changed(rows, 1, Type=""))
revolut.bad("clock", "unexpected Started Date", changed(rows, 1, Started_Date="2026-01-02"))
revolut.bad("state", "unexpected State", changed(rows, 1, State="PENDING"))
revolut.bad("product", "unexpected Product", changed(rows, 1, Product="Savings"))

# One balance per currency: another currency's row does not break the chain.
other = revolut_row("Deposit", "Current", "2026-01-02 10:00:00", "2026-01-02 10:00:00", "In", "5.00", "0.00", "USD", "COMPLETED", "5.00")
accepts("revolut currencies", revolut.contents([rows[0], other, rows[1]]), "revolut")

# Wise.
wise = from_fixture("wise")
wise.good()
index = next(i for i, r in enumerate(wise.rows) if r["Transaction Type"] == "DEBIT")
day = wise.rows[index]["Date"]
wise.bad("date", "disagrees with Date Time", changed(wise.rows, index, Date="01-01-1999"))
wise.bad("clock", "unexpected Date Time", changed(wise.rows, index, Date_Time=day + " 10:00:00"))
wise.bad("direction", "unexpected Amount", changed(wise.rows, index, Amount="1.00"))
wise.bad("identifier", "unexpected TransferWise ID", changed(wise.rows, index, TransferWise_ID=""))
wise.bad("fees", "unexpected Total fees", changed(wise.rows, index, Total_fees="x"))
wise.bad("kind", "unexpected Transaction Type", changed(wise.rows, index, Transaction_Type="OTHER"))
wise.bad("balance", "unexpected Running Balance", changed(wise.rows, index, Running_Balance="1 2,3"))

# bunq.
bunq = from_fixture("bunq")
bunq.good()
bunq.bad("dot", "unexpected Amount", changed(bunq.rows, 0, Amount="-1.50"))
bunq.bad("one decimal", "unexpected Amount", changed(bunq.rows, 0, Amount="-1,5"))
bunq.bad("account", "unexpected Account", changed(bunq.rows, 0, Account="a b"))

# BoursoBank states its amount with a comma and its balance with a dot.
header = ["Date Opération", "Date Valeur", "Libellé", "Libellé Suggéré", "Catégorie", "Catégorie Parente", "Solde", "Commentaire", "Numéro Compte", "Libellé Compte", "Solde", "Pointage"]
def bourso(amount, balance):
    row = [""] * len(header)
    row[0] = row[1] = "2026-01-02"
    row[2] = row[3] = "Shop"
    row[8] = "123"
    row[6], row[10] = amount, balance
    return text(header, [row], ";")
accepts("boursobank unchanged", bourso("-1 234,50", "12.50"), "boursobank")
refuses("boursobank dot in the amount", "invalid quantity", bourso("-1.234", "12.50"), "boursobank")
refuses("boursobank comma in the balance", "invalid quantity", bourso("-1,50", "12,50"), "boursobank")

# Kraken: two eras of one ledger, one mapping.
old_header = ["txid", "refid", "time", "type", "subtype", "aclass", "asset", "amount", "fee", "balance"]
new_header = ["txid", "refid", "time", "type", "subtype", "aclass", "subclass", "asset", "wallet", "amount",
              "fee", "balance", "amountusd", "feeusd", "balanceusd", "feecurrency"]
def kraken_new(**row):
    return [row.get(c, "") for c in new_header]
old_file = text(old_header, [
    ["", "F1", "2020-01-02 10:00:00", "deposit", "", "currency", "ZEUR", "100.00", "0.00", ""],
    ["L1", "F1", "2020-01-02 10:05:00", "deposit", "", "currency", "ZEUR", "100.00", "0.00", "100.00"],
    ["L2", "T1", "2020-01-03 09:00:00", "trade", "", "currency", "ZEUR", "-50.00", "1.00", "49.00"],
    ["L3", "T1", "2020-01-03 09:00:00", "trade", "", "currency", "XXBT", "0.01", "0", "0.01"],
    ["L4", "T1", "2020-01-03 09:00:00", "trade", "", "currency", "KFEE", "-2", "0", "8"],
])
new_file = text(new_header, [kraken_new(
    txid="L5", refid="W1", time="2025-06-01 09:00:00", type="withdrawal", aclass="currency", asset="EUR",
    amount="-10.00", fee="0.00", balance="39.00", wallet="spot / main", amountusd="-11", feeusd="0",
    balanceusd="40", feecurrency="EUR")])
result = run(old_file, "kraken")
assert result.returncode == 0, result.stderr
assert "left out 1 row" in " ".join(result.stderr.split()), result.stderr
standalone = run(old_file, "--definition", str(INSTITUTIONS / "lua/kraken.lua"))
assert standalone.returncode == 0, standalone.stderr
assert "left out 1 row" in " ".join(standalone.stderr.split()), standalone.stderr
assert standalone.stdout == result.stdout
lines = [json.loads(line) for line in result.stdout.splitlines()]
records = {l["id"]: l for l in lines if l["kind"] == "record"}
assert sorted(records) == ["L1", "L2", "L3", "L4"]
# Codes are translated where they are read, and stay as stated in the fields.
assert {l["id"] for l in lines if l["kind"] == "account"} == {"EUR", "BTC", "FEE"}
assert records["L1"]["movements"][0]["commodity"] == {"kind": "fiat", "code": "EUR"}
assert records["L3"]["movements"][0]["commodity"]["kind"] == "reported"
assert records["L3"]["movements"][0]["commodity"]["code"] == "BTC"
assert ["asset", "XXBT"] in records["L3"]["details"]["fields"]
# A trade's rows are one group, its fee credit only supporting it; a deposit is alone.
assert {records[i].get("group") for i in ("L2", "L3", "L4")} == {"T1"}
assert records["L1"].get("group") is None
assert records["L4"].get("supporting") is True and not records["L2"].get("supporting")
assert records["L2"]["fee"] == "1"
accepts("kraken newer header", new_file, "kraken")
# The eras are one account: the newer file's rows land on the same EUR.
first, second = ROOT / "old.csv", ROOT / "new.csv"
first.write_text(old_file)
second.write_text(new_file)
both = subprocess.run([FIN, "convert", "kraken", str(first), str(second)], env=ENV, capture_output=True, text=True)
assert both.returncode == 0, both.stderr
assert sum('"kind":"account"' in l.replace(" ", "") and '"id":"EUR"' in l.replace(" ", "") for l in both.stdout.splitlines()) == 1
# Each file's balances are checked, whichever header it has.
refuses("kraken balance", "states a balance", old_file.replace("49.00", "48.00"), "kraken")
refuses("kraken derivative row", "unexpected aclass", old_file.replace("currency,KFEE", "future,KFEE"), "kraken")

for line in FAILED:
    print(line)
sys.exit(1 if FAILED else 0)
