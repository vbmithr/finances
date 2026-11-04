#!/usr/bin/env python3
"""CSV converters: what a definition says, and that bundled ones produce valid records."""
from decimal import Decimal
import json
import pathlib
import subprocess
import sys
import tempfile

FIN, ROOT, FIXTURES = sys.argv[1], pathlib.Path(sys.argv[2]), pathlib.Path(sys.argv[3])
try:
    import jsonschema

    SCHEMA = json.loads((ROOT / "fin-records.schema.json").read_text())
except ImportError:
    print("csv converter test: python jsonschema is not installed, schema not checked")
    SCHEMA = None


def convert(*args, check=True):
    done = subprocess.run([FIN, "convert", *map(str, args)], capture_output=True, text=True)
    if check:
        assert done.returncode == 0, done.stderr
        lines = [json.loads(line) for line in done.stdout.splitlines()]
        if SCHEMA:
            for line in lines:
                jsonschema.Draft202012Validator(SCHEMA).validate(line)
        return lines
    return done


def kind(lines, name):
    return [line for line in lines if line["kind"] == name]


def details(record):
    return record.get("details", {})


directory = tempfile.TemporaryDirectory()
root = pathlib.Path(directory.name)


def write(name, text):
    path = root / name
    path.write_text(text)
    return path


# Revolut: a reverted row has no effect, and the balance moves by amount minus fee.
revolut = write("revolut.csv", """Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance
Deposit,Current,2026-01-01 09:00:00,2026-01-01 09:00:01,In,150.00,0.00,EUR,COMPLETED,150.00
Card Payment,Current,2026-01-02 09:00:00,2026-01-03 23:00:00,Shop,-10.00,1.00,EUR,COMPLETED,139.00
Card Payment,Current,2026-01-04 09:00:00,,Undone,-5.00,0.00,EUR,REVERTED,
Exchange,Savings,2026-01-05 09:00:00,2026-01-05 09:00:00,Other product,-1.00,0.00,EUR,COMPLETED,1.00
""")
lines = convert("revolut", revolut)
records = kind(lines, "record")
assert len(records) == 3, "another product is not this account"
assert kind(lines, "header")[0]["identity"] == "inventory"
assert records[1]["movements"][0]["quantity"] == "-11", records[1]
assert records[1]["fee"] == "1"
assert "fee" not in records[0], "a zero fee is no fee"
assert records[1]["occurred"] == "2026-01-03", "a clock with no offset states a date"
assert details(records[1])["balance_after"] == "139"
assert records[2]["movements"] == [] and records[2]["occurred"] == "2026-01-04"
assert details(records[2])["reported"] == {"quantity": "-5", "symbol": "EUR"}
assert records[0]["id"] == "2026-01-01/1"

# CDC: legs depend on the kind and the signs, and an unknown kind proposes no movement.
header = "Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n"
cdc = write("cdc.csv", header + """2025-01-02 10:00:00,Swap,CRO,-2,EUR,1.5,EUR,1.5,1.6,crypto_exchange,
2025-01-02 11:00:00,Mystery,CRO,1,,,EUR,1,1.1,never_seen,
2025-01-02 12:00:00,Reward,CRO,1,,,EUR,1,1.1,referral_bonus,0xSYNTHETIC
""")
lines = convert("cdc", cdc)
exchange, unknown, reward = kind(lines, "record")
assert [m["role"] for m in exchange["movements"]] == ["from", "to"]
assert exchange["movements"][1]["commodity"] == {"kind": "reported", "code": "EUR", "provider": "cdc"}
assert exchange["occurred"] == "2025-01-02T10:00:00Z", "the column says UTC"
assert unknown["movements"] == [] and details(unknown)["reported"]["symbol"] == "CRO"
assert details(reward)["references"]["hash"] == "0xSYNTHETIC"
assert details(reward)["valuations"][1] == {
    "quantity": "1.1", "commodity": {"kind": "fiat", "code": "USD"}}
assert exchange["id"].startswith("row-sha256-") and kind(lines, "header")[0]["identity"] == "source"

# FDM: amounts are spending, a refund is negative, and the undated row is a total.
fdm = write("fdm.csv", """Merchant,Category,Date,Local currency,Local amount,Exchange rate,USD
Shop,Shopping,2025-01-02,EUR,10.00,1.1,11.00
Shop,Shopping,2025-01-03,EUR,-1.00,1.1,-1.10
Total,,,,,,9.90
""")
lines = convert("fdm", fdm)
records, statements = kind(lines, "record"), kind(lines, "statement")
assert len(records) == 2 and len(statements) == 1
assert records[0]["movements"][0]["quantity"] == "-11"
assert records[1]["movements"][0]["quantity"] == "1.1"
assert details(records[1])["exchange"]["target_quantity"] == "-1"
assert statements[0]["total"]["quantity"] == "-9.9"
assert all(statements[0]["id"] in r["statements"] for r in records)

# BoursoBank: a repeated header name, comma decimals, and one balance per value date.
bourso = write("bourso.csv", """Date Opération;Date Valeur;Libellé;Libellé Suggéré;Catégorie;Catégorie Parente;Solde;Commentaire;Numéro Compte;Libellé Compte;Solde;Pointage
2025-01-02;2025-01-03;A ;A;C;P;-1 234,50;;111;Main;100.00;Non
2025-01-02;2025-01-03;B;B;C;P;-1,00;;111;Main;100.00;Non
2025-01-04;2025-01-05;C;C;C;P;2,00;;111;Main;102.00;Non
""")
lines = convert("boursobank", bourso)
records, balances = kind(lines, "record"), kind(lines, "balance")
assert records[0]["movements"][0]["quantity"] == "-1234.5"
assert records[0]["value_on"] == "2025-01-03"
assert details(records[0])["descriptions"][0] == "A ", "text is kept as stated"
assert all(r["observation"] for r in records)
assert [(b["type"], b["quantity"], b["as_of"]) for b in balances] == [
    ("day_end", "100", "2025-01-03"), ("day_end", "102", "2025-01-05")]

# bunq: a narrow no-break space groups digits.
bunq = write("bunq.csv", "Date,Interest Date,Amount,Account,Counterparty,Name,Description\n"
             '2025-01-02,2025-01-03,"-1 234,50",SYNTHETIC,,Shop,Text\n')
assert kind(convert("bunq", bunq), "record")[0]["movements"][0]["quantity"] == "-1234.5"

# Wise's CSV reads through its definition, and its references match the CAMT's.
camt = kind(convert("wise", FIXTURES / "wise.xml"), "record")
csv = kind(convert("wise", FIXTURES / "wise.csv"), "record")
assert all(r["observation"] for r in csv)
assert {details(r)["references"]["servicer"] for r in csv} == {
    details(r)["references"]["servicer"] for r in camt}

# bunq: MT940 and CSV name one account; each file adds what it knows, and IDs do not collide.
lines = convert("bunq", FIXTURES / "bunq.mt940", FIXTURES / "bunq.csv")
assert len(kind(lines, "header")) == 1
assert len(kind(lines, "account")) == 1, kind(lines, "account")
assert kind(lines, "account")[0].get("type"), "from the MT940"
counted = [r for r in kind(lines, "record") if not r.get("observation")]
observed = [r for r in kind(lines, "record") if r.get("observation")]
assert counted and observed and all(r["id"].startswith("observation/") for r in observed)
assert all(r["value_on"] for r in counted)
statement = kind(lines, "statement")[0]
assert all(statement["id"] in r["statements"] for r in counted)
balances = {b["type"]: Decimal(b["quantity"]) for b in kind(lines, "balance")}
assert set(balances) == {"opening", "closing"}
assert balances["opening"] + sum(
    Decimal(r["movements"][0]["quantity"]) for r in counted) == balances["closing"]

# BoursoBank: OFX has row IDs and its CSV does not, so they are two sections of one file.
lines = convert("boursobank", FIXTURES / "bank.ofx", bourso)
assert [h["identity"] for h in kind(lines, "header")] == ["source", "inventory"], lines[0]
assert len(kind(lines, "end")) == 2
ofx = [r for r in kind(lines, "record") if not r.get("observation")]
assert ofx and all(details(r)["references"]["servicer"] == r["id"] for r in ofx)
with_output = root / "bourso.jsonl"
done = subprocess.run([FIN, "convert", "boursobank", str(FIXTURES / "bank.ofx"), str(bourso),
                       "-o", str(with_output)], capture_output=True, text=True, check=True)
assert len(done.stdout.splitlines()) == 2, done.stdout
check = subprocess.run([FIN, "records", "check", str(with_output)],
                       capture_output=True, text=True, check=True)
assert check.stdout == done.stdout

# A header alone is a statement of no activity.
empty = write("empty.csv", revolut.read_text().splitlines()[0] + "\n")
lines = convert("revolut", empty)
assert kind(lines, "record") == [] and kind(lines, "account") == []

# Anyone's definition, and refusals that say what is wrong.
definition = write("mine.json", json.dumps({
    "format": "fin-csv-converter", "version": 1, "institution": "mine",
    "institution_type": "bank",
    "header": ["When", "What"],
    "account": {"id": "main", "currency": {"code": "EUR"}},
    "occurred": {"column": "When", "layout": "day-first"},
    "cases": [{"movements": [{"role": "primary", "quantity": {"column": "What"},
                              "commodity": {"code": "EUR"}}]}],
}))
statement = write("mine.csv", "When,What\n02-01-2025,-3.50\n")
lines = convert("--definition", definition, statement)
assert kind(lines, "header")[0]["institution"] == "mine"
assert kind(lines, "record")[0]["occurred"] == "2025-01-02"

broken = write("broken.json", definition.read_text().replace('"What"}', '"Wat"}'))
done = convert("--definition", broken, statement, check=False)
assert done.returncode != 0 and "Wat" in done.stderr and done.stdout == "", done.stderr
bad_row = write("bad.csv", "When,What\n02-01-2025,abc\n")
done = convert("--definition", definition, bad_row, check=False)
assert done.returncode != 0 and "row 1" in " ".join(done.stderr.split()), done.stderr
other = write("other.csv", "A,B\n1,2\n")
done = convert("--definition", definition, other, check=False)
assert done.returncode != 0 and done.stdout == "", done.stderr
done = convert("nowhere", other, check=False)
assert done.returncode != 0 and "--definition" in " ".join(done.stderr.split()), done.stderr
print("Verified CSV converters")
