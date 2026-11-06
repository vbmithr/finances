#!/usr/bin/env python3
"""An institution imported through records, from statement to journal."""
import os
import pathlib
import subprocess
import sys
import tempfile

FIN = sys.argv[1]

with tempfile.TemporaryDirectory() as directory:
    root = pathlib.Path(directory)
    env = dict(os.environ, HOME=str(root), XDG_DATA_HOME=str(root / "data"),
               XDG_CONFIG_HOME=str(root / "config"), XDG_STATE_HOME=str(root / "state"),
               XDG_CACHE_HOME=str(root / "cache"))

    def run(*args, check=True):
        done = subprocess.run([FIN, *map(str, args)], env=env, capture_output=True, text=True,
                              stdin=subprocess.DEVNULL)
        assert not check or done.returncode == 0, (args, done.stderr)
        return done

    statement = root / "statement.csv"
    statement.write_text("""Merchant,Category,Date,Local currency,Local amount,Exchange rate,USD
Shop,Shopping,2025-01-02,EUR,10.00,1.1,11.00
Shop,Shopping,2025-01-02,EUR,10.00,1.1,11.00
Refund,Shopping,2025-01-03,EUR,-1.00,1.1,-1.10
Total,,,,,,20.90
""")

    def imported(name, source):
        database, journal = root / f"{name}.sqlite3", root / f"{name}.journal"
        run("init", "-d", database)
        first = run("import", "fdm", source, "-d", database, "--journal", journal)
        assert "imported=1" in first.stdout and "inserted=3" in first.stdout, first.stdout
        again = run("import", "fdm", source, "-d", database, "--journal", journal)
        assert "unchanged=1" in again.stdout and "inserted=0" in again.stdout, again.stdout
        return database, journal

    # From the CSV, identical rows included.
    database, journal = imported("csv", statement)
    text = journal.read_text()
    postings = [line for line in text.splitlines()
                if line.startswith(" ") and "assets:cards:fdm:card" in line]
    assert len(postings) == 3, text
    assert "USD" in text, text
    listed = run("operation", "list", "--institution", "fdm", "-d", database).stdout
    assert "Refund" in listed, listed
    shown = run("institution", "show", "fdm", "-d", database).stdout
    assert "Kind: Records" in shown, shown
    assert "fdm" in run("status", "-d", database).stdout

    # A records file gives the same journal, plain or compressed.
    for name in ("records.jsonl", "records.jsonl.zst"):
        records = root / name
        run("convert", "fdm", statement, "-o", records)
        _, other = imported(name, records)
        assert other.read_text() == text, name

    # A records file of another institution is not this one's, and a broken one says why.
    other = root / "other.jsonl"
    other.write_text(records.parent.joinpath("records.jsonl").read_text().replace(
        '"institution":"fdm"', '"institution":"another"'))
    database = root / "refused.sqlite3"
    run("init", "-d", database)
    done = run("import", "fdm", other, "-d", database, "--journal", root / "r.journal", check=False)
    assert done.returncode != 0 or "imported=0" in done.stdout, done.stdout
    cut = root / "cut.jsonl"
    cut.write_text("".join(
        records.parent.joinpath("records.jsonl").read_text().splitlines(keepends=True)[:-1]))
    done = run("import", "fdm", cut, "-d", database, "--journal", root / "r.journal", check=False)
    assert "imported=0" in done.stdout or done.returncode != 0, done.stdout
    assert "end line" in done.stdout + done.stderr, done.stdout + done.stderr

print("Verified the records import")
