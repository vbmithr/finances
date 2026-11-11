#!/usr/bin/env python3
"""An institution imported through records, from statement to journal."""
import os
import pathlib
import json
import subprocess
import sys
import tempfile

FIN, LEGACY_DEFINITION, LEGACY_REVOLUT = sys.argv[1:4]

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
        totals = json.loads(run("statement", "totals", "--institution", "fdm", "-d", database).stdout)
        assert len(totals) == 1, totals
        total = totals[0]
        assert total["stated"] == "-20.9", total
        assert total["movements"] == "-20.9", total
        assert total["entries"] == 3 and total["difference"] == "0", total
        assert total["reconciled"] is True, total
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

    # A source discrepancy remains visible instead of rejecting its evidence.
    mismatch = root / "mismatch.csv"
    mismatch.write_text(statement.read_text().replace("Total,,,,,,20.90", "Total,,,,,,20.91"))
    mismatch_db = root / "mismatch.sqlite3"
    run("init", "-d", mismatch_db)
    run("import", "fdm", mismatch, "-d", mismatch_db, "--journal", root / "mismatch.journal")
    mismatch_total = json.loads(run("statement", "totals", "-d", mismatch_db).stdout)[0]
    assert mismatch_total["difference"] == "0.01" and mismatch_total["reconciled"] is False, mismatch_total

    # Evidence imported through the old JSON converter matches the new Lua CSV.
    legacy = root / "legacy.jsonl"
    run("convert", "--definition", LEGACY_DEFINITION, statement, "-o", legacy)
    migrated, migrated_journal = imported("legacy", legacy)
    replay = run("import", "fdm", statement, "-d", migrated, "--journal", migrated_journal)
    assert "inserted=0 existing=3" in replay.stdout, replay.stdout

    # A records file gives the same journal, plain or compressed.
    for name in ("records.jsonl", "records.jsonl.zst"):
        records = root / name
        run("convert", "fdm", statement, "-o", records)
        _, other = imported(name, records)
        assert other.read_text() == text, name

    # A different stated commodity cannot be compared to the USD rows.
    converted = records.parent.joinpath("records.jsonl").read_text().splitlines()
    other_commodity = root / "other-commodity.jsonl"
    out = []
    for line in converted:
        row = json.loads(line)
        if row["kind"] == "statement":
            row["total"]["commodity"]["code"] = "EUR"
        out.append(json.dumps(row))
    other_commodity.write_text("\n".join(out) + "\n")
    other_db = root / "other-commodity.sqlite3"
    run("init", "-d", other_db)
    run("import", "fdm", other_commodity, "-d", other_db, "--journal", root / "other.journal")
    comparison = json.loads(run("statement", "totals", "-d", other_db).stdout)[0]
    assert comparison["movements"] is None and comparison["reconciled"] is None, comparison

    # An observation is not a counted event; do not claim a partial sum reconciles.
    observed = root / "observed.jsonl"
    out = []
    marked = False
    for line in converted:
        row = json.loads(line)
        if row["kind"] == "record" and not marked:
            row["observation"] = True
            marked = True
        out.append(json.dumps(row))
    observed.write_text("\n".join(out) + "\n")
    observed_db = root / "observed.sqlite3"
    run("init", "-d", observed_db)
    run("import", "fdm", observed, "-d", observed_db, "--journal", root / "observed.journal")
    comparison = json.loads(run("statement", "totals", "-d", observed_db).stdout)[0]
    assert comparison["entries"] == 2 and comparison["reconciled"] is None, comparison

    # Existing Revolut records produced by the old definition reimport under Lua.
    rev = root / "revolut.csv"
    rev.write_text("""Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance
Deposit,Current,2026-01-01 09:00:00,2026-01-01 09:00:01,In,150.00,0.00,EUR,COMPLETED,150.00
Card Payment,Current,2026-01-02 09:00:00,2026-01-03 23:00:00,Shop,-10.00,1.00,EUR,COMPLETED,139.00
Card Payment,Current,2026-01-04 09:00:00,,Undone,-5.00,0.00,EUR,REVERTED,
""")
    old_rev = root / "old-revolut.jsonl"
    run("convert", "--definition", LEGACY_REVOLUT, rev, "-o", old_rev)
    rev_db, rev_journal = root / "revolut.sqlite3", root / "revolut.journal"
    run("init", "-d", rev_db)
    run("import", "revolut", old_rev, "-d", rev_db, "--journal", rev_journal)
    replay = run("import", "revolut", rev, "-d", rev_db, "--journal", rev_journal, check=False)
    assert "overlap is ambiguous" in replay.stdout + replay.stderr, (replay.stdout, replay.stderr)
    # Completed-only rows are bank movements and can be reimported across converters.
    completed = root / "revolut-completed.csv"
    completed.write_text("\n".join(rev.read_text().splitlines()[:-1]) + "\n")
    old_completed = root / "old-revolut-completed.jsonl"
    run("convert", "--definition", LEGACY_REVOLUT, completed, "-o", old_completed)
    completed_db, completed_journal = root / "revolut-completed.sqlite3", root / "revolut-completed.journal"
    run("init", "-d", completed_db)
    run("import", "revolut", old_completed, "-d", completed_db, "--journal", completed_journal)
    replay = run("import", "revolut", completed, "-d", completed_db, "--journal", completed_journal)
    assert "inserted=0 existing=2" in replay.stdout, replay.stdout

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

    # A later export may state details the stored record lacks, and the record is unchanged;
    # changing a detail it states, or its fixed part, is refused.
    plain = records.parent.joinpath("records.jsonl").read_text().splitlines()
    def rewritten(name, change):
        out = []
        for line in plain:
            row = json.loads(line)
            if row["kind"] == "record":
                change(row)
            out.append(json.dumps(row))
        path = root / name
        path.write_text("\n".join(out) + "\n")
        return path
    database = root / "enriched.sqlite3"
    run("init", "-d", database)
    run("import", "fdm", records.parent / "records.jsonl", "-d", database, "--journal", root / "e.journal")
    referenced = rewritten("referenced.jsonl", lambda row: row.setdefault("details", {}).update(
        references={"hash": "added-later"}))
    done = run("import", "fdm", referenced, "-d", database, "--journal", root / "e.journal")
    assert "rejected=0" in done.stdout and "inserted=0 existing=3" in done.stdout, done.stdout
    def other_description(row):
        row["details"]["descriptions"] = ["Changed"]
    changed = rewritten("described.jsonl", other_description)
    done = run("import", "fdm", changed, "-d", database, "--journal", root / "e.journal", check=False)
    assert "rejected=1" in done.stdout, done.stdout
    def other_quantity(row):
        row["movements"][0]["quantity"] = "-123456.78"
    changed = rewritten("changed.jsonl", other_quantity)
    done = run("import", "fdm", changed, "-d", database, "--journal", root / "e.journal", check=False)
    assert "rejected=1" in done.stdout, done.stdout

print("Verified the records import")
