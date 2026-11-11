#!/usr/bin/env python3
"""An institution imported through records, from statement to journal."""
import os
import pathlib
import json
import subprocess
import sqlite3
import sys
import tempfile

FIN, LEGACY_REVOLUT = sys.argv[1:3]

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

    # An FDM export is one card, named by its file.
    statement = root / "1118.csv"
    statement.write_text("""Type,Merchant,Category,Date,Local currency,Local amount,Exchange rate,USD,Balance after,Note
Purchase,Shop,Shopping,2025-01-02,EUR,10.00,1.1,11.00,,
Purchase,Shop,Shopping,2025-01-02,EUR,10.00,1.1,11.00,,
Refund,Refund,Shopping,2025-01-03,EUR,-1.00,1.1,-1.10,,
Total spent,,,,,,,20.90,,
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
    # Account deletion must remove statement totals and their event links, then
    # restore immutability so the same evidence can be imported again.
    account = json.loads(run("account", "list", "--json", "-d", database).stdout)[0]
    run("account", "delete", account["id"], "--yes", "-d", database)
    with sqlite3.connect(database) as db:
        assert db.execute("SELECT count(*) FROM statement_totals").fetchone()[0] == 0
        assert db.execute("SELECT count(*) FROM statement_total_events").fetchone()[0] == 0
        assert db.execute("PRAGMA foreign_key_check").fetchall() == []
        for table in ("statement_totals", "statement_total_events"):
            assert db.execute("SELECT count(*) FROM sqlite_schema WHERE type='trigger' AND name=?",
                              (table + "_no_delete",)).fetchone()[0] == 1
    run("import", "fdm", statement, "-d", database, "--journal", journal)
    assert json.loads(run("statement", "totals", "--institution", "fdm", "-d", database).stdout)[0]["reconciled"] is True
    text = journal.read_text()
    postings = [line for line in text.splitlines()
                if line.startswith(" ") and "assets:cards:fdm:1118" in line]
    assert len(postings) == 3, text
    assert "USD" in text, text
    listed = run("operation", "list", "--institution", "fdm", "-d", database).stdout
    assert "Refund" in listed, listed
    shown = run("institution", "show", "fdm", "-d", database).stdout
    assert "Kind: Records" in shown, shown
    assert "fdm" in run("status", "-d", database).stdout

    # Current FDM exports report purchases with negative USD amounts. Their
    # balance-after column may not form a complete consecutive balance chain.
    signed = root / "signed.csv"
    signed.write_text("""Type,Merchant,Category,Date,Local currency,Local amount,Exchange rate,USD,Balance after,Note
Purchase,Shop,Shopping,2025-01-03,,,,"-5.00",50.00,
Adjustment,Correction,Other,2025-01-02,,,,"2.00",70.00,
Deposit,Top up,Other,2025-01-01,,,,"10.00",30.00,
Total spent,,,,,,,5.00,,
""")
    signed_records = root / "signed.jsonl"
    run("convert", "fdm", signed, "-o", signed_records)
    signed_rows = [json.loads(line) for line in signed_records.read_text().splitlines()]
    movements = [row for row in signed_rows if row["kind"] == "record"]
    assert [r["movements"][0]["quantity"] for r in movements] == ["-5", "2", "10"]
    assert all("balance_after" not in r["details"] for r in movements)
    assert all(any(k == "Balance after" and v for k, v in r["details"]["fields"]) for r in movements)
    signed_db = root / "signed.sqlite3"
    run("init", "-d", signed_db)
    run("import", "fdm", signed, "-d", signed_db, "--journal", root / "signed.journal")
    assert len(json.loads(run("operation", "list", "--institution", "fdm", "--json", "-d", signed_db).stdout)) == 3

    # A source discrepancy remains visible instead of rejecting its evidence.
    mismatch = root / "mismatch.csv"
    mismatch.write_text(statement.read_text().replace("Total spent,,,,,,,20.90", "Total spent,,,,,,,20.91"))
    mismatch_db = root / "mismatch.sqlite3"
    run("init", "-d", mismatch_db)
    run("import", "fdm", mismatch, "-d", mismatch_db, "--journal", root / "mismatch.journal")
    mismatch_total = json.loads(run("statement", "totals", "-d", mismatch_db).stdout)[0]
    assert mismatch_total["difference"] == "0.01" and mismatch_total["reconciled"] is False, mismatch_total

    # Cards in one folder each get their own account, from their file names.
    cards = root / "cards"
    cards.mkdir()
    for name in ("1118", "2233"):
        # Each card's file differs: identical files are one statement.
        (cards / f"{name}.csv").write_text(statement.read_text().replace("Shop", f"Shop {name}"))
    cards_db, cards_journal = root / "cards.sqlite3", root / "cards.journal"
    run("init", "-d", cards_db)
    batch = run("import", "fdm", cards, "-d", cards_db, "--journal", cards_journal)
    assert "imported=2" in batch.stdout, batch.stdout
    cards_text = cards_journal.read_text()
    for name in ("1118", "2233"):
        assert len([line for line in cards_text.splitlines()
                    if line.startswith(" ") and f"assets:cards:fdm:{name}" in line]) == 3, cards_text
    assert "assets:cards:fdm:card" not in cards_text, cards_text
    # An explicit ID overrides the file name.
    named_db, named_journal = root / "named.sqlite3", root / "named.journal"
    run("init", "-d", named_db)
    run("import", "fdm", statement, "--account-id", "9999", "-d", named_db, "--journal", named_journal)
    assert "assets:cards:fdm:9999" in named_journal.read_text()

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
    replay = run("import", "revolut", rev, "-d", rev_db, "--journal", rev_journal)
    assert "inserted=0 existing=3" in replay.stdout, replay.stdout
    reverse_db, reverse_journal = root / "revolut-reverse.sqlite3", root / "revolut-reverse.journal"
    run("init", "-d", reverse_db)
    run("import", "revolut", rev, "-d", reverse_db, "--journal", reverse_journal)
    replay = run("import", "revolut", old_rev, "-d", reverse_db, "--journal", reverse_journal)
    assert "inserted=0 existing=3" in replay.stdout, replay.stdout

    # A changed stored operation proposal still makes a complete replay fail.
    damaged_db, damaged_journal = root / "revolut-damaged.sqlite3", root / "revolut-damaged.journal"
    run("init", "-d", damaged_db)
    run("import", "revolut", rev, "-d", damaged_db, "--journal", damaged_journal)
    with sqlite3.connect(damaged_db) as db:
        db.execute("UPDATE operations SET occurred='2020-01-01' WHERE source_event_id IS NOT NULL")
    rejected = run("import", "revolut", old_rev, "-d", damaged_db, "--journal", damaged_journal, check=False)
    assert "stored operation" in rejected.stdout + rejected.stderr, (rejected.stdout, rejected.stderr)

    # A claimed source hash cannot make a partial or altered records file a replay.
    old_lines = [json.loads(line) for line in old_rev.read_text().splitlines()]
    partial = root / "revolut-partial.jsonl"
    dropped = False
    subset = []
    for row in old_lines:
        row = dict(row)
        if row["kind"] == "record" and not dropped:
            dropped = True
            continue
        if row["kind"] == "end":
            row["records"] -= 1
        subset.append(row)
    partial.write_text("\n".join(map(json.dumps, subset)) + "\n")
    rejected = run("import", "revolut", partial, "-d", reverse_db, "--journal", reverse_journal, check=False)
    assert "overlap is ambiguous" in rejected.stdout + rejected.stderr, (rejected.stdout, rejected.stderr)
    altered = root / "revolut-altered.jsonl"
    changed_rows = [json.loads(line) for line in old_rev.read_text().splitlines()]
    reverted = next(row for row in changed_rows if row["kind"] == "record" and not row["movements"])
    reverted["details"]["reported"]["quantity"] = "-6"
    altered.write_text("\n".join(map(json.dumps, changed_rows)) + "\n")
    rejected = run("import", "revolut", altered, "-d", reverse_db, "--journal", reverse_journal, check=False)
    assert "overlap is ambiguous" in rejected.stdout + rejected.stderr, (rejected.stdout, rejected.stderr)
    wrong_source = root / "revolut-wrong-source.jsonl"
    changed_rows = [json.loads(line) for line in old_rev.read_text().splitlines()]
    next(row for row in changed_rows if row["kind"] == "source")["sha256"] = "0" * 64
    wrong_source.write_text("\n".join(map(json.dumps, changed_rows)) + "\n")
    rejected = run("import", "revolut", wrong_source, "-d", reverse_db, "--journal", reverse_journal, check=False)
    assert "overlap is ambiguous" in rejected.stdout + rejected.stderr, (rejected.stdout, rejected.stderr)
    # A shortened raw CSV is also not a replay of the archived records file.
    partial_csv = root / "revolut-partial.csv"
    rev_lines = rev.read_text().splitlines()
    partial_csv.write_text("\n".join([rev_lines[0], *rev_lines[2:]]) + "\n")
    rejected = run("import", "revolut", partial_csv, "-d", rev_db, "--journal", rev_journal, check=False)
    assert "overlap is ambiguous" in rejected.stdout + rejected.stderr, (rejected.stdout, rejected.stderr)

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

    # Source-hash identities also work when the complete source is converted.
    cdc = root / "cdc.csv"
    cdc.write_text("""Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash
2025-06-09 17:56:16,Reward,CRO,1,,,EUR,0.10,0.12,crypto_deposit,
""")
    cdc_records = root / "cdc.jsonl"
    run("convert", "cdc", cdc, "-o", cdc_records)
    cdc_db, cdc_journal = root / "cdc.sqlite3", root / "cdc.journal"
    run("init", "-d", cdc_db)
    run("import", "cdc", cdc, "-d", cdc_db, "--journal", cdc_journal)
    replay = run("import", "cdc", cdc_records, "-d", cdc_db, "--journal", cdc_journal)
    assert "inserted=0 existing=1" in replay.stdout, replay.stdout
    cdc_damaged, cdc_damaged_journal = root / "cdc-damaged.sqlite3", root / "cdc-damaged.journal"
    run("init", "-d", cdc_damaged)
    run("import", "cdc", cdc, "-d", cdc_damaged, "--journal", cdc_damaged_journal)
    with sqlite3.connect(cdc_damaged) as db:
        db.execute("UPDATE operation_movements SET quantity='999'")
    rejected = run("import", "cdc", cdc_records, "-d", cdc_damaged, "--journal", cdc_damaged_journal, check=False)
    assert "stored operation" in rejected.stdout + rejected.stderr, (rejected.stdout, rejected.stderr)

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
