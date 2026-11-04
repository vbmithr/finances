#!/usr/bin/env python3
"""What fin convert prints is accepted by the schema and by fin records check."""
import json
import pathlib
import subprocess
import sys
import tempfile
import zipfile

FIN, ROOT, FIXTURES = sys.argv[1], pathlib.Path(sys.argv[2]), pathlib.Path(sys.argv[3])
try:
    import jsonschema

    SCHEMA = json.loads((ROOT / "fin-records.schema.json").read_text())
except ImportError:
    print("records convert test: python jsonschema is not installed, schema not checked")
    SCHEMA = None


def run(*args):
    return subprocess.run([FIN, *args], capture_output=True, text=True, check=True).stdout


def validate(text):
    lines = [json.loads(line) for line in text.splitlines()]
    if SCHEMA:
        for line in lines:
            jsonschema.Draft202012Validator(SCHEMA).validate(line)
    return lines


def of_kind(lines, kind):
    return [line for line in lines if line["kind"] == kind]


camt, csv = str(FIXTURES / "wise.xml"), str(FIXTURES / "wise.csv")
for path, observation in ((camt, False), (csv, True)):
    lines = validate(run("convert", "wise", path))
    records = of_kind(lines, "record")
    assert records, path
    assert all(r.get("observation", False) == observation for r in records), path
    assert lines[0]["kind"] == "header" and lines[-1] == {"kind": "end", "records": len(records)}
    # The source's own name, without the directories it was read from.
    assert of_kind(lines, "source")[0]["name"] == pathlib.Path(path).name

# Several files make one section, in order, and an overlap is kept once.
merged = validate(run("convert", "wise", camt, csv, camt))
assert [s["name"] for s in of_kind(merged, "source")] == ["wise.xml", "wise.csv"]
assert len(of_kind(merged, "header")) == 1
assert len(of_kind(merged, "record")) == 6
assert sum(r.get("observation", False) for r in of_kind(merged, "record")) == 3

with tempfile.TemporaryDirectory() as directory:
    plain = pathlib.Path(directory) / "wise.jsonl"
    packed = pathlib.Path(directory) / "wise.jsonl.zst"
    summary = "institution=wise accounts=2 statements=1 records=6\n"
    assert run("convert", "wise", camt, csv, "-o", str(plain)) == summary
    assert run("convert", "wise", camt, csv, "--output", str(packed)) == summary
    assert plain.read_bytes()[:1] == b"{"
    assert packed.read_bytes()[:4] == b"\x28\xb5\x2f\xfd"
    assert len(packed.read_bytes()) < len(plain.read_bytes())
    assert run("records", "check", str(plain)) == summary
    assert run("records", "check", str(packed)) == summary
    # Concatenated files read as one document for the institution.
    both = pathlib.Path(directory) / "both.jsonl"
    both.write_bytes(plain.read_bytes() * 2)
    assert run("records", "check", str(both)) == summary

# Every statement in an archive is converted; the rest is skipped and reported.
header = pathlib.Path(csv).read_text().splitlines()[0] + "\n"
with tempfile.TemporaryDirectory() as directory:
    def archive(name, members):
        path = pathlib.Path(directory) / name
        with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as zip_file:
            for member, contents in members.items():
                zip_file.writestr(member, contents)
        return str(path)

    def refused(path, reason):
        run_ = subprocess.run([FIN, "convert", "wise", path], capture_output=True, text=True)
        assert run_.returncode != 0 and reason in " ".join(run_.stderr.split()), run_.stderr
        assert run_.stdout == "", run_.stdout

    statements = {
        "wise.xml": pathlib.Path(camt).read_text(),
        "wise.csv": pathlib.Path(csv).read_text(),
        "empty.csv": header,
        "statement.mt940": ":20:SYNTHETIC\n",
        ".hidden.csv": "not read",
    }
    packed = archive("statements.zip", statements)
    done = subprocess.run(
        [FIN, "convert", "wise", packed], capture_output=True, text=True, check=True
    )
    lines = validate(done.stdout)
    assert len(of_kind(lines, "record")) == 6
    assert [s["name"] for s in of_kind(lines, "source")] == [
        "statements.zip/empty.csv", "statements.zip/wise.csv", "statements.zip/wise.xml",
    ], of_kind(lines, "source")
    assert "statement.mt940: not a recognized statement" in done.stderr, done.stderr
    assert ".hidden" not in done.stderr, done.stderr
    # An archive and the file it holds state the same rows once.
    both = validate(run("convert", "wise", packed, camt))
    assert len(of_kind(both, "record")) == 6

    # Wise exports one archive per format, so one with no statement is not an error
    # as long as another input has one.
    other = archive("other.zip", {"notes.txt": "synthetic"})
    none = archive("none.zip", {})
    done = subprocess.run(
        [FIN, "convert", "wise", other, none, packed],
        capture_output=True, text=True, check=True,
    )
    assert len(of_kind(validate(done.stdout), "record")) == 6
    assert "notes.txt: not a recognized statement" in done.stderr, done.stderr
    assert "none.zip: empty archive" in done.stderr, done.stderr
    refused(other, "no recognized statement in what was given")
    refused(none, "no recognized statement in what was given")
    refused(archive("nested.zip", {"inner.zip": "synthetic"}), "nested ZIP")
    corrupt = pathlib.Path(directory) / "corrupt.zip"
    corrupt.write_bytes(b"PK not an archive")
    refused(str(corrupt), "corrupt.zip")

# A directory is walked like an import: statements are converted, the rest reported.
with tempfile.TemporaryDirectory() as directory:
    root = pathlib.Path(directory)
    (root / "sub" / "deeper").mkdir(parents=True)
    (root / "empty-directory").mkdir()
    (root / "b.csv").write_text(pathlib.Path(csv).read_text())
    (root / "sub" / "deeper" / "a.xml").write_text(pathlib.Path(camt).read_text())
    (root / "notes.txt").write_text("synthetic")
    (root / ".hidden.csv").write_text("not read")
    (root / "partial.csv.part").write_text("not read")
    (root / "link.csv").symlink_to(root / "b.csv")
    with open(root / "huge.bin", "wb") as huge:
        huge.truncate(64 * 1024 * 1024 + 1)
    with zipfile.ZipFile(root / "s.zip", "w") as zip_file:
        zip_file.writestr("empty.csv", header)
    done = subprocess.run(
        [FIN, "convert", "wise", str(root)], capture_output=True, text=True, check=True
    )
    lines = validate(done.stdout)
    assert len(of_kind(lines, "record")) == 6
    assert [s["name"] for s in of_kind(lines, "source")] == ["b.csv", "s.zip/empty.csv", "a.xml"], (
        of_kind(lines, "source"))
    assert "notes.txt: not a recognized statement" in done.stderr, done.stderr
    assert "link.csv: not a regular file or directory" in done.stderr, done.stderr
    assert "huge.bin: larger than the read limit" in done.stderr, done.stderr
    assert "hidden" not in done.stderr and "partial" not in done.stderr, done.stderr
    # A statement that fails to parse is not skipped, and nothing is printed.
    (root / "bad.xml").write_text("<a>camt.053</a>")
    failed = subprocess.run([FIN, "convert", "wise", str(root)], capture_output=True, text=True)
    assert failed.returncode != 0 and "bad.xml" in failed.stderr and failed.stdout == "", failed

    nothing = root / "empty-directory"
    failed = subprocess.run([FIN, "convert", "wise", str(nothing)], capture_output=True, text=True)
    assert failed.returncode != 0 and "no recognized statement" in failed.stderr, failed.stderr

missing = subprocess.run([FIN, "convert", "nowhere", csv], capture_output=True, text=True)
assert missing.returncode != 0 and "no converter for nowhere" in missing.stderr, missing.stderr
print("Verified fin convert")
