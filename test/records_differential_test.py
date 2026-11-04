#!/usr/bin/env python3
"""fin and the records schema agree on mutated files.

Every line of every example, and every member of the lines, is deleted and replaced
by values of the wrong kind. A file with a line the schema refuses must be refused by
fin. A file whose lines the schema accepts may only be refused for a reason the schema
does not check, which is the list in docs/records-format.md.
"""
import concurrent.futures
import copy
import json
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import threading

try:
    import jsonschema
except ImportError:
    print("records differential test skipped: python jsonschema is not installed")
    sys.exit(0)

FIN, ROOT = sys.argv[1], pathlib.Path(sys.argv[2])
VALIDATOR = jsonschema.Draft202012Validator(
    json.loads((ROOT / "fin-records.schema.json").read_text())
)
VALUES = [
    None, True, False, 0, 1, 1.5, "", " ", "x", "EUR", "eur", "ZZZ", "1", "-1",
    "1.50", "+1", "1e3", "1,5", ".5", "1.", "NaN", "2025-01-02", "2025-02-31",
    "2025-1-2", "2025-01-02T10:00:00Z", "2025-01-02T10:00:00+01:00",
    "2025-01-02T10:00:00", "2025-13-01T10:00:00Z", "2025-01-02T25:00:00Z",
    "UPPER", "a b", "a.b", "tab\there", "line\nbreak", "é", [], {}, [[]],
    ["a", "b"], ["a", "b", "c"], [""], {"unknown": 1},
]
SEMANTIC = re.compile(
    r"listed twice|unknown account|unknown statement|another account|repeated id"
    r"|repeated role|exactly one movement|ISO 4217|invalid (date|date or instant)"
    r"|expected a header|a header before|no end line|the end line counts|names no source"
    r"|stated differently|the file is empty|lists statement"
)


def paths(value, prefix=()):
    """Every container member of a value, as a path. The first element of a nested
    array stands for the others, though each can be replaced whole."""
    items = (
        value.items() if isinstance(value, dict)
        else enumerate(value) if isinstance(value, list)
        else ()
    )
    for key, child in items:
        yield prefix + (key,)
        if key == 0 or not isinstance(key, int):
            yield from paths(child, prefix + (key,))


def line_paths(lines):
    """Every line, and the members of the first line of each kind."""
    seen = set()
    for number, line in enumerate(lines):
        yield (number,)
        if line["kind"] not in seen:
            seen.add(line["kind"])
            yield from paths(line, (number,))


def at(document, path):
    for key in path[:-1]:
        document = document[key]
    return document


def mutations(document):
    for path in line_paths(document):
        deleted = copy.deepcopy(document)
        del at(deleted, path)[path[-1]]
        yield path, "deleted", deleted
        for value in VALUES:
            changed = copy.deepcopy(document)
            at(changed, path)[path[-1]] = value
            yield path, repr(value), changed


def compare(where, document, directory):
    """The disagreement over one document, if any, and how it was classified."""
    target = pathlib.Path(directory) / f"{threading.get_ident()}.jsonl"
    target.write_text("".join(json.dumps(line) + "\n" for line in document))
    run = subprocess.run(
        [FIN, "records", "check", str(target)], capture_output=True, text=True
    )
    schema = all(VALIDATOR.is_valid(line) for line in document)
    fin = run.returncode == 0
    # The command line wraps its error messages.
    message = " ".join(run.stderr.split())
    if run.returncode not in (0, 124):
        return "crashed", f"{where}: {message[:300]}"
    if fin and not schema:
        return "fin accepts, schema refuses", where
    if not fin and schema and not SEMANTIC.search(message):
        return "fin refuses, schema accepts", f"{where}: {message[:200]}"
    return ("accepted" if fin else "semantic" if schema else "refused"), None


def main():
    documents = {}
    for example in sorted((ROOT / "examples" / "valid").glob("*.jsonl")):
        lines = [json.loads(line) for line in example.read_text().splitlines()]
        for path, change, document in mutations(lines):
            where = f"{example.name} {'/'.join(map(str, path))} = {change}"
            documents.setdefault(json.dumps(document), (where, document))
    with tempfile.TemporaryDirectory() as directory:
        with concurrent.futures.ThreadPoolExecutor(os.cpu_count()) as pool:
            results = list(pool.map(
                lambda item: compare(item[0], item[1], directory), documents.values()
            ))
    counts = {}
    for kind, _ in results:
        counts[kind] = counts.get(kind, 0) + 1
    print(f"Compared {len(results)} documents: {counts}")
    disagreements = [f"{kind}: {detail}" for kind, detail in results if detail]
    for line in disagreements:
        print(line)
    # Both directions must have been exercised for agreement to mean anything.
    assert counts.get("accepted") and counts.get("refused") and counts.get("semantic")
    sys.exit(1 if disagreements else 0)


if __name__ == "__main__":
    main()
