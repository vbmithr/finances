#!/usr/bin/env python3
"""The records schema accepts and refuses what the format says, line by line.

semantic/ holds files whose lines are all well formed and whose meaning is not: the
schema accepts every line, and they are the reader's job.
"""
import json
import pathlib
import sys
import unittest

try:
    import jsonschema
except ImportError:
    print("records schema test skipped: python jsonschema is not installed")
    sys.exit(0)

ROOT = pathlib.Path(sys.argv.pop(1))
SCHEMA = json.loads((ROOT / "fin-records.schema.json").read_text())
VALIDATOR = jsonschema.Draft202012Validator(SCHEMA)


def files(kind):
    found = sorted((ROOT / "examples" / kind).glob("*.jsonl"))
    assert found, kind
    return found


def lines(path):
    return [json.loads(line) for line in path.read_text().splitlines()]


class RecordsSchema(unittest.TestCase):
    def test_schema_is_valid(self):
        jsonschema.Draft202012Validator.check_schema(SCHEMA)

    def test_accepted(self):
        for kind in ("valid", "semantic"):
            for path in files(kind):
                with self.subTest(path.name):
                    errors = [e.message for l in lines(path) for e in VALIDATOR.iter_errors(l)]
                    self.assertEqual(errors, [])

    def test_refused(self):
        for path in files("invalid"):
            with self.subTest(path.name):
                self.assertFalse(all(VALIDATOR.is_valid(l) for l in lines(path)))


if __name__ == "__main__":
    unittest.main()
