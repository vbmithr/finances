#!/usr/bin/env python3
"""A statement with only its header is a valid statement of no activity, for every institution."""
import json
import os
import pathlib
import subprocess
import sys
import tempfile
import tomllib

FIN, INSTITUTIONS = sys.argv[1], pathlib.Path(sys.argv[2])


def profiles(value):
    """Every CSV layout a definition declares, once for each header it accepts."""
    if isinstance(value, dict):
        if isinstance(value.get("header"), list) and value["header"]:
            yield value
        for header in value.get("headers", []) if isinstance(value.get("headers"), list) else []:
            yield {**value, "header": header}
        for child in value.values():
            yield from profiles(child)


with tempfile.TemporaryDirectory() as directory:
    root = pathlib.Path(directory)
    env = dict(os.environ, HOME=str(root), XDG_DATA_HOME=str(root / "data"),
               XDG_CONFIG_HOME=str(root / "config"), XDG_STATE_HOME=str(root / "state"),
               XDG_CACHE_HOME=str(root / "cache"))
    checked = 0
    for definition in sorted(INSTITUTIONS.glob("*.toml")):
        if definition.stem == "companion_example":
            continue
        name = definition.stem
        # A statement format's settings can name the CSV's header too.
        toml = tomllib.loads(definition.read_text())
        layouts = [json.loads((INSTITUTIONS / s["definition"]).read_text())
                   for s in toml["statements"] if s["format"] == "csv" and "definition" in s]
        headers = {tuple(p["header"]): p for layout in layouts for p in profiles(layout)}
        for profile in headers.values():
            statement = root / f"{name}.csv"
            statement.write_text(profile.get("separator", ",").join(profile["header"]) + "\n")
            database = root / f"{name}.sqlite3"
            subprocess.run([FIN, "init", "-d", str(database)], env=env, check=True,
                           capture_output=True)
            command = [FIN, "import", name, str(statement), "-d", str(database),
                       "--journal", str(root / f"{name}.journal"), "--account-id", "x"]
            done = subprocess.run(command, env=env, capture_output=True, text=True,
                                  stdin=subprocess.DEVNULL)
            if done.returncode != 0 and "account" in done.stderr:
                done = subprocess.run(command[:-2], env=env, capture_output=True, text=True,
                                      stdin=subprocess.DEVNULL)
            assert done.returncode == 0, (name, done.stderr)
            assert "inserted=0" in done.stdout and "imported=1" in done.stdout, (name, done.stdout)
            again = subprocess.run(done.args, env=env, capture_output=True, text=True,
                                   stdin=subprocess.DEVNULL, check=True)
            assert "unchanged=1" in again.stdout, (name, again.stdout)
            checked += 1
    assert checked >= 5, checked
print(f"Verified {checked} header-only statements")
