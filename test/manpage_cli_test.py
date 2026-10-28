"""Verify the package man-page list against the CLI and render every page."""
import re
import runpy
import subprocess
import sys

executable, build_script = sys.argv[1:]
groups = runpy.run_path(build_script)['MANPAGE_COMMANDS']
help_text = subprocess.run([executable, '--help=plain'], check=True, capture_output=True, text=True).stdout
section = help_text.split('COMMANDS\n', 1)[1].split('COMMON OPTIONS\n', 1)[0]
commands = set(re.findall(r'^       ([a-z]+)(?=\s)', section, flags=re.MULTILINE))
assert set(groups) == commands | {None}, (set(groups), commands)
for group in groups:
    args = [executable] + ([group] if group else []) + ['--help=groff']
    page = subprocess.run(args, check=True, capture_output=True, text=True).stdout
    assert '.TH ' in page, f'no man page for {group or "fin"}'
print(f'Verified {len(groups)} package man pages')
