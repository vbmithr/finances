"""Exercise sync onboarding and account settings against a disposable database."""
import json
import os
from pathlib import Path
import pty
import select
import subprocess
import sys
import tempfile
import time
import unittest
import zipfile

FIN = str(Path(sys.argv.pop(1)).resolve())
FIXTURE = Path('fixtures/revolut.csv').read_text()
IBAN = 'BE68539007547034'


class SyncOnboarding(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix='fin-sync-')
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.env = dict(os.environ, XDG_DATA_HOME=str(self.root / 'data'),
                        XDG_CONFIG_HOME=str(self.root / 'config'))
        self.run_fin('init')
        self.csv = self.root / 'eur.csv'
        self.csv.write_text(FIXTURE)
        self.usd = self.root / 'usd.csv'
        self.usd.write_text(FIXTURE.replace(',EUR,', ',USD,'))
        self.config = self.root / 'config/fin/accounts.json'

    def run_fin(self, *args, check=True):
        result = subprocess.run([FIN, *map(str, args)], env=self.env,
                                stdin=subprocess.DEVNULL, capture_output=True, text=True)
        if check:
            self.assertEqual(result.returncode, 0, result.stderr)
        return result

    def terminal(self, args, answers):
        master, slave = pty.openpty()
        child = subprocess.Popen([FIN, *map(str, args)], env=self.env,
                                 stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        transcript = b''
        window = b''
        pending = list(answers)
        deadline = time.monotonic() + 20
        try:
            while time.monotonic() < deadline:
                if not select.select([master], [], [], 0.1)[0]:
                    continue
                try:
                    chunk = os.read(master, 65536)
                except OSError:
                    break
                if not chunk:
                    break
                transcript += chunk
                window += chunk
                if pending and pending[0][0].encode() in window:
                    _, answer = pending.pop(0)
                    os.write(master, (answer + '\n').encode())
                    window = b''
            child.wait(timeout=max(0.1, deadline - time.monotonic()))
            self.assertFalse(pending, transcript.decode(errors='replace'))
            self.assertEqual(child.returncode, 0, transcript.decode(errors='replace'))
            return transcript.decode(errors='replace')
        finally:
            if child.poll() is None:
                child.kill()
                child.wait()
            os.close(master)

    def accounts(self):
        return json.loads(self.run_fin('accounts', 'export').stdout)['accounts']

    def test_scripts_require_label_and_never_prompt(self):
        result = self.run_fin('bank', 'sync', 'revolut', self.csv, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('--account-id LABEL', result.stderr)
        self.run_fin('bank', 'sync', 'revolut', self.csv, '--account-id', 'main')
        self.assertTrue(all(row['iban'] is None for row in self.accounts()))

    def test_label_and_iban_retry_once_for_two_currencies(self):
        output = self.terminal(['bank', 'sync', 'revolut', self.csv, self.usd], [
            ('Label for this account', 'bad label'),
            ('Label for this account', 'main'),
            ('IBAN (Enter to skip):', 'invalid'),
            ('IBAN (Enter to skip):', IBAN),
        ])
        self.assertEqual(output.count('New account:'), 1)
        self.assertIn('identity_changes=2', output)
        self.assertEqual(len(self.accounts()), 2)
        self.assertTrue(all(row['iban'] == IBAN for row in self.accounts()))
        history = json.loads(self.run_fin('accounts', 'history').stdout)
        self.assertEqual(len(history), 2)
        replay = self.terminal(['bank', 'sync', 'revolut', self.csv, self.usd,
                                '--account-id', 'main'], [])
        self.assertNotIn('IBAN (Enter to skip):', replay)

    def test_skip_is_not_repeated(self):
        self.terminal(['bank', 'sync', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', '')])
        replay = self.terminal(['bank', 'sync', 'revolut', self.csv,
                                '--account-id', 'main'], [])
        self.assertNotIn('IBAN (Enter to skip):', replay)
        self.assertIsNone(self.accounts()[0]['iban'])

    def test_snapshot_import_is_explicit_and_supports_dry_run(self):
        self.terminal(['bank', 'sync', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', '')])
        snapshot = self.root / 'saved-accounts.json'
        self.run_fin('accounts', 'export', '-o', snapshot)
        rows = json.loads(snapshot.read_text())
        rows['accounts'][0]['iban'] = IBAN
        snapshot.write_text(json.dumps(rows))
        self.run_fin('accounts', 'import', snapshot, '--dry-run')
        self.assertIsNone(self.accounts()[0]['iban'])
        self.run_fin('accounts', 'import', snapshot)
        self.assertEqual(self.accounts()[0]['iban'], IBAN)
        self.assertNotEqual(self.run_fin('accounts', 'import', check=False).returncode, 0)
        before = snapshot.read_bytes()
        self.terminal(['bank', 'sync', 'revolut', self.usd, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', IBAN)])
        self.assertEqual(snapshot.read_bytes(), before)
        self.assertFalse(self.config.exists())

    def test_legacy_config_is_not_used_or_changed(self):
        self.config.parent.mkdir(parents=True)
        self.config.write_text('Not a settings input')
        self.terminal(['bank', 'sync', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', IBAN)])
        self.assertEqual(self.config.read_text(), 'Not a settings input')
        self.assertEqual(self.accounts()[0]['iban'], IBAN)
        paths = json.loads(self.run_fin('paths').stdout)
        self.assertEqual(set(paths), {'data_dir', 'database'})
        self.assertNotEqual(self.run_fin('bank', 'sync', 'revolut', self.csv,
                                        '--account-id', 'main', '--accounts', self.config,
                                        check=False).returncode, 0)

    def test_editor_saves_only_to_database_and_recovers_draft(self):
        self.run_fin('bank', 'sync', 'revolut', self.csv, '--account-id', 'main')
        draft = self.root / 'data/fin/fin.sqlite3.edit'
        self.env['VISUAL'] = 'false'
        self.assertNotEqual(self.run_fin('accounts', 'edit', check=False).returncode, 0)
        self.assertTrue(draft.exists())
        edited = json.loads(draft.read_text())
        edited['accounts'][0]['iban'] = 'invalid'
        draft.write_text(json.dumps(edited))
        self.env['VISUAL'] = 'true'
        self.assertNotEqual(self.run_fin('accounts', 'edit', check=False).returncode, 0)
        self.assertIsNone(self.accounts()[0]['iban'])
        edited['accounts'][0]['iban'] = IBAN
        draft.write_text(json.dumps(edited))
        result = self.run_fin('accounts', 'edit')
        self.assertIn('Continuing the draft', result.stdout)
        self.assertEqual(self.accounts()[0]['iban'], IBAN)
        self.assertFalse(draft.exists())
        self.assertFalse(self.config.exists())
        self.run_fin('accounts', 'edit')
        self.assertFalse(draft.exists())

    def test_backup_and_delete_preserve_exported_snapshots(self):
        self.terminal(['bank', 'sync', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', IBAN)])
        snapshot = self.root / 'data/fin/accounts.json'
        self.run_fin('accounts', 'export', '-o', snapshot)
        original = snapshot.read_bytes()
        directory = self.root / 'new/nested/backups'
        self.run_fin('backup', directory)
        backup = next(directory.glob('fin-backup-*'))
        self.assertEqual(directory.stat().st_mode & 0o777, 0o700)
        self.assertEqual(backup.stat().st_mode & 0o777, 0o700)
        self.assertEqual([p.name for p in backup.iterdir()], ['fin.sqlite3'])
        restored = json.loads(self.run_fin('accounts', 'export', '--database',
                                         backup / 'fin.sqlite3').stdout)
        self.assertEqual(restored['accounts'][0]['iban'], IBAN)
        # Only a pending draft is disposable app state; a user-exported snapshot
        # beside the database remains the user's file.
        self.env['VISUAL'] = 'false'
        self.run_fin('accounts', 'edit', check=False)
        self.run_fin('delete', '--yes')
        self.assertEqual(snapshot.read_bytes(), original)
        self.assertFalse((self.root / 'data/fin/fin.sqlite3.edit').exists())
        self.assertFalse(self.config.exists())

    def test_checking_prompts_but_associated_card_does_not(self):
        ofx = self.root / 'checking-and-card.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text()
                       .replace('<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>'))
        output = self.terminal(['bank', 'sync', 'boursobank', ofx],
                               [('IBAN (Enter to skip):', IBAN)])
        self.assertEqual(output.count('New account:'), 1)
        accounts = self.accounts()
        self.assertEqual(sum(row['iban'] == IBAN for row in accounts), 1)
        self.assertFalse(self.config.exists())

    def test_zip_cli_outcomes_and_history(self):
        archive = self.root / 'export.zip'
        args = ['bank', 'sync', 'revolut', archive, '--account-id', 'main']
        with zipfile.ZipFile(archive, 'w', compression=zipfile.ZIP_DEFLATED) as z:
            z.writestr('statement.csv', FIXTURE)
            z.writestr('README.txt', 'An unrelated file')
        self.run_fin(*args)
        history = json.loads(self.run_fin('bank', 'history', '--json').stdout)
        self.assertTrue(any(row['path'] == str(archive) + '/statement.csv' for row in history))
        self.assertIn('imported=0', self.run_fin(*args).stdout)
        # Unknown members must fail when the explicitly named archive has no
        # recognizable bank records, but do not fail a mixed archive above.
        with zipfile.ZipFile(archive, 'w') as z:
            z.writestr('README.txt', 'An unrelated file')
        self.assertNotEqual(self.run_fin(*args, check=False).returncode, 0)
        archive.write_bytes(b'not a ZIP')
        self.assertNotEqual(self.run_fin(*args, check=False).returncode, 0)

    def test_card_never_prompts(self):
        csv = self.root / 'card.ofx'
        csv.write_text(Path('fixtures/bank.ofx').read_text()
                       .replace('<ACCTTYPE>CHECKING</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>')
                       .replace('<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>'))
        output = self.terminal(['bank', 'sync', 'boursobank', csv], [])
        self.assertNotIn('IBAN (Enter to skip):', output)


if __name__ == '__main__':
    unittest.main()
