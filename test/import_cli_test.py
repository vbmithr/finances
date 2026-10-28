"""Exercise import onboarding and account settings against a disposable database."""
import json
import os
from pathlib import Path
import pty
import select
import shlex
import sqlite3
import subprocess
import sys
import tempfile
import time
import unittest
import zipfile

FIN = str(Path(sys.argv.pop(1)).resolve())
FIXTURE = Path('fixtures/revolut.csv').read_text()
IBAN = 'BE68539007547034'


class ImportOnboarding(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix='fin-import-')
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
        return json.loads(self.run_fin('account', 'export').stdout)['accounts']

    def test_generic_provider_prompt_retries_invalid_values(self):
        path = self.root / 'provider.csv'
        path.write_text('Merchant,Date,USD\nShop,2026-10-01,11.00\n')
        output = self.terminal(['import', str(path), '--account-id', 'card'], [
            ('Provider name (required):', ''),
            ('Provider name (required):', 'bad name'),
            ('Provider name (required):', 'provider'),
            ('Use these columns?', '1'),
            ('Account type:', '9'),
            ('Choose one of the numbered options.', ''),
            ('Choose one of the numbered options.', '1'),
            ('Are purchases positive amounts in the file?', '1'),
            ('Skip a summary row with an empty date?', '2'),
            ('Import these transactions?', '1'),
        ])
        self.assertEqual(output.count('Provider name (required):'), 3)
        self.assertIn('A provider name is required.', output)
        self.assertEqual(output.count('Choose one of the numbered options.'), 2)
        self.assertNotIn('examplecard', output.lower())
        self.assertIn('inserted=1', output)
        self.assertEqual(self.accounts()[0]['institution'], 'provider')

    def test_generic_csv_setup_and_reimport(self):
        csv_path = self.root / 'card.csv'
        csv_path.write_text('Merchant,Category,Date,Local currency,Local amount,Exchange rate,USD\n'
                            'Shop,Food,2026-10-01,EUR,10.00,1.1,11.00\n'
                            'Total,,,,,,11.00\n')
        output = self.terminal(['import', str(csv_path), '--account-id', 'card-a', '--provider', 'card-provider'], [
            ('Use these columns?', '1'),
            ('Account type:', '1'),
            ('Are purchases positive amounts in the file?', '1'),
            ('Skip this summary row?', '1'),
            ('Import these transactions?', '1'),
        ])
        self.assertIn('Detected separator: comma.', output)
        self.assertIn('Suggested mapping:', output)
        self.assertIn('1. Prepaid card', output)
        self.assertIn('2. Credit card', output)
        self.assertIn('CSV amount: 11.00 USD; account movement: -11 USD', output)
        self.assertIn('CSV details: Category: Food; Local currency: EUR', output)
        self.assertIn('inserted=1', output)
        result = self.run_fin('import', csv_path, '--account-id', 'card-a', '--provider', 'card-provider')
        self.assertIn('inserted=0', result.stdout)
        status = json.loads(self.run_fin('status', '--json').stdout)
        self.assertEqual(status['movements'], 1)
        accounts = self.accounts()
        self.assertEqual([a['institution'] for a in accounts], ['card-provider'])
        self.assertEqual(accounts[0]['ledger_account'], 'assets:cards:card-provider:card-a')

    def test_generic_csv_numbered_separator_selection(self):
        path = self.root / 'ambiguous.csv'
        path.write_text('Merchant;Date;USD,Other\nShop;2026-10-01;11.00\n')
        output = self.terminal(['import', str(path), '--account-id', 'card', '--provider', 'provider'], [
            ('Separator:', '2'),
            ('Column containing the transaction date', '2'),
            ('Column containing the merchant or description:', '1'),
            ('Column containing the amount charged to the account:', '3'),
            ('Account currency (e.g. USD):', 'USD'),
            ('Account type:', '1'),
            ('Are purchases positive amounts in the file?', '1'),
            ('Skip a summary row with an empty date?', '2'),
            ('Import these transactions?', '1'),
        ])
        self.assertIn('1. Comma', output)
        self.assertIn('2. Semicolon', output)
        self.assertIn('inserted=1', output)

    def test_generic_csv_can_change_suggested_columns(self):
        path = self.root / 'manual-card.csv'
        path.write_text('Merchant,Date,USD,Other\nShop,2026-10-01,11.00,9.00\n')
        output = self.terminal(['import', str(path), '--account-id', 'manual', '--provider', 'manual-provider'], [
            ('Use these columns?', '2'),
            ('Column containing the transaction date', '2'),
            ('Column containing the merchant or description:', '1'),
            ('Column containing the amount charged to the account:', '4'),
            ('Account currency (e.g. USD):', 'USD'),
            ('Account type:', '1'),
            ('Are purchases positive amounts in the file?', '1'),
            ('Skip a summary row with an empty date?', '2'),
            ('Import these transactions?', '1'),
        ])
        self.assertIn('-9 USD', output)
        self.assertIn('inserted=1', output)

    def test_delete_one_account_and_reimport(self):
        a = self.root / 'delete-a.csv'
        b = self.root / 'delete-b.csv'
        a.write_text('Merchant,Date,USD\nDelete-only merchant,2026-10-01,11.00\n')
        b.write_text('Merchant,Date,USD\nKeep-this merchant,2026-10-01,12.00\n')
        self.terminal(['import', str(a), '--account-id', 'a', '--provider', 'provider'], [
            ('Use these columns?', '1'),
            ('Account type:', '1'),
            ('Are purchases positive amounts in the file?', '1'),
            ('Skip a summary row with an empty date?', '2'),
            ('Import these transactions?', '1'),
        ])
        self.run_fin('import', b, '--account-id', 'b', '--provider', 'provider')
        listed = json.loads(self.run_fin('account', 'list', '--json').stdout)
        account_id = next(x['id'] for x in listed if x['account_id'] == 'a')
        self.assertIn('ID: ' + account_id, self.run_fin('account', 'list').stdout)
        self.assertNotEqual(self.run_fin('account', 'delete', 'unknown', '--yes', check=False).returncode, 0)
        preview = self.run_fin('account', 'delete', account_id, '--dry-run')
        self.assertIn('Transactions: 1', preview.stdout)
        self.assertEqual(json.loads(self.run_fin('status', '--json').stdout)['movements'], 2)
        self.assertNotEqual(self.run_fin('account', 'delete', account_id, check=False).returncode, 0)
        result = self.run_fin('account', 'delete', account_id, '--yes')
        self.assertIn('Account and its records deleted.', result.stdout)
        self.assertEqual(json.loads(self.run_fin('status', '--json').stdout)['movements'], 1)
        self.assertEqual([x['account_id'] for x in self.accounts()], ['b'])
        journal = (self.root / 'data/fin/bank.journal').read_text()
        self.assertNotIn('Delete-only merchant', journal)
        self.assertIn('Keep-this merchant', journal)
        self.run_fin('import', a, '--account-id', 'a', '--provider', 'provider')
        self.assertEqual(json.loads(self.run_fin('status', '--json').stdout)['movements'], 2)
        listed_after = json.loads(self.run_fin('account', 'list', '--json').stdout)
        self.assertEqual(next(x['id'] for x in listed_after if x['account_id'] == 'a'), account_id)

    def test_account_delete_refuses_shared_document(self):
        ofx = self.root / 'shared.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text())
        self.run_fin('import', 'boursobank', ofx)
        accounts = self.accounts()
        self.assertGreater(len(accounts), 1)
        before = json.loads(self.run_fin('status', '--json').stdout)['movements']
        account = json.loads(self.run_fin('account', 'list', '--json').stdout)[0]
        result = self.run_fin('account', 'delete', account['id'], '--yes', check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('shared documents', result.stderr)
        self.assertEqual(json.loads(self.run_fin('status', '--json').stdout)['movements'], before)

    def test_transaction_list_filters_and_reviewed_rows(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        accounts = json.loads(self.run_fin('account', 'list', '--json').stdout)
        account_id = accounts[0]['id']
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        self.assertEqual(len(rows), 4)
        self.assertEqual([x['booked_on'] for x in rows],
                         ['2026-01-03', '2026-01-02', '2026-01-02', '2026-01-01'])
        self.assertTrue(all(x['account_id'] == account_id for x in rows))
        self.assertEqual(len({x['event_id'] for x in rows}), 4)
        selected = json.loads(self.run_fin('transaction', 'list', '--json', '--account', account_id,
                                           '--from', '2026-01-02', '--to', '2026-01-02',
                                           '--search', 'REPEATED').stdout)
        self.assertEqual(len(selected), 2)
        self.assertTrue(all(x['description'] == 'Repeated transfer' for x in selected))
        self.assertEqual(len(json.loads(self.run_fin('transaction', 'list', '--json', '--limit', '1').stdout)), 1)
        self.assertEqual(json.loads(self.run_fin('transaction', 'list', '--json', '--institution', 'other').stdout), [])
        text = self.run_fin('transaction', 'list', '--limit', '1').stdout
        self.assertIn('ACCOUNT ID', text)
        self.assertIn('INSTITUTION', text)
        self.assertIn('revolut', text)
        self.assertEqual(rows[0]['institution'], 'revolut')
        self.assertIn(account_id, text)
        for args in [('--account', 'unknown'), ('--from', '2026-02-30'),
                     ('--from', '2026-01-03', '--to', '2026-01-01'), ('--limit', '0')]:
            self.assertNotEqual(self.run_fin('transaction', 'list', *args, check=False).returncode, 0)
        decisions = self.root / 'transaction-category.json'
        decisions.write_text(json.dumps([{'event_id': rows[0]['event_id'],
                                          'account': 'expenses:shopping', 'reason': 'Reviewed'}]))
        self.run_fin('bank', 'categories', 'apply', decisions)
        reviewed = json.loads(self.run_fin('transaction', 'list', '--json', '--limit', '1').stdout)
        self.assertEqual(reviewed[0]['counter_account'], 'expenses:shopping')
        self.assertEqual(reviewed[0]['reason'], 'Reviewed')

    def test_welcome_and_init_show_transaction_list(self):
        self.assertIn('fin transaction list', self.run_fin().stdout)
        db = self.root / 'separate.sqlite3'
        output = self.run_fin('init', '--database', db).stdout
        line = next(line for line in output.splitlines() if 'fin transaction list' in line)
        self.assertEqual(shlex.split(line), ['fin', 'transaction', 'list', '--database', str(db)])

    def test_account_command_is_singular(self):
        help_text = self.run_fin('account', '--help=plain').stdout
        self.assertIn('delete', help_text)
        self.assertNotEqual(self.run_fin('accounts', '--help', check=False).returncode, 0)

    def test_scripts_require_label_and_never_prompt(self):
        help_text = self.run_fin('import', '--help=plain').stdout
        self.assertIn('BANK', help_text)
        self.assertIn('--account-id', help_text)
        self.assertNotEqual(self.run_fin('bank', 'sync', '--help', check=False).returncode, 0)
        result = self.run_fin('import', 'revolut', self.csv, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('--account-id LABEL', result.stderr)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.assertTrue(all(row['iban'] is None for row in self.accounts()))

    def test_label_and_iban_retry_once_for_two_currencies(self):
        output = self.terminal(['import', 'revolut', self.csv, self.usd], [
            ('Label for this account', 'bad label'),
            ('Label for this account', 'main'),
            ('IBAN (Enter to skip):', 'invalid'),
            ('IBAN (Enter to skip):', IBAN),
        ])
        self.assertEqual(output.count('New account:'), 1)
        self.assertIn('identity_changes=2', output)
        self.assertEqual(len(self.accounts()), 2)
        self.assertTrue(all(row['iban'] == IBAN for row in self.accounts()))
        history = json.loads(self.run_fin('account', 'history').stdout)
        self.assertEqual(len(history), 2)
        replay = self.terminal(['import', 'revolut', self.csv, self.usd,
                                '--account-id', 'main'], [])
        self.assertNotIn('IBAN (Enter to skip):', replay)

    def test_skip_is_not_repeated(self):
        self.terminal(['import', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', '')])
        replay = self.terminal(['import', 'revolut', self.csv,
                                '--account-id', 'main'], [])
        self.assertNotIn('IBAN (Enter to skip):', replay)
        self.assertIsNone(self.accounts()[0]['iban'])

    def test_snapshot_import_is_explicit_and_supports_dry_run(self):
        self.terminal(['import', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', '')])
        snapshot = self.root / 'saved-accounts.json'
        self.run_fin('account', 'export', '-o', snapshot)
        rows = json.loads(snapshot.read_text())
        rows['accounts'][0]['iban'] = IBAN
        snapshot.write_text(json.dumps(rows))
        self.run_fin('account', 'import', snapshot, '--dry-run')
        self.assertIsNone(self.accounts()[0]['iban'])
        self.run_fin('account', 'import', snapshot)
        self.assertEqual(self.accounts()[0]['iban'], IBAN)
        self.assertNotEqual(self.run_fin('account', 'import', check=False).returncode, 0)
        before = snapshot.read_bytes()
        self.terminal(['import', 'revolut', self.usd, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', IBAN)])
        self.assertEqual(snapshot.read_bytes(), before)
        self.assertFalse(self.config.exists())

    def test_restore_recovers_settings_reviews_and_evidence(self):
        self.terminal(['import', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', IBAN)])
        review = json.loads(self.run_fin('bank', 'review', '--json').stdout)
        decisions = self.root / 'decisions.json'
        decisions.write_text(json.dumps([{'event_id': review[0]['event_id'],
                                         'account': 'expenses:food', 'reason': 'Reviewed'}]))
        self.run_fin('bank', 'categories', 'apply', decisions)
        expected = {name: self.run_fin(*args).stdout for name, args in {
            'accounts': ['account', 'export'],
            'events': ['db', 'events'],
            'review': ['bank', 'review', '--all', '--json'],
            'history': ['bank', 'categories', 'history'],
            'journal': ['journal', 'export'],
        }.items()}
        self.run_fin('backup', self.root / 'backups')
        backup = next((self.root / 'backups').glob('fin-backup-*'))
        source = backup / 'fin.sqlite3'
        source_bytes = source.read_bytes()
        self.run_fin('delete', '--yes')
        self.run_fin('restore', backup)
        for name, args in {
            'accounts': ['account', 'export'], 'events': ['db', 'events'],
            'review': ['bank', 'review', '--all', '--json'],
            'history': ['bank', 'categories', 'history'], 'journal': ['journal', 'export'],
        }.items():
            self.assertEqual(self.run_fin(*args).stdout, expected[name], name)
        self.assertEqual(source.read_bytes(), source_bytes)
        target = self.root / 'alternative/nested/fin.sqlite3'
        self.run_fin('restore', source, '--database', target)
        self.assertEqual(target.stat().st_mode & 0o777, 0o600)
        self.assertEqual(target.parent.stat().st_mode & 0o777, 0o700)
        self.assertEqual(self.run_fin('account', 'export', '--database', target).stdout,
                         expected['accounts'])

    def test_restore_requires_confirmation_and_clears_stale_outputs(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('backup', self.root / 'backups')
        backup = next((self.root / 'backups').glob('fin-backup-*'))
        expected = self.accounts()
        changed = self.accounts()
        changed[0]['ledger_account'] = 'assets:bank:renamed'
        snapshot = self.root / 'settings.json'
        snapshot.write_text(json.dumps({'version': 1, 'accounts': changed}))
        self.run_fin('account', 'import', snapshot)
        self.env['VISUAL'] = 'false'
        self.run_fin('account', 'edit', check=False)
        draft = self.root / 'data/fin/fin.sqlite3.edit'
        journal = self.root / 'data/fin/bank.journal'
        self.assertTrue(draft.exists())
        self.assertTrue(journal.exists())
        result = self.run_fin('restore', backup, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('--yes', result.stderr)
        self.terminal(['restore', backup], [('Type yes to restore:', 'no')])
        self.assertEqual(self.accounts(), changed)
        self.assertTrue(draft.exists())
        self.assertTrue(journal.exists())
        self.terminal(['restore', backup], [('Type yes to restore:', 'yes')])
        self.assertEqual(self.accounts(), expected)
        self.assertFalse(draft.exists())
        self.assertFalse(journal.exists())
        self.assertTrue(snapshot.exists())
        self.run_fin('restore', backup, '--yes')

    def test_restore_rejects_invalid_or_same_source_without_changing_database(self):
        database = self.root / 'data/fin/fin.sqlite3'
        original = database.read_bytes()
        invalid = self.root / 'invalid.sqlite3'
        invalid.write_bytes(b'not SQLite')
        self.assertNotEqual(self.run_fin('restore', invalid, '--yes', check=False).returncode, 0)
        self.assertEqual(database.read_bytes(), original)
        invalid.unlink()
        with sqlite3.connect(invalid) as conn:
            conn.execute('PRAGMA user_version=12')
        source_bytes = invalid.read_bytes()
        self.assertNotEqual(self.run_fin('restore', invalid, '--yes', check=False).returncode, 0)
        self.assertEqual(database.read_bytes(), original)
        self.assertEqual(invalid.read_bytes(), source_bytes)
        target = self.root / 'absent/fin.sqlite3'
        self.assertNotEqual(self.run_fin('restore', invalid, '--database', target,
                                        check=False).returncode, 0)
        self.assertFalse(target.exists())
        self.assertNotEqual(self.run_fin('restore', database, '--yes', check=False).returncode, 0)
        self.assertEqual(database.read_bytes(), original)

    def test_legacy_config_is_not_used_or_changed(self):
        self.config.parent.mkdir(parents=True)
        self.config.write_text('Not a settings input')
        self.terminal(['import', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', IBAN)])
        self.assertEqual(self.config.read_text(), 'Not a settings input')
        self.assertEqual(self.accounts()[0]['iban'], IBAN)
        paths = json.loads(self.run_fin('paths').stdout)
        self.assertEqual(set(paths), {'data_dir', 'database'})
        self.assertNotEqual(self.run_fin('import', 'revolut', self.csv,
                                        '--account-id', 'main', '--accounts', self.config,
                                        check=False).returncode, 0)

    def test_editor_saves_only_to_database_and_recovers_draft(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        draft = self.root / 'data/fin/fin.sqlite3.edit'
        self.env['VISUAL'] = 'false'
        self.assertNotEqual(self.run_fin('account', 'edit', check=False).returncode, 0)
        self.assertTrue(draft.exists())
        edited = json.loads(draft.read_text())
        edited['accounts'][0]['iban'] = 'invalid'
        draft.write_text(json.dumps(edited))
        self.env['VISUAL'] = 'true'
        self.assertNotEqual(self.run_fin('account', 'edit', check=False).returncode, 0)
        self.assertIsNone(self.accounts()[0]['iban'])
        edited['accounts'][0]['iban'] = IBAN
        draft.write_text(json.dumps(edited))
        result = self.run_fin('account', 'edit')
        self.assertIn('Continuing the draft', result.stdout)
        self.assertEqual(self.accounts()[0]['iban'], IBAN)
        self.assertFalse(draft.exists())
        self.assertFalse(self.config.exists())
        self.run_fin('account', 'edit')
        self.assertFalse(draft.exists())

    def test_backup_and_delete_preserve_exported_snapshots(self):
        self.terminal(['import', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', IBAN)])
        snapshot = self.root / 'data/fin/accounts.json'
        self.run_fin('account', 'export', '-o', snapshot)
        original = snapshot.read_bytes()
        directory = self.root / 'new/nested/backups'
        self.run_fin('backup', directory)
        backup = next(directory.glob('fin-backup-*'))
        self.assertEqual(directory.stat().st_mode & 0o777, 0o700)
        self.assertEqual(backup.stat().st_mode & 0o777, 0o700)
        self.assertEqual([p.name for p in backup.iterdir()], ['fin.sqlite3'])
        restored = json.loads(self.run_fin('account', 'export', '--database',
                                         backup / 'fin.sqlite3').stdout)
        self.assertEqual(restored['accounts'][0]['iban'], IBAN)
        # Only a pending draft is disposable app state; a user-exported snapshot
        # beside the database remains the user's file.
        self.env['VISUAL'] = 'false'
        self.run_fin('account', 'edit', check=False)
        self.run_fin('delete', '--yes')
        self.assertEqual(snapshot.read_bytes(), original)
        self.assertFalse((self.root / 'data/fin/fin.sqlite3.edit').exists())
        self.assertFalse(self.config.exists())

    def test_checking_prompts_but_associated_card_does_not(self):
        ofx = self.root / 'checking-and-card.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text()
                       .replace('<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>'))
        output = self.terminal(['import', 'boursobank', ofx],
                               [('IBAN (Enter to skip):', IBAN)])
        self.assertEqual(output.count('New account:'), 1)
        accounts = self.accounts()
        self.assertEqual(sum(row['iban'] == IBAN for row in accounts), 1)
        self.assertFalse(self.config.exists())

    def test_zip_cli_outcomes_and_history(self):
        archive = self.root / 'export.zip'
        args = ['import', 'revolut', archive, '--account-id', 'main']
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
        output = self.terminal(['import', 'boursobank', csv], [])
        self.assertNotIn('IBAN (Enter to skip):', output)


if __name__ == '__main__':
    unittest.main()
