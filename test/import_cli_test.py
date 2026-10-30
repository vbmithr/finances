"""Exercise import onboarding and account settings against a disposable database."""
import json
import os
import fcntl
from pathlib import Path
import pty
import select
import signal
import shlex
import sqlite3
import subprocess
import struct
import sys
import tempfile
import termios
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

    def fullscreen(self, args, answers, size=(30, 120)):
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', *size, 0, 0))
        env = dict(self.env, TERM='xterm-256color')
        child = subprocess.Popen([FIN, *map(str, args)], env=env,
                                 stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        transcript = b''
        pending = list(answers)
        window = b''
        deadline = time.monotonic() + 20
        try:
            while time.monotonic() < deadline:
                if not select.select([master], [], [], 0.1)[0]:
                    if child.poll() is not None:
                        break
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
                    os.write(master, answer.encode())
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

    def test_generic_bank_csv_localized_formats(self):
        csv_path = self.root / 'localized.csv'
        csv_path.write_text('Date;Description;EUR\n03/04/2026;Rent;-123,45\n14/04/2026;Salary;950,50\n')
        output = self.terminal(['import', str(csv_path), '--account-id', 'main', '--provider', 'localbank'], [
            ('Use these columns?', '1'),
            ('Date format:', '2'),
            ('Amount decimal mark:', '2'),
            ('Account type:', '3'),
            ('Are outgoing amounts positive in the file?', '2'),
            ('Skip a summary row with an empty date?', '2'),
            ('Import these transactions?', '1'),
            ('IBAN (Enter to skip):', ''),
        ])
        self.assertIn('account movement: -123.45 EUR', output)
        rows = json.loads(self.run_fin('tx', 'list', '--json').stdout)
        rent = next(row for row in rows if row['description'] == 'Rent')
        self.assertEqual((rent['booked_on'], rent['amount']), ('2026-04-03', '-123.45'))
        self.assertIn('inserted=0', self.run_fin('import', csv_path, '--account-id', 'main', '--provider', 'localbank').stdout)

    def test_generic_bank_csv_setup_and_reimport(self):
        csv_path = self.root / 'checking.csv'
        csv_path.write_text('Date,Description,EUR\n2026-10-01,Rent,-120.00\n2026-10-02,Salary,950.00\n')
        output = self.terminal(['import', str(csv_path), '--account-id', 'main', '--provider', 'mybank'], [
            ('Use these columns?', '1'),
            ('Account type:', '3'),
            ('Are outgoing amounts positive in the file?', '2'),
            ('Skip a summary row with an empty date?', '2'),
            ('Import these transactions?', '1'),
            ('IBAN (Enter to skip):', ''),
        ])
        self.assertIn('3. Current account', output)
        self.assertIn('account movement: -120 EUR', output)
        self.assertIn('inserted=2', output)
        rows = json.loads(self.run_fin('tx', 'list', '--json').stdout)
        self.assertEqual({(r['description'], r['amount']) for r in rows}, {('Rent', '-120'), ('Salary', '950')})
        account = self.accounts()[0]
        self.assertEqual(account['ledger_account'], 'assets:bank:mybank:current:main')
        self.assertEqual(account['account_type'], 'current')
        self.assertIn('inserted=0', self.run_fin('import', csv_path, '--account-id', 'main', '--provider', 'mybank').stdout)
        self.assertEqual(len(json.loads(self.run_fin('tx', 'list', '--json').stdout)), 2)

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

    def test_interactive_classification_review(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('transaction', 'list', '--json', '--search', 'Repeated').stdout)
        first, second = (row['event_id'] for row in rows)
        output = self.terminal(['transaction', 'category', 'review', str(first)], [
            ('Category name or number', 'expenses:food'),
            ('Select matches', 'all'),
            ('[Y/n]:', 'yes'),
        ])
        self.assertIn('category_changes=2', output)
        rows = json.loads(self.run_fin('transaction', 'list', '--json', '--search', 'Repeated').stdout)
        self.assertEqual({x['counter_account'] for x in rows}, {'expenses:food'})
        self.assertEqual(len(json.loads(self.run_fin('transaction', 'category', 'history').stdout)), 2)
        self.assertNotEqual(self.run_fin('transaction', 'category', 'review', first, check=False).returncode, 0)
        journal = self.run_fin('journal', 'export').stdout
        self.assertIn('expenses:food', journal)

    def test_interactive_classification_cancel_and_select_none(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('transaction', 'list', '--json', '--search', 'Repeated').stdout)
        first, second = (row['event_id'] for row in rows)
        before = self.run_fin('transaction', 'review', '--all', '--json').stdout
        output = self.terminal(['transaction', 'category', 'review', str(first)], [
            ('Category name or number', 'expenses:food'),
            ('Select matches', 'none'),
            ('[Y/n]:', 'no'),
        ])
        self.assertIn('Nothing classified.', output)
        self.assertEqual(self.run_fin('transaction', 'review', '--all', '--json').stdout, before)
        output = self.terminal(['transaction', 'category', 'review', str(first)], [
            ('Category name or number', 'expenses:food'),
            ('Select matches', 'none'),
            ('[Y/n]:', 'yes'),
        ])
        self.assertIn('category_changes=1', output)
        rows = json.loads(self.run_fin('transaction', 'list', '--json', '--search', 'Repeated').stdout)
        self.assertEqual({r['event_id']: r['counter_account'] for r in rows},
                         {first: 'expenses:food', second: 'equity:unclassified'})

    def test_category_similar_is_read_only_and_reports_conflicts(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('transaction', 'list', '--json', '--search', 'Repeated').stdout)
        self.assertEqual(len(rows), 2)
        first, second = (row['event_id'] for row in rows)
        self.assertNotEqual(self.run_fin('transaction', 'category', 'similar', first, check=False).returncode, 0)
        decisions = self.root / 'decisions.json'
        decisions.write_text(json.dumps([{'event_id': first, 'account': 'expenses:food', 'reason': 'Reviewed'}]))
        self.run_fin('transaction', 'category', 'apply', decisions)
        before = self.run_fin('transaction', 'review', '--all', '--json').stdout
        preview = json.loads(self.run_fin('transaction', 'category', 'similar', first, '--json').stdout)
        self.assertEqual(preview['example']['account'], 'expenses:food')
        self.assertEqual([r['event_id'] for r in preview['candidates']], [second])
        self.assertEqual(preview['conflicts'], [])
        text = self.run_fin('transaction', 'category', 'similar', first).stdout
        self.assertIn('Matching unclassified transactions', text)
        self.assertIn('expenses:food', text)
        self.assertIn(str(second), text)
        self.assertEqual(self.run_fin('transaction', 'review', '--all', '--json').stdout, before)
        decisions.write_text(json.dumps([{'event_id': second, 'account': 'expenses:other', 'reason': 'Reviewed'}]))
        self.run_fin('transaction', 'category', 'apply', decisions)
        preview = json.loads(self.run_fin('transaction', 'category', 'similar', first, '--json').stdout)
        self.assertEqual(preview['candidates'], [])
        self.assertEqual([r['event_id'] for r in preview['conflicts']], [second])
        self.assertNotEqual(self.run_fin('transaction', 'category', 'similar', '999999', check=False).returncode, 0)

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
        self.run_fin('transaction', 'category', 'apply', decisions)
        reviewed = json.loads(self.run_fin('transaction', 'list', '--json', '--limit', '1').stdout)
        self.assertEqual(reviewed[0]['counter_account'], 'expenses:shopping')
        self.assertEqual(reviewed[0]['reason'], 'Reviewed')
        selected = json.loads(self.run_fin('tx', 'list', '--classification', 'category', '--json').stdout)
        self.assertEqual([r['event_id'] for r in selected], [rows[0]['event_id']])
        self.assertEqual(len(json.loads(self.run_fin('tx', 'list', '--classification', 'unclassified', '--json').stdout)), 3)
        self.assertNotEqual(self.run_fin('tx', 'list', '--classification', 'invalid', check=False).returncode, 0)

    def test_manual_movement_types_and_history(self):
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        rows = json.loads(self.run_fin('tx', 'list', '--json').stdout)
        event = rows[0]['event_id']
        decisions = self.root / 'type-classification.json'
        decisions.write_text(json.dumps([{'event_id': event, 'account': 'expenses:shopping', 'reason': 'Reviewed'}]))
        self.run_fin('transaction', 'category', 'apply', decisions)
        before = next(r for r in json.loads(self.run_fin('tx', 'list', '--json').stdout) if r['event_id'] == event)
        journal = self.run_fin('journal', 'export').stdout
        listing = self.run_fin('tx', 'type', 'list').stdout
        self.assertIn('bonus', listing)
        self.assertIn('fee', listing)
        self.assertNotEqual(self.run_fin('tx', 'type', 'set', event, 'unknown', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('tx', 'type', 'set', event, 'made-up', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('tx', 'type', 'set', '9999999', 'bonus', check=False).returncode, 0)
        self.run_fin('tx', 'type', 'set', event, 'bonus')
        self.run_fin('tx', 'type', 'set', event, 'bonus')
        row = next(r for r in json.loads(self.run_fin('tx', 'list', '--json').stdout) if r['event_id'] == event)
        self.assertEqual(row['movement_type'], 'bonus')
        self.assertEqual(row['imported_movement_type'], before['movement_type'])
        self.assertEqual(row['movement_type_override'], 'bonus')
        self.assertEqual(row['classification'], before['classification'])
        self.assertEqual(self.run_fin('journal', 'export').stdout, journal)
        self.assertIn('manual override', self.run_fin('tx', 'type', 'show', event).stdout)
        self.assertEqual([r['event_id'] for r in json.loads(self.run_fin('tx', 'list', '--type', 'bonus', '--json').stdout)], [event])
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        with sqlite3.connect(self.root / 'data/fin/fin.sqlite3') as db:
            trigger = db.execute("SELECT sql FROM sqlite_schema WHERE name='movements_no_update'").fetchone()[0]
            db.execute('DROP TRIGGER movements_no_update')
            db.execute("UPDATE movements SET movement_type='fee' WHERE event_id=?", (event,))
            db.execute(trigger)
        self.assertIn('movement_types_changed=1', self.run_fin('tx', 'refresh-types', '--yes').stdout)
        row = next(r for r in json.loads(self.run_fin('tx', 'list', '--json').stdout) if r['event_id'] == event)
        self.assertEqual(row['movement_type_override'], 'bonus')
        self.run_fin('tx', 'type', 'set', event, 'fee')
        self.run_fin('tx', 'type', 'reset', event)
        self.run_fin('tx', 'type', 'reset', event)
        row = next(r for r in json.loads(self.run_fin('tx', 'list', '--json').stdout) if r['event_id'] == event)
        self.assertEqual(row['movement_type'], before['movement_type'])
        self.assertIsNone(row['movement_type_override'])
        history = json.loads(self.run_fin('tx', 'type', 'history', '--json').stdout)
        self.assertEqual([(r['previous_type'], r['movement_type']) for r in history],
                         [(None, 'bonus'), ('bonus', 'fee'), ('fee', None)])
        self.assertTrue(all(r['event_id'] == event for r in history))

    def test_type_history_is_removed_with_account(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        row = json.loads(self.run_fin('tx', 'list', '--json').stdout)[0]
        self.run_fin('tx', 'type', 'set', row['event_id'], 'bonus')
        self.assertEqual(len(json.loads(self.run_fin('tx', 'type', 'history', '--json').stdout)), 1)
        self.run_fin('account', 'delete', row['account_id'], '--yes')
        self.assertEqual(json.loads(self.run_fin('tx', 'type', 'history', '--json').stdout), [])

    def test_type_list_includes_observed_provider_types(self):
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        rows = json.loads(self.run_fin('tx', 'list', '--json').stdout)
        source, target = rows[0]['event_id'], rows[1]['event_id']
        with sqlite3.connect(self.root / 'data/fin/fin.sqlite3') as db:
            trigger = db.execute("SELECT sql FROM sqlite_schema WHERE name='movements_no_update'").fetchone()[0]
            db.execute('DROP TRIGGER movements_no_update')
            db.execute("UPDATE movements SET movement_type='example:reported' WHERE event_id=?", (source,))
            db.execute(trigger)
        self.assertIn('example:reported', self.run_fin('tx', 'type', 'list').stdout)
        self.run_fin('tx', 'type', 'set', target, 'example:reported')
        matches = json.loads(self.run_fin('tx', 'list', '--type', 'example:reported', '--json').stdout)
        self.assertEqual({r['event_id'] for r in matches}, {source, target})

    def test_automatic_typing_rules(self):
        rules = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        self.assertTrue(any(r['institution'] == 'boursobank' and r['mode'] == 'automatic' for r in rules))
        self.run_fin('rule', 'add', 'my-card', '^CAFÉ & CO$', '--institution', 'boursobank', '--set-type', 'card', '--automatic', '--priority=-1')
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        rows = json.loads(self.run_fin('tx', 'list', '--json').stdout)
        row = next(r for r in rows if r['description'] == 'CAFÉ & CO')
        self.assertEqual(row['movement_type'], 'card')
        self.run_fin('tx', 'type', 'set', row['event_id'], 'fee')
        self.run_fin('rule', 'delete', 'my-card', '--yes')
        self.run_fin('tx', 'refresh-types', '--institution', 'boursobank', '--yes')
        row = next(r for r in json.loads(self.run_fin('tx', 'list', '--json').stdout) if r['event_id'] == row['event_id'])
        self.assertEqual(row['movement_type'], 'fee')
        self.assertIsNone(row['imported_movement_type'])

    def test_automatic_rule_categories_and_modes(self):
        self.run_fin('rule', 'add', 'shop', '^CAFÉ & CO$', '--institution', 'boursobank', '--match-type', 'unknown', '--set-type', 'card', '--category', 'expenses:shopping', '--automatic')
        self.run_fin('rule', 'mode', 'shop', 'disabled')
        rules = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        self.assertEqual(next(r for r in rules if r['name'] == 'shop')['mode'], 'disabled')
        self.run_fin('rule', 'mode', 'shop', 'automatic')
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        row = next(r for r in json.loads(self.run_fin('tx', 'list', '--json').stdout) if r['description'] == 'CAFÉ & CO')
        self.assertEqual(row['movement_type'], 'card')
        self.assertEqual(row['counter_account'], 'expenses:shopping')
        self.assertIn('Automatic rule shop', self.run_fin('tx', 'category', 'history').stdout)
        preview = json.loads(self.run_fin('rule', 'preview', 'shop', '--json').stdout)
        self.assertIn(row['event_id'], [m['transaction']['event_id'] for m in preview])
        self.run_fin('rule', 'add', 'shop', '^CAFÉ & CO$', '--institution', 'boursobank', '--set-type', 'fee', '--automatic', '--replace')
        self.run_fin('rule', 'mode', 'shop', 'disabled')
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        row2 = next(r for r in json.loads(self.run_fin('tx', 'list', '--json').stdout) if r['event_id'] == row['event_id'])
        self.assertEqual(row2['counter_account'], 'expenses:shopping')
        self.assertEqual(row2['movement_type'], 'card')

    def test_user_institution_imports_generic_ofx(self):
        self.run_fin('institution', 'add', 'provider')
        result = self.run_fin('import', 'provider', 'fixtures/bank.ofx')
        self.assertIn('imported=1', result.stdout)
        rows = json.loads(self.run_fin('tx', 'list', '--json').stdout)
        self.assertTrue(rows)
        self.assertTrue(all(row['institution'] == 'provider' for row in rows))
        self.assertTrue(json.loads(self.run_fin('account', 'export').stdout)['accounts'])
        self.run_fin('status')
        self.run_fin('tx', 'refresh-types', '--institution', 'provider', '--yes')
        self.assertIn('unchanged=1', self.run_fin('import', 'provider', 'fixtures/bank.ofx').stdout)
        directory = self.root / 'generic-ofx'
        directory.mkdir()
        (directory / 'again.ofx').write_bytes(Path('fixtures/bank.ofx').read_bytes())
        self.assertIn('unchanged=1', self.run_fin('import', 'provider', directory).stdout)
        self.assertEqual(len(json.loads(self.run_fin('tx', 'list', '--json').stdout)), len(rows))
        mixed = self.run_fin('import', 'provider', 'fixtures/bank.ofx', self.csv, check=False)
        self.assertNotEqual(mixed.returncode, 0)
        self.assertIn('separate movement sources', mixed.stderr)
        sequential = self.run_fin('import', 'provider', self.csv, check=False)
        self.assertNotEqual(sequential.returncode, 0)
        self.assertIn('another generic format', sequential.stderr)

    def test_generic_ofx_csv_companion_profile(self):
        self.run_fin('institution', 'add', 'provider')
        template = json.loads(self.run_fin('institution', 'companion', 'show', 'boursobank').stdout)
        template['currency'] = 'EUR'
        path = self.root / 'companion.json'
        path.write_text(json.dumps(template, ensure_ascii=False))
        self.run_fin('institution', 'companion', 'set', 'provider', path, '--yes')
        self.assertEqual(json.loads(self.run_fin('institution', 'companion', 'show', 'provider').stdout), template)
        csv_result = self.run_fin('import', 'provider', 'fixtures/bank.csv')
        self.assertIn('inserted=0', csv_result.stdout)
        self.assertEqual(json.loads(self.run_fin('tx', 'list', '--json').stdout), [])
        self.run_fin('import', 'provider', 'fixtures/bank.ofx')
        rows = json.loads(self.run_fin('tx', 'list', '--json').stdout)
        self.assertEqual(len(rows), 3)
        observations = json.loads(self.run_fin('statement', 'companions', '--institution', 'provider').stdout)
        self.assertEqual(len(observations), 3)
        self.assertTrue(all(row['state'] == 'matched' for row in observations))
        clear = self.run_fin('institution', 'companion', 'clear', 'provider', '--yes', check=False)
        self.assertNotEqual(clear.returncode, 0)
        self.assertIn('while companion rows exist', clear.stderr)
        self.assertIn('matched=3', self.run_fin('statement', 'match', '--institution', 'provider').stdout)
        self.assertIn('unchanged=2', self.run_fin('import', 'provider', 'fixtures/bank.csv', 'fixtures/bank.ofx').stdout)
        template['category'] = None
        path.write_text(json.dumps(template, ensure_ascii=False))
        self.run_fin('institution', 'companion', 'set', 'provider', path, '--yes')
        self.assertIn('matched=3', self.run_fin('statement', 'match', '--institution', 'provider').stdout)
        observations2 = json.loads(self.run_fin('statement', 'companions', '--institution', 'provider').stdout)
        self.assertEqual(observations, observations2)
        refused = self.run_fin('import', 'provider', 'fixtures/bank.csv', check=False)
        self.assertNotEqual(refused.returncode, 0)
        self.assertIn('different context', refused.stdout)
        self.assertNotEqual(self.run_fin('import', self.csv, '--provider', 'provider', '--account-id', 'main', check=False).returncode, 0)
        template.update({
            'name': 'Another layout', 'header': ['Date', 'Account', 'Text', 'Amount', 'Category'],
            'separator': ';', 'account_id': 1, 'booked_on': 0, 'amount': 3, 'description': 2,
            'value_on': None, 'suggested_description': None, 'category': 4,
            'parent_category': None, 'comment': None, 'account_label': None,
            'reported_balance': None, 'pointage': None, 'date_format': 'day-first',
            'amount_format': 'comma', 'balance_format': 'dot',
        })
        path.write_text(json.dumps(template, ensure_ascii=False))
        self.run_fin('institution', 'companion', 'set', 'provider', path, '--yes')
        newer = self.root / 'another-layout.csv'
        newer.write_text('Date;Account;Text;Amount;Category\n03/01/2026;CHECKING;CAFÉ & CO;-0,10;Food, drinks\n')
        self.run_fin('import', 'provider', newer)
        self.assertIn('matched=4', self.run_fin('statement', 'match', '--institution', 'provider').stdout)
        self.assertEqual(len(json.loads(self.run_fin('statement', 'companions', '--institution', 'provider').stdout)), 4)
        self.assertEqual(len(json.loads(self.run_fin('tx', 'list', '--json').stdout)), 3)
        primary = self.root / 'primary.csv'
        primary.write_text('Date,Description,EUR\n2026-02-01,Shop,5.00\n')
        self.terminal(['import', str(primary), '--account-id', 'card', '--provider', 'other'], [
            ('Use these columns?', '1'), ('Account type:', '1'),
            ('Are purchases positive amounts in the file?', '1'),
            ('Skip a summary row with an empty date?', '2'), ('Import these transactions?', '1')])
        self.assertEqual(len(json.loads(self.run_fin('tx', 'list', '--json').stdout)), 4)
        self.assertIn('matched=4', self.run_fin('statement', 'match', '--institution', 'provider').stdout)

    def test_companion_currency_prevents_wrong_match(self):
        self.run_fin('institution', 'add', 'provider')
        template = json.loads(self.run_fin('institution', 'companion', 'show', 'boursobank').stdout)
        path = self.root / 'companion.json'
        invalid = dict(template, account_id=999)
        path.write_text(json.dumps(invalid, ensure_ascii=False))
        invalid_result = self.run_fin('institution', 'companion', 'set', 'provider', path, '--yes', check=False)
        self.assertNotEqual(invalid_result.returncode, 0)
        self.assertIn('outside the header', invalid_result.stderr)
        path.write_text(json.dumps(template, ensure_ascii=False))
        missing = self.run_fin('institution', 'companion', 'set', 'provider', path, '--yes', check=False)
        self.assertNotEqual(missing.returncode, 0)
        self.assertIn('must specify its ISO currency', missing.stderr)
        template['currency'] = 'USD'
        path.write_text(json.dumps(template, ensure_ascii=False))
        self.run_fin('institution', 'companion', 'set', 'provider', path, '--yes')
        self.run_fin('import', 'provider', 'fixtures/bank.ofx', 'fixtures/bank.csv')
        observations = json.loads(self.run_fin('statement', 'companions', '--institution', 'provider').stdout)
        self.assertEqual(len(observations), 3)
        self.assertTrue(all(row['state'] == 'unmatched' for row in observations))

    def test_unused_companion_institution_can_be_deleted(self):
        self.run_fin('institution', 'add', 'tempbank')
        template = json.loads(self.run_fin('institution', 'companion', 'show', 'boursobank').stdout)
        template['currency'] = 'EUR'
        path = self.root / 'companion.json'
        path.write_text(json.dumps(template, ensure_ascii=False))
        self.run_fin('institution', 'companion', 'set', 'tempbank', path, '--yes')
        self.run_fin('institution', 'companion', 'clear', 'tempbank', '--yes')
        self.assertNotEqual(self.run_fin('institution', 'companion', 'show', 'tempbank', check=False).returncode, 0)
        self.run_fin('institution', 'companion', 'set', 'tempbank', path, '--yes')
        self.run_fin('institution', 'delete', 'tempbank', '--yes')
        self.assertNotIn('tempbank', self.run_fin('institution', 'list').stdout)

    def test_institution_list_reports_builtin_and_added(self):
        self.run_fin('institution', 'add', 'provider')
        result = self.run_fin('institution', 'list').stdout
        self.assertIn('boursobank (built-in; aliases: boursorama, bourso)', result)
        self.assertIn('wise', result)
        self.assertIn('bunq', result)
        self.assertIn('revolut', result)
        self.assertIn('provider (generic)', result)
        self.assertNotEqual(self.run_fin('institution', 'add', 'provider', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('institution', 'add', 'bourso', check=False).returncode, 0)

    def test_institution_setup_and_directory_import(self):
        directory = self.root / 'cards'
        directory.mkdir()
        first = directory / 'a.csv'
        second = directory / 'b.csv'
        first.write_text('Merchant,Date,USD\nFirst,2026-10-01,11.00\n')
        second.write_text('Merchant,Date,USD\nSecond,2026-10-02,12.00\n')
        self.terminal(['institution', 'add', 'provider', str(first)], [
            ('Use these columns?', '1'), ('Account type:', '1'),
            ('Are purchases positive amounts in the file?', '1'),
            ('Skip a summary row with an empty date?', '2'), ('Use this CSV layout?', '1'),
        ])
        self.assertEqual(json.loads(self.run_fin('status', '--json').stdout)['movements'], 0)
        self.terminal(['import', 'provider', str(directory)], [
            ('Label for this account', 'first-card'), ('Import these transactions?', '1'),
            ('Label for this account', 'second-card'), ('Import these transactions?', '1'),
        ])
        accounts = self.accounts()
        self.assertEqual({a['account_id'] for a in accounts}, {'first-card', 'second-card'})
        self.assertTrue(all(a['institution'] == 'provider' for a in accounts))
        self.assertEqual(json.loads(self.run_fin('status', '--json').stdout)['movements'], 2)
        self.assertNotEqual(self.run_fin('institution', 'delete', 'provider', '--yes', check=False).returncode, 0)
        self.run_fin('import', 'provider', first, '--account-id', 'first-card')
        self.assertEqual(json.loads(self.run_fin('status', '--json').stdout)['movements'], 2)

    def test_delete_unused_institution_preserves_builtins(self):
        self.run_fin('institution', 'add', 'provider')
        for name in ('wise', 'boursobank', 'bourso', 'missing'):
            self.assertNotEqual(self.run_fin('institution', 'delete', name, '--yes', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('institution', 'delete', 'provider', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('institution', 'delete', '--yes', check=False).returncode, 0)
        output = self.terminal(['institution', 'delete', 'provider'], [('Delete this institution? [Y/n]:', 'no')])
        self.assertIn('Nothing deleted.', output)
        self.assertIn('provider (generic)', self.run_fin('institution', 'list').stdout)
        self.run_fin('institution', 'delete', 'provider', '--yes')
        result = self.run_fin('institution', 'list').stdout
        self.assertNotIn('provider (generic)', result)
        self.assertIn('wise (built-in)', result)

    def test_institution_first_import_saves_profile(self):
        self.run_fin('institution', 'add', 'provider')
        csv = self.root / 'first.csv'
        csv.write_text('Merchant,Date,USD\nFirst,2026-10-01,11.00\n')
        self.terminal(['import', 'provider', str(csv), '--account-id', 'card'], [
            ('Use these columns?', '1'), ('Account type:', '1'),
            ('Are purchases positive amounts in the file?', '1'),
            ('Skip a summary row with an empty date?', '2'), ('Import these transactions?', '1'),
        ])
        with sqlite3.connect(self.root / 'data/fin/fin.sqlite3') as conn:
            profile = conn.execute('SELECT profile FROM institutions WHERE name=?', ('provider',)).fetchone()[0]
        self.assertIsNotNone(profile)
        self.run_fin('import', 'provider', csv, '--account-id', 'card')

    def test_import_help_shows_distinct_forms(self):
        help_text = self.run_fin('import', '--help=plain').stdout
        self.assertIn('fin import [OPTION]… INSTITUTION PATH…', help_text)
        self.assertIn('fin import [OPTION]… CSV-FILE', help_text)
        self.assertNotIn('fin import [OPTION]… csv CSV-FILE', help_text)
        self.assertNotIn('BANK-OR-CSV', help_text)
        rejected = self.run_fin('import', 'csv', self.csv, check=False)
        self.assertNotEqual(rejected.returncode, 0)
        directory = self.run_fin('import', self.root, check=False)
        self.assertNotEqual(directory.returncode, 0)
        self.assertIn('name the bank before a directory', directory.stderr)

    def test_boursobank_import_aliases(self):
        self.run_fin('import', 'bourso', 'fixtures/bank.ofx')
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        self.assertEqual(len(rows), 3)
        self.assertTrue(all(r['institution'] == 'boursobank' for r in rows))
        self.run_fin('import', 'boursorama', 'fixtures/bank.ofx')
        self.assertEqual(len(json.loads(self.run_fin('transaction', 'list', '--json').stdout)), 3)

    def test_command_aliases(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        expected = self.run_fin('transaction', 'list', '--json').stdout
        for alias in ('tx', 'txn'):
            self.assertEqual(self.run_fin(alias, 'list', '--json').stdout, expected)
            self.assertIn('fin-transaction-list', self.run_fin(alias, 'list', '--help=plain').stdout)
        self.assertEqual(self.run_fin('acc', 'list', '--json').stdout,
                         self.run_fin('account', 'list', '--json').stdout)
        self.assertEqual(self.run_fin('inst', 'list').stdout,
                         self.run_fin('institution', 'list').stdout)
        self.assertNotEqual(self.run_fin('txx', 'list', check=False).returncode, 0)

    def test_transfer_candidate_evidence_and_interactive_review(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        other = self.root / 'other.csv'
        other.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                         'Transfer,Current,2026-01-02 10:00:00,2026-01-02 10:00:01,Transfer,-10.00,0.00,EUR,COMPLETED,0.00\n')
        self.run_fin('import', 'revolut', other, '--account-id', 'other')
        report = json.loads(self.run_fin('transfer', 'candidates', '--json').stdout)
        self.assertTrue(report['candidates'])
        candidate = report['candidates'][0]
        first, second = candidate['first']['event_id'], candidate['second']['event_id']
        answers = [('Current pair or batch choice [c/r/s/a/d/q]:', 'c'), ('Current pair or batch choice [c/r/s/a/d/q]:', 'd'),
                   ('Apply these decisions? [Y/n]:', 'n')]
        output = self.terminal(['transfer', 'review'], answers)
        self.assertIn('Nothing changed.', output)
        self.assertLess(output.index('Current pair: #'), output.index('Current pair or batch choice'))
        self.assertLess(output.index('Pending decisions:'), output.index('Apply these decisions?'))
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'history').stdout)), 0)
        answers[-1] = ('Apply these decisions? [Y/n]:', 'y')
        self.assertIn('transfer_changes=1', self.terminal(['transfer', 'review'], answers))
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'history').stdout)), 1)
        report = json.loads(self.run_fin('transfer', 'candidates', '--all', '--json').stdout)
        self.assertTrue(any(c['state'] == 'confirmed' for c in report['all_pairs']))
        self.assertIn('Nothing changed.', self.terminal(['transfer', 'undo', first, second],
            [('Undo this transfer? [Y/n]:', 'n')]))
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'history').stdout)), 1)
        self.assertIn('transfer_changes=1', self.run_fin('transfer', 'undo', second, first, '--yes').stdout)
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'history').stdout)), 2)
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        self.assertTrue(all(r['classification'] == 'unclassified' for r in rows if r['event_id'] in (first, second)))
        self.assertNotEqual(self.run_fin('transfer', 'undo', first, second, '--yes', check=False).returncode, 0)

    def test_transfer_review_select_all_supported_and_cancel(self):
        header = 'Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
        def rows(sign):
            return header + ''.join(
                f'Transfer,Current,2026-01-0{i} 10:00:00,2026-01-0{i} 10:00:01,Transfer,{sign}{amount}.00,0.00,EUR,COMPLETED,{sign}{balance}.00\n'
                for i, amount, balance in [(2, 10, 10), (3, 20, 30)])
        self.csv.write_text(rows(''))
        other = self.root / 'other.csv'
        other.write_text(rows('-'))
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('import', 'revolut', other, '--account-id', 'other')
        report = json.loads(self.run_fin('transfer', 'candidates', '--json').stdout)
        self.assertEqual(len(report['candidates']), 2)
        self.assertTrue(all(r['assignment_status'] == 'supported' for r in report['candidates']))
        self.assertIn('Nothing changed.', self.terminal(['transfer', 'review'],
            [('Current pair or batch choice [c/r/s/a/d/q]:', 'q')]))
        self.assertIn('Nothing changed.', self.terminal(['transfer', 'review'],
            [('Current pair or batch choice [c/r/s/a/d/q]:', 'a'), ('Apply these decisions? [Y/n]:', 'n')]))
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'history').stdout)), 0)
        self.assertIn('transfer_changes=2', self.terminal(['transfer', 'review'],
            [('Current pair or batch choice [c/r/s/a/d/q]:', 'a'), ('Apply these decisions? [Y/n]:', 'y')]))
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'history').stdout)), 2)

    def test_transfer_supported_shortcut_finishes_before_ambiguous_pairs(self):
        self.csv.write_text(FIXTURE +
            'Transfer,Current,2026-01-04 10:00:00,2026-01-04 10:00:01,Unique,20.00,0.00,EUR,COMPLETED,89.50\n')
        other = self.root / 'other.csv'
        other.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                         'Transfer,Current,2026-01-02 10:00:00,2026-01-02 10:00:01,Ambiguous,-10.00,0.00,EUR,COMPLETED,-10.00\n'
                         'Transfer,Current,2026-01-04 10:00:00,2026-01-04 10:00:01,Unique,-20.00,0.00,EUR,COMPLETED,-30.00\n')
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('import', 'revolut', other, '--account-id', 'other')
        report = json.loads(self.run_fin('transfer', 'candidates', '--json').stdout)
        self.assertTrue(any(r['assignment_status'] == 'supported' for r in report['candidates']))
        self.assertTrue(any(r['assignment_status'] != 'supported' for r in report['candidates']))
        output = self.terminal(['transfer', 'review'],
            [('Current pair or batch choice [c/r/s/a/d/q]:', 'a'),
             ('Apply these decisions? [Y/n]:', 'n')])
        self.assertEqual(output.count('Current pair or batch choice'), 1)
        self.assertIn('Remaining candidates are left for later review.', output)
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'history').stdout)), 0)

    def test_withdrawal_reversal_review_and_undo(self):
        ofx = self.root / 'reversal.ofx'
        data = Path('fixtures/bank.ofx').read_text().replace('Synthetic debit', 'RETRAIT DAB example')
        data = data.replace('</BANKTRANLIST></STMTRS>\n</STMTTRNRS>',
            '<STMTTRN><DTPOSTED>20260105</DTPOSTED><TRNTYPE>CREDIT</TRNTYPE>'
            '<TRNAMT>42.95</TRNAMT><FITID>atm-reversal</FITID>'
            '<NAME>ANNUL RETRAIT DAB example</NAME></STMTTRN>'
            '<STMTTRN><DTPOSTED>20260105</DTPOSTED><TRNTYPE>CREDIT</TRNTYPE>'
            '<TRNAMT>42.95</TRNAMT><FITID>positive-withdrawal</FITID>'
            '<NAME>RETRAIT DAB unrelated</NAME></STMTTRN>'
            '</BANKTRANLIST></STMTRS>\n</STMTTRNRS>')
        ofx.write_text(data)
        self.run_fin('import', 'boursobank', ofx)
        rows = json.loads(self.run_fin('tx', 'list', '--json').stdout)
        original = next(r for r in rows if r['movement_type'] == 'withdrawal' and float(r['amount']) < 0)
        reversal = next(r for r in rows if r['description'] == 'ANNUL RETRAIT DAB example')
        a, b = original['event_id'], reversal['event_id']
        candidates = self.run_fin('reversal', 'candidates').stdout
        self.assertIn(f'Original #{a}; reversal #{b}', candidates)
        self.assertEqual(candidates.count('Original #'), 1)
        self.assertIn('Nothing changed.', self.terminal(['reversal', 'review', a, b],
            [('Apply this decision? [Y/n]:', 'n')]))
        self.run_fin('reversal', 'review', a, b, '--yes')
        rows = json.loads(self.run_fin('tx', 'list', '--classification', 'reversal', '--json').stdout)
        self.assertEqual({r['event_id'] for r in rows}, {a, b})
        self.assertTrue(all(r['counter_account'] == f'assets:reversals:pair-{a}-{b}' for r in rows))
        journal = self.run_fin('journal', 'export').stdout
        self.assertIn(f'assets:reversals:pair-{a}-{b}', journal)
        self.assertIn('No unreviewed reversal candidates.', self.run_fin('reversal', 'candidates').stdout)
        self.assertIn('confirmed', self.run_fin('reversal', 'history').stdout)
        with sqlite3.connect(self.root / 'data/fin/fin.sqlite3') as db:
            trigger = db.execute("SELECT sql FROM sqlite_schema WHERE name='movements_no_update'").fetchone()[0]
            db.execute('DROP TRIGGER movements_no_update')
            db.execute("UPDATE movements SET movement_type = 'withdrawal-reversal' WHERE event_id = ?", (b,))
            db.execute(trigger)
        self.assertIn('movement_types_changed=1', self.run_fin('tx', 'refresh-types', '--yes').stdout)
        refreshed = json.loads(self.run_fin('tx', 'list', '--classification', 'reversal', '--json').stdout)
        self.assertTrue(all(r['movement_type'] == 'withdrawal' for r in refreshed))
        self.assertNotEqual(self.run_fin('reversal', 'review', a, b, '--yes', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('account', 'delete', original['account_id'], '--yes', check=False).returncode, 0)
        decisions = self.root / 'reversal-category.json'
        decisions.write_text(json.dumps([{'event_id': a, 'account': 'expenses:shopping', 'reason': 'test'}]))
        self.assertNotEqual(self.run_fin('transaction', 'category', 'apply', decisions, check=False).returncode, 0)
        self.run_fin('reversal', 'undo', a, b, '--yes')
        self.assertIn('undone', self.run_fin('reversal', 'history').stdout)
        self.assertNotIn(f'assets:reversals:pair-{a}-{b}', self.run_fin('journal', 'export').stdout)
        rows = json.loads(self.run_fin('tx', 'list', '--classification', 'unclassified', '--json').stdout)
        self.assertTrue({a, b}.issubset({r['event_id'] for r in rows}))
        self.assertNotEqual(self.run_fin('reversal', 'undo', a, b, '--yes', check=False).returncode, 0)

    def test_boursobank_types_from_source_descriptions(self):
        ofx = self.root / 'boursobank.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            'CAFÉ &amp; CO', 'VIR SEPA Example').replace(
            'Synthetic precision example', 'PRLV Example').replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>').replace(
            'Synthetic debit', 'CARTE Example').replace(
            '</BANKTRANLIST>', '<STMTTRN><DTPOSTED>20260103</DTPOSTED>'
            '<TRNTYPE>DEBIT</TRNTYPE><TRNAMT>-10</TRNAMT><FITID>payment-card</FITID>'
            '<NAME>PAIEMENT CARTE 021014 Example</NAME></STMTTRN>'
            '<STMTTRN><DTPOSTED>20260103</DTPOSTED><TRNTYPE>CREDIT</TRNTYPE>'
            '<TRNAMT>1.25</TRNAMT><FITID>interest</FITID>'
            '<NAME>*INTERETS AU 31/12/25</NAME></STMTTRN></BANKTRANLIST>', 1))
        self.run_fin('import', 'boursobank', ofx)
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        types = {r['description']: r['movement_type'] for r in rows}
        self.assertEqual(types, {'VIR SEPA Example': 'transfer', 'PRLV Example': 'direct-debit',
                                 'CARTE Example': 'card', 'PAIEMENT CARTE 021014 Example': 'card',
                                 '*INTERETS AU 31/12/25': 'interest'})
        self.assertTrue(all(r['counter_account'] == 'equity:unclassified' for r in rows))
        interest = next(r for r in rows if r['movement_type'] == 'interest')
        with sqlite3.connect(self.root / 'data/fin/fin.sqlite3') as db:
            trigger = db.execute("SELECT sql FROM sqlite_schema WHERE name='movements_no_update'").fetchone()[0]
            db.execute('DROP TRIGGER movements_no_update')
            db.execute('UPDATE movements SET movement_type = NULL WHERE event_id = ?', (interest['event_id'],))
            db.execute(trigger)
        self.assertIn('movement_types_changed=1', self.run_fin('transaction', 'refresh-types', '--yes').stdout)
        updated = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        self.assertEqual(next(r for r in updated if r['event_id'] == interest['event_id'])['movement_type'], 'interest')
        with sqlite3.connect(self.root / 'data/fin/fin.sqlite3') as db:
            with self.assertRaises(sqlite3.IntegrityError):
                db.execute('UPDATE movements SET movement_type = NULL WHERE event_id = ?', (interest['event_id'],))

    def test_boursobank_fee_cheque_and_interest_types(self):
        examples = [('*CION CB OPERAT.ETRANGER EN DEV', '-1.20', 'fee'),
                    ('*CION CB OP. ETR 04/26', '-1.68', 'fee'),
                    ('*FRAIS CB VISA ULTIM', '-5.00', 'fee'),
                    ('*RETROC *FRAIS CB VISA ULTIM', '5.00', 'fee'),
                    ('REM CHQ N. 123', '40.00', 'deposit'),
                    ('CHQ. N.123', '-40.00', 'cheque'),
                    ('*INTER.BRUTS 31/12/25', '2.00', 'interest'),
                    ('*INT DEB AU 31/12/25', '-0.11', 'interest'),
                    ('Relev{ diff{r{ Carte 1234********5678', '-10.00', 'card-settlement'),
                    ('AVOIR 310116 SG IKEA 139,00', '88.56', 'card'),
                    ('ANNUL RETRAIT DAB example', '42.95', 'withdrawal'),
                    ('TDF EMIS VIA CB EXAMPLE', '-5.00', None)]
        entries = ''.join(
            f'<STMTTRN><DTPOSTED>20260103</DTPOSTED><TRNTYPE>OTHER</TRNTYPE>'
            f'<TRNAMT>{amount}</TRNAMT><FITID>type-example-{i}</FITID>'
            f'<NAME>{description}</NAME></STMTTRN>'
            for i, (description, amount, _) in enumerate(examples))
        ofx = self.root / 'types.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace('</BANKTRANLIST>', entries + '</BANKTRANLIST>', 1))
        self.run_fin('import', 'boursobank', ofx)
        rows = json.loads(self.run_fin('tx', 'list', '--json').stdout)
        for description, amount, expected in examples:
            row = next(r for r in rows if r['description'] == description)
            self.assertEqual(row['movement_type'], expected)
            self.assertEqual(row['classification'], 'unclassified')
            self.assertEqual(float(row['amount']), float(amount))
        self.assertEqual(len(json.loads(self.run_fin('tx', 'list', '--type', 'fee', '--json').stdout)), 4)

    def test_regex_category_rule_preview_and_explicit_apply(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.assertNotEqual(self.run_fin('rule', 'add', 'bad', '[', 'expenses:food', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('rule', 'add', 'bad', 'transfer', 'assets:checking', check=False).returncode, 0)
        self.run_fin('rule', 'add', 'repeat', 'repeated transfer', 'expenses:food', '--institution', 'revolut')
        preview = json.loads(self.run_fin('rule', 'preview', 'repeat', '--json').stdout)
        self.assertEqual(len(preview), 2)
        self.assertTrue(all(row['eligible'] for row in preview))
        before = self.run_fin('transaction', 'review', '--all', '--json').stdout
        output = self.terminal(['rule', 'apply', 'repeat'], [('Apply to these transactions?', 'no')])
        self.assertIn('Nothing changed.', output)
        self.assertEqual(self.run_fin('transaction', 'review', '--all', '--json').stdout, before)
        self.assertIn('rule_changes=2', self.run_fin('rule', 'apply', 'repeat', '--yes').stdout)
        reviewed = json.loads(self.run_fin('transaction', 'list', '--json', '--search', 'Repeated').stdout)
        self.assertTrue(all(r['counter_account'] == 'expenses:food' and 'Rule repeat:' in r['reason'] for r in reviewed))
        self.run_fin('rule', 'add', 'occupied', 'other', 'expenses:food')
        self.assertNotEqual(self.run_fin('rule', 'rename', 'repeat', 'occupied', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('rule', 'rename', 'missing', 'new', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('rule', 'rename', 'repeat', 'bad name', check=False).returncode, 0)
        self.run_fin('rule', 'rename', 'repeat', 'renamed')
        rules = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        renamed = next(r for r in rules if r['name'] == 'renamed')
        self.assertEqual(renamed['pattern'], 'repeated transfer')
        self.assertEqual(renamed['institution'], 'revolut')
        self.assertEqual(json.loads(self.run_fin('transaction', 'list', '--json', '--search', 'Repeated').stdout), reviewed)
        later = self.root / 'later.csv'
        later.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                         'Transfer,Current,2026-01-04 10:00:00,2026-01-04 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,79.50\n')
        self.run_fin('import', 'revolut', later, '--account-id', 'main')
        preview = json.loads(self.run_fin('rule', 'preview', 'renamed', '--json').stdout)
        self.assertEqual(sum(row['eligible'] for row in preview), 1)
        self.assertEqual(sum(not row['eligible'] for row in preview), 2)
        self.assertIn('rule_changes=1', self.run_fin('rule', 'apply', 'renamed', '--yes').stdout)
        self.run_fin('rule', 'delete', 'renamed', '--yes')
        self.assertEqual(len(json.loads(self.run_fin('transaction', 'category', 'history').stdout)), 3)

    def test_rule_type_and_category_actions_are_independent(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        later = self.root / 'rule-extra.csv'
        later.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                         'Transfer,Current,2026-01-04 10:00:00,2026-01-04 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,79.50\n')
        self.run_fin('import', 'revolut', later, '--account-id', 'main')
        rows = json.loads(self.run_fin('tx', 'list', '--search', 'Repeated transfer', '--json').stdout)
        manual, categorized, fresh = [r['event_id'] for r in rows]
        self.run_fin('tx', 'type', 'set', manual, 'bonus')
        decisions = self.root / 'rule-existing.json'
        decisions.write_text(json.dumps([{'event_id': categorized, 'account': 'expenses:shopping', 'reason': 'Already checked'}]))
        self.run_fin('tx', 'category', 'apply', decisions)
        self.run_fin('rule', 'add', 'wise-example', '--institution', 'revolut', '--description', '^Repeated transfer$',
                     '--set-type', 'interest', '--category', 'income:interest:wise')
        preview = json.loads(self.run_fin('rule', 'preview', 'wise-example', '--json').stdout)
        by_id = {r['transaction']['event_id']: r for r in preview}
        self.assertEqual((by_id[manual]['apply_type'], by_id[manual]['apply_account']), (False, True))
        self.assertEqual((by_id[categorized]['apply_type'], by_id[categorized]['apply_account']), (True, False))
        self.assertEqual((by_id[fresh]['apply_type'], by_id[fresh]['apply_account']), (True, True))
        output = self.terminal(['rule', 'apply', 'wise-example'], [('Apply to these transactions?', 'n')])
        self.assertIn('Nothing changed.', output)
        self.assertIn('rule_changes=3', self.run_fin('rule', 'apply', 'wise-example', '--yes').stdout)
        rows = {r['event_id']: r for r in json.loads(self.run_fin('tx', 'list', '--search', 'Repeated transfer', '--json').stdout)}
        self.assertEqual((rows[manual]['movement_type'], rows[manual]['counter_account']), ('bonus', 'income:interest:wise'))
        self.assertEqual((rows[categorized]['movement_type'], rows[categorized]['counter_account']), ('interest', 'expenses:shopping'))
        self.assertEqual((rows[fresh]['movement_type'], rows[fresh]['counter_account']), ('interest', 'income:interest:wise'))
        history = json.loads(self.run_fin('tx', 'type', 'history', '--json').stdout)
        self.assertEqual(sum('Rule wise-example' in r['reason'] for r in history), 2)
        self.run_fin('rule', 'add', 'interest-any', '--match-type', 'interest', '--category', 'income:interest')
        preview = json.loads(self.run_fin('rule', 'preview', 'interest-any', '--json').stdout)
        self.assertEqual({r['transaction']['event_id'] for r in preview}, {categorized, fresh})
        self.assertTrue(all(not r['eligible'] for r in preview))
        self.run_fin('rule', 'add', 'type-only', '--description', '^Initial deposit$', '--set-type', 'interest')
        self.assertIn('rule_changes=1', self.run_fin('rule', 'apply', 'type-only', '--yes').stdout)
        self.assertIn('Nothing to apply.', self.run_fin('rule', 'apply', 'type-only', '--yes').stdout)
        preview = json.loads(self.run_fin('rule', 'preview', 'interest-any', '--json').stdout)
        self.assertEqual(sum(r['eligible'] for r in preview), 1)
        self.assertIn('rule_changes=1', self.run_fin('rule', 'apply', 'interest-any', '--yes').stdout)
        future = self.root / 'rule-future.csv'
        future.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                          'Transfer,Current,2026-01-05 10:00:00,2026-01-05 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,89.50\n')
        self.run_fin('import', 'revolut', future, '--account-id', 'main')
        new_row = next(r for r in json.loads(self.run_fin('tx', 'list', '--json').stdout) if r['booked_on'] == '2026-01-05')
        self.assertIsNone(new_row['movement_type'])
        self.assertEqual(new_row['classification'], 'unclassified')
        self.assertEqual(sum(r['eligible'] for r in json.loads(self.run_fin('rule', 'preview', 'wise-example', '--json').stdout)), 1)
        self.run_fin('rule', 'add', 'untyped', '--match-type', 'unknown', '--set-type', 'bonus')
        untyped = json.loads(self.run_fin('rule', 'preview', 'untyped', '--json').stdout)
        self.assertTrue(untyped)
        self.assertTrue(all(r['transaction']['movement_type'] is None for r in untyped))
        self.assertEqual(next(r for r in json.loads(self.run_fin('rule', 'list', '--json').stdout) if r['name'] == 'untyped')['match_type'], 'unknown')
        self.run_fin('rule', 'add', 'institution-only', '--institution', 'revolut', '--category', 'expenses:misc')
        self.assertEqual(len(json.loads(self.run_fin('rule', 'preview', 'institution-only', '--json').stdout)), 6)
        self.assertNotEqual(self.run_fin('rule', 'add', 'no-condition', '--set-type', 'interest', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('rule', 'add', 'no-action', '--match-type', 'interest', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('rule', 'add', 'unknown-type', '--match-type', 'interest', '--set-type', 'unknown', check=False).returncode, 0)

    def test_regex_settlement_rule_preview_and_apply(self):
        ofx = self.root / 'rule-settlement.ofx'
        extra = ('<STMTTRN><DTPOSTED>20260104</DTPOSTED><TRNTYPE>DEBIT</TRNTYPE>'
                 '<TRNAMT>-0.20</TRNAMT><FITID>another-settlement</FITID>'
                 '<NAME>Relevé différé Carte SAVINGS</NAME></STMTTRN>')
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>').replace(
            'CAFÉ &amp; CO', 'Relevé différé Carte SAVINGS').replace('</BANKTRANLIST>', extra + '</BANKTRANLIST>', 1))
        self.run_fin('import', 'boursobank', ofx)
        accounts = json.loads(self.run_fin('account', 'list', '--json').stdout)
        card = next(a for a in accounts if a['account_type'] == 'CREDITCRD')
        checking = next(a for a in accounts if a['account_type'] == 'CHECKING')
        self.run_fin('account', 'link-card', card['id'], checking['id'], '--yes')
        self.run_fin('rule', 'add-settlement', 'card-pay', 'Relevé différé Carte', card['id'], '--institution', 'boursobank')
        preview = json.loads(self.run_fin('rule', 'preview', 'card-pay', '--json').stdout)
        self.assertEqual(len(preview), 2)
        self.assertTrue(all(row['eligible'] for row in preview))
        self.assertIn('rule_changes=2', self.run_fin('rule', 'apply', 'card-pay', '--yes').stdout)
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        settlements = [r for r in rows if r['description'] == 'Relevé différé Carte SAVINGS']
        self.assertTrue(all(r['classification'] == 'settlement' for r in settlements))
        self.assertTrue(all(r['movement_type'] == 'card-settlement' for r in settlements))
        selected = json.loads(self.run_fin('tx', 'list', '--type', 'card-settlement',
                                           '--classification', 'settlement', '--json').stdout)
        self.assertEqual({r['event_id'] for r in selected}, {r['event_id'] for r in settlements})
        self.assertEqual(json.loads(self.run_fin('tx', 'list', '--type', 'interest', '--classification', 'settlement', '--json').stdout), [])
        self.assertTrue(all(r['movement_type'] is None for r in json.loads(
            self.run_fin('tx', 'list', '--type', 'unknown', '--json').stdout)))
        database = self.root / 'data/fin/fin.sqlite3'
        with sqlite3.connect(database) as db:
            trigger = db.execute("SELECT sql FROM sqlite_schema WHERE name='movements_no_update'").fetchone()[0]
            db.execute('DROP TRIGGER movements_no_update')
            db.executemany('UPDATE movements SET movement_type = NULL WHERE event_id = ?',
                           [(r['event_id'],) for r in settlements])
            db.execute(trigger)
        history = self.run_fin('settlement', 'history').stdout
        journal = self.run_fin('journal', 'export').stdout
        preview = self.terminal(['transaction', 'refresh-types', '--institution', 'boursobank'],
            [('Apply these movement type changes? [Y/n]:', 'n')])
        self.assertIn('unknown -> card-settlement', preview)
        self.assertIn('Nothing changed.', preview)
        self.assertIn('movement_types_changed=2', self.run_fin('transaction', 'refresh-types', '--yes').stdout)
        restored = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        self.assertTrue(all(r['classification'] == 'settlement' and r['movement_type'] == 'card-settlement'
                            for r in restored if r['event_id'] in [s['event_id'] for s in settlements]))
        self.assertEqual(self.run_fin('settlement', 'history').stdout, history)
        self.assertEqual(self.run_fin('journal', 'export').stdout, journal)
        self.assertIn('No movement type changes.', self.run_fin('transaction', 'refresh-types').stdout)
        self.assertNotEqual(self.run_fin('transaction', 'refresh-types', '--dry-run', check=False).returncode, 0)
        self.env['PAGER'] = 'sed s/CLASSIFICATION/PAGED/'
        output = self.terminal(['transaction', 'list'], [])
        self.assertIn('PAGED', output)
        self.assertIn('settlement', output)
        self.assertIn('CLASSIFICATION', self.run_fin('transaction', 'list').stdout)
        self.assertTrue(all(r['counter_account'].startswith('liabilities:cards:') and 'Rule card-pay:' in r['reason'] for r in settlements))
        self.assertEqual(len(json.loads(self.run_fin('settlement', 'history').stdout)), 2)

    def test_link_credit_card_without_editing_json(self):
        ofx = self.root / 'link-card.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>'))
        self.run_fin('import', 'boursobank', ofx)
        accounts = json.loads(self.run_fin('account', 'list', '--json').stdout)
        card = next(a for a in accounts if a['account_type'] == 'CREDITCRD')
        checking = next(a for a in accounts if a['account_type'] == 'CHECKING')
        self.assertNotEqual(self.run_fin('account', 'link-card', card['id'], check=False).returncode, 0)
        output = self.terminal(['account', 'link-card', card['id'], checking['id']],
                               [('Apply this card setup? [Y/n]:', 'n')])
        self.assertIn('Nothing changed.', output)
        output = self.terminal(['account', 'link-card', card['id'], checking['id']],
                               [('Apply this card setup? [Y/n]:', '')])
        self.assertIn('Card setup complete.', output)
        card = next(a for a in self.accounts() if a['account_type'] == 'CREDITCRD')
        self.assertTrue(card['ledger_account'].startswith('liabilities:cards:'))
        self.assertEqual(card['settlement_account']['account_id'], 'CHECKING')

    def test_guided_card_settlement_setup_and_review(self):
        ofx = self.root / 'guided-settlement.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>').replace(
            'CAFÉ &amp; CO', 'Relevé différé Carte SAVINGS'))
        self.run_fin('import', 'boursobank', ofx)
        output = self.run_fin('settlement', 'candidates').stdout
        self.assertIn('SAVINGS', output)
        self.assertIn('-0.1', output)
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        event = next(r['event_id'] for r in rows if r['description'] == 'Relevé différé Carte SAVINGS')
        self.assertIn(f'#{event}', output)
        self.assertNotRegex(output, r'(?m)^\d+\. #')
        output = self.terminal(['settlement', 'review'], [('Choose an event ID', str(event)),
                                                           ('Apply this setup and record this one settlement?', 'no')])
        self.assertIn('Nothing changed.', output)
        card = next(a for a in self.accounts() if a['account_type'] == 'CREDITCRD')
        self.assertIsNone(card.get('settlement_account'))
        self.assertTrue(card['ledger_account'].startswith('assets:'))
        output = self.terminal(['settlement', 'review'], [('Choose an event ID', str(event)),
                                                           ('Apply this setup and record this one settlement?', 'yes')])
        self.assertIn('Card settlement recorded.', output)
        card = next(a for a in self.accounts() if a['account_type'] == 'CREDITCRD')
        self.assertTrue(card['ledger_account'].startswith('liabilities:cards:'))
        self.assertEqual(card['settlement_account']['account_id'], 'CHECKING')
        self.assertIn('No unambiguous card settlements found.', self.run_fin('settlement', 'candidates').stdout)

    def test_interrupt_settlement_before_confirmation_changes_nothing(self):
        ofx = self.root / 'interrupt-settlement.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>').replace(
            'CAFÉ &amp; CO', 'Relevé différé Carte SAVINGS'))
        self.run_fin('import', 'boursobank', ofx)
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        event = next(r['event_id'] for r in rows if r['description'] == 'Relevé différé Carte SAVINGS')
        before = (self.run_fin('account', 'export').stdout,
                  self.run_fin('transaction', 'review', '--all', '--json').stdout,
                  self.run_fin('journal', 'export').stdout,
                  self.run_fin('account', 'history').stdout,
                  self.run_fin('settlement', 'history').stdout)
        master, slave = pty.openpty()
        child = subprocess.Popen([FIN, 'settlement', 'review', str(event)], env=self.env,
                                 stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        transcript = b''
        deadline = time.monotonic() + 20
        try:
            while b'Apply this setup and record this one settlement?' not in transcript:
                self.assertLess(time.monotonic(), deadline, transcript.decode(errors='replace'))
                if select.select([master], [], [], 0.1)[0]:
                    transcript += os.read(master, 65536)
            child.send_signal(signal.SIGINT)
            child.wait(timeout=5)
            self.assertNotEqual(child.returncode, 0)
        finally:
            if child.poll() is None:
                child.kill()
                child.wait()
            os.close(master)
        after = (self.run_fin('account', 'export').stdout,
                 self.run_fin('transaction', 'review', '--all', '--json').stdout,
                 self.run_fin('journal', 'export').stdout,
                 self.run_fin('account', 'history').stdout,
                 self.run_fin('settlement', 'history').stdout)
        self.assertEqual(after, before)

    def test_settlement_selection_uses_event_id_and_only_records_selected(self):
        ofx = self.root / 'two-settlements.ofx'
        extra = ('<STMTTRN><DTPOSTED>20260104</DTPOSTED><TRNTYPE>DEBIT</TRNTYPE>'
                 '<TRNAMT>-0.20</TRNAMT><FITID>another-settlement</FITID>'
                 '<NAME>Relevé différé Carte SAVINGS</NAME></STMTTRN>')
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>').replace(
            'CAFÉ &amp; CO', 'Relevé différé Carte SAVINGS').replace('</BANKTRANLIST>', extra + '</BANKTRANLIST>', 1))
        self.run_fin('import', 'boursobank', ofx)
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        matches = [r for r in rows if r['description'] == 'Relevé différé Carte SAVINGS']
        chosen = next(r['event_id'] for r in matches if r['amount'] == '-0.2')
        other = next(r['event_id'] for r in matches if r['amount'] == '-0.1')
        self.terminal(['settlement', 'review'], [('Choose an event ID', str(chosen)),
                                                 ('Apply this setup and record this one settlement?', 'y')])
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        by_id = {r['event_id']: r for r in rows}
        self.assertTrue(by_id[chosen]['counter_account'].startswith('liabilities:cards:'))
        self.assertEqual(by_id[other]['counter_account'], 'equity:unclassified')
        self.assertEqual([h['event_id'] for h in json.loads(self.run_fin('settlement', 'history').stdout)], [chosen])

    def test_fullscreen_settlement_cancel_and_confirm(self):
        ofx = self.root / 'fullscreen-settlement.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>').replace(
            'CAFÉ &amp; CO', 'Relevé différé Carte SAVINGS'))
        self.run_fin('import', 'boursobank', ofx)
        before = self.run_fin('account', 'export').stdout
        output = self.fullscreen(['settlement', 'review', '--tui'],
                                 [('Card settlement review', '\x03')])
        self.assertIn('Nothing changed.', output)
        self.assertEqual(self.run_fin('account', 'export').stdout, before)
        self.assertEqual(json.loads(self.run_fin('settlement', 'history').stdout), [])
        output = self.fullscreen(['settlement', 'review', '--tui'],
                                 [('Terminal too small', '\ryq')], size=(8, 40))
        self.assertIn('Nothing changed.', output)
        self.assertEqual(self.run_fin('account', 'export').stdout, before)
        output = self.fullscreen(['settlement', 'review', '--tui'],
                                 [('Card settlement review', '\r'),
                                  ('Confirm this one settlement?', 'q')])
        self.assertIn('Nothing changed.', output)
        self.assertEqual(self.run_fin('account', 'export').stdout, before)
        output = self.fullscreen(['settlement', 'review', '--tui'],
                                 [('Card settlement review', '\r'),
                                  ('Confirm this one settlement?', 'y')])
        self.assertIn('Card settlement recorded.', output)
        self.assertEqual([r['action'] for r in json.loads(self.run_fin('settlement', 'history').stdout)],
                         ['confirmed'])
        self.assertNotEqual(self.run_fin('settlement', 'review', '--tui', '--yes', check=False).returncode, 0)

    def test_settlement_candidates_exclude_card_side_payment(self):
        ofx = self.root / 'paired-settlement.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>').replace(
            'CAFÉ &amp; CO', 'Relevé différé Carte SAVINGS').replace(
            '<TRNAMT>-42.95</TRNAMT>', '<TRNAMT>0.10</TRNAMT>'))
        self.run_fin('import', 'boursobank', ofx)
        self.assertIn('No unambiguous card settlements found.', self.run_fin('settlement', 'candidates').stdout)
        accounts = json.loads(self.run_fin('account', 'list', '--json').stdout)
        card = next(a for a in accounts if a['account_type'] == 'CREDITCRD')
        checking = next(a for a in accounts if a['account_type'] == 'CHECKING')
        self.run_fin('account', 'link-card', card['id'], checking['id'], '--yes')
        self.run_fin('rule', 'add-settlement', 'payment', 'Relevé différé Carte', card['id'])
        preview = json.loads(self.run_fin('rule', 'preview', 'payment', '--json').stdout)
        self.assertEqual(len(preview), 1)
        self.assertFalse(preview[0]['eligible'])
        self.assertIn('card-side payment', preview[0]['explanation'])
        self.assertIn('Nothing to apply.', self.run_fin('rule', 'apply', 'payment', '--yes').stdout)
        self.assertEqual(json.loads(self.run_fin('settlement', 'history').stdout), [])

    def test_settlement_candidates_require_exact_card_identifier(self):
        ofx = self.root / 'unmatched-settlement.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>').replace(
            'CAFÉ &amp; CO', 'Relevé différé Carte OTHER-CARD'))
        self.run_fin('import', 'boursobank', ofx)
        self.assertIn('No unambiguous card settlements found.', self.run_fin('settlement', 'candidates').stdout)

    def test_interactive_card_settlement(self):
        ofx = self.root / 'settlement.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace(
            '<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>'))
        self.run_fin('import', 'boursobank', ofx)
        snapshot = json.loads(self.run_fin('account', 'export').stdout)
        cash = next(a for a in snapshot['accounts'] if a['account_type'] == 'CHECKING')
        card = next(a for a in snapshot['accounts'] if a['account_type'] == 'CREDITCRD')
        card['ledger_account'] = 'liabilities:cards:example'
        card['settlement_account'] = {key: cash[key] for key in ('institution', 'bank_id', 'account_id', 'account_type')}
        config = self.root / 'settlement-settings.json'
        config.write_text(json.dumps(snapshot))
        self.run_fin('account', 'import', config)
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        event = next(r['event_id'] for r in rows if r['source_account'] == 'CHECKING' and r['amount'] == '-0.1')
        output = self.terminal(['settlement', 'review', str(event)], [('Record this card settlement? [Y/n]:', 'no')])
        self.assertIn('Nothing changed.', output)
        output = self.terminal(['settlement', 'review', str(event)], [('Record this card settlement? [Y/n]:', 'yes')])
        self.assertIn('Card settlement recorded.', output)
        result = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        self.assertEqual(next(r for r in result if r['event_id'] == event)['counter_account'], 'liabilities:cards:example')
        self.assertNotEqual(self.run_fin('settlement', 'review', event, '--yes', check=False).returncode, 0)
        self.run_fin('settlement', 'undo', event, '--yes')
        result = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        self.assertEqual(next(r for r in result if r['event_id'] == event)['counter_account'], 'equity:unclassified')
        history = json.loads(self.run_fin('settlement', 'history').stdout)
        self.assertEqual([r['action'] for r in history], ['confirmed', 'undone'])
        self.assertTrue(all(r['event_id'] == event for r in history))

    def test_wise_enrichment_shows_merchant_and_matches_different_descriptions(self):
        self.run_fin('import', 'wise', 'fixtures/wise.xml', 'fixtures/wise.csv')
        rows = json.loads(self.run_fin('transaction', 'list', '--json').stdout)
        self.assertEqual(len(rows), 3)
        self.assertTrue(all(r['display_name'] == 'Synthetic shop' and
                            r['movement_type'] == 'card' and r['detail_sources'] for r in rows))
        self.assertGreater(len({r['description'] for r in rows}), 1)
        output = self.run_fin('transaction', 'list').stdout
        self.assertIn('Synthetic shop', output)
        self.assertIn('card', output)
        decisions = self.root / 'wise-category.json'
        decisions.write_text(json.dumps([{'event_id': rows[0]['event_id'],
                                          'account': 'expenses:shopping', 'reason': 'Reviewed'}]))
        self.run_fin('transaction', 'category', 'apply', decisions)
        result = json.loads(self.run_fin('transaction', 'category', 'similar', rows[0]['event_id'],
                                         '--json').stdout)
        self.assertEqual(len(result['candidates']), 2)

    def test_transaction_list_order_uses_timestamp_before_limit(self):
        csv = self.root / 'times.csv'
        csv.write_text(self.csv.read_text().replace(
            '2026-01-02 10:00:00,2026-01-02 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,160.00',
            '2026-01-02 10:00:00,2026-01-02 10:00:01,Earlier transfer,10.00,0.00,EUR,COMPLETED,160.00').replace(
            '2026-01-02 10:00:00,2026-01-02 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,170.00',
            '2026-01-02 11:00:00,2026-01-02 11:00:01,Later transfer,10.00,0.00,EUR,COMPLETED,170.00'))
        self.run_fin('import', 'revolut', csv, '--account-id', 'main')
        asc = json.loads(self.run_fin('transaction', 'list', '--json', '--order', 'asc').stdout)
        desc = json.loads(self.run_fin('transaction', 'list', '--json', '--order', 'desc').stdout)
        self.assertEqual([r['event_id'] for r in asc], [r['event_id'] for r in reversed(desc)])
        self.assertEqual([r['description'] for r in asc],
                         ['Initial deposit', 'Earlier transfer', 'Later transfer',
                          'Transfer to another bank'])
        first = json.loads(self.run_fin('transaction', 'list', '--json', '--order', 'asc',
                                        '--limit', '1').stdout)
        self.assertEqual(first, asc[:1])
        self.assertNotEqual(self.run_fin('transaction', 'list', '--order', 'invalid',
                                         check=False).returncode, 0)

    def test_welcome_and_init_show_transaction_list(self):
        welcome = self.run_fin().stdout
        self.assertIn('fin transaction list', welcome)
        self.assertIn('fin institution list', welcome)
        self.assertIn('fin institution add', welcome)
        self.assertNotIn('bank statements', welcome)
        db = self.root / 'separate.sqlite3'
        output = self.run_fin('init', '--database', db).stdout
        for command in ('fin institution list', 'fin institution add', 'fin import INSTITUTION'):
            self.assertIn(command, output)
            line = next(line for line in output.splitlines() if command in line)
            self.assertTrue(shlex.split(line)[-2:] == ['--database', str(db)])
        line = next(line for line in output.splitlines() if 'fin transaction list' in line)
        self.assertEqual(shlex.split(line), ['fin', 'transaction', 'list', '--database', str(db)])

    def test_account_command_is_singular(self):
        help_text = self.run_fin('account', '--help=plain').stdout
        self.assertIn('delete', help_text)
        self.assertNotEqual(self.run_fin('accounts', '--help', check=False).returncode, 0)

    def test_scripts_require_label_and_never_prompt(self):
        help_text = self.run_fin('import', '--help=plain').stdout
        self.assertIn('INSTITUTION', help_text)
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
        review = json.loads(self.run_fin('transaction', 'review', '--json').stdout)
        decisions = self.root / 'decisions.json'
        decisions.write_text(json.dumps([{'event_id': review[0]['event_id'],
                                         'account': 'expenses:food', 'reason': 'Reviewed'}]))
        self.run_fin('transaction', 'category', 'apply', decisions)
        expected = {name: self.run_fin(*args).stdout for name, args in {
            'accounts': ['account', 'export'],
            'events': ['db', 'events'],
            'review': ['transaction', 'review', '--all', '--json'],
            'history': ['transaction', 'category', 'history'],
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
            'review': ['transaction', 'review', '--all', '--json'],
            'history': ['transaction', 'category', 'history'], 'journal': ['journal', 'export'],
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
        self.terminal(['restore', backup], [('Restore? [Y/n]:', 'no')])
        self.assertEqual(self.accounts(), changed)
        self.assertTrue(draft.exists())
        self.assertTrue(journal.exists())
        self.terminal(['restore', backup], [('Restore? [Y/n]:', 'yes')])
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
        history = json.loads(self.run_fin('institution', 'history', '--json').stdout)
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
