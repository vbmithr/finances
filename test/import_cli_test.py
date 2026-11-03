"""Exercise import onboarding and account settings against a disposable database."""
import json
from decimal import Decimal
import os
import fcntl
from pathlib import Path
import pty
import re
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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        rent = next(row for row in rows if row['description'] == 'Rent')
        self.assertEqual((rent['booked_on'], rent['amount']), ('2026-04-03', '-123.45'))
        self.assertIn('inserted=0', self.run_fin('import', csv_path, '--account-id', 'main', '--provider', 'localbank').stdout)

    def test_cdc_inspection_is_read_only_and_conservative(self):
        path = self.root / 'cdc.csv'
        path.write_text('Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n'
                        '2025-06-09 17:56:16,Reward,CRO,1.23456789,,,EUR,0.10,0.12,referral_card_cashback,\n'
                        '2025-06-09 17:56:17,Exchange,CRO,-2,EUR,0.20,EUR,0.20,0.23,crypto_viban_exchange,\n'
                        '2025-06-09 17:56:18,Payment,CRO,-3,CRO,-3,EUR,1,1,crypto_payment,\n'
                        '2025-06-09 17:56:19,Unknown,CRO,-1,EUR,,EUR,1,1,unknown_kind,\n')
        env = dict(self.env, XDG_DATA_HOME=str(self.root / 'unused-data'))
        result = subprocess.run([FIN, 'import', 'cdc', '--inspect', '--json', str(path)],
                                env=env, stdin=subprocess.DEVNULL, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        rows = json.loads(result.stdout)
        self.assertEqual([r['interpretation'] for r in rows], ['single', 'exchange', 'review', 'review'])
        self.assertEqual(rows[0]['proposed_legs'], [{'provider': 'cdc', 'commodity': 'CRO', 'amount': '1.23456789'}])
        self.assertEqual([leg['amount'] for leg in rows[1]['proposed_legs']], ['-2', '0.2'])
        self.assertEqual(rows[2]['proposed_legs'], [])
        self.assertEqual(rows[2]['fields']['To Amount'], '-3')
        self.assertIn('Incomplete', rows[3]['review_reason'])
        self.assertFalse((self.root / 'unused-data').exists())
        copy = self.root / 'cdc-copy.csv'
        copy.write_bytes(Path(path).read_bytes())
        summary = json.loads(self.run_fin('import', 'cdc', '--inspect', '--summary', '--json', path, str(copy)).stdout)
        self.assertEqual(summary['total'], 8)
        self.assertEqual(len(summary['identical_rows']), 4)
        self.assertEqual(next(k for k in summary['by_kind'] if k['kind'] == 'crypto_payment')['review'], 2)
        self.assertTrue(all(len(group) == 2 for group in summary['identical_rows']))
        self.assertNotEqual(self.run_fin('import', 'cdc', '--inspect', check=False).returncode, 0)
        journal = self.run_fin('journal', 'export').stdout
        self.assertEqual(json.loads(self.run_fin('db', 'events').stdout), [])
        self.assertIn('inserted=4', self.run_fin('import', 'cdc', path).stdout)
        reimport = self.run_fin('import', 'cdc', path).stdout
        self.assertIn('unchanged=1', reimport)
        self.assertIn('inserted=0', reimport)
        status = json.loads(self.run_fin('status', '--json').stdout)
        self.assertEqual([(o['institution'], o['operations']) for o in status['operations']], [('cdc', 4)])
        self.assertIn('Operations without a bank account', self.run_fin('status').stdout)
        self.assertRegex(self.run_fin('institution', 'show').stdout, r'cdc\s+crypto exchange\s+built-in\s+-\s+4\s+')
        self.assertIn('cdc (crypto exchange, built-in)', self.run_fin('institution', 'list').stdout)
        shown = self.run_fin('institution', 'show', 'cdc').stdout
        self.assertIn('Type: crypto exchange', shown)
        self.assertIn('Imported data: 4 operations', shown)
        sources = json.loads(self.run_fin('db', 'sources', '--institution', 'cdc', '--json').stdout)
        self.assertEqual(len(sources), 4)
        self.assertEqual(next(s for s in sources if s['fields']['Transaction Description'] == 'Exchange')['fields']['To Amount'], '0.20')
        self.assertIn('Exchange', self.run_fin('db', 'sources', '--institution', 'cdc').stdout)
        events = json.loads(self.run_fin('db', 'events').stdout)
        self.assertEqual(len(events), 4)
        exchange_event = next(e for e in events if e['occurred_at'].startswith('2025-06-09T17:56:17'))
        operation = json.loads(self.run_fin('db', 'event', 'operations', exchange_event['id'], '--json').stdout)[0]
        self.assertEqual([m['identity']['role'] for m in operation['movements']], ['from', 'to'])
        self.assertEqual([m['quantity'] for m in operation['movements']], ['-2', '0.2'])
        listed = json.loads(self.run_fin('operation', 'list', '--institution', 'cdc', '--json').stdout)
        unknown = next(row for row in listed if row['description'] == 'Unknown')
        self.assertEqual((unknown['reported_amount'], unknown['reported_currency']), ('-1', 'CRO'))
        self.assertIsNone(unknown['amount'])
        self.assertEqual(unknown['movements'], [])
        table = self.run_fin('operation', 'list', '--institution', 'cdc').stdout
        self.assertIn('-1 CRO (reported)', table)
        self.assertIn('needs interpretation', table)
        self.assertIn('2025-06-09', table)
        self.assertNotIn('2025-06-09T17:56', table)
        self.assertTrue(any('2025-06-09T17:56:17' in str(row['occurred']) for row in listed))
        exchange_row = next(row for row in listed if row['description'] == 'Exchange')
        op = exchange_row['operation_id']
        native = json.dumps({'kind': 'onchain-native', 'network': 'cronos'})
        fiat = json.dumps({'kind': 'fiat', 'code': 'EUR'})
        self.assertIn('movement_resolution_changes=1', self.run_fin('operation', 'resolve', op, 'from', 'assets:cdc:wallet', native, '--symbol', 'CRO').stdout)
        self.assertIn('movement_resolution_changes=1', self.run_fin('operation', 'resolve', op, 'to', 'assets:cdc:fiat', fiat).stdout)
        other_native = json.dumps({'kind': 'onchain-native', 'network': 'another-network'})
        collision = self.run_fin('operation', 'resolve', op, 'to', 'assets:cdc:fiat', other_native, '--symbol', 'CRO', check=False)
        self.assertNotEqual(collision.returncode, 0)
        self.assertIn('different mapping', collision.stderr)
        self.assertIn('movement_resolution_changes=0', self.run_fin('operation', 'resolve', op, 'to', 'assets:cdc:fiat', fiat).stdout)
        self.assertNotEqual(self.run_fin('operation', 'resolve', op, 'missing', 'assets:test', fiat, check=False).returncode, 0)
        resolved = next(row for row in json.loads(self.run_fin('operation', 'list', '--institution', 'cdc', '--json').stdout) if row['operation_id'] == op)
        self.assertEqual({m['role']: m['account'] for m in resolved['movements']}, {'from': 'assets:cdc:wallet', 'to': 'assets:cdc:fiat'})
        portable = self.root / 'operation-resolutions.json'
        portable.write_text(self.run_fin('operation', 'export').stdout)
        saved = json.loads(portable.read_text())
        self.assertEqual((saved['version'], len(saved['resolutions'])), (5, 2))
        rebuilt = self.root / 'crypto-rebuilt.sqlite3'
        self.run_fin('init', '--database', rebuilt)
        self.run_fin('import', 'cdc', path, '--database', rebuilt)
        preview = self.run_fin('operation', 'restore', portable, '--database', rebuilt).stdout
        self.assertIn('Movement resolutions to restore: 2', preview)
        self.run_fin('operation', 'restore', portable, '--database', rebuilt, '--apply', '--yes')
        rebuilt_rows = json.loads(self.run_fin('operation', 'list', '--database', rebuilt, '--institution', 'cdc', '--json').stdout)
        exchange2 = next(row for row in rebuilt_rows if row['description'] == 'Exchange')
        self.assertEqual({m['role']: m['account'] for m in exchange2['movements']}, {'from': 'assets:cdc:wallet', 'to': 'assets:cdc:fiat'})
        saved['resolutions'][0]['source_quantity'] = '999'
        portable.write_text(json.dumps(saved))
        blocked = self.run_fin('operation', 'restore', portable, '--database', rebuilt, '--apply', '--yes', check=False)
        self.assertNotEqual(blocked.returncode, 0)
        self.assertIn('movement interpretation differs', blocked.stdout)
        self.assertIn('unchanged=1', self.run_fin('import', 'cdc', path).stdout)
        self.assertEqual(self.run_fin('journal', 'export').stdout, journal)
        self.assertIn('exchange_changes=1', self.run_fin('operation', 'exchange', 'confirm', op, '--yes').stdout)
        after = self.run_fin('journal', 'export').stdout
        self.assertIn('equity:trading', after)
        self.assertIn('CRO', after)
        self.assertIn('operation:exchange', after)
        exchanges = self.root / 'exchanges.json'
        exchanges.write_text(self.run_fin('operation', 'exchange', 'export').stdout)
        self.assertIn('Exchanges to restore: 1', self.run_fin('operation', 'exchange', 'restore', exchanges, '--database', rebuilt).stdout)
        self.assertIn('exchange_changes=1', self.run_fin('operation', 'exchange', 'restore', exchanges, '--database', rebuilt, '--apply', '--yes').stdout)
        self.assertEqual(self.run_fin('journal', 'export', '--database', rebuilt).stdout, after)
        self.assertIn('already matching: 1', self.run_fin('operation', 'exchange', 'restore', exchanges, '--database', rebuilt).stdout)
        tampered = json.loads(exchanges.read_text())
        tampered['exchanges'][0]['projection_sha256'] = 'changed'
        exchanges.write_text(json.dumps(tampered))
        self.assertNotEqual(self.run_fin('operation', 'exchange', 'restore', exchanges, '--database', rebuilt, '--apply', '--yes', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('operation', 'resolve', op, 'to', 'assets:other', fiat, check=False).returncode, 0)
        self.assertIn('exchange_changes=1', self.run_fin('operation', 'exchange', 'undo', op, '--yes').stdout)
        self.assertEqual(self.run_fin('journal', 'export').stdout, journal)
        overlap = self.root / 'overlap.csv'
        overlap.write_text(path.read_text() + '2025-06-09 17:56:20,Another,CRO,2,,,EUR,1,1,referral_bonus,\n')
        blocked = self.run_fin('import', 'cdc', overlap, check=False)
        self.assertNotEqual(blocked.returncode, 0)
        self.assertIn('overlap is ambiguous', blocked.stdout)
        self.assertEqual(len(json.loads(self.run_fin('db', 'events').stdout)), 4)
        duplicated = self.root / 'duplicated.csv'
        header, first, *_ = path.read_text().splitlines()
        duplicated.write_text(header + '\n' + first + '\n' + first + '\n')
        blocked = self.run_fin('import', 'cdc', duplicated, check=False)
        self.assertNotEqual(blocked.returncode, 0)
        self.assertIn('identical source rows are ambiguous', blocked.stdout)

    def test_fiat_exchange_uses_same_review_and_trading_postings(self):
        path = self.root / 'forex.csv'
        path.write_text('Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n'
                        '2025-06-09 17:56:17,Forex,EUR,-100,USD,108,EUR,-100,-108,crypto_viban_exchange,\n')
        self.run_fin('import', 'cdc', path)
        row = json.loads(self.run_fin('operation', 'list', '--json').stdout)[0]
        op = row['operation_id']
        for role, account, currency in [('from', 'assets:forex:eur', 'EUR'), ('to', 'assets:forex:usd', 'USD')]:
            self.run_fin('operation', 'resolve', op, role, account, json.dumps({'kind': 'fiat', 'code': currency}))
        self.run_fin('operation', 'exchange', 'confirm', op, '--yes')
        journal = self.run_fin('journal', 'export').stdout
        self.assertIn('assets:forex:eur', journal)
        self.assertIn('assets:forex:usd', journal)
        self.assertIn('equity:trading', journal)
        self.assertIn('100', journal)
        self.assertIn('108', journal)

    def test_bank_wallet_transfer_review_and_restore(self):
        bank = self.root / 'outgoing.csv'
        bank.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                        'Transfer,Current,2026-01-02 10:00:00,2026-01-02 10:00:01,To wallet,-10.00,0.00,EUR,COMPLETED,0.00\n')
        wallet = self.root / 'wallet.csv'
        wallet.write_text('Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n'
                          '2026-01-02 10:01:00,Deposit,EUR,10,,,EUR,10,11,crypto_deposit,\n')
        self.run_fin('import', 'revolut', bank, '--account-id', 'main')
        self.run_fin('import', 'cdc', wallet)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        bank_op = next(r['operation_id'] for r in rows if r['institution'] == 'revolut')
        wallet_op = next(r['operation_id'] for r in rows if r['institution'] == 'cdc')
        fiat = json.dumps({'kind': 'fiat', 'code': 'EUR'})
        self.run_fin('operation', 'resolve', wallet_op, 'primary', 'assets:wallet:eur', fiat)
        resolutions = self.root / 'resolutions.json'
        resolutions.write_text(self.run_fin('operation', 'export').stdout)
        self.assertIn('transfer_changes=1', self.run_fin('transfer', 'link', bank_op, wallet_op, '--yes').stdout)
        grouped = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(len(grouped), 1)
        self.assertEqual(len(grouped[0]['movements']), 2)
        self.assertEqual(grouped[0]['decision'], 'transfer')
        self.assertEqual(json.loads(self.run_fin('operation', 'status', '--json').stdout)['transfers'], 1)
        journal = self.run_fin('journal', 'export').stdout
        self.assertIn('assets:wallet:eur', journal)
        self.assertNotIn('equity:unclassified', journal)
        self.assertNotEqual(self.run_fin('operation', 'resolve', wallet_op, 'primary', 'assets:other', fiat, check=False).returncode, 0)
        transfers = self.root / 'general-transfers.json'
        transfers.write_text(self.run_fin('transfer', 'export').stdout)
        rebuilt = self.root / 'transfer-general-rebuilt.sqlite3'
        self.run_fin('init', '--database', rebuilt)
        self.run_fin('import', 'cdc', wallet, '--database', rebuilt)
        self.run_fin('import', 'revolut', bank, '--account-id', 'main', '--database', rebuilt)
        self.run_fin('operation', 'restore', resolutions, '--database', rebuilt, '--apply', '--yes')
        self.assertIn('Transfers to restore: 1', self.run_fin('transfer', 'restore', transfers, '--database', rebuilt).stdout)
        self.assertIn('transfer_changes=1', self.run_fin('transfer', 'restore', transfers, '--database', rebuilt, '--apply', '--yes').stdout)
        self.assertIn('already matching: 1', self.run_fin('transfer', 'restore', transfers, '--database', rebuilt).stdout)
        self.assertIn('assets:wallet:eur', self.run_fin('journal', 'export', '--database', rebuilt).stdout)
        self.assertIn('transfer_changes=1', self.run_fin('transfer', 'unlink', bank_op, '--yes').stdout)
        self.assertIn('equity:unclassified', self.run_fin('journal', 'export').stdout)

    def test_wallet_to_wallet_transfer_and_symbol_collisions(self):
        path = self.root / 'wallet-transfers.csv'
        path.write_text('Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n'
                        '2026-01-02 10:01:00,Wallet withdrawal,BTC,-0.12345678,,,EUR,0,0,supercharger_deposit,\n'
                        '2026-01-02 10:02:00,Wallet deposit,BTC,0.12345678,,,EUR,0,0,crypto_deposit,\n')
        self.run_fin('import', 'cdc', path)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        first = next(r['operation_id'] for r in rows if r['description'] == 'Wallet withdrawal')
        second = next(r['operation_id'] for r in rows if r['description'] == 'Wallet deposit')
        commodity = json.dumps({'kind': 'onchain-native', 'network': 'bitcoin'})
        self.run_fin('operation', 'resolve', first, 'primary', 'assets:wallet:one', commodity, '--symbol', 'BTC')
        self.run_fin('operation', 'resolve', second, 'primary', 'assets:wallet:two', commodity, '--symbol', 'BTC')
        self.assertIn('transfer_changes=1', self.run_fin('transfer', 'link', first, second, '--yes').stdout)
        journal = self.run_fin('journal', 'export').stdout
        self.assertIn('assets:wallet:one', journal)
        self.assertIn('assets:wallet:two', journal)
        self.assertIn('0.12345678 BTC', journal)
        self.assertNotIn('equity:unclassified', journal)
        self.assertIn('transfer_changes=1', self.run_fin('transfer', 'unlink', first, '--yes').stdout)
        self.assertNotIn('assets:wallet:one', self.run_fin('journal', 'export').stdout)

    def test_generic_csv_timestamp_setup(self):
        path = self.root / 'timestamp.csv'
        path.write_text('Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n'
                        '2025-06-09 17:56:16,Crv*Le Genre Urbain,EUR,-4.3,,,EUR,-4.3,-4.938386557,,\n')
        output = self.terminal(['import', path, '--provider', 'mycdc', '--account-id', 'main'], [
            ('Column containing the transaction date:', '1'),
            ('Column containing the merchant or description:', '2'),
            ('Column containing the amount charged to the account:', '4'),
            ('Account currency (e.g. USD):', 'EUR'),
            ('Account type:', '3'),
            ('Are outgoing amounts positive in the file?', '2'),
            ('Skip a summary row with an empty date?', '2'),
            ('Import these transactions?', '1'),
            ('IBAN (Enter to skip):', ''),
        ])
        self.assertIn('Detected date and time', output)
        row = json.loads(self.run_fin('operation', 'list', '--json').stdout)[0]
        self.assertEqual((row['booked_on'], row['amount'], row['description']),
                         ('2025-06-09', '-4.3', 'Crv*Le Genre Urbain'))
        self.assertIn('inserted=0', self.run_fin('import', path, '--provider', 'mycdc', '--account-id', 'main').stdout)

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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual({(r['description'], r['amount']) for r in rows}, {('Rent', '-120'), ('Salary', '950')})
        account = self.accounts()[0]
        self.assertEqual(account['ledger_account'], 'assets:bank:mybank:current:main')
        self.assertEqual(account['account_type'], 'current')
        self.assertIn('inserted=0', self.run_fin('import', csv_path, '--account-id', 'main', '--provider', 'mybank').stdout)
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--json').stdout)), 2)

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

    def test_portable_categories_roundtrip_with_different_event_ids(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        first = json.loads(self.run_fin('operation', 'list', '--search', 'Initial deposit', '--json').stdout)[0]['event_id']
        self.run_fin('cat', 'add', 'expenses:unused')
        self.run_fin('operation', 'category', 'set', 'income:salary', first)
        draft = self.run_fin('rule', 'from-category', first, 'deposit-rule', '--contains', 'Initial').stdout
        self.assertIn('Draft only', draft)
        self.assertNotIn('deposit-rule', self.run_fin('rule', 'list').stdout)
        self.run_fin('rule', 'from-category', first, 'deposit-rule', '--contains', 'Initial', '--save', '--yes')
        self.run_fin('rule', 'add', 'rule-only', 'unlikely-shop', 'expenses:rule-only')
        self.run_fin('rule', 'add', 'type-only-export', '--description', 'unlikely', '--set-type', 'bonus', '--automatic')
        self.run_fin('rule', 'add-evidence', 'evidence-export', 'withdrawal-reversal', '^unlikely', '--institution', 'revolut')
        self.run_fin('rule', 'add', 'mixed-export', '--description', 'unlikely', '--set-type', 'bonus', '--category', 'expenses:mixed')
        snapshot = self.root / 'categories-portable.json'
        catalog = self.root / 'catalog.json'
        rules = self.root / 'rules.json'
        catalog.write_text(self.run_fin('cat', 'export').stdout)
        rules.write_text(self.run_fin('rule', 'export').stdout)
        snapshot.write_text(self.run_fin('operation', 'export').stdout)
        c, r, exported = (json.loads(p.read_text()) for p in (catalog, rules, snapshot))
        self.assertEqual(set(c), {'version', 'categories'})
        self.assertEqual(set(r), {'version', 'rules'})
        self.assertEqual(set(exported), {'version', 'decisions', 'resolutions'})
        self.assertTrue(all(x['version'] == 2 for x in (c, r)))
        self.assertEqual(exported['version'], 5)
        self.assertTrue(all(d['movement_role'] == 'primary' for d in exported['decisions']))
        self.assertIn('expenses:unused', c['categories'])
        self.assertTrue(any(rule['name'] == 'deposit-rule' for rule in r['rules']))
        self.assertEqual(len(exported['decisions']), 1)
        older = self.root / 'category-v2.json'
        older.write_text(json.dumps({'version': 2, 'decisions': [{k: v for k, v in d.items() if k != 'movement_role'} for d in exported['decisions']]}))
        previous = self.root / 'transaction-v3.json'
        previous.write_text(json.dumps({'version': 3, 'decisions': [{k: v for k, v in d.items() if k != 'movement_role'} for d in exported['decisions']]}))
        previous_v4 = self.root / 'transaction-v4.json'
        previous_v4.write_text(json.dumps({'version': 4, 'decisions': exported['decisions']}))
        other = self.root / 'rebuilt.sqlite3'
        self.run_fin('init', '--database', other)
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx', '--database', other)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main', '--database', other)
        current = json.loads(self.run_fin('operation', 'list', '--database', other, '--search', 'Initial deposit', '--json').stdout)[0]
        self.assertNotEqual(current['event_id'], first)
        preview = self.run_fin('operation', 'restore', snapshot, '--database', other).stdout
        self.assertIn('Decisions to restore: 1', preview)
        self.assertIn('Preview only', preview)
        self.assertEqual(json.loads(self.run_fin('cat', 'list', '--database', other, '--json').stdout), [])
        self.assertIn('category_changes=1', self.run_fin('operation', 'restore', snapshot, '--database', other, '--apply', '--yes').stdout)
        current = json.loads(self.run_fin('operation', 'list', '--database', other, '--search', 'Initial deposit', '--json').stdout)[0]
        self.assertEqual(current['counter_account'], 'income:salary')
        self.assertIn('income:salary', json.loads(self.run_fin('cat', 'list', '--database', other, '--json').stdout))
        self.assertIn('category_changes=0', self.run_fin('operation', 'restore', older, '--database', other, '--apply', '--yes').stdout)
        self.assertIn('category_changes=0', self.run_fin('operation', 'restore', previous, '--database', other, '--apply', '--yes').stdout)
        self.assertIn('category_changes=0', self.run_fin('operation', 'restore', previous_v4, '--database', other, '--apply', '--yes').stdout)
        self.assertNotIn('expenses:unused', json.loads(self.run_fin('cat', 'list', '--database', other, '--json').stdout))
        self.run_fin('cat', 'restore', catalog, '--database', other, '--apply', '--yes')
        self.assertIn('expenses:rule-only', self.run_fin('rule', 'restore', rules, '--database', other).stdout)
        self.run_fin('rule', 'restore', rules, '--database', other, '--apply', '--yes')
        self.assertIn('expenses:rule-only', json.loads(self.run_fin('cat', 'list', '--database', other, '--json').stdout))
        self.assertIn('type-only-export', self.run_fin('rule', 'list', '--database', other).stdout)
        self.assertIn('evidence-export', self.run_fin('rule', 'list', '--database', other).stdout)
        self.assertIn('expenses:mixed', json.loads(self.run_fin('cat', 'list', '--database', other, '--json').stdout))
        category_rule = next(rule for rule in r['rules'] if rule['name'] == 'rule-only')
        missing_card = dict(category_rule, name='missing-card', action='settlement', target='missing-card-id')
        new_category = dict(category_rule, name='blocked-category', target='expenses:blocked')
        broken_rules = self.root / 'broken-rules.json'
        broken_rules.write_text(json.dumps({'version': 2, 'rules': [missing_card, new_category]}))
        blocked = self.run_fin('rule', 'restore', broken_rules, '--database', other, '--apply', '--yes', check=False)
        self.assertNotEqual(blocked.returncode, 0)
        self.assertIn('missing card', blocked.stdout)
        self.assertNotIn('expenses:blocked', json.loads(self.run_fin('cat', 'list', '--database', other, '--json').stdout))
        self.assertNotIn('blocked-category', self.run_fin('rule', 'list', '--database', other).stdout)
        self.assertIn('expenses:unused', json.loads(self.run_fin('cat', 'list', '--database', other, '--json').stdout))
        self.assertIn('deposit-rule', self.run_fin('rule', 'list', '--database', other).stdout)
        self.assertNotEqual(self.run_fin('cat', 'restore', snapshot, '--database', other, check=False).returncode, 0)
        legacy = self.root / 'legacy.json'
        legacy.write_text(json.dumps({'version': 1, 'categories': c['categories'], 'rules': r['rules'], 'decisions': exported['decisions']}))
        legacy_db = self.root / 'legacy.sqlite3'
        self.run_fin('init', '--database', legacy_db)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main', '--database', legacy_db)
        self.run_fin('operation', 'restore', legacy, '--database', legacy_db, '--apply', '--yes')
        self.assertNotIn('expenses:unused', json.loads(self.run_fin('cat', 'list', '--database', legacy_db, '--json').stdout))
        self.run_fin('rule', 'restore', legacy, '--database', legacy_db, '--apply', '--yes')
        self.assertIn('deposit-rule', self.run_fin('rule', 'list', '--database', legacy_db).stdout)
        self.run_fin('cat', 'restore', legacy, '--database', legacy_db, '--apply', '--yes')
        self.assertIn('expenses:unused', json.loads(self.run_fin('cat', 'list', '--database', legacy_db, '--json').stdout))
        self.assertIn('category_changes=0', self.run_fin('operation', 'restore', snapshot, '--database', other, '--apply', '--yes').stdout)

    def test_portable_transaction_types_roundtrip_and_conflict(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        first, second = (r['event_id'] for r in rows[:2])
        self.run_fin('operation', 'category', 'set', 'expenses:food', first)
        self.run_fin('operation', 'type', 'set', 'fee', first, second)
        snapshot = self.root / 'transaction-decisions.json'
        exported = json.loads(self.run_fin('operation', 'export').stdout)
        self.assertEqual(len(exported['decisions']), 2)
        self.assertEqual(sum('account' in d for d in exported['decisions']), 1)
        self.assertTrue(all(d['type_override'] == 'fee' for d in exported['decisions']))
        snapshot.write_text(json.dumps(exported))

        other = self.root / 'rebuilt-types.sqlite3'
        self.run_fin('init', '--database', other)
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx', '--database', other)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main', '--database', other)
        preview = self.run_fin('operation', 'restore', snapshot, '--database', other).stdout
        self.assertIn('Type overrides to restore: 2', preview)
        current = json.loads(self.run_fin('operation', 'list', '--database', other, '--institution', 'revolut', '--json').stdout)
        target = next(r for r in current if r['description'] == rows[0]['description'])
        self.run_fin('operation', 'type', 'set', 'bonus', target['event_id'], '--database', other)
        blocked = self.run_fin('operation', 'restore', snapshot, '--database', other, '--apply', '--yes', check=False)
        self.assertNotEqual(blocked.returncode, 0)
        self.assertIn('existing type override differs', blocked.stdout)
        self.assertEqual(json.loads(self.run_fin('cat', 'list', '--database', other, '--json').stdout), [])
        self.run_fin('operation', 'type', 'reset', target['event_id'], '--database', other)
        applied = self.run_fin('operation', 'restore', snapshot, '--database', other, '--apply', '--yes').stdout
        self.assertIn('category_changes=1 type_changes=2', applied)
        restored = json.loads(self.run_fin('operation', 'list', '--database', other, '--institution', 'revolut', '--json').stdout)
        self.assertEqual(sum(r['movement_type_override'] == 'fee' for r in restored), 2)
        self.assertEqual(sum(r['counter_account'] == 'expenses:food' for r in restored), 1)
        self.assertIn('category_changes=0 type_changes=0', self.run_fin('operation', 'restore', snapshot, '--database', other, '--apply', '--yes').stdout)

    def test_portable_restore_rejects_indistinguishable_duplicates(self):
        csv = self.csv.read_text().replace('Repeated transfer,10.00,0.00,EUR,COMPLETED,160.00', 'Repeated transfer,0.00,0.00,EUR,COMPLETED,150.00')
        csv = csv.replace('Repeated transfer,10.00,0.00,EUR,COMPLETED,170.00', 'Repeated transfer,0.00,0.00,EUR,COMPLETED,150.00').replace(',69.50', ',49.50')
        self.csv.write_text(csv)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated transfer', '--json').stdout)
        self.assertEqual(len(rows), 2)
        self.run_fin('operation', 'category', 'set', 'income:other', rows[0]['event_id'])
        snapshot = self.root / 'duplicates.json'
        snapshot.write_text(self.run_fin('operation', 'export').stdout)
        result = self.run_fin('operation', 'restore', snapshot, '--apply', '--yes', check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('ambiguous duplicate source evidence', result.stdout)

    def test_rule_from_category_can_be_applied_to_other_transactions(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)
        first, second = (r['event_id'] for r in rows)
        self.run_fin('operation', 'category', 'set', 'income:other', first)
        self.run_fin('rule', 'from-category', first, 'repeated', '--contains', 'Repeated', '--save', '--yes')
        self.run_fin('rule', 'apply', 'repeated', '--yes')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)
        self.assertEqual({r['counter_account'] for r in rows}, {'income:other'})
        self.assertEqual({r['event_id'] for r in rows}, {first, second})

    def test_rule_from_category_escapes_literal_metacharacters(self):
        csv = self.csv.read_text().replace('Repeated transfer,10.00,0.00,EUR,COMPLETED,160.00',
                                           'Shop*One,10.00,0.00,EUR,COMPLETED,160.00')
        csv = csv.replace('Repeated transfer,10.00,0.00,EUR,COMPLETED,170.00',
                          'ShopXOne,10.00,0.00,EUR,COMPLETED,170.00')
        self.csv.write_text(csv)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        seed = json.loads(self.run_fin('operation', 'list', '--search', 'Shop*One', '--json').stdout)[0]['event_id']
        self.run_fin('operation', 'category', 'set', 'expenses:shopping', seed)
        self.run_fin('rule', 'from-category', seed, 'literal-shop', '--contains', 'Shop*One', '--save', '--yes')
        matches = json.loads(self.run_fin('rule', 'preview', 'literal-shop', '--json').stdout)
        self.assertEqual(len(matches), 1)
        self.assertEqual(matches[0]['transaction']['event_id'], seed)

    def test_portable_restore_blocks_conflicts_and_missing_evidence(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        first = json.loads(self.run_fin('operation', 'list', '--search', 'Initial deposit', '--json').stdout)[0]['event_id']
        self.run_fin('operation', 'category', 'set', 'income:salary', first)
        snapshot = self.root / 'categories-portable.json'
        exported = json.loads(self.run_fin('operation', 'export').stdout)
        exported['decisions'][0]['account'] = 'expenses:should-not-appear'
        exported['decisions'][0]['payload_sha256'] = '0' * 64
        snapshot.write_text(json.dumps(exported))
        result = self.run_fin('operation', 'restore', snapshot, '--apply', '--yes', check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('source evidence differs', result.stdout)
        self.assertNotIn('expenses:should-not-appear', self.run_fin('cat', 'list').stdout)
        exported['decisions'][0]['payload_sha256'] = json.loads(self.run_fin('operation', 'export').stdout)['decisions'][0]['payload_sha256']
        exported['decisions'].append(dict(exported['decisions'][0]))
        snapshot.write_text(json.dumps(exported))
        result = self.run_fin('operation', 'restore', snapshot, '--apply', '--yes', check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('duplicate transaction identities', result.stdout)
        self.assertNotIn('expenses:should-not-appear', self.run_fin('cat', 'list').stdout)
        exported['decisions'] = [exported['decisions'][0]]
        exported['decisions'][0]['movement_role'] = 'to'
        snapshot.write_text(json.dumps(exported))
        result = self.run_fin('operation', 'restore', snapshot, '--apply', '--yes', check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('unsupported movement role', result.stdout)
        self.assertNotIn('expenses:should-not-appear', self.run_fin('cat', 'list').stdout)
        del exported['decisions'][0]['movement_role']
        snapshot.write_text(json.dumps(exported))
        self.assertNotEqual(self.run_fin('operation', 'restore', snapshot, check=False).returncode, 0)
        exported['decisions'][0]['movement_role'] = 'primary'
        snapshot.write_text(json.dumps(exported))
        with sqlite3.connect(self.root / 'data/fin/fin.sqlite3') as db:
            db.execute('DELETE FROM operation_movement_sources WHERE movement_id IN '
                       '(SELECT id FROM operation_movements WHERE anchor_event_id=?)', (first,))
        result = self.run_fin('operation', 'restore', snapshot, '--apply', '--yes', check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('primary movement or its source link is missing', result.stdout)
        self.assertNotEqual(self.run_fin('operation', 'export', check=False).returncode, 0)

    def test_ledger_wrapper_streams_journal_and_forwards_arguments(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        expected = self.run_fin('journal', 'export').stdout
        bindir = self.root / 'bin'
        bindir.mkdir()
        executable = bindir / 'hledger'
        executable.write_text("#!/usr/bin/python3\nimport json, sys\nprint(json.dumps({'args': sys.argv[1:], 'journal': sys.stdin.read()}))\n")
        executable.chmod(0o755)
        self.env['PATH'] = str(bindir)
        result = self.run_fin('ledger', 'balance', 'expenses', '-Y', '--tree')
        actual = json.loads(result.stdout)
        self.assertEqual(actual['args'], ['-f', '-', 'balance', 'expenses', '-Y', '--tree'])
        self.assertEqual(actual['journal'], expected)
        database = self.root / 'data/fin/fin.sqlite3'
        result = self.run_fin('ledger', '--database', database, 'balance', '--help')
        self.assertEqual(json.loads(result.stdout)['args'], ['-f', '-', 'balance', '--help'])
        executable.write_text('#!/usr/bin/python3\nimport sys\nsys.exit(7)\n')
        self.assertEqual(self.run_fin('ledger', 'balance', check=False).returncode, 7)
        executable.unlink()
        missing = self.run_fin('ledger', 'balance', check=False)
        self.assertNotEqual(missing.returncode, 0)
        self.assertIn('hledger', missing.stderr)

    def test_transaction_status_counts_and_percentages(self):
        empty = json.loads(self.run_fin('operation', 'status', '--json').stdout)
        self.assertEqual(empty['total'], 0)
        self.assertIsNone(empty['classified_percent'])
        self.assertIsNone(empty['categorized_percent'])
        self.assertEqual((empty['by_category'], empty['by_type']), ([], []))
        self.assertIn('n/a', self.run_fin('operation', 'status').stdout)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        event = json.loads(self.run_fin('operation', 'list', '--json').stdout)[0]['event_id']
        self.run_fin('operation', 'category', 'set', 'expenses:food', event)
        status = json.loads(self.run_fin('operation', 'status', '--json').stdout)
        self.assertEqual((status['total'], status['classified'], status['categorized'], status['unclassified']), (4, 1, 1, 3))
        self.assertEqual((status['classified_percent'], status['categorized_percent']), (25, 25))
        self.assertEqual((status['transfers'], status['settlements'], status['reversals']), (0, 0, 0))
        self.assertEqual(status['by_category'], [{'account': 'expenses:food', 'count': 1}])
        self.run_fin('operation', 'type', 'set', 'bonus', event)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        counts = {}
        for row in rows:
            kind = row['movement_type'] or 'unknown'
            counts[kind] = counts.get(kind, 0) + 1
        status = json.loads(self.run_fin('operation', 'status', '--json').stdout)
        self.assertEqual(status['by_type'], [{'type': kind, 'count': count} for kind, count in sorted(counts.items())])
        self.assertIn({'type': 'bonus', 'count': 1}, status['by_type'])
        output = self.run_fin('operation', 'status').stdout
        self.assertIn('Classified: 1 (25.0%)', output)
        self.assertIn('expenses:food: 1', output)
        self.assertIn('bonus: 1', output)
        self.run_fin('operation', 'category', 'set', 'equity:unclassified', event)
        self.assertEqual(json.loads(self.run_fin('operation', 'status', '--json').stdout)['classified'], 0)

    def test_category_catalog_and_init(self):
        self.run_fin('cat', 'add', 'expenses:custom')
        self.assertNotEqual(self.run_fin('cat', 'add', 'assets:bank', check=False).returncode, 0)
        self.terminal(['cat', 'init'], [('Select all', '1 3'), ('Add these categories?', 'n')])
        self.assertEqual(json.loads(self.run_fin('cat', 'list', '--json').stdout), ['expenses:custom'])
        self.terminal(['cat', 'init'], [('Select all', '1 3'), ('Add these categories?', 'y')])
        self.assertEqual(json.loads(self.run_fin('cat', 'list', '--json').stdout),
                         ['expenses:custom', 'expenses:food:groceries', 'expenses:home'])
        self.terminal(['cat', 'init'], [('Select all', 'all'), ('Add these categories?', 'y')])
        self.assertIn('already exist', self.terminal(['cat', 'init'], []))

    def test_category_delete(self):
        self.run_fin('cat', 'add', 'expenses:mistake')
        self.run_fin('cat', 'add', 'expenses:mistake:child')
        self.run_fin('cat', 'delete', 'expenses:mistake')
        self.assertEqual(json.loads(self.run_fin('cat', 'list', '--json').stdout), ['expenses:mistake:child'])
        missing = self.run_fin('cat', 'delete', 'expenses:mistake', check=False)
        self.assertNotEqual(missing.returncode, 0)
        self.assertIn('unknown category', missing.stderr)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        event = json.loads(self.run_fin('operation', 'list', '--json').stdout)[0]['event_id']
        self.run_fin('cat', 'add', 'expenses:used')
        self.run_fin('operation', 'category', 'set', 'expenses:used', event)
        used = self.run_fin('cat', 'delete', 'expenses:used', check=False)
        self.assertNotEqual(used.returncode, 0)
        self.assertIn('assigned to transactions', used.stderr)
        self.run_fin('operation', 'category', 'set', 'equity:unclassified', event)
        self.run_fin('cat', 'delete', 'expenses:used')
        self.assertTrue(any(change['account'] == 'expenses:used' for change in json.loads(self.run_fin('operation', 'category', 'history').stdout)))
        self.run_fin('cat', 'add', 'expenses:rule')
        self.run_fin('rule', 'add', 'test-rule', 'shop', 'expenses:rule')
        in_rule = self.run_fin('cat', 'delete', 'expenses:rule', check=False)
        self.assertNotEqual(in_rule.returncode, 0)
        self.assertIn('used by a rule', in_rule.stderr)
        self.run_fin('rule', 'delete', 'test-rule', '--yes')
        self.run_fin('cat', 'delete', 'expenses:rule')

    def test_category_set_changes_and_resets(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        event = json.loads(self.run_fin('operation', 'list', '--json').stdout)[0]['event_id']
        self.assertIn('category_changes=1', self.run_fin('operation', 'category', 'set', 'expenses:food', event).stdout)
        self.assertIn('category_changes=1', self.run_fin('operation', 'category', 'set', 'expenses:coffee', event).stdout)
        self.assertIn('category_changes=0', self.run_fin('operation', 'category', 'set', 'expenses:coffee', event).stdout)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(next(r for r in rows if r['event_id'] == event)['counter_account'], 'expenses:coffee')
        self.assertNotEqual(self.run_fin('operation', 'category', 'set', 'assets:bank', event, check=False).returncode, 0)
        self.assertIn('category_changes=1', self.run_fin('operation', 'category', 'set', 'equity:unclassified', event).stdout)
        self.assertEqual(len(json.loads(self.run_fin('operation', 'category', 'history').stdout)), 3)

    def test_category_set_multiple_transactions_is_atomic(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        first, second = (row['event_id'] for row in rows[:2])
        self.assertIn('category_changes=2', self.run_fin('operation', 'category', 'set', 'expenses:food', first, second).stdout)
        self.assertIn('category_changes=0', self.run_fin('operation', 'category', 'set', 'expenses:food', first, second).stdout)
        before = self.run_fin('operation', 'list', '--json').stdout
        history = self.run_fin('operation', 'category', 'history').stdout
        self.assertNotEqual(self.run_fin('operation', 'category', 'set', 'expenses:other', first, '9999999', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('operation', 'category', 'set', 'expenses:other', first, first, check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('operation', 'category', 'set', 'expenses:other', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('operation', 'category', 'set', first, 'expenses:other', check=False).returncode, 0)
        self.assertEqual(self.run_fin('operation', 'list', '--json').stdout, before)
        self.assertEqual(self.run_fin('operation', 'category', 'history').stdout, history)
        self.assertIn('category_changes=2', self.run_fin('operation', 'category', 'set', 'equity:unclassified', first, second).stdout)
        after = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertTrue(all(row['counter_account'] == 'equity:unclassified' for row in after if row['event_id'] in (first, second)))

    def test_category_set_from_filtered_transaction_list(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        selected = self.run_fin('operation', 'list', '--search', 'Repeated', '--decision', 'unclassified', '--json').stdout
        ids = {row['event_id'] for row in json.loads(selected)}
        self.assertEqual(len(ids), 2)

        def set_from_input(contents, *args):
            return subprocess.run([FIN, 'operation', 'category', 'set', *map(str, args)],
                                  input=contents, env=self.env, capture_output=True, text=True)

        result = set_from_input(selected, 'expenses:food', '-')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('category_changes=2', result.stdout)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual({r['event_id'] for r in rows if r['counter_account'] == 'expenses:food'}, ids)
        self.assertEqual(set_from_input('[]', 'expenses:food', '-').stdout.strip(), 'category_changes=0')
        self.assertNotEqual(set_from_input('[]', 'assets:invalid', '-').returncode, 0)

        before = self.run_fin('operation', 'category', 'history').stdout
        first = next(iter(ids))
        for contents, args in [('', ('expenses:other', '-')),
                               ('not json', ('expenses:other', '-')),
                               ('[{}]', ('expenses:other', '-')),
                               ('[{"event_id":9007199254740992}]', ('expenses:other', '-')),
                               (selected, ('expenses:other', first, '-')),
                               (selected, ('expenses:other', '-', '-'))]:
            self.assertNotEqual(set_from_input(contents, *args).returncode, 0)
        self.assertEqual(self.run_fin('operation', 'category', 'history').stdout, before)

    def test_interactive_classification_review(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json', '--search', 'Repeated').stdout)
        first, second = (row['event_id'] for row in rows)
        self.assertEqual(json.loads(self.run_fin('category', 'list', '--json').stdout), [])
        output = self.terminal(['operation', 'category', 'review', str(first)], [
            ('Category name or number', 'expenses:food'),
            ('Select matches', 'all'),
            ('[Y/n]:', 'yes'),
        ])
        self.assertIn('category_changes=2', output)
        self.assertEqual(self.run_fin('category', 'list').stdout, 'expenses:food\n')
        self.assertEqual(self.run_fin('cat', 'list').stdout, 'expenses:food\n')
        self.assertEqual(json.loads(self.run_fin('category', 'list', '--json').stdout), ['expenses:food'])
        rows = json.loads(self.run_fin('operation', 'list', '--json', '--search', 'Repeated').stdout)
        self.assertEqual({x['counter_account'] for x in rows}, {'expenses:food'})
        self.assertEqual(len(json.loads(self.run_fin('operation', 'category', 'history').stdout)), 2)
        journal = self.run_fin('journal', 'export').stdout
        self.assertIn('expenses:food', journal)

    def test_interactive_classification_cancel_and_select_none(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json', '--search', 'Repeated').stdout)
        first, second = (row['event_id'] for row in rows)
        before = self.run_fin('operation', 'review', '--all', '--json').stdout
        output = self.terminal(['operation', 'category', 'review', str(first)], [
            ('Category name or number', 'expenses:food'),
            ('Select matches', 'none'),
            ('[Y/n]:', 'no'),
        ])
        self.assertIn('Nothing classified.', output)
        self.assertEqual(self.run_fin('operation', 'review', '--all', '--json').stdout, before)
        output = self.terminal(['operation', 'category', 'review', str(first)], [
            ('Category name or number', 'expenses:food'),
            ('Select matches', 'none'),
            ('[Y/n]:', 'yes'),
        ])
        self.assertIn('category_changes=1', output)
        rows = json.loads(self.run_fin('operation', 'list', '--json', '--search', 'Repeated').stdout)
        self.assertEqual({r['event_id']: r['counter_account'] for r in rows},
                         {first: 'expenses:food', second: 'equity:unclassified'})

    def test_category_review_search_and_selection(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        seed = json.loads(self.run_fin('operation', 'list', '--search', 'Initial deposit', '--json').stdout)[0]['event_id']
        output = self.terminal(['operation', 'category', 'review', seed], [
            ('Category name or number', 'income:other'),
            ('Select matches', 'search'),
            ('Search description', 'repeated'),
            ('Select matches', '1'),
            ('Select matches', 'search'),
            ('Search description', 'another bank'),
            ('Select matches', '1'),
            ('Select matches', '1'),
            ('Select matches', 'done'),
            ('[Y/n]:', 'yes'),
        ])
        self.assertIn('category_changes=2', output)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(sum(r['counter_account'] == 'income:other' for r in rows), 2)
        self.assertTrue(any(r['description'] == 'Transfer to another bank' and
                            r['counter_account'] == 'equity:unclassified' for r in rows))

    def test_category_review_hides_already_matching_categories(self):
        line = 'Transfer,Current,2026-01-02 10:00:00,2026-01-02 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,170.00'
        more = [line.replace('Repeated transfer,', 'Repeated transfers,').replace(',170.00', ',180.00'),
                line.replace('Repeated transfer,', 'Repeated transferring,').replace(',170.00', ',190.00')]
        self.csv.write_text(self.csv.read_text().replace(line, '\n'.join([line, *more])).replace(',69.50', ',89.50'))
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)
        exact = [r['event_id'] for r in rows if r['description'] == 'Repeated transfer']
        fuzzy_same = next(r['event_id'] for r in rows if r['description'] == 'Repeated transfers')
        fuzzy_other = next(r['event_id'] for r in rows if r['description'] == 'Repeated transferring')
        for event in [*exact, fuzzy_same]:
            self.run_fin('operation', 'category', 'set', 'expenses:food', event)
        self.run_fin('operation', 'category', 'set', 'expenses:other', fuzzy_other)
        history_before = json.loads(self.run_fin('operation', 'category', 'history').stdout)
        output = self.terminal(['operation', 'category', 'review', exact[0]], [
            ('Category name or number', 'expenses:food'),
            ('Select matches', 'search'), ('Search description', 'Repeated'),
            ('Select matches', 'all'), ('[Y/n]:', 'yes'),
        ])
        self.assertNotIn(f'#{exact[1]} ', output)
        self.assertNotIn(f'#{fuzzy_same} ', output)
        self.assertIn(f'#{fuzzy_other} ', output)
        self.assertIn('category_changes=1', output)
        history_after = json.loads(self.run_fin('operation', 'category', 'history').stdout)
        self.assertEqual(len(history_after), len(history_before) + 1)
        self.assertEqual(history_after[-1]['event_id'], fuzzy_other)
        output = self.terminal(['operation', 'category', 'review', exact[0]], [
            ('Category name or number', 'expenses:food'), ('Select matches', 'all'),
        ])
        self.assertIn('No category changes.', output)
        self.assertEqual(len(json.loads(self.run_fin('operation', 'category', 'history').stdout)), len(history_after))

    def test_category_review_suggestions_exclude_categorized_matches(self):
        line = 'Transfer,Current,2026-01-02 10:00:00,2026-01-02 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,170.00'
        extra = line.replace('Repeated transfer,', 'Repeated transfers,').replace(',170.00', ',180.00')
        self.csv.write_text(self.csv.read_text().replace(line, line + '\n' + extra).replace(',69.50', ',79.50'))
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)
        exact = [r['event_id'] for r in rows if r['description'] == 'Repeated transfer']
        fuzzy = next(r['event_id'] for r in rows if r['description'] == 'Repeated transfers')
        self.run_fin('operation', 'category', 'set', 'expenses:other', exact[1])
        self.run_fin('operation', 'category', 'set', 'expenses:other', fuzzy)
        output = self.terminal(['operation', 'category', 'review', exact[0]], [
            ('Category name or number', 'expenses:food'),
            ('Select matches', 'all'), ('[Y/n]:', 'yes'),
        ])
        self.assertNotIn(f'#{exact[1]} ', output)
        self.assertNotIn(f'#{fuzzy} ', output)
        self.assertIn('category_changes=1', output)
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)
        self.assertEqual(sum(r['counter_account'] == 'expenses:other' for r in rows), 2)

    def test_category_review_fuzzy_is_shown_and_selectable(self):
        csv = self.csv.read_text().replace('Repeated transfer,10.00,0.00,EUR,COMPLETED,170.00',
                                           'Repeated transfers,10.00,0.00,EUR,COMPLETED,170.00')
        self.csv.write_text(csv)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated transfer', '--json').stdout)
        seed = next(r['event_id'] for r in rows if r['description'] == 'Repeated transfer')
        output = self.terminal(['operation', 'category', 'review', seed], [
            ('Category name or number', 'income:other'),
            ('Select matches', '1'), ('Select matches', 'done'), ('[Y/n]:', 'yes'),
        ])
        self.assertIn('fuzzy ', output)
        self.assertIn('Repeated transfers', output)
        self.assertIn('category_changes=2', output)

    def test_category_review_all_selects_exact_and_fuzzy(self):
        line = 'Transfer,Current,2026-01-02 10:00:00,2026-01-02 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,170.00'
        self.csv.write_text(self.csv.read_text().replace(line, line + '\n' + line.replace('Repeated transfer,', 'Repeated transfers,').replace(',170.00', ',180.00')).replace(',69.50', ',79.50'))
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated transfer', '--json').stdout)
        seed = next(r['event_id'] for r in rows if r['description'] == 'Repeated transfer')
        output = self.terminal(['operation', 'category', 'review', seed], [
            ('Category name or number', 'income:other'), ('Select matches', 'all'), ('[Y/n]:', 'yes'),
        ])
        self.assertIn('fuzzy ', output)
        self.assertIn('category_changes=3', output)
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated transfers', '--json').stdout)
        self.assertEqual(rows[0]['counter_account'], 'income:other')

    def test_category_review_ranges_toggle_and_validate(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        seed = json.loads(self.run_fin('operation', 'list', '--search', 'Initial deposit', '--json').stdout)[0]['event_id']
        output = self.terminal(['operation', 'category', 'review', seed], [
            ('Category name or number', 'income:other'),
            ('Select matches', 'search'), ('Search description', 'repeated'),
            ('Select matches', '2-1'), ('Select matches', '1-2,2'),
            ('Select matches', '1-2'), ('Select matches', '1-2'),
            ('Select matches', '1-1,2'), ('Select matches', 'done'), ('[Y/n]:', 'yes'),
        ])
        self.assertIn('ascending ranges', output)
        self.assertIn('Select each match only once', output)
        self.assertIn('category_changes=3', output)

    def test_category_review_all_changes_categorized_search_results(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        seed = next(r['event_id'] for r in rows if r['description'] == 'Initial deposit')
        others = [r['event_id'] for r in rows if r['description'] == 'Repeated transfer']
        decisions = self.root / 'reviewed.json'
        decisions.write_text(json.dumps([{'event_id': e, 'account': 'income:salary', 'reason': 'Reviewed'} for e in others]))
        self.run_fin('operation', 'category', 'apply', decisions)
        output = self.terminal(['operation', 'category', 'review', seed], [
            ('Category name or number', 'income:other'),
            ('Select matches', 'search'), ('Search description', 'repeated'),
            ('Select matches', 'all'), ('[Y/n]:', 'yes'),
        ])
        self.assertIn('income:salary', output)
        self.assertIn('category_changes=3', output)
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'repeated', '--json').stdout)
        self.assertEqual({r['counter_account'] for r in rows}, {'income:other'})

    def test_category_rereview_changes_selected_categorized_matches(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)
        first, second = (r['event_id'] for r in rows)
        self.run_fin('operation', 'category', 'set', 'expenses:food', first)
        self.run_fin('operation', 'category', 'set', 'expenses:food', second)
        output = self.terminal(['operation', 'category', 'review', first], [
            ('Category name or number', 'expenses:coffee'),
            ('Select matches', 'search'), ('Search description', 'Repeated'),
            ('Select matches', '1'), ('Select matches', 'done'), ('[Y/n]:', 'no'),
        ])
        self.assertIn('expenses:food', output)
        self.assertIn('Nothing classified', output)
        self.assertEqual({r['counter_account'] for r in json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)}, {'expenses:food'})
        output = self.terminal(['operation', 'category', 'review', first], [
            ('Category name or number', 'expenses:coffee'),
            ('Select matches', 'search'), ('Search description', 'Repeated'),
            ('Select matches', '1'), ('Select matches', 'done'), ('[Y/n]:', 'yes'),
        ])
        self.assertIn('category_changes=2', output)
        self.assertEqual({r['counter_account'] for r in json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)}, {'expenses:coffee'})

    def test_category_similar_is_read_only_and_reports_conflicts(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json', '--search', 'Repeated').stdout)
        self.assertEqual(len(rows), 2)
        first, second = (row['event_id'] for row in rows)
        self.assertNotEqual(self.run_fin('operation', 'category', 'similar', first, check=False).returncode, 0)
        decisions = self.root / 'decisions.json'
        decisions.write_text(json.dumps([{'event_id': first, 'account': 'expenses:food', 'reason': 'Reviewed'}]))
        self.run_fin('operation', 'category', 'apply', decisions)
        before = self.run_fin('operation', 'review', '--all', '--json').stdout
        preview = json.loads(self.run_fin('operation', 'category', 'similar', first, '--json').stdout)
        self.assertEqual(preview['example']['account'], 'expenses:food')
        self.assertEqual([r['event_id'] for r in preview['candidates']], [second])
        self.assertEqual(preview['conflicts'], [])
        text = self.run_fin('operation', 'category', 'similar', first).stdout
        self.assertIn('Matching unclassified transactions', text)
        self.assertIn('expenses:food', text)
        self.assertIn(str(second), text)
        self.assertEqual(self.run_fin('operation', 'review', '--all', '--json').stdout, before)
        decisions.write_text(json.dumps([{'event_id': second, 'account': 'expenses:other', 'reason': 'Reviewed'}]))
        self.run_fin('operation', 'category', 'apply', decisions)
        preview = json.loads(self.run_fin('operation', 'category', 'similar', first, '--json').stdout)
        self.assertEqual(preview['candidates'], [])
        self.assertEqual([r['event_id'] for r in preview['conflicts']], [second])
        self.assertNotEqual(self.run_fin('operation', 'category', 'similar', '999999', check=False).returncode, 0)

    def test_transaction_list_filters_and_reviewed_rows(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        accounts = json.loads(self.run_fin('account', 'list', '--json').stdout)
        account_id = accounts[0]['id']
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(len(rows), 4)
        self.assertEqual([x['booked_on'] for x in rows],
                         ['2026-01-03', '2026-01-02', '2026-01-02', '2026-01-01'])
        self.assertTrue(all(x['account_id'] == account_id for x in rows))
        self.assertEqual(len({x['event_id'] for x in rows}), 4)
        selected = json.loads(self.run_fin('operation', 'list', '--json', '--account', account_id,
                                           '--from', '2026-01-02', '--to', '2026-01-02',
                                           '--search', 'REPEATED').stdout)
        self.assertEqual(len(selected), 2)
        self.assertTrue(all(x['description'] == 'Repeated transfer' for x in selected))
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--json', '--limit', '1').stdout)), 1)
        self.assertEqual(json.loads(self.run_fin('operation', 'list', '--json', '--institution', 'other').stdout), [])
        text = self.run_fin('operation', 'list', '--limit', '1').stdout
        self.assertIn('ACCOUNT ID', text)
        self.assertIn('INSTITUTION', text)
        self.assertIn('revolut', text)
        self.assertEqual(rows[0]['institution'], 'revolut')
        self.assertIn(account_id, text)
        for args in [('--account', 'unknown'), ('--from', '2026-02-30'),
                     ('--from', '2026-01-03', '--to', '2026-01-01'), ('--limit', '0')]:
            self.assertNotEqual(self.run_fin('operation', 'list', *args, check=False).returncode, 0)
        decisions = self.root / 'transaction-category.json'
        decisions.write_text(json.dumps([{'event_id': rows[0]['event_id'],
                                          'account': 'expenses:shopping', 'reason': 'Reviewed'}]))
        self.run_fin('operation', 'category', 'apply', decisions)
        reviewed = json.loads(self.run_fin('operation', 'list', '--json', '--limit', '1').stdout)
        self.assertEqual(reviewed[0]['counter_account'], 'expenses:shopping')
        self.assertEqual(reviewed[0]['reason'], 'Reviewed')
        table = self.run_fin('operation', 'list', '--limit', '1').stdout
        self.assertIn('COUNTER-ACCOUNT', table)
        self.assertIn('expenses:shopping', table)
        selected = json.loads(self.run_fin('operation', 'list', '--decision', 'category', '--json').stdout)
        self.assertEqual([r['event_id'] for r in selected], [rows[0]['event_id']])
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--decision', 'unclassified', '--json').stdout)), 3)
        self.assertEqual([r['event_id'] for r in json.loads(self.run_fin('operation', 'list', '--category', 'expenses:shopping', '--json').stdout)], [rows[0]['event_id']])
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--counter-account', 'equity:unclassified', '--json').stdout)), 3)
        self.assertEqual(json.loads(self.run_fin('operation', 'list', '--category', 'expenses', '--json').stdout), [])
        self.assertEqual(json.loads(self.run_fin('operation', 'list', '--category', 'expenses:shopping', '--decision', 'unclassified', '--json').stdout), [])
        self.assertNotEqual(self.run_fin('operation', 'list', '--decision', 'invalid', check=False).returncode, 0)

    def test_operation_list_shows_one_row_per_operation(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        other = self.root / 'other.csv'
        other.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                         'Transfer,Current,2026-01-02 10:00:00,2026-01-02 10:00:01,Transfer,-10.00,0.00,EUR,COMPLETED,0.00\n')
        self.run_fin('import', 'revolut', other, '--account-id', 'other')
        cdc = self.root / 'cdc.csv'
        cdc.write_text('Timestamp (UTC),Transaction Description,Currency,Amount,To Currency,To Amount,Native Currency,Native Amount,Native Amount (in USD),Transaction Kind,Transaction Hash\n'
                       '2025-06-09 17:56:16,Reward,CRO,1.23456789,,,EUR,0.10,0.12,referral_card_cashback,\n'
                       '2025-06-09 17:56:17,Exchange,CRO,-2,EUR,0.20,EUR,0.20,0.23,crypto_viban_exchange,\n')
        self.run_fin('import', 'cdc', cdc)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(len(rows), 7)
        exchange = next(r for r in rows if r['institution'] == 'cdc' and r['description'] == 'Exchange')
        self.assertEqual(sorted(m['role'] for m in exchange['movements']), ['from', 'to'])
        self.assertEqual(sorted(m['quantity'] for m in exchange['movements']), ['-2', '0.2'])
        self.assertIsNone(exchange['event_id'])
        self.assertIsNone(exchange['account_id'])
        self.assertEqual([m['account'] for m in exchange['movements']], [None, None])
        self.assertEqual(len(exchange['movement_rows']), 0)
        table = self.run_fin('operation', 'list').stdout
        self.assertIn('AMOUNTS', table)
        self.assertIn('0.2 EUR → -2 CRO', table)
        self.assertIn('unresolved', table)
        self.assertIn('ID', table)
        reward = next(r for r in rows if r['description'] == 'Reward')
        self.assertEqual(reward['amount'], '1.23456789')
        self.assertIsNone(reward['currency'])
        self.assertEqual(reward['movements'][0]['commodity']['code'], 'CRO')
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--search', 'Exchange', '--json').stdout)), 1)
        self.assertNotEqual(self.run_fin('operation', 'list', '--movements', check=False).returncode, 0)
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--institution', 'cdc', '--from', '2025-06-09', '--to', '2025-06-09', '--json').stdout)), 2)
        self.assertEqual(json.loads(self.run_fin('operation', 'list', '--institution', 'cdc', '--from', '2025-06-10', '--json').stdout), [])
        bank = [r for r in rows if r['institution'] == 'revolut']
        self.assertTrue(all(len(r['movements']) == 1 for r in bank))
        report = json.loads(self.run_fin('transfer', 'candidates', '--json').stdout)
        first, second = report['candidates'][0]['first']['event_id'], report['candidates'][0]['second']['event_id']
        answers = [('Current pair or batch choice [c/r/s/a/d/q]:', 'c'),
                   ('Current pair or batch choice [c/r/s/a/d/q]:', 'd'),
                   ('Apply these decisions? [Y/n]:', 'y')]
        self.assertIn('transfer_changes=1', self.terminal(['transfer', 'review'], answers))
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(len(rows), 6)
        grouped = next(r for r in rows if r['event_id'] == first)
        self.assertEqual(len(grouped['movements']), 2)
        self.assertEqual(len(grouped['movement_rows']), 2)
        accounts = {m['account_id'] for m in grouped['movement_rows']}
        self.assertEqual(len(accounts), 2)
        for account in accounts:
            filtered = json.loads(self.run_fin('operation', 'list', '--account', account, '--json').stdout)
            self.assertIn(grouped['operation_id'], [r['operation_id'] for r in filtered])
        self.assertIn('Operation movements', self.run_fin('operation', 'show', grouped['operation_id']).stdout)
        for command in [('operation', 'category', 'set', 'expenses:test'), ('operation', 'type', 'set', 'fee')]:
            refused = self.run_fin(*command, grouped['operation_id'], check=False)
            self.assertNotEqual(refused.returncode, 0)
            self.assertIn('multiple movements', refused.stderr)
        self.assertIn('assets:bank:revolut', self.run_fin('operation', 'list').stdout)
        bank_event = next(r['event_id'] for r in rows if r['institution'] == 'revolut')
        self.assertIn(f'Operation: #{bank_event}', self.run_fin('operation', 'show', bank_event).stdout)
        crypto_event = next(e['id'] for e in json.loads(self.run_fin('db', 'events').stdout)
                            if e['venue'] == 'cdc')
        shown = self.run_fin('operation', 'show', crypto_event).stdout
        self.assertIn('This source event has no bank movement', shown)
        self.assertIn('account not assigned in operation', shown)

    def test_operation_show_uses_operation_id(self):
        db = self.root / 'data/fin/fin.sqlite3'
        with sqlite3.connect(db) as conn:
            conn.execute("INSERT INTO operations(id, occurred) VALUES (100, '2025-01-01')")
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        row = next(r for r in rows if r['event_id'] is not None)
        self.assertNotEqual(row['event_id'], row['operation_id'])
        shown = self.run_fin('operation', 'show', row['operation_id']).stdout
        self.assertIn(f"Operation: #{row['operation_id']}", shown)
        self.assertNotEqual(self.run_fin('operation', 'show', row['event_id'], check=False).returncode, 0)
        self.assertIn('category_changes=1', self.run_fin('operation', 'category', 'set', 'expenses:food', row['operation_id']).stdout)
        self.assertIn('type_changes=1', self.run_fin('operation', 'type', 'set', 'fee', row['operation_id']).stdout)
        updated = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['operation_id'] == row['operation_id'])
        self.assertEqual((updated['counter_account'], updated['movement_type']), ('expenses:food', 'fee'))
        selected = json.dumps([row])
        piped = subprocess.run([FIN, 'operation', 'category', 'set', 'expenses:other', '-'],
                               input=selected, env=self.env, capture_output=True, text=True)
        self.assertEqual(piped.returncode, 0, piped.stderr)
        self.assertIn('category_changes=1', piped.stdout)
        self.assertNotEqual(self.run_fin('operation', 'type', 'show', row['event_id'], check=False).returncode, 0)

    def test_restore_decisions_for_one_institution(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        row = json.loads(self.run_fin('operation', 'list', '--json').stdout)[0]
        self.run_fin('operation', 'category', 'set', 'expenses:food', row['event_id'])
        snapshot = json.loads(self.run_fin('operation', 'export').stdout)
        missing = dict(snapshot['decisions'][0], institution='wise', source_id='missing')
        snapshot['decisions'].append(missing)
        path = self.root / 'mixed-decisions.json'
        path.write_text(json.dumps(snapshot))
        rebuilt = self.root / 'filtered.sqlite3'
        self.run_fin('init', '--database', rebuilt)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main', '--database', rebuilt)
        blocked = self.run_fin('operation', 'restore', path, '--database', rebuilt, '--apply', '--yes', check=False)
        self.assertNotEqual(blocked.returncode, 0)
        self.assertIn('transaction not found', blocked.stdout)
        preview = self.run_fin('operation', 'restore', path, '--database', rebuilt, '--institution', 'revolut').stdout
        self.assertIn('decisions skipped: 1', preview)
        self.assertIn('Decisions to restore: 1', preview)
        self.assertNotIn('Problems', preview)
        applied = self.run_fin('operation', 'restore', path, '--database', rebuilt, '--institution', 'revolut', '--apply', '--yes').stdout
        self.assertIn('category_changes=1', applied)
        replay = self.run_fin('operation', 'restore', path, '--database', rebuilt, '--institution', 'revolut').stdout
        self.assertIn('already matching: 1', replay)
        self.assertNotEqual(self.run_fin('operation', 'restore', path, '--database', rebuilt, '--institution', 'typo', check=False).returncode, 0)
        snapshot['decisions'][0]['payload_sha256'] = 'bad'
        path.write_text(json.dumps(snapshot))
        conflict = self.run_fin('operation', 'restore', path, '--database', rebuilt, '--institution', 'revolut', '--apply', '--yes', check=False)
        self.assertNotEqual(conflict.returncode, 0)
        self.assertIn('source evidence differs', conflict.stdout)

    def test_manual_movement_types_and_history(self):
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        event = rows[0]['event_id']
        decisions = self.root / 'type-classification.json'
        decisions.write_text(json.dumps([{'event_id': event, 'account': 'expenses:shopping', 'reason': 'Reviewed'}]))
        self.run_fin('operation', 'category', 'apply', decisions)
        before = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == event)
        journal = self.run_fin('journal', 'export').stdout
        listing = self.run_fin('operation', 'type', 'list').stdout
        self.assertIn('bonus', listing)
        self.assertIn('fee', listing)
        self.assertNotEqual(self.run_fin('operation', 'type', 'set', 'unknown', event, check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('operation', 'type', 'set', 'made-up', event, check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('operation', 'type', 'set', 'bonus', '9999999', check=False).returncode, 0)
        self.run_fin('operation', 'type', 'set', 'bonus', event)
        self.run_fin('operation', 'type', 'set', 'bonus', event)
        row = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == event)
        self.assertEqual(row['movement_type'], 'bonus')
        self.assertEqual(row['imported_movement_type'], before['movement_type'])
        self.assertEqual(row['movement_type_override'], 'bonus')
        self.assertEqual(row['decision'], before['decision'])
        self.assertEqual(self.run_fin('journal', 'export').stdout, journal)
        self.assertIn('manual override', self.run_fin('operation', 'type', 'show', event).stdout)
        self.assertEqual([r['event_id'] for r in json.loads(self.run_fin('operation', 'list', '--type', 'bonus', '--json').stdout)], [event])
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        # Types are derived: a rule retypes at once, and a manual override still wins.
        self.run_fin('rule', 'add', 'retype-me', '--institution', 'boursobank', '--description', '.',
                     '--set-type', 'fee', '--automatic', '--priority=-5')
        row = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == event)
        self.assertEqual(row['imported_movement_type'], 'fee')
        self.assertEqual(row['movement_type_override'], 'bonus')
        self.assertEqual(row['movement_type'], 'bonus')
        self.run_fin('rule', 'delete', 'retype-me', '--yes')
        self.run_fin('operation', 'type', 'set', 'fee', event)
        self.run_fin('operation', 'type', 'reset', event)
        self.run_fin('operation', 'type', 'reset', event)
        row = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == event)
        self.assertEqual(row['movement_type'], before['movement_type'])
        self.assertIsNone(row['movement_type_override'])
        history = [r for r in json.loads(self.run_fin('operation', 'type', 'history', '--json').stdout)
                   if not (r.get('reason') or '').startswith('rule ')]
        self.assertEqual([(r['previous_type'], r['movement_type']) for r in history],
                         [(None, 'bonus'), ('bonus', 'fee'), ('fee', None)])
        self.assertTrue(all(r['event_id'] == event for r in history))

    def test_type_set_multiple_is_atomic(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        first, second = (row['event_id'] for row in rows[:2])
        before = self.run_fin('operation', 'type', 'history', '--json').stdout
        for ids in [(first, '9999999'), (first, first), ()]:
            self.assertNotEqual(self.run_fin('operation', 'type', 'set', 'fee', *ids, check=False).returncode, 0)
        self.assertEqual(self.run_fin('operation', 'type', 'history', '--json').stdout, before)
        self.assertIn('type_changes=2', self.run_fin('operation', 'type', 'set', 'fee', first, second).stdout)
        self.assertIn('type_changes=0', self.run_fin('operation', 'type', 'set', 'fee', first, second).stdout)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual({r['movement_type_override'] for r in rows if r['event_id'] in (first, second)}, {'fee'})
        self.assertEqual(len(json.loads(self.run_fin('operation', 'type', 'history', '--json').stdout)), 2)

    def test_type_history_is_removed_with_account(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        row = json.loads(self.run_fin('operation', 'list', '--json').stdout)[0]
        self.run_fin('operation', 'type', 'set', 'bonus', row['event_id'])
        self.assertEqual(len(json.loads(self.run_fin('operation', 'type', 'history', '--json').stdout)), 1)
        self.run_fin('account', 'delete', row['account_id'], '--yes')
        self.assertEqual(json.loads(self.run_fin('operation', 'type', 'history', '--json').stdout), [])

    def test_type_list_includes_observed_provider_types(self):
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        source, target = rows[0]['event_id'], rows[1]['event_id']
        with sqlite3.connect(self.root / 'data/fin/fin.sqlite3') as db:
            trigger = db.execute("SELECT sql FROM sqlite_schema WHERE name='movements_no_update'").fetchone()[0]
            db.execute('DROP TRIGGER movements_no_update')
            db.execute("UPDATE movements SET movement_type='example:reported' WHERE event_id=?", (source,))
            db.execute(trigger)
        self.assertIn('example:reported', self.run_fin('operation', 'type', 'list').stdout)
        self.run_fin('operation', 'type', 'set', 'example:reported', target)
        matches = json.loads(self.run_fin('operation', 'list', '--type', 'example:reported', '--json').stdout)
        self.assertEqual({r['event_id'] for r in matches}, {source, target})

    def test_automatic_typing_rules(self):
        rules = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        self.assertTrue(any(r['institution'] == 'boursobank' and r['mode'] == 'automatic' for r in rules))
        self.run_fin('rule', 'add', 'my-card', '^CAFÉ & CO$', '--institution', 'boursobank', '--set-type', 'card', '--automatic', '--priority=-1')
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        row = next(r for r in rows if r['description'] == 'CAFÉ & CO')
        self.assertEqual(row['movement_type'], 'card')
        self.run_fin('operation', 'type', 'set', 'fee', row['event_id'])
        self.run_fin('rule', 'delete', 'my-card', '--yes')
        row = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == row['event_id'])
        self.assertEqual(row['movement_type'], 'fee')
        self.assertIsNone(row['imported_movement_type'])

    def test_automatic_rule_categories_and_modes(self):
        self.run_fin('rule', 'add', 'shop', '^CAFÉ & CO$', '--institution', 'boursobank', '--match-type', 'unknown', '--set-type', 'card', '--category', 'expenses:shopping', '--automatic')
        self.run_fin('rule', 'mode', 'shop', 'disabled')
        rules = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        self.assertEqual(next(r for r in rules if r['name'] == 'shop')['mode'], 'disabled')
        self.run_fin('rule', 'mode', 'shop', 'automatic')
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        row = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['description'] == 'CAFÉ & CO')
        self.assertEqual(row['movement_type'], 'card')
        self.assertEqual(row['counter_account'], 'expenses:shopping')
        self.assertIn('Automatic rule shop', self.run_fin(('op'), 'category', 'history').stdout)
        preview = json.loads(self.run_fin('rule', 'preview', 'shop', '--json').stdout)
        self.assertIn(row['event_id'], [m['transaction']['event_id'] for m in preview])
        self.run_fin('rule', 'add', 'shop', '^CAFÉ & CO$', '--institution', 'boursobank', '--set-type', 'fee', '--automatic', '--replace')
        self.run_fin('rule', 'mode', 'shop', 'disabled')
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        row2 = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == row['event_id'])
        self.assertEqual(row2['counter_account'], 'expenses:shopping')
        # Types are derived: with the only matching rule disabled, nothing types the movement.
        self.assertIsNone(row2['movement_type'])

    def test_user_institution_imports_generic_ofx(self):
        self.run_fin('institution', 'add', 'provider')
        result = self.run_fin('import', 'provider', 'fixtures/bank.ofx')
        self.assertIn('imported=1', result.stdout)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertTrue(rows)
        self.assertTrue(all(row['institution'] == 'provider' for row in rows))
        self.assertTrue(json.loads(self.run_fin('account', 'export').stdout)['accounts'])
        self.run_fin('status')
        self.assertIn('unchanged=1', self.run_fin('import', 'provider', 'fixtures/bank.ofx').stdout)
        directory = self.root / 'generic-ofx'
        directory.mkdir()
        (directory / 'again.ofx').write_bytes(Path('fixtures/bank.ofx').read_bytes())
        self.assertIn('unchanged=1', self.run_fin('import', 'provider', directory).stdout)
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--json').stdout)), len(rows))
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
        self.assertEqual(json.loads(self.run_fin('operation', 'list', '--json').stdout), [])
        self.run_fin('import', 'provider', 'fixtures/bank.ofx')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
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
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--json').stdout)), 3)
        primary = self.root / 'primary.csv'
        primary.write_text('Date,Description,EUR\n2026-02-01,Shop,5.00\n')
        self.terminal(['import', str(primary), '--account-id', 'card', '--provider', 'other'], [
            ('Use these columns?', '1'), ('Account type:', '1'),
            ('Are purchases positive amounts in the file?', '1'),
            ('Skip a summary row with an empty date?', '2'), ('Import these transactions?', '1')])
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--json').stdout)), 4)
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
        self.assertIn('boursobank (bank, built-in; aliases: boursorama, bourso)', result)
        self.assertIn('wise', result)
        self.assertIn('bunq', result)
        self.assertIn('revolut', result)
        self.assertIn('provider (bank, generic)', result)
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
        self.assertIn('provider (bank, generic)', self.run_fin('institution', 'list').stdout)
        self.run_fin('institution', 'delete', 'provider', '--yes')
        result = self.run_fin('institution', 'list').stdout
        self.assertNotIn('provider (bank, generic)', result)
        self.assertIn('wise (bank, built-in)', result)

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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(len(rows), 3)
        self.assertTrue(all(r['institution'] == 'boursobank' for r in rows))
        self.run_fin('import', 'boursorama', 'fixtures/bank.ofx')
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--json').stdout)), 3)

    def test_command_aliases(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        expected = self.run_fin('operation', 'list', '--json').stdout
        self.assertEqual(self.run_fin('op', 'list', '--json').stdout, expected)
        self.assertIn('fin-operation-list', self.run_fin('op', 'list', '--help=plain').stdout)
        for old in ('tx', 'txn', 'transaction'):
            self.assertNotEqual(self.run_fin(old, 'list', check=False).returncode, 0)
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
        portable = self.root / 'transfers-portable.json'
        portable.write_text(self.run_fin('transfer', 'export').stdout)
        saved = json.loads(portable.read_text())
        self.assertEqual((saved['version'], len(saved['links'])), (2, 1))
        rebuilt = self.root / 'transfer-rebuilt.sqlite3'
        self.run_fin('init', '--database', rebuilt)
        self.run_fin('import', 'revolut', other, '--account-id', 'other', '--database', rebuilt)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main', '--database', rebuilt)
        journal_before = self.run_fin('journal', 'export', '--database', rebuilt).stdout
        preview = self.run_fin('transfer', 'restore', portable, '--database', rebuilt).stdout
        self.assertIn('Transfers to restore: 1', preview)
        self.assertIn('transfer_changes=1', self.run_fin('transfer', 'restore', portable, '--database', rebuilt, '--apply', '--yes').stdout)
        self.assertIn('already matching: 1', self.run_fin('transfer', 'restore', portable, '--database', rebuilt).stdout)
        restored = json.loads(self.run_fin('operation', 'list', '--database', rebuilt, '--json').stdout)
        self.assertTrue(any(len(r['movements']) == 2 for r in restored))
        journal_after = self.run_fin('journal', 'export', '--database', rebuilt).stdout
        self.assertEqual(journal_before.count('equity:unclassified') - journal_after.count('equity:unclassified'), 2)
        duplicate = dict(saved, links=saved['links'] * 2)
        portable.write_text(json.dumps(duplicate))
        self.assertNotEqual(self.run_fin('transfer', 'restore', portable, '--database', rebuilt, '--apply', '--yes', check=False).returncode, 0)
        saved['links'][0]['first']['payload_sha256'] = 'changed'
        portable.write_text(json.dumps(saved))
        blocked = self.run_fin('transfer', 'restore', portable, '--database', rebuilt, '--apply', '--yes', check=False)
        self.assertNotEqual(blocked.returncode, 0)
        self.assertIn('source evidence differs', blocked.stdout)
        tx_status = json.loads(self.run_fin('operation', 'status', '--json').stdout)
        self.assertEqual((tx_status['transfers'], tx_status['classified'], tx_status['categorized']), (2, 2, 0))
        operation = json.loads(self.run_fin('db', 'event', 'operations', first, '--json').stdout)
        other_operation = json.loads(self.run_fin('db', 'event', 'operations', second, '--json').stdout)
        self.assertEqual(len(operation), 1)
        self.assertEqual(operation[0]['id'], other_operation[0]['id'])
        self.assertEqual({x['event_id'] for x in operation[0]['evidence']}, {first, second})
        self.assertEqual(len(operation[0]['movements']), 2)
        self.assertIn('Sources:', self.run_fin('db', 'event', 'operations', first).stdout)
        self.assertNotEqual(self.run_fin('db', 'event', 'operations', '9999999', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('operation', 'type', 'set', 'fee', operation[0]['id'], '--source-event', '999999', check=False).returncode, 0)
        self.run_fin('operation', 'type', 'set', 'fee', operation[0]['id'], '--source-event', first)
        snapshot = self.root / 'transfer-type.json'
        snapshot.write_text(self.run_fin('operation', 'export').stdout)
        self.run_fin('operation', 'type', 'reset', operation[0]['id'], '--source-event', first)
        self.assertIn('type_changes=1', self.run_fin('operation', 'restore', snapshot, '--apply', '--yes').stdout)
        transfer = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == first)
        self.assertEqual((transfer['decision'], transfer['movement_type_override']), ('transfer', 'fee'))
        self.run_fin('rule', 'add', 'force-transfer-test', 'Transfer', 'expenses:other')
        preview = json.loads(self.run_fin('rule', 'preview', 'force-transfer-test', '--force', '--json').stdout)
        self.assertTrue(all(not r['apply_account'] for r in preview if r['transaction']['event_id'] in (first, second)))
        self.run_fin('rule', 'apply', 'force-transfer-test', '--force', '--yes')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertTrue(all(r['decision'] == 'transfer' for r in rows if r['event_id'] in (first, second)))
        report = json.loads(self.run_fin('transfer', 'candidates', '--all', '--json').stdout)
        self.assertTrue(any(c['state'] == 'confirmed' for c in report['all_pairs']))
        self.assertIn('Nothing changed.', self.terminal(['transfer', 'undo', first, second],
            [('Undo this transfer? [Y/n]:', 'n')]))
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'history').stdout)), 1)
        self.assertIn('transfer_changes=1', self.run_fin('transfer', 'undo', second, first, '--yes').stdout)
        first_operation = json.loads(self.run_fin('db', 'event', 'operations', first, '--json').stdout)
        second_operation = json.loads(self.run_fin('db', 'event', 'operations', second, '--json').stdout)
        self.assertNotEqual(first_operation[0]['id'], second_operation[0]['id'])
        self.assertEqual(len(first_operation[0]['movements']), 1)
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'history').stdout)), 2)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertTrue(all(r['decision'] == 'unclassified' for r in rows if r['event_id'] in (first, second)))
        self.assertNotEqual(self.run_fin('transfer', 'undo', first, second, '--yes', check=False).returncode, 0)

    def test_transfer_candidates_and_review_order_by_score(self):
        header = 'Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
        self.csv.write_text(header + ''.join(
            f'Transfer,Current,2026-01-01 10:00:00,2026-01-01 10:00:01,Incoming,{amount}.00,0.00,EUR,COMPLETED,{balance}.00\n'
            for amount, balance in [(10, 10), (20, 30), (30, 60)]))
        other = self.root / 'transfer-scores.csv'
        other.write_text(header + ''.join(
            f'Transfer,Current,2026-01-{day:02} 10:00:00,2026-01-{day:02} 10:00:01,Outgoing,-{amount}.00,0.00,EUR,COMPLETED,-{balance}.00\n'
            for day, amount, balance in [(1, 20, 20), (1, 30, 50), (7, 10, 60)]))
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('import', 'revolut', other, '--account-id', 'other')
        candidates = json.loads(self.run_fin('transfer', 'candidates', '--json').stdout)['candidates']
        self.assertEqual(len(candidates), 3)
        scores = [candidate['score'] for candidate in candidates]
        self.assertEqual(scores, sorted(scores, reverse=True))
        self.assertGreater(scores[0], scores[-1])
        self.assertEqual(scores[0], scores[1])
        for left, right in zip(candidates, candidates[1:]):
            if left['score'] == right['score']:
                self.assertLess((int(left['first']['event_id']), int(left['second']['event_id'])),
                                (int(right['first']['event_id']), int(right['second']['event_id'])))
        first = candidates[0]
        output = self.terminal(['transfer', 'review'], [('Current pair or batch choice [c/r/s/a/d/q]:', 'q')])
        self.assertIn(f"Current pair: #{first['first']['event_id']} and #{first['second']['event_id']}", output)

    def test_transfer_candidates_exclude_categorized_movements(self):
        header = 'Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
        self.csv.write_text(header + 'Transfer,Current,2026-01-02 10:00:00,2026-01-02 10:00:01,Incoming,10.00,0.00,EUR,COMPLETED,10.00\n')
        other = self.root / 'transfer-out.csv'
        other.write_text(header + 'Transfer,Current,2026-01-02 10:00:00,2026-01-02 10:00:01,Outgoing,-10.00,0.00,EUR,COMPLETED,-10.00\n')
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('import', 'revolut', other, '--account-id', 'other')
        report = json.loads(self.run_fin('transfer', 'candidates', '--json').stdout)
        self.assertEqual(len(report['candidates']), 1)
        event = report['candidates'][0]['first']['event_id']
        self.run_fin('operation', 'category', 'set', 'expenses:food', event)
        self.assertEqual(json.loads(self.run_fin('transfer', 'candidates', '--json').stdout)['candidates'], [])
        self.assertEqual(json.loads(self.run_fin('transfer', 'candidates', '--all', '--json').stdout)['all_pairs'], [])
        self.assertEqual(json.loads(self.run_fin('status', '--json').stdout)['transfer_candidate_pairs'], 0)
        review = self.terminal(['transfer', 'review'], [])
        self.assertIn('Nothing changed.', review)
        self.assertNotIn('Current pair: #', review)
        self.run_fin('operation', 'category', 'set', 'equity:unclassified', event)
        self.assertEqual(len(json.loads(self.run_fin('transfer', 'candidates', '--json').stdout)['candidates']), 1)

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

    def test_evidence_rules_apply_at_import_and_refresh_explicitly(self):
        ofx = self.root / 'reversal-evidence.ofx'
        data = Path('fixtures/bank.ofx').read_text().replace('Synthetic debit', 'RETRAIT DAB example')
        data = data.replace('</BANKTRANLIST></STMTRS>\n</STMTTRNRS>',
            '<STMTTRN><DTPOSTED>20260105</DTPOSTED><TRNTYPE>CREDIT</TRNTYPE>'
            '<TRNAMT>42.95</TRNAMT><FITID>atm-reversal</FITID>'
            '<NAME>ANNUL RETRAIT DAB example</NAME></STMTTRN>'
            '</BANKTRANLIST></STMTRS>\n</STMTTRNRS>')
        ofx.write_text(data)
        rules = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        self.assertTrue(any(r['name'] == 'boursobank-withdrawal-reversal' and r['mode'] == 'automatic' for r in rules))
        self.run_fin('rule', 'mode', 'boursobank-withdrawal-reversal', 'disabled')
        self.run_fin('import', 'boursobank', ofx)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        reversal = next(r for r in rows if r['description'] == 'ANNUL RETRAIT DAB example')
        self.assertFalse(reversal['source_withdrawal_reversal'])
        self.assertNotIn('atm-reversal', self.run_fin('reversal', 'candidates').stdout)
        self.run_fin('rule', 'mode', 'boursobank-withdrawal-reversal', 'automatic')
        self.assertFalse(next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == reversal['event_id'])['source_withdrawal_reversal'])
        journal = self.run_fin('journal', 'export').stdout
        preview = self.terminal(['operation', 'refresh-evidence', '--institution', 'boursobank'], [
            ('Apply these source evidence changes? [Y/n]:', 'n')])
        self.assertIn('Nothing changed.', preview)
        self.assertFalse(next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == reversal['event_id'])['source_withdrawal_reversal'])
        self.assertIn('source_evidence_changed=1', self.run_fin('operation', 'refresh-evidence', '--institution', 'boursobank', '--yes').stdout)
        reversal = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == reversal['event_id'])
        self.assertTrue(reversal['source_withdrawal_reversal'])
        self.assertIn(str(reversal['event_id']), self.run_fin('reversal', 'candidates').stdout)
        self.assertEqual(journal, self.run_fin('journal', 'export').stdout)

    def test_evidence_refresh_rechecks_changed_rules(self):
        self.run_fin('import', 'boursobank', 'fixtures/bank.ofx')
        self.run_fin('rule', 'add-evidence', 'example-evidence', 'withdrawal-reversal', '^CAFÉ', '--institution', 'boursobank')
        master, slave = pty.openpty()
        child = subprocess.Popen([FIN, 'operation', 'refresh-evidence', '--institution', 'boursobank'], env=self.env,
                                 stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        transcript = b''
        deadline = time.monotonic() + 20
        try:
            while b'Apply these source evidence changes? [Y/n]:' not in transcript:
                if time.monotonic() >= deadline or child.poll() is not None:
                    self.fail(transcript.decode(errors='replace'))
                if select.select([master], [], [], 0.1)[0]:
                    transcript += os.read(master, 65536)
            self.run_fin('rule', 'mode', 'example-evidence', 'disabled')
            os.write(master, b'y\n')
            child.wait(timeout=max(0.1, deadline - time.monotonic()))
            while select.select([master], [], [], 0)[0]:
                try:
                    transcript += os.read(master, 65536)
                except OSError:
                    break
            self.assertNotEqual(child.returncode, 0)
            self.assertIn('changed since preview', transcript.decode(errors='replace'))
        finally:
            if child.poll() is None:
                child.kill()
                child.wait()
            os.close(master)
        row = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['description'] == 'CAFÉ & CO')
        self.assertFalse(row['source_withdrawal_reversal'])

    def test_custom_card_reference_evidence_rule(self):
        self.run_fin('institution', 'add', 'provider')
        invalid = self.run_fin('rule', 'add-evidence', 'bad', 'card-reference', '^CARD$', '--institution', 'provider', check=False)
        self.assertNotEqual(invalid.returncode, 0)
        self.assertIn('capturing group', invalid.stderr)
        self.run_fin('rule', 'add-evidence', 'card-id', 'card-reference', r'^CARD (.+)$', '--institution', 'provider')
        ofx = self.root / 'card-ref.ofx'
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace('CAFÉ &amp; CO', 'CARD masked123'))
        self.run_fin('import', 'provider', ofx)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        found = next(r for r in rows if r['description'] == 'CARD masked123')
        self.assertEqual(found['source_card_reference'], 'masked123')
        self.run_fin('rule', 'mode', 'card-id', 'disabled')
        self.assertEqual(next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == found['event_id'])['source_card_reference'], 'masked123')
        self.run_fin('operation', 'refresh-evidence', '--institution', 'provider', '--yes')
        self.assertIsNone(next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['event_id'] == found['event_id'])['source_card_reference'])

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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        original = next(r for r in rows if r['movement_type'] == 'withdrawal' and float(r['amount']) < 0)
        reversal = next(r for r in rows if r['description'] == 'ANNUL RETRAIT DAB example')
        a, b = original['event_id'], reversal['event_id']
        candidates = self.run_fin('reversal', 'candidates').stdout
        self.assertIn(f'Original #{a}; reversal #{b}', candidates)
        self.assertEqual(candidates.count('Original #'), 1)
        self.assertIn('Nothing changed.', self.terminal(['reversal', 'review', a, b],
            [('Apply this decision? [Y/n]:', 'n')]))
        self.run_fin('reversal', 'review', a, b, '--yes')
        rows = json.loads(self.run_fin('operation', 'list', '--decision', 'reversal', '--json').stdout)
        self.assertEqual({r['event_id'] for r in rows}, {a, b})
        self.assertTrue(all(r['counter_account'] == f'assets:reversals:pair-{a}-{b}' for r in rows))
        journal = self.run_fin('journal', 'export').stdout
        self.assertIn(f'assets:reversals:pair-{a}-{b}', journal)
        self.assertIn('No unreviewed reversal candidates.', self.run_fin('reversal', 'candidates').stdout)
        self.assertIn('confirmed', self.run_fin('reversal', 'history').stdout)
        refreshed = json.loads(self.run_fin('operation', 'list', '--decision', 'reversal', '--json').stdout)
        self.assertTrue(all(r['movement_type'] == 'withdrawal' for r in refreshed))
        self.assertNotEqual(self.run_fin('reversal', 'review', a, b, '--yes', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('account', 'delete', original['account_id'], '--yes', check=False).returncode, 0)
        decisions = self.root / 'reversal-category.json'
        decisions.write_text(json.dumps([{'event_id': a, 'account': 'expenses:shopping', 'reason': 'test'}]))
        self.assertNotEqual(self.run_fin('operation', 'category', 'apply', decisions, check=False).returncode, 0)
        self.run_fin('reversal', 'undo', a, b, '--yes')
        self.assertIn('undone', self.run_fin('reversal', 'history').stdout)
        self.assertNotIn(f'assets:reversals:pair-{a}-{b}', self.run_fin('journal', 'export').stdout)
        rows = json.loads(self.run_fin('operation', 'list', '--decision', 'unclassified', '--json').stdout)
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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        types = {r['description']: r['movement_type'] for r in rows}
        self.assertEqual(types, {'VIR SEPA Example': 'transfer', 'PRLV Example': 'direct-debit',
                                 'CARTE Example': 'card', 'PAIEMENT CARTE 021014 Example': 'card',
                                 '*INTERETS AU 31/12/25': 'interest'})
        self.assertTrue(all(r['counter_account'] == 'equity:unclassified' for r in rows))
        interest = next(r for r in rows if r['movement_type'] == 'interest')
        with sqlite3.connect(self.root / 'data/fin/fin.sqlite3') as db:
            # The derived type may be recomputed, the evidence it comes from may not.
            with self.assertRaises(sqlite3.IntegrityError):
                db.execute("UPDATE movements SET amount = '1' WHERE event_id = ?", (interest['event_id'],))
            with self.assertRaises(sqlite3.IntegrityError):
                db.execute('DELETE FROM movements WHERE event_id = ?', (interest['event_id'],))

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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        for description, amount, expected in examples:
            row = next(r for r in rows if r['description'] == description)
            self.assertEqual(row['movement_type'], expected)
            self.assertEqual(row['decision'], 'unclassified')
            self.assertEqual(float(row['amount']), float(amount))
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--type', 'fee', '--json').stdout)), 4)

    def test_regex_category_rule_preview_and_explicit_apply(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.assertNotEqual(self.run_fin('rule', 'add', 'bad', '[', 'expenses:food', check=False).returncode, 0)
        self.assertNotEqual(self.run_fin('rule', 'add', 'bad', 'transfer', 'assets:checking', check=False).returncode, 0)
        self.run_fin('rule', 'add', 'repeat', 'repeated transfer', 'expenses:food', '--institution', 'revolut')
        preview = json.loads(self.run_fin('rule', 'preview', 'repeat', '--json').stdout)
        self.assertEqual(len(preview), 2)
        self.assertTrue(all(row['eligible'] for row in preview))
        before = self.run_fin('operation', 'review', '--all', '--json').stdout
        output = self.terminal(['rule', 'apply', 'repeat'], [('Apply to these transactions?', 'no')])
        self.assertIn('Nothing changed.', output)
        self.assertEqual(self.run_fin('operation', 'review', '--all', '--json').stdout, before)
        self.assertIn('rule_changes=2', self.run_fin('rule', 'apply', 'repeat', '--yes').stdout)
        reviewed = json.loads(self.run_fin('operation', 'list', '--json', '--search', 'Repeated').stdout)
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
        self.assertEqual(json.loads(self.run_fin('operation', 'list', '--json', '--search', 'Repeated').stdout), reviewed)
        later = self.root / 'later.csv'
        later.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                         'Transfer,Current,2026-01-04 10:00:00,2026-01-04 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,79.50\n')
        self.run_fin('import', 'revolut', later, '--account-id', 'main')
        preview = json.loads(self.run_fin('rule', 'preview', 'renamed', '--json').stdout)
        self.assertEqual(sum(row['eligible'] for row in preview), 1)
        self.assertEqual(sum(not row['eligible'] for row in preview), 2)
        self.assertIn('rule_changes=1', self.run_fin('rule', 'apply', 'renamed', '--yes').stdout)
        self.run_fin('rule', 'delete', 'renamed', '--yes')
        self.assertEqual(len(json.loads(self.run_fin('operation', 'category', 'history').stdout)), 3)

    def test_rule_apply_force_replaces_categories(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)
        first, second = (r['event_id'] for r in rows)
        self.run_fin('operation', 'category', 'set', 'expenses:old', first)
        self.run_fin('rule', 'add', 'repeat-force', 'Repeated', 'expenses:new')
        normal = json.loads(self.run_fin('rule', 'preview', 'repeat-force', '--json').stdout)
        forced = json.loads(self.run_fin('rule', 'preview', 'repeat-force', '--force', '--json').stdout)
        self.assertEqual(sum(r['apply_account'] for r in normal), 1)
        self.assertEqual(sum(r['apply_account'] for r in forced), 2)
        self.assertIn('rule_changes=2', self.run_fin('rule', 'apply', 'repeat-force', '--force', '--yes').stdout)
        self.assertEqual({r['counter_account'] for r in json.loads(self.run_fin('operation', 'list', '--search', 'Repeated', '--json').stdout)}, {'expenses:new'})
        self.assertIn('Nothing to apply.', self.run_fin('rule', 'apply', 'repeat-force', '--force', '--yes').stdout)
        history = json.loads(self.run_fin('operation', 'category', 'history').stdout)
        self.assertTrue(any(r['event_id'] == first and r['account'] == 'expenses:new' and 'Rule repeat-force' in r['reason'] for r in history))

    def test_rule_explain_traces_the_type(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        deposit = next(r['event_id'] for r in rows if r['description'] == 'Initial deposit')
        out = self.run_fin('rule', 'explain', deposit).stdout
        self.assertIn('Type: deposit  (type rules)', out)
        self.assertIn('Type rules, each on the field it names', out)
        self.assertIn('decides  revolut-reported-deposit', out)
        self.assertIn('[Type: Deposit]', out)
        self.assertIn('rules did not match (use --all)', out)
        self.assertNotIn('no match  revolut-reported-card-payment', out)
        full = self.run_fin('rule', 'explain', deposit, '--all').stdout
        self.assertIn('no match  revolut-reported-card-payment', full)
        self.assertIn('Category: none recorded.', out)
        # A manual override outranks the rules and is reported as such.
        self.run_fin('operation', 'type', 'set', 'bonus', deposit)
        self.assertIn('Type: bonus  (manual override)', self.run_fin('rule', 'explain', deposit).stdout)
        self.assertNotEqual(self.run_fin('rule', 'explain', '999999', check=False).returncode, 0)

    def test_institution_show_describes_fields_rules_and_data(self):
        before = self.run_fin('institution', 'show', 'revolut').stdout
        self.assertIn('Running-balance CSV (primary)', before)
        self.assertIn('any column of the file, named by its header, such as "Type", with --field', before)
        self.assertIn('Default rules (7):', before)
        self.assertIn('Imported data: none.', before)
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        after = self.run_fin('institution', 'show', 'revolut').stdout
        self.assertIn('Imported data: 4 movements', after)
        self.assertRegex(after, r'\n\s+3\s+transfer\n')
        wise = self.run_fin('institution', 'show', 'wise').stdout
        self.assertIn('Columns fin does not read:', wise)
        self.assertIn('Transaction Details Type', wise)
        self.assertNotEqual(self.run_fin('institution', 'show', 'nosuchbank', check=False).returncode, 0)

    def test_delete_confirmation_defaults_to_no(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        database = self.root / 'data/fin/fin.sqlite3'
        self.assertTrue(database.exists())
        for answer in ('', 'n', 'maybe'):
            output = self.terminal(['delete'], [('Delete these files? [y/N]', answer)])
            self.assertIn('Nothing deleted.', output, answer)
            self.assertTrue(database.exists(), answer)
        output = self.terminal(['delete'], [('Delete these files? [y/N]', 'y')])
        self.assertIn('Deleted.', output)
        self.assertFalse(database.exists())

    def test_revolut_types_come_from_the_type_column(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(
            sorted((r['description'], r['movement_type']) for r in rows),
            [('Initial deposit', 'deposit'), ('Repeated transfer', 'transfer'),
             ('Repeated transfer', 'transfer'), ('Transfer to another bank', 'transfer')])
        self.assertIn('Every movement has a type.', self.run_fin('rule', 'gaps', '--institution', 'revolut').stdout)
        # An unmapped code is kept as a provider-specific type, not dropped.
        extra = self.root / 'revolut-other.csv'
        extra.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                         'Cashback,Current,2026-01-05 10:00:00,2026-01-05 10:00:01,Reward,1.00,0.00,EUR,COMPLETED,90.50\n')
        self.run_fin('import', 'revolut', extra, '--account-id', 'main')
        row = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['booked_on'] == '2026-01-05')
        self.assertEqual(row['movement_type'], 'revolut:Cashback')

    def test_rule_type_and_category_actions_are_independent(self):
        # These rules are tested on movements without a type, so drop the bundled
        # mapping of Revolut's own Type column first.
        for code in ('card-payment', 'card-refund', 'deposit', 'transfer', 'atm', 'charge', 'other'):
            self.run_fin('rule', 'delete', 'revolut-reported-' + code, '--yes')
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        later = self.root / 'rule-extra.csv'
        later.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                         'Transfer,Current,2026-01-04 10:00:00,2026-01-04 10:00:01,Repeated transfer,10.00,0.00,EUR,COMPLETED,79.50\n')
        self.run_fin('import', 'revolut', later, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated transfer', '--json').stdout)
        manual, categorized, fresh = [r['event_id'] for r in rows]
        self.run_fin('operation', 'type', 'set', 'bonus', manual)
        decisions = self.root / 'rule-existing.json'
        decisions.write_text(json.dumps([{'event_id': categorized, 'account': 'expenses:shopping', 'reason': 'Already checked'}]))
        self.run_fin(('op'), 'category', 'apply', decisions)
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
        rows = {r['event_id']: r for r in json.loads(self.run_fin('operation', 'list', '--search', 'Repeated transfer', '--json').stdout)}
        self.assertEqual((rows[manual]['movement_type'], rows[manual]['counter_account']), ('bonus', 'income:interest:wise'))
        self.assertEqual((rows[categorized]['movement_type'], rows[categorized]['counter_account']), ('interest', 'expenses:shopping'))
        self.assertEqual((rows[fresh]['movement_type'], rows[fresh]['counter_account']), ('interest', 'income:interest:wise'))
        history = json.loads(self.run_fin('operation', 'type', 'history', '--json').stdout)
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
        new_row = next(r for r in json.loads(self.run_fin('operation', 'list', '--json').stdout) if r['booked_on'] == '2026-01-05')
        self.assertIsNone(new_row['movement_type'])
        self.assertEqual(new_row['decision'], 'unclassified')
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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        settlements = [r for r in rows if r['description'] == 'Relevé différé Carte SAVINGS']
        self.assertTrue(all(r['decision'] == 'settlement' for r in settlements))
        self.assertTrue(all(r['movement_type'] == 'card-settlement' for r in settlements))
        selected = json.loads(self.run_fin('operation', 'list', '--type', 'card-settlement',
                                           '--decision', 'settlement', '--json').stdout)
        self.assertEqual({r['event_id'] for r in selected}, {r['event_id'] for r in settlements})
        self.assertEqual(json.loads(self.run_fin('operation', 'list', '--type', 'interest', '--decision', 'settlement', '--json').stdout), [])
        self.assertTrue(all(r['movement_type'] is None for r in json.loads(
            self.run_fin('operation', 'list', '--type', 'unknown', '--json').stdout)))
        # Retyping on a rule change leaves reviewed settlements, their history and the
        # journal untouched.
        history = self.run_fin('settlement', 'history').stdout
        journal = self.run_fin('journal', 'export').stdout
        self.run_fin('rule', 'add', 'noop-type', '--institution', 'boursobank', '--description',
                     '^NO SUCH DESCRIPTION$', '--set-type', 'fee', '--automatic')
        restored = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertTrue(all(r['decision'] == 'settlement' and r['movement_type'] == 'card-settlement'
                            for r in restored if r['event_id'] in [s['event_id'] for s in settlements]))
        self.assertEqual(self.run_fin('settlement', 'history').stdout, history)
        self.assertEqual(self.run_fin('journal', 'export').stdout, journal)
        self.env['PAGER'] = 'sed s/COUNTER-ACCOUNT/PAGED/'
        output = self.terminal(['operation', 'list'], [])
        self.assertIn('PAGED', output)
        self.assertIn('settlement', output)
        self.assertIn('COUNTER-ACCOUNT', self.run_fin('operation', 'list').stdout)
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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        event = next(r['event_id'] for r in rows if r['description'] == 'Relevé différé Carte SAVINGS')
        before = (self.run_fin('account', 'export').stdout,
                  self.run_fin('operation', 'review', '--all', '--json').stdout,
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
                 self.run_fin('operation', 'review', '--all', '--json').stdout,
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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        matches = [r for r in rows if r['description'] == 'Relevé différé Carte SAVINGS']
        chosen = next(r['event_id'] for r in matches if r['amount'] == '-0.2')
        other = next(r['event_id'] for r in matches if r['amount'] == '-0.1')
        self.terminal(['settlement', 'review'], [('Choose an event ID', str(chosen)),
                                                 ('Apply this setup and record this one settlement?', 'y')])
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        by_id = {r['event_id']: r for r in rows}
        self.assertTrue(by_id[chosen]['counter_account'].startswith('liabilities:cards:'))
        self.assertEqual(by_id[other]['counter_account'], 'equity:unclassified')
        self.assertEqual([h['event_id'] for h in json.loads(self.run_fin('settlement', 'history').stdout)], [chosen])

    def test_fullscreen_category_queue(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('cat', 'add', 'expenses:food')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        output = self.fullscreen(['operation', 'category', 'queue', '--key', 'f=expenses:food'], [
            ('Category queue', 'f'),
            ('Staged expenses:food', 'u'),
            ('Last assignment undone.', '/'),
            ('Search:', 'food\r'),
            ('Staged expenses:food', 's'),
            ('Category queue', 'a'),
            ('New category:', 'expenses:coffee\r'),
            ('Added expenses:coffee', '/'),
            ('Search:', 'coffee\r'),
            ('Staged expenses:coffee', 'q'),
        ])
        self.assertIn('Category queue', output)
        after = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        accounts = [row['counter_account'] for row in after]
        self.assertEqual(accounts.count('expenses:food'), 1)
        self.assertEqual(accounts.count('expenses:coffee'), 1)
        self.assertEqual(accounts.count('equity:unclassified'), len(rows) - 2)
        self.assertIn('expenses:coffee', json.loads(self.run_fin('cat', 'list', '--json').stdout))
        self.assertNotEqual(self.run_fin('operation', 'category', 'queue', check=False).returncode, 0)
        master, slave = pty.openpty()
        try:
            result = subprocess.run([FIN, 'operation', 'category', 'queue', '--key', 'q=expenses:food'],
                                    env=self.env, stdin=slave, stdout=slave, stderr=subprocess.PIPE,
                                    text=True, timeout=5)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn('invalid shortcut', result.stderr)
        finally:
            os.close(master)
            os.close(slave)

    def test_category_queue_stages_and_discards_changes(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('cat', 'add', 'expenses:food')
        before = self.run_fin('operation', 'list', '--json').stdout
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 30, 120, 0, 0))
        child = subprocess.Popen([FIN, 'operation', 'category', 'queue', '--key', 'f=expenses:food'],
                                 env=dict(self.env, TERM='xterm-256color'),
                                 stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        def wait_for(expected):
            output = b''
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline and expected not in output:
                if select.select([master], [], [], 0.2)[0]:
                    output += os.read(master, 65536)
            self.assertIn(expected, output)
        try:
            wait_for(b'Category queue')
            os.write(master, b'f')
            wait_for(b'Staged expenses:food')
            self.assertEqual(self.run_fin('operation', 'list', '--json').stdout, before)
            self.assertEqual(json.loads(self.run_fin('operation', 'category', 'history').stdout), [])
            os.write(master, b'a')
            wait_for(b'New category:')
            os.write(master, b'expenses:staged\r')
            wait_for(b'Added expenses:staged')
            self.assertNotIn('expenses:staged', json.loads(self.run_fin('cat', 'list', '--json').stdout))
            os.write(master, b'\x03')
            child.wait(timeout=5)
            self.assertEqual(child.returncode, 0)
            self.assertEqual(self.run_fin('operation', 'list', '--json').stdout, before)
            self.assertEqual(json.loads(self.run_fin('operation', 'category', 'history').stdout), [])
            self.assertNotIn('expenses:staged', json.loads(self.run_fin('cat', 'list', '--json').stdout))
        finally:
            if child.poll() is None:
                child.kill()
                child.wait()
            os.close(master)

    def test_category_queue_rejects_concurrent_changes(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('cat', 'add', 'expenses:food')
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 30, 120, 0, 0))
        child = subprocess.Popen([FIN, 'operation', 'category', 'queue', '--key', 'f=expenses:food'],
                                 env=dict(self.env, TERM='xterm-256color'),
                                 stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        transcript = b''
        match = None
        try:
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline:
                if select.select([master], [], [], 0.2)[0]:
                    transcript += os.read(master, 65536)
                    match = re.search(rb'#(\d+)', transcript)
                    if match:
                        break
            self.assertIsNotNone(match, transcript)
            event = match.group(1).decode()
            self.run_fin('operation', 'category', 'set', 'expenses:other', event)
            os.write(master, b'f')
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline and b'Staged expenses:food' not in transcript:
                if select.select([master], [], [], 0.2)[0]:
                    transcript += os.read(master, 65536)
            self.assertIn(b'Staged expenses:food', transcript)
            os.write(master, b'q')
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline and b'changed since review' not in transcript:
                if select.select([master], [], [], 0.2)[0]:
                    transcript += os.read(master, 65536)
            self.assertIn(b'changed since review', transcript)
            os.write(master, b'\x03')
            child.wait(timeout=5)
            self.assertEqual(child.returncode, 0)
            row = next(row for row in json.loads(self.run_fin('operation', 'list', '--json').stdout)
                       if str(row['event_id']) == event)
            self.assertEqual(row['counter_account'], 'expenses:other')
        finally:
            if child.poll() is None:
                child.kill()
                child.wait()
            os.close(master)

    def test_category_queue_filters_movement_types(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        deposit = next(row for row in rows if row['description'] == 'Initial deposit')
        repeated = [row for row in rows if row['description'] == 'Repeated transfer']
        self.run_fin('operation', 'type', 'set', 'card', deposit['event_id'])
        for row in repeated:
            self.run_fin('operation', 'type', 'set', 'bonus', row['event_id'])
        self.run_fin('operation', 'category', 'set', 'income:other', repeated[0]['event_id'])
        output = self.fullscreen(['operation', 'category', 'queue', '--type', 'card', '--type', 'bonus'], [
            ('Initial deposit', 's'), ('Repeated transfer', 's'), ('Queue complete', 'q')])
        self.assertNotIn('Transfer to another bank', output)
        self.fullscreen(['operation', 'category', 'queue', '--type', 'deposit'], [('Queue complete', 'q')])
        self.run_fin('import', 'wise', 'fixtures/wise.xml')
        unknown = json.loads(self.run_fin('operation', 'list', '--type', 'unknown', '--decision', 'unclassified', '--json').stdout)
        self.assertTrue(unknown)
        output = self.fullscreen(['operation', 'category', 'queue', '--type', 'unknown'], [('Category queue', 'q')])
        self.assertIn(f"{len(unknown)} left", output)
        self.assertNotIn('Initial deposit', output)
        custom = self.root / 'custom-type.csv'
        custom.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                          'Mystery,Current,2026-01-05 10:00:00,2026-01-05 10:00:01,Provider special,1.00,0.00,EUR,COMPLETED,1.00\n')
        self.run_fin('import', 'revolut', custom, '--account-id', 'custom')
        self.fullscreen(['operation', 'category', 'queue', '--type', 'revolut:Mystery'], [('Provider special', 'q')])
        master, slave = pty.openpty()
        try:
            result = subprocess.run([FIN, 'operation', 'category', 'queue', '--type', ''], env=self.env,
                                    stdin=slave, stdout=slave, stderr=subprocess.PIPE, text=True, timeout=5)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn('--type must not be empty', result.stderr)
        finally:
            os.close(master)
            os.close(slave)

    def test_category_queue_exact_matches_and_batch_undo(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        wise_csv = self.root / 'wise-same-merchant.csv'
        wise_csv.write_text(Path('fixtures/wise.csv').read_text().replace('Synthetic shop', '  repeated   TRANSFER '))
        self.run_fin('import', 'wise', 'fixtures/wise.xml', wise_csv)
        self.run_fin('cat', 'add', 'expenses:food')
        output = self.fullscreen(['operation', 'category', 'queue', '--key', 'f=expenses:food'], [
            ('Exact matches: 4', 'f'),
            ('(5 transaction(s))', 'u'),
            ('Last assignment undone.', 'x'),
            ('Exact matches: 4  (x: off)', 'f'),
            ('(1 transaction(s))', 'q'),
        ])
        self.assertIn('Exact matches: 4', output)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        assigned = [row for row in rows if row['counter_account'] == 'expenses:food']
        self.assertEqual(len(assigned), 1)
        self.assertEqual(assigned[0]['institution'], 'wise')
        history = json.loads(self.run_fin('operation', 'category', 'history').stdout)
        self.assertEqual(sum(row['account'] == 'equity:unclassified' for row in history), 0)
        self.assertEqual(sum(row['account'] == 'expenses:food' for row in history), 1)
        card_matches = [row for row in rows if row['institution'] == 'wise'
                        and row['movement_type'] == 'card' and row['counter_account'] == 'equity:unclassified']
        self.assertTrue(card_matches)
        outside = {row['event_id']: row['counter_account'] for row in rows if row['movement_type'] != 'card'}
        self.fullscreen(['operation', 'category', 'queue', '--type', 'card', '--key', 'f=expenses:food'], [
            ('Category queue', 'f'), ('Staged expenses:food', 'q')])
        after = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertTrue(all(row['counter_account'] == 'expenses:food' for row in after
                            if row['event_id'] in {item['event_id'] for item in card_matches}))
        self.assertEqual({row['event_id']: row['counter_account'] for row in after if row['event_id'] in outside}, outside)

    def test_category_queue_batch_rejects_stale_match(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('cat', 'add', 'expenses:food')
        repeated = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated transfer', '--json').stdout)
        self.assertEqual(len(repeated), 2)
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 30, 120, 0, 0))
        child = subprocess.Popen([FIN, 'operation', 'category', 'queue', '--key', 'f=expenses:food'],
                                 env=dict(self.env, TERM='xterm-256color'),
                                 stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        transcript = b''
        try:
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline and b'Initial deposit' not in transcript:
                if select.select([master], [], [], 0.2)[0]:
                    transcript += os.read(master, 65536)
            self.assertIn(b'Initial deposit', transcript)
            os.write(master, b's')
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline and b'Exact matches: 1' not in transcript:
                if select.select([master], [], [], 0.2)[0]:
                    transcript += os.read(master, 65536)
            self.assertIn(b'Exact matches: 1', transcript)
            self.run_fin('operation', 'category', 'set', 'expenses:other', repeated[0]['event_id'])
            os.write(master, b'f')
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline and b'Staged expenses:food' not in transcript:
                if select.select([master], [], [], 0.2)[0]:
                    transcript += os.read(master, 65536)
            self.assertIn(b'Staged expenses:food', transcript)
            os.write(master, b'q')
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline and b'changed since review' not in transcript:
                if select.select([master], [], [], 0.2)[0]:
                    transcript += os.read(master, 65536)
            self.assertIn(b'changed since review', transcript)
            os.write(master, b'\x03')
            child.wait(timeout=5)
            rows = json.loads(self.run_fin('operation', 'list', '--search', 'Repeated transfer', '--json').stdout)
            self.assertEqual({row['counter_account'] for row in rows}, {'expenses:other', 'equity:unclassified'})
        finally:
            if child.poll() is None:
                child.kill()
                child.wait()
            os.close(master)

    def test_category_queue_amount_colors(self):
        self.env.pop('NO_COLOR', None)
        csv = self.root / 'colors.csv'
        csv.write_text('Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance\n'
                       'Transfer,Current,2026-01-01 09:00:00,2026-01-01 09:00:01,Incoming sample,10.00,0.00,EUR,COMPLETED,10.00\n'
                       'Transfer,Current,2026-01-02 09:00:00,2026-01-02 09:00:01,Outgoing sample,-10.00,0.00,EUR,COMPLETED,0.00\n'
                       'Transfer,Current,2026-01-03 09:00:00,2026-01-03 09:00:01,Neutral sample,0.00,0.00,EUR,COMPLETED,0.00\n')
        self.run_fin('import', 'revolut', csv, '--account-id', 'colors')
        answers = [('Incoming sample', 's'), ('Outgoing sample', 's'), ('Neutral sample', 'q')]
        output = self.fullscreen(['operation', 'category', 'queue'], answers)
        self.assertRegex(output, r'\x1b\[[0-9;]*32m[^\x1b]*10 EUR')
        self.assertRegex(output, r'\x1b\[[0-9;]*31m[^\x1b]*-10 EUR')
        self.assertNotRegex(output, r'\x1b\[[0-9;]*(?:31|32)m[^\x1b]*\b0 EUR')
        self.env['NO_COLOR'] = '1'
        plain = self.fullscreen(['operation', 'category', 'queue'], answers)
        self.assertNotRegex(plain, r'\x1b\[[0-9;]*(?:31|32)m')

    def test_category_queue_hierarchical_browsing(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        self.run_fin('cat', 'add', 'expenses:food')
        self.run_fin('cat', 'add', 'expenses:food:restaurants')
        self.fullscreen(['operation', 'category', 'queue'], [
            ('Category queue', 'b'),
            ('Browse: root', '\r'),
            ('Browse: expenses', '\r'),
            ('Browse: expenses:food', '\r'),
            ('Staged expenses:food:restaurants', 'q'),
        ])
        self.fullscreen(['operation', 'category', 'queue'], [
            ('Category queue', 'b'),
            ('Browse: root', '\r'),
            ('Browse: expenses', ' '),
            ('Staged expenses:food', 'q'),
        ])
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(sum(row['counter_account'] == 'expenses:food:restaurants' for row in rows), 1)
        self.assertEqual(sum(row['counter_account'] == 'expenses:food' for row in rows), 2)
        self.fullscreen(['operation', 'category', 'queue'], [('Too small', 'q')], size=(8, 40))
        self.assertEqual(rows, json.loads(self.run_fin('operation', 'list', '--json').stdout))

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
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        event = next(r['event_id'] for r in rows if r['source_account'] == 'CHECKING' and r['amount'] == '-0.1')
        output = self.terminal(['settlement', 'review', str(event)], [('Record this card settlement? [Y/n]:', 'no')])
        self.assertIn('Nothing changed.', output)
        output = self.terminal(['settlement', 'review', str(event)], [('Record this card settlement? [Y/n]:', 'yes')])
        self.assertIn('Card settlement recorded.', output)
        result = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(next(r for r in result if r['event_id'] == event)['counter_account'], 'liabilities:cards:example')
        self.assertNotEqual(self.run_fin('settlement', 'review', event, '--yes', check=False).returncode, 0)
        self.run_fin('settlement', 'undo', event, '--yes')
        result = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(next(r for r in result if r['event_id'] == event)['counter_account'], 'equity:unclassified')
        history = json.loads(self.run_fin('settlement', 'history').stdout)
        self.assertEqual([r['action'] for r in history], ['confirmed', 'undone'])
        self.assertTrue(all(r['event_id'] == event for r in history))

    def test_builtin_bunq_and_revolut_definitions(self):
        for institution, kind, sample in [('bunq', 'mt940-csv', 'fixtures/bunq.mt940'),
                                          ('revolut', 'running-csv', 'fixtures/revolut.csv')]:
            with self.subTest(institution=institution):
                profile = json.loads(self.run_fin('institution', 'definition', 'show', institution).stdout)
                self.assertEqual(profile['kind'], kind)
                path = self.root / (institution + '.json')
                path.write_text(json.dumps(profile))
                preview = self.run_fin('institution', 'definition', 'check', path, sample).stdout
                self.assertIn('transactions', preview)
                self.assertIn('no data was imported', preview)

    def test_primary_csv_institution_definition(self):
        definition = self.root / 'csv-definition.json'
        definition.write_text(json.dumps({'version': 1, 'kind': 'csv', 'csv': {
            'institution': 'examplecard', 'aliases': ['example'], 'profile': {
                'separator': ',', 'header': ['Date', 'Merchant', 'Amount'],
                'date': 'Date', 'description': 'Merchant', 'amount': 'Amount',
                'currency': 'EUR', 'account_type': 'prepaid-card',
                'invert_amount': True, 'skip_column': None, 'skip_value': None}}}))
        sample = self.root / 'example.csv'
        sample.write_text('Date,Merchant,Amount\n2025-01-01,Shop,12.50\n')
        self.assertIn('1 transactions', self.run_fin('institution', 'definition', 'check', definition, sample).stdout)
        self.run_fin('institution', 'add', 'examplecard')
        self.run_fin('institution', 'definition', 'set', 'examplecard', definition, '--yes')
        self.run_fin('import', 'example', sample, '--account-id', 'main')
        self.run_fin('import', 'examplecard', sample, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--institution', 'examplecard', '--json').stdout)
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]['amount'], '-12.5')

    def test_definition_rules_are_previewed_and_preserve_edits(self):
        self.run_fin('institution', 'add', 'examplecard')
        self.run_fin('rule', 'add', 'existing-rule', '--institution', 'examplecard', '--description', '^Shop$', '--set-type', 'fee', '--automatic')
        definition = self.root / 'rules-definition.json'
        data = {'version': 1, 'kind': 'csv', 'csv': {
            'institution': 'examplecard', 'aliases': [], 'profile': {
                'separator': ',', 'header': ['Date', 'Merchant', 'Amount'],
                'date': 'Date', 'description': 'Merchant', 'amount': 'Amount',
                'currency': 'EUR', 'account_type': 'prepaid-card',
                'invert_amount': True, 'skip_column': None, 'skip_value': None}},
            'rules': [
                {'name': 'existing-rule', 'pattern': '^Shop$', 'action': 'type', 'target': 'card'},
                {'name': 'bonus-default', 'pattern': '^Gift$', 'action': 'type', 'target': 'bonus'}]}
        definition.write_text(json.dumps(data))
        transcript = self.terminal(['institution', 'definition', 'set', 'examplecard', str(definition)], [('Attach this definition?', 'n')])
        self.assertIn('1 new rules, 1 existing rules retained', transcript)
        self.assertIn('Nothing changed.', transcript)
        before = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        self.assertFalse(any(r['name'] == 'bonus-default' for r in before))
        self.run_fin('institution', 'definition', 'set', 'examplecard', definition, '--yes')
        after = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        self.assertEqual(next(r['target'] for r in after if r['name'] == 'existing-rule'), 'fee')
        self.assertEqual(next(r['target'] for r in after if r['name'] == 'bonus-default'), 'bonus')
        sample = self.root / 'example.csv'
        sample.write_text('Date,Merchant,Amount\n2025-01-01,Shop,12.50\n2025-01-02,Gift,5.00\n')
        self.run_fin('import', 'examplecard', sample, '--account-id', 'main')
        transactions = json.loads(self.run_fin('operation', 'list', '--institution', 'examplecard', '--json').stdout)
        self.assertEqual({r['movement_type'] for r in transactions}, {'fee', 'bonus'})
        data['rules'].append({'name': 'extra-default', 'pattern': '^Other$', 'action': 'type', 'target': 'card'})
        definition.write_text(json.dumps(data))
        self.run_fin('institution', 'definition', 'set', 'examplecard', definition, '--yes')
        rules = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        self.assertTrue(any(r['name'] == 'extra-default' for r in rules))
        self.assertEqual({r['movement_type'] for r in json.loads(self.run_fin('operation', 'list', '--institution', 'examplecard', '--json').stdout)}, {'fee', 'bonus'})
        data['csv']['profile']['invert_amount'] = False
        definition.write_text(json.dumps(data))
        self.assertNotEqual(self.run_fin('institution', 'definition', 'set', 'examplecard', definition, '--yes', check=False).returncode, 0)

    def test_runtime_institution_definition_and_sample_preview(self):
        profile = json.loads(self.run_fin('institution', 'definition', 'show', 'wise').stdout)
        profile['camt']['institution'] = 'samplebank'
        profile['camt']['camt_profile']['institution'] = 'samplebank'
        profile['camt']['camt_profile']['event_kind'] = 'samplebank-transaction'
        profile['camt']['camt_profile']['payload_tag'] = 'samplebank-camt-v1'
        profile['camt']['csv_profile']['institution'] = 'samplebank'
        profile['camt']['metadata_prefix'] = 'samplebank'
        profile['camt']['aliases'] = ['sample']
        definition = self.root / 'samplebank.json'
        definition.write_text(json.dumps(profile))
        preview = self.run_fin('institution', 'definition', 'check', definition, 'fixtures/wise.xml').stdout
        self.assertIn('3 transactions', preview)
        self.assertIn('no data was imported', preview)
        self.assertEqual(json.loads(self.run_fin('operation', 'list', '--json').stdout), [])
        self.run_fin('institution', 'add', 'samplebank')
        self.run_fin('institution', 'definition', 'set', 'samplebank', definition, '--yes')
        self.assertIn('samplebank (bank, definition)', self.run_fin('institution', 'list').stdout)
        self.assertEqual(profile['type'], 'bank')
        typed = dict(profile, type='crypto-exchange')
        definition.write_text(json.dumps(typed))
        self.run_fin('institution', 'definition', 'set', 'samplebank', definition, '--yes')
        self.assertIn('samplebank (crypto exchange, definition)', self.run_fin('institution', 'list').stdout)
        definition.write_text(json.dumps(profile))
        self.run_fin('institution', 'definition', 'set', 'samplebank', definition, '--yes')
        self.run_fin('import', 'sample', 'fixtures/wise.csv', 'fixtures/wise.xml')
        rows = json.loads(self.run_fin('operation', 'list', '--institution', 'samplebank', '--json').stdout)
        self.assertEqual(len(rows), 3)
        definition.unlink()
        self.assertEqual(len(json.loads(self.run_fin('operation', 'list', '--institution', 'samplebank', '--json').stdout)), 3)
        self.assertNotEqual(self.run_fin('institution', 'delete', 'samplebank', '--yes', check=False).returncode, 0)
        invalid = profile.copy()
        invalid['version'] = 99
        definition.write_text(json.dumps(invalid))
        self.assertNotEqual(self.run_fin('institution', 'definition', 'check', definition, check=False).returncode, 0)

    def test_wise_balance_cashback_interest_preserves_category(self):
        self.run_fin('import', 'wise', 'fixtures/wise.csv', 'fixtures/wise.xml')
        rows = json.loads(self.run_fin('operation', 'list', '--institution', 'wise', '--json').stdout)
        interest = [row for row in rows if row['movement_type'] == 'interest']
        self.assertEqual(len(interest), 1)
        event = interest[0]['event_id']
        self.run_fin('operation', 'category', 'set', 'income:interest:wise', event)
        arguments = ['rule', 'add', 'wise-id-cashback', '--institution', 'wise', '--field', 'TransferWise ID',
                     '--description', '^BALANCE_CASHBACK-', '--case-sensitive', '--automatic', '--priority=-99', '--replace']
        self.run_fin(*arguments, '--set-type', 'bonus')
        self.run_fin(*arguments, '--set-type', 'interest')
        after = json.loads(self.run_fin('operation', 'list', '--institution', 'wise', '--json').stdout)
        row = next(row for row in after if row['event_id'] == event)
        self.assertEqual(row['movement_type'], 'interest')
        self.assertEqual(row['counter_account'], 'income:interest:wise')
        self.assertIn('wise-id-cashback', self.run_fin('rule', 'explain', event).stdout)

    def test_column_rules_retype_at_once(self):
        # This is about the bank's code, so drop the bundled rules on TransferWise ID.
        for name in ('wise-id-fee', 'wise-id-cashback'):
            self.run_fin('rule', 'delete', name, '--yes')
        self.run_fin('import', 'wise', 'fixtures/wise.csv', 'fixtures/wise.xml')
        rules = json.loads(self.run_fin('rule', 'list', '--json').stdout)
        self.assertTrue(any(r['name'] == 'wise-reported-card' for r in rules))
        self.run_fin('rule', 'add', 'my-card', '--institution', 'wise', '--field', 'Transaction Details Type',
                     '--description', '^CARD$', '--set-type', 'bonus', '--case-sensitive', '--automatic',
                     '--priority=-1000')
        # Types are derived from the rules: saving, disabling and deleting a rule retype at once.
        after = json.loads(self.run_fin('operation', 'list', '--institution', 'wise', '--json').stdout)
        self.assertTrue(all(r['movement_type'] == 'bonus' for r in after))
        self.run_fin('rule', 'mode', 'my-card', 'disabled')
        off = json.loads(self.run_fin('operation', 'list', '--institution', 'wise', '--json').stdout)
        self.assertTrue(all(r['movement_type'] == 'card' for r in off))
        self.run_fin('rule', 'mode', 'my-card', 'automatic')
        on = json.loads(self.run_fin('operation', 'list', '--institution', 'wise', '--json').stdout)
        self.assertTrue(all(r['movement_type'] == 'bonus' for r in on))
        # Every change is in the type history, with the rule that caused it.
        history = self.run_fin('operation', 'type', 'history').stdout
        self.assertIn('rule my-card', history)

    def test_every_column_of_a_primary_csv_is_a_field(self):
        # Revolut: the columns the payload drops are kept beside the event.
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        deposit = next(r['event_id'] for r in rows if r['description'] == 'Initial deposit')
        shown = self.run_fin('operation', 'show', deposit).stdout
        self.assertIn('Source columns (stored, matchable by rules)', shown)
        self.assertRegex(shown, r'Product\s+Current')
        self.assertRegex(shown, r'Balance\s+150')
        test = self.run_fin('rule', 'test', 'by-balance', '--institution', 'revolut', '--field', 'Balance',
                            '--description', '^150', '--set-type', 'bonus').stdout
        self.assertIn('1 match.', test)
        # A user-defined institution: a column the profile never named is a field too.
        csv_path = self.root / 'extra.csv'
        csv_path.write_text('Date,Description,EUR,Merchant ID\n2026-10-01,Rent,-120.00,M-100\n'
                            '2026-10-02,Salary,950.00,M-200\n')
        self.terminal(['import', str(csv_path), '--account-id', 'main', '--provider', 'mybank'], [
            ('Use these columns?', '1'),
            ('Account type:', '3'),
            ('Are outgoing amounts positive in the file?', '2'),
            ('Skip a summary row with an empty date?', '2'),
            ('Import these transactions?', '1'),
            ('IBAN (Enter to skip):', ''),
        ])
        test = self.run_fin('rule', 'test', 'by-merchant', '--institution', 'mybank', '--field', 'Merchant ID',
                            '--description', '^M-2', '--set-type', 'deposit').stdout
        self.assertIn('1 match.', test)
        self.assertIn('Salary', test)
        self.assertIn('newly typed: 1', test)
        own = next(r['event_id'] for r in json.loads(self.run_fin('operation', 'list', '--institution', 'mybank', '--json').stdout)
                   if r['description'] == 'Rent')
        self.assertRegex(self.run_fin('operation', 'show', own).stdout, r'Merchant ID\s+M-100')

    def test_type_rules_can_match_a_named_field_of_the_primary(self):
        # A CAMT statement alone: the bank's transaction code is the field reference.
        self.run_fin('import', 'wise', 'fixtures/wise.xml')
        test = self.run_fin('rule', 'test', 'by-ref', '--institution', 'wise', '--field', 'reference',
                            '--description', '^BALANCE_CASHBACK-', '--set-type', 'bonus').stdout
        self.assertIn('1 match.', test)
        self.assertIn('newly typed: 1', test)
        # The field is part of the saved rule and needs an automatic rule.
        refused = self.run_fin('rule', 'add', 'by-ref', '--institution', 'wise', '--field', 'reference',
                               '--description', '^BALANCE_CASHBACK-', '--set-type', 'bonus', check=False)
        self.assertNotEqual(refused.returncode, 0)
        self.run_fin('rule', 'add', 'by-ref', '--institution', 'wise', '--field', 'reference',
                     '--description', '^BALANCE_CASHBACK-', '--set-type', 'bonus', '--automatic')
        listed = {r['name']: r for r in json.loads(self.run_fin('rule', 'list', '--json').stdout)}
        self.assertEqual(listed['by-ref']['field'], 'reference')
        rows = json.loads(self.run_fin('operation', 'list', '--institution', 'wise', '--json').stdout)
        self.assertEqual(sorted(r['movement_type'] or '' for r in rows), ['', '', 'bonus'])
        event = next(r['event_id'] for r in rows if r['movement_type'] == 'bonus')
        explain = self.run_fin('rule', 'explain', event).stdout
        self.assertIn('decides  by-ref', explain)
        self.assertIn('[reference: BALANCE_CASHBACK-', explain)

    def test_tx_show_lists_source_files_fields_and_companions(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        deposit = next(r['event_id'] for r in rows if r['description'] == 'Initial deposit')
        out = self.run_fin('operation', 'show', deposit).stdout
        self.assertIn('Type: deposit (type rules)', out)
        self.assertIn('Imported from', out)
        self.assertIn('file: ' + str(self.csv), out)
        self.assertIn('"transaction_type": "Deposit"', out)
        self.assertIn('Companion files: no matched rows', out)
        self.assertIn('fin rule explain OPERATION-ID', out)
        self.run_fin('import', 'wise', 'fixtures/wise.csv', 'fixtures/wise.xml')
        wise = json.loads(self.run_fin('operation', 'list', '--institution', 'wise', '--json').stdout)[0]['event_id']
        out = self.run_fin('operation', 'show', wise).stdout
        self.assertIn('Companion files: 1 matched', out)
        self.assertRegex(out, r'TransferWise ID\s+\S+')
        self.assertIn('Transaction Details Type', out)
        self.assertIn('display name: Synthetic shop', out)
        self.assertIn('wise-csv-v1', out)
        self.assertNotEqual(self.run_fin('operation', 'show', '999999', check=False).returncode, 0)

    def test_type_rules_can_match_any_companion_column(self):
        for name in ('wise-id-fee', 'wise-id-cashback'):
            self.run_fin('rule', 'delete', name, '--yes')
        self.run_fin('import', 'wise', 'fixtures/wise.csv', 'fixtures/wise.xml')
        # The cashback row's bank code is CARD; its TransferWise ID says otherwise.
        self.run_fin('rule', 'add', 'id-cashback', '--institution', 'wise', '--field', 'TransferWise ID',
                     '--description', '^BALANCE_CASHBACK-', '--set-type', 'bonus', '--case-sensitive',
                     '--automatic', '--priority=-2000')
        listed = {r['name']: r for r in json.loads(self.run_fin('rule', 'list', '--json').stdout)}
        self.assertEqual(listed['id-cashback']['field'], 'TransferWise ID')
        self.assertEqual(listed['wise-reported-card']['field'], 'Transaction Details Type')
        # The rule retypes at once; no refresh step exists any more.
        types = sorted(r['movement_type'] for r in json.loads(self.run_fin('operation', 'list', '--institution', 'wise', '--json').stdout))
        self.assertEqual(types, ['bonus', 'card', 'card'])
        self.assertNotEqual(self.run_fin(('op'), 'refresh-types', check=False).returncode, 0)
        # Removing the rule puts the bank code back, and both changes are in the history.
        self.run_fin('rule', 'delete', 'id-cashback', '--yes')
        types = sorted(r['movement_type'] for r in json.loads(self.run_fin('operation', 'list', '--institution', 'wise', '--json').stdout))
        self.assertEqual(types, ['card', 'card', 'card'])
        history = json.loads(self.run_fin('operation', 'type', 'history', '--json').stdout)
        self.assertEqual(sorted(h['reason'] for h in history),
                         ['rule id-cashback added', 'rule id-cashback deleted'])
        # Only automatic type rules may name a column.
        bad = self.run_fin('rule', 'add', 'x', '--description', 'a', '--set-type', 'card', '--field', 'A', check=False)
        self.assertNotEqual(bad.returncode, 0)

    def test_wise_definition_uses_merchant_as_companion_description(self):
        definition = json.loads(self.run_fin('institution', 'definition', 'show', 'wise').stdout)
        self.assertEqual(definition['camt']['csv_profile']['columns']['description'], 'Merchant')

    def test_wise_enrichment_shows_merchant_and_matches_different_descriptions(self):
        self.run_fin('import', 'wise', 'fixtures/wise.xml', 'fixtures/wise.csv')
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        self.assertEqual(len(rows), 3)
        self.assertTrue(all(r['description'] == 'Synthetic shop' and r['detail_sources'] for r in rows))
        # The shipped rules on TransferWise ID type the fixture's cashback row, whose
        # bank code is CARD, as interest.
        self.assertEqual(sorted(r['movement_type'] for r in rows), ['card', 'card', 'interest'])
        self.assertEqual({r['description'] for r in rows}, {'Synthetic shop'})
        self.assertTrue(all('display_name' not in r for r in rows))
        self.assertIn('Synthetic shop', self.run_fin('journal', 'export').stdout)
        output = self.run_fin('operation', 'list').stdout
        self.assertIn('Synthetic shop', output)
        self.assertIn('card', output)
        # Similarity uses the one effective description regardless of movement type.
        seed = next(r for r in rows if r['movement_type'] == 'card')
        decisions = self.root / 'wise-category.json'
        decisions.write_text(json.dumps([{'event_id': seed['event_id'],
                                          'account': 'expenses:shopping', 'reason': 'Reviewed'}]))
        self.run_fin('operation', 'category', 'apply', decisions)
        result = json.loads(self.run_fin('operation', 'category', 'similar', seed['event_id'],
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
        asc = json.loads(self.run_fin('operation', 'list', '--json', '--asc').stdout)
        desc = json.loads(self.run_fin('operation', 'list', '--json', '--desc').stdout)
        self.assertEqual([r['event_id'] for r in asc], [r['event_id'] for r in reversed(desc)])
        self.assertEqual([r['description'] for r in asc],
                         ['Initial deposit', 'Earlier transfer', 'Later transfer',
                          'Transfer to another bank'])
        first = json.loads(self.run_fin('operation', 'list', '--json', '--asc',
                                        '--limit', '1').stdout)
        self.assertEqual(first, asc[:1])
        self.assertNotEqual(self.run_fin('operation', 'list', '--order-by', 'invalid',
                                         check=False).returncode, 0)

    def test_transaction_list_sorts_multiple_columns_and_exact_amounts(self):
        self.run_fin('import', 'revolut', self.csv, '--account-id', 'main')
        ofx = self.root / 'exact-sort.ofx'
        extra = ('<STMTTRN><DTPOSTED>20260104</DTPOSTED><TRNTYPE>CREDIT</TRNTYPE>'
                 '<TRNAMT>0.10000000000000000002</TRNAMT><FITID>precise-high</FITID><NAME>precise High</NAME></STMTTRN>'
                 '<STMTTRN><DTPOSTED>20260104</DTPOSTED><TRNTYPE>CREDIT</TRNTYPE>'
                 '<TRNAMT>0.10000000000000000001</TRNAMT><FITID>precise-low</FITID><NAME>precise Low</NAME></STMTTRN>')
        ofx.write_text(Path('fixtures/bank.ofx').read_text().replace('</BANKTRANLIST>', extra + '</BANKTRANLIST>', 1))
        self.run_fin('import', 'boursobank', ofx)
        sample = json.loads(self.run_fin('operation', 'list', '--json').stdout)[0]['event_id']
        self.run_fin('operation', 'type', 'set', 'bonus', sample)
        rows = json.loads(self.run_fin('operation', 'list', '--json').stdout)
        def key(row, column):
            return {'date': lambda r: r['booked_on'], 'amount': lambda r: Decimal(r['amount']),
                    'currency': lambda r: r['currency'], 'institution': lambda r: r['institution'],
                    'account': lambda r: r['account_id'], 'type': lambda r: r['movement_type'] or '-',
                    'counter-account': lambda r: '-' if r['counter_account'] == 'equity:unclassified' else r['counter_account'],
                    'description': lambda r: r['description'], 'event': lambda r: int(r['event_id'])}[column](row)
        for columns in (['amount'], ['date'], ['currency'], ['institution'], ['account'], ['type'],
                        ['counter-account'], ['description'], ['event'], ['type', 'currency', 'date'],
                        ['institution', 'amount', 'description']):
            expected = sorted(rows, key=lambda row: tuple(key(row, col) for col in columns) + (int(row['event_id']),))
            ordered = json.loads(self.run_fin('operation', 'list', '--json', '--order-by', ','.join(columns), '--asc').stdout)
            self.assertEqual([r['event_id'] for r in ordered], [r['event_id'] for r in expected], columns)
            desc = json.loads(self.run_fin('operation', 'list', '--json', '--order-by', ','.join(columns), '--desc').stdout)
            self.assertEqual([r['event_id'] for r in desc], [r['event_id'] for r in reversed(expected)], columns)
        precise = json.loads(self.run_fin('operation', 'list', '--json', '--order-by', 'amount', '--asc', '--search', 'precise').stdout)
        self.assertEqual([r['description'] for r in precise], ['precise Low', 'precise High'])
        self.assertEqual(json.loads(self.run_fin('operation', 'list', '--json', '--order-by', 'account_id,event_id', '--asc').stdout),
                         json.loads(self.run_fin('operation', 'list', '--json', '--order-by', 'account,event', '--asc').stdout))
        filtered = json.loads(self.run_fin('operation', 'list', '--json', '--institution', 'revolut', '--order-by', 'amount', '--asc', '--limit', '2').stdout)
        expected = sorted((r for r in rows if r['institution'] == 'revolut'), key=lambda r: (Decimal(r['amount']), int(r['event_id'])))[:2]
        self.assertEqual([r['event_id'] for r in filtered], [r['event_id'] for r in expected])
        for arguments in (('--order-by', ''), ('--order-by', 'date,bad'), ('--order-by', 'event,event-id'), ('--asc', '--desc')):
            self.assertNotEqual(self.run_fin('operation', 'list', *arguments, check=False).returncode, 0)

    def test_welcome_and_init_show_transaction_list(self):
        welcome = self.run_fin().stdout
        self.assertIn('fin operation list', welcome)
        self.assertIn('fin institution list', welcome)
        self.assertIn('fin institution add', welcome)
        self.assertNotIn('bank statements', welcome)
        db = self.root / 'separate.sqlite3'
        output = self.run_fin('init', '--database', db).stdout
        for command in ('fin institution list', 'fin institution add', 'fin import INSTITUTION'):
            self.assertIn(command, output)
            line = next(line for line in output.splitlines() if command in line)
            self.assertTrue(shlex.split(line)[-2:] == ['--database', str(db)])
        line = next(line for line in output.splitlines() if 'fin operation list' in line)
        self.assertEqual(shlex.split(line), ['fin', 'operation', 'list', '--database', str(db)])

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
        review = json.loads(self.run_fin('operation', 'review', '--json').stdout)
        decisions = self.root / 'decisions.json'
        decisions.write_text(json.dumps([{'event_id': review[0]['event_id'],
                                         'account': 'expenses:food', 'reason': 'Reviewed'}]))
        self.run_fin('operation', 'category', 'apply', decisions)
        expected = {name: self.run_fin(*args).stdout for name, args in {
            'accounts': ['account', 'export'],
            'events': ['db', 'events'],
            'review': ['operation', 'review', '--all', '--json'],
            'history': ['operation', 'category', 'history'],
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
            'review': ['operation', 'review', '--all', '--json'],
            'history': ['operation', 'category', 'history'], 'journal': ['journal', 'export'],
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
