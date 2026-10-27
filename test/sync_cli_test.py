"""Exercise terminal-only sync onboarding against a disposable database."""
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

    def test_existing_config_keeps_answer_on_reapply(self):
        self.terminal(['bank', 'sync', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', '')])
        self.config.parent.mkdir(parents=True, exist_ok=True)
        self.config.write_text(self.run_fin('accounts', 'export').stdout)
        # Rebuild with an existing configuration whose explicit null must be
        # replaced. The USD account is new and is absent from the config.
        (self.root / 'data/fin/fin.sqlite3').unlink()
        self.run_fin('init')
        self.terminal(['bank', 'sync', 'revolut', self.csv, self.usd,
                       '--account-id', 'main'], [('IBAN (Enter to skip):', IBAN)])
        saved = json.loads(self.config.read_text())['accounts']
        self.assertEqual(len(saved), 2)
        self.assertTrue(all(row['iban'] == IBAN for row in saved))
        self.run_fin('accounts', 'apply')
        self.assertTrue(all(row['iban'] == IBAN for row in self.accounts()))

    def test_explicit_config_preserves_unrelated_settings(self):
        self.terminal(['bank', 'sync', 'revolut', self.csv, '--account-id', 'main'],
                      [('IBAN (Enter to skip):', '')])
        config = self.root / 'explicit.json'
        config.write_text(self.run_fin('accounts', 'export').stdout)
        previous = json.loads(config.read_text())['accounts'][0]
        self.terminal(['bank', 'sync', 'revolut', self.usd, '--account-id', 'main',
                       '--accounts', config], [('IBAN (Enter to skip):', IBAN)])
        saved = json.loads(config.read_text())['accounts']
        self.assertEqual(saved[0], previous)
        self.assertEqual(saved[1]['iban'], IBAN)
        self.run_fin('accounts', 'apply', config)
        self.assertTrue(any(row['iban'] == IBAN for row in self.accounts()))
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

    def test_card_never_prompts(self):
        csv = self.root / 'card.ofx'
        csv.write_text(Path('fixtures/bank.ofx').read_text()
                       .replace('<ACCTTYPE>CHECKING</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>')
                       .replace('<ACCTTYPE>SAVINGS</ACCTTYPE>', '<ACCTTYPE>CREDITCRD</ACCTTYPE>'))
        output = self.terminal(['bank', 'sync', 'boursobank', csv], [])
        self.assertNotIn('IBAN (Enter to skip):', output)


if __name__ == '__main__':
    unittest.main()
