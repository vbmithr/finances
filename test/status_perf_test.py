"""Guard status performance on a reproducible synthetic multi-account database."""
import csv
import datetime
import json
import os
from pathlib import Path
import resource
import statistics
import subprocess
import sys
import tempfile
import time

FIN = str(Path(sys.argv[1]).resolve())
ROWS_PER_ACCOUNT = 1100
ACCOUNTS = 4
# CPU time excludes scheduler delays. Override on slower machines when needed.
MAX_CPU_SECONDS = float(os.environ.get('FIN_STATUS_MAX_CPU_SECONDS', '0.60'))


def run(*args, **kwargs):
    result = subprocess.run([FIN, *map(str, args)], stdin=subprocess.DEVNULL,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, **kwargs)
    if result.returncode:
        raise RuntimeError(f'{args}: {result.stdout.decode()} {result.stderr.decode()}')
    return result


def main():
    with tempfile.TemporaryDirectory(prefix='fin-status-perf-') as directory:
        root = Path(directory)
        db = root / 'fin.sqlite3'
        run('init', '--database', db)
        header = ['Type', 'Product', 'Started Date', 'Completed Date', 'Description',
                  'Amount', 'Fee', 'Currency', 'State', 'Balance']
        for account in range(ACCOUNTS):
            path = root / f'account-{account}.csv'
            balance = 0
            with path.open('w', newline='') as stream:
                writer = csv.writer(stream)
                writer.writerow(header)
                for i in range(ROWS_PER_ACCOUNT):
                    clock = datetime.datetime(2020, 1, 1) + datetime.timedelta(hours=i)
                    amount = (1 if (i + account) % 2 else -1) * (1 + i % 900)
                    balance += amount
                    stamp = clock.strftime('%Y-%m-%d %H:%M:%S')
                    writer.writerow(['Transfer', 'Current', stamp, stamp, f'Payment {account}-{i}',
                                     str(amount), '0', 'EUR', 'COMPLETED', str(balance)])
            definition = root / f'account-{account}.json'
            definition.write_text(json.dumps({
                'format': 'fin-csv-converter', 'version': 1, 'institution': 'revolut',
                'institution_type': 'bank', 'header': header,
                'account': {'id': f'account-{account}', 'type': 'current',
                            'currency': {'fiat': 'Currency'}},
                'occurred': {'column': 'Completed Date', 'layout': 'iso-datetime'},
                'cases': [{'movements': [{'role': 'primary', 'quantity': {'column': 'Amount'},
                                          'commodity': {'fiat': 'Currency'}}]}],
                'descriptions': ['Description'], 'balance_after': 'Balance',
            }))
            records = root / f'account-{account}.jsonl'
            run('convert', '--definition', definition, path, '-o', records)
            run('import', 'revolut', records, '--database', db)
        def status():
            before = resource.getrusage(resource.RUSAGE_CHILDREN)
            start = time.monotonic()
            result = run('status', '--json', '--database', db)
            after = resource.getrusage(resource.RUSAGE_CHILDREN)
            cpu = (after.ru_utime + after.ru_stime) - (before.ru_utime + before.ru_stime)
            return result, cpu, time.monotonic() - start

        result, _, _ = status()  # Warm the filesystem cache and check the fixture.
        report = json.loads(result.stdout)
        if report['movements'] != ROWS_PER_ACCOUNT * ACCOUNTS:
            raise AssertionError(f"unexpected movement count: {report['movements']}")
        measurements = [status() for _ in range(5)]
        cpu = statistics.median(sample[1] for sample in measurements)
        wall = statistics.median(sample[2] for sample in measurements)
        print(f'status: {ROWS_PER_ACCOUNT * ACCOUNTS} movements, '
              f'median CPU {cpu * 1000:.1f} ms, wall {wall * 1000:.1f} ms '
              f'(CPU limit {MAX_CPU_SECONDS * 1000:.0f} ms)', flush=True)
        if cpu > MAX_CPU_SECONDS:
            raise SystemExit('status performance regression: median CPU time exceeds limit')

        # A page of the list reads only that page, so it costs no more than status.
        def listing(*options):
            before = resource.getrusage(resource.RUSAGE_CHILDREN)
            result = run('operation', 'list', '--json', '--database', db, *options)
            after = resource.getrusage(resource.RUSAGE_CHILDREN)
            return result, (after.ru_utime + after.ru_stime) - (before.ru_utime + before.ru_stime)
        page = [listing('--limit', '20') for _ in range(5)]
        if len(json.loads(page[0][0].stdout)) != 20:
            raise AssertionError('expected a page of 20 operations')
        cpu = statistics.median(sample[1] for sample in page)
        print(f'operation list --limit 20: median CPU {cpu * 1000:.1f} ms '
              f'(CPU limit {MAX_CPU_SECONDS * 1000:.0f} ms)', flush=True)
        if cpu > MAX_CPU_SECONDS:
            raise SystemExit('operation list performance regression: median CPU time exceeds limit')


if __name__ == '__main__':
    main()


def scaling_database(root, size):
    """A database of this many bank rows, each a confirmed transfer, and as many trades."""
    env = dict(os.environ, HOME=str(root), XDG_DATA_HOME=str(root / 'd'), XDG_CONFIG_HOME=str(root / 'c'),
               XDG_STATE_HOME=str(root / 's'), XDG_CACHE_HOME=str(root / 'k'))
    fin = lambda *args: subprocess.run([FIN, *map(str, args)], env=env, stdin=subprocess.DEVNULL,
                                       capture_output=True, text=True, check=True)
    fin('init')
    bank = [['Type', 'Product', 'Started Date', 'Completed Date', 'Description', 'Amount', 'Fee', 'Currency', 'State', 'Balance']]
    ledger = [['txid', 'refid', 'time', 'type', 'subtype', 'aclass', 'asset', 'amount', 'fee', 'balance']]
    balance, received = 10 ** 7, 0
    for i in range(size):
        clock = (datetime.datetime(2015, 1, 1) + datetime.timedelta(hours=2 * i)).strftime('%Y-%m-%d %H:%M:%S')
        amount = 1000 + i
        balance -= amount
        received += amount
        bank.append(['Transfer', 'Current', clock, clock, f'Row {i}', f'-{amount}.00', '0.00', 'EUR', 'COMPLETED', f'{balance}.00'])
        ledger.append([f'L{i}', f'R{i}', clock, 'deposit', '', 'currency', 'ZEUR', f'{amount}.00', '0.00', f'{received}.00'])
        ledger.append([f'B{i}', f'T{i}', clock, 'trade', '', 'currency', 'XXBT', '1', '0', str(i + 1)])
        ledger.append([f'C{i}', f'T{i}', clock, 'trade', '', 'currency', 'XETH', '1', '0', str(i + 1)])
    for name, rows in (('bank.csv', bank), ('ledger.csv', ledger)):
        with open(root / name, 'w', newline='') as out:
            csv.writer(out).writerows(rows)
    fin('import', 'revolut', root / 'bank.csv')
    fin('import', 'kraken', root / 'ledger.csv')
    pairs = json.loads(fin('transfer', 'candidates', '--json').stdout)['proposed_pairs']
    assert len(pairs) == size, len(pairs)
    decisions = root / 'decisions.json'
    decisions.write_text(json.dumps([
        {'first_event_id': p['first_event_id'], 'second_event_id': p['second_event_id'],
         'decision': 'confirm', 'reason': 'Scaling'} for p in pairs]))
    fin('transfer', 'review', decisions)
    return env


def cpu(env, *args):
    samples = []
    for _ in range(3):
        before = resource.getrusage(resource.RUSAGE_CHILDREN)
        subprocess.run([FIN, *args], env=env, stdin=subprocess.DEVNULL, capture_output=True, check=True)
        after = resource.getrusage(resource.RUSAGE_CHILDREN)
        samples.append((after.ru_utime + after.ru_stime) - (before.ru_utime + before.ru_stime))
    return statistics.median(samples)


# A cost that grows with the square of the data passes any fixed budget until it does not. Four
# times the data should cost about four times as much; a square would cost sixteen.
with tempfile.TemporaryDirectory(prefix='fin-scaling-') as directory:
    small = scaling_database(Path(directory) / 'small', 750) if (Path(directory) / 'small').mkdir() is None else None
    large = scaling_database(Path(directory) / 'large', 3000) if (Path(directory) / 'large').mkdir() is None else None
    for args in (['status'], ['operation', 'list', '--limit', '20'], ['operation', 'list', '--json'],
                 ['operation', 'review', '--all', '--json'], ['journal', 'export']):
        at_small, at_large = cpu(small, *args), cpu(large, *args)
        print(f"{' '.join(args)}: {at_small * 1000:.0f} ms, then {at_large * 1000:.0f} ms at 4x", flush=True)
        if at_large > max(8 * at_small, 0.25):
            raise SystemExit(f"{' '.join(args)} grows faster than the data")
