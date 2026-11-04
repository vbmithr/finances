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


if __name__ == '__main__':
    main()
