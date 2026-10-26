#!/usr/bin/env python3
"""Build a native local snapshot and a pacman-installable package; never install it."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shlex
import shutil
import subprocess
import tarfile
import tempfile


def main():
    root = Path(__file__).resolve().parents[2]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--switch', help='Prepared opam switch to use')
    parser.add_argument('--output', type=Path, default=root / 'dist' / 'arch')
    parser.add_argument('--scratch-root', type=Path,
                        default=Path.home() / '.cache' / 'finances' / 'build')
    args = parser.parse_args()
    output = args.output.resolve()
    scratch_root = args.scratch_root.resolve()
    if any(scratch_root == p or p in scratch_root.parents
           for p in (Path('/tmp'), Path('/var/tmp'))):
        parser.error('scratch-root must be disk-backed, outside /tmp and /var/tmp')
    output.mkdir(parents=True, exist_ok=True)
    scratch_root.mkdir(parents=True, exist_ok=True)
    version = re.search(r'\(version\s+([a-zA-Z0-9.+_]+)\)',
                        (root / 'dune-project').read_text()).group(1)
    arch = platform.machine()
    if arch != 'x86_64':
        parser.error('this Arch recipe currently targets x86_64')
    opam = ['opam', 'exec']
    if args.switch:
        opam += ['--switch', args.switch]
    with tempfile.TemporaryDirectory(prefix='finances-arch-', dir=scratch_root) as tmp:
        work = Path(tmp)
        (work / 'tmp').mkdir()
        env = dict(os.environ, TMPDIR=str(work / 'tmp'))

        def run(command, *, cwd=root, **kwargs):
            return subprocess.run(command, cwd=cwd, env=env, check=True, **kwargs)

        build = work / 'dune'
        dune = opam + ['--', 'dune']
        run(dune + ['build', '--profile', 'release', '--build-dir', str(build),
                    'bin/main.exe'])
        run(dune + ['runtest', '--profile', 'release', '--build-dir', str(build)])
        executable = build / 'default' / 'bin' / 'main.exe'
        reported = run([str(executable), '--version'], capture_output=True,
                       text=True).stdout.strip()
        if reported != version:
            raise RuntimeError('CLI and dune-project versions disagree')
        payload = work / f'finances-{version}'
        payload.mkdir()
        shutil.copy2(executable, payload / 'finances')
        # Explicit public-only allowlist: never archive the checkout or personal/.
        for name in ('LICENSE.md', 'README.md'):
            shutil.copy2(root / name, payload / name)
        (payload / 'docs').mkdir()
        for doc in sorted((root / 'docs').glob('*.md')):
            shutil.copy2(doc, payload / 'docs' / doc.name)
        (payload / 'man').mkdir()
        for group in (None, 'accounts', 'bank', 'transfers', 'journal', 'db'):
            command = [str(executable)] + ([group] if group else []) + ['--help=groff']
            name = 'finances' + (f'-{group}' if group else '')
            (payload / 'man' / f'{name}.1').write_text(
                run(command, capture_output=True, text=True).stdout)
        revision = run(['git', 'rev-parse', 'HEAD'], capture_output=True,
                       text=True).stdout.strip()
        dirty = bool(run(['git', 'status', '--porcelain'], capture_output=True,
                         text=True).stdout.strip())
        (payload / 'build-info.json').write_text(json.dumps(
            dict(version=version, revision=revision, working_tree_dirty=dirty),
            indent=2) + '\n')
        archive_name = f'finances-{version}-{arch}.tar.gz'
        archive = work / archive_name
        with tarfile.open(archive, 'w:gz') as tar:
            tar.add(payload, arcname=payload.name)
        digest = hashlib.sha256(archive.read_bytes()).hexdigest()
        recipe = (root / 'packaging' / 'arch' / 'PKGBUILD.in').read_text()
        recipe = recipe.replace('@VERSION@', version).replace('@ARCH@', arch)
        recipe = recipe.replace('@SHA256@', digest)
        (work / 'PKGBUILD').write_text(recipe)
        config = work / 'makepkg.conf'
        config.write_text('source /etc/makepkg.conf\n'
                          "PKGEXT='.pkg.tar.zst'\n"
                          'COMPRESSZST=(zstd -c -T2 -6 -)\n'
                          f'PKGDEST={shlex.quote(str(output))}\n'
                          f'SRCDEST={shlex.quote(str(work))}\n'
                          f'BUILDDIR={shlex.quote(str(work / "build"))}\n')
        run(['makepkg', '--config', str(config), '--force', '--cleanbuild',
             '--noconfirm'], cwd=work)
        shutil.copy2(archive, output / archive_name)
        (output / 'PKGBUILD').write_text(recipe)
        srcinfo = run(['makepkg', '--printsrcinfo'], cwd=work,
                      capture_output=True, text=True).stdout
        (output / '.SRCINFO').write_text(srcinfo)
        package = output / f'finances-{version}-1-{arch}.pkg.tar.zst'
        if not package.exists():
            raise RuntimeError('makepkg did not produce the expected artifact')
        (output / 'SHA256SUMS').write_text(
            f'{hashlib.sha256(package.read_bytes()).hexdigest()}  {package.name}\n'
            f'{digest}  {archive_name}\n')
        run(['pacman', '-Qip', str(package)])
        print(f'Package ready: {package}')


if __name__ == '__main__':
    main()
