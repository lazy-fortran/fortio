"""Check deferred lifecycle outputs using the independent system HDF5 reader."""
from pathlib import Path
import subprocess
import sys


executable, prefix = sys.argv[1:]
for mode in ('existing', 'fresh'):
    Path(prefix + '.' + mode + '.h5').unlink(missing_ok=True)
subprocess.run([executable, prefix], check=True)
for mode in ('existing', 'fresh'):
    output = subprocess.run(
        ['h5dump', '-d', 'old', '-d', 'seed', '-d', 'updated', '-d', 'matrix',
         prefix + '.' + mode + '.h5'], check=True, capture_output=True,
        text=True).stdout
    for expected in ('(0): 19', '(0): -7', '(0): 31', 'seed-retained', 'keV',
                     '(0,0): 1.25, 2.25', '(2,0): 5.25, 6.25'):
        if expected not in output:
            raise AssertionError(f'{mode}: system HDF5 reader missed {expected}')
print('Independent HDF5 reader confirms existing and fresh deferred updates.')
