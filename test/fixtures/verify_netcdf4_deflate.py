#!/usr/bin/env python3
import subprocess
import sys

import h5py
import numpy as np

path = sys.argv[1]
dump = subprocess.run(
    ["ncdump", "-h", path], check=True, capture_output=True, text=True
).stdout
for fragment in [
    "particle = 64",
    "timestep = 32",
    "double field(timestep, particle)",
]:
    if fragment not in dump:
        raise SystemExit(f"ncdump metadata is missing {fragment!r}\n{dump}")

with h5py.File(path, "r") as handle:
    dataset = handle["field"]
    if dataset.compression != "gzip" or dataset.compression_opts != 4:
        raise SystemExit("field is not encoded with deflate level 4")
    if not dataset.shuffle:
        raise SystemExit("field is not encoded with the shuffle filter")
    expected = np.empty((32, 64), dtype=np.float64)
    for j in range(32):
        for i in range(64):
            expected[j, i] = (i + 1) + 10 * (j + 1)
    np.testing.assert_array_equal(dataset[...], expected)
