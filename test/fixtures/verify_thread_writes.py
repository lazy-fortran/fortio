#!/usr/bin/env python3
import sys

import h5py


path = sys.argv[1]
count = int(sys.argv[2])
with h5py.File(path, "r") as handle:
    assert len(handle) == count
    for value in range(1, count + 1):
        assert handle[f"thread_{value}"][()] == value
