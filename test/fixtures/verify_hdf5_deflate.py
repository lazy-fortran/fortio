#!/usr/bin/env python3
import sys

import h5py
import numpy as np

with h5py.File(sys.argv[1], "r") as handle:
    dataset = handle["field"]
    if dataset.compression != "gzip" or dataset.compression_opts != 4:
        raise SystemExit("dataset is not encoded with deflate level 4")
    if not dataset.shuffle:
        raise SystemExit("dataset is not encoded with the shuffle filter")
    if list(dataset.dims[0].keys()) != ["timestep"]:
        raise SystemExit("first HDF5 dimension scale is not timestep")
    if list(dataset.dims[1].keys()) != ["particle"]:
        raise SystemExit("second HDF5 dimension scale is not particle")
    expected = np.empty((32, 64), dtype=np.float64)
    for j in range(32):
        for i in range(64):
            expected[j, i] = (i + 1) + 10 * (j + 1)
    np.testing.assert_array_equal(dataset[...], expected)
