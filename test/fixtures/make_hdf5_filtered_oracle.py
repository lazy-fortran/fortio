"""Independent h5py inputs for both legacy and modern filtered headers."""
import sys

import h5py
import numpy as np

for version in ("earliest", "latest"):
    for shuffle in (False, True):
        path = f"{sys.argv[1]}-{version}-{int(shuffle)}.h5"
        with h5py.File(path, "w", libver=version) as handle:
            handle.create_dataset(
                "integer", data=np.array([3, -17, 42], dtype=np.int32),
                chunks=(3,), compression="gzip", shuffle=shuffle,
            )
            handle.create_dataset(
                "matrix", data=np.arange(1, 513, dtype=np.float64).reshape(16, 32),
                chunks=(16, 32), compression="gzip", shuffle=shuffle,
            )
            handle.create_dataset(
                "multichunk", data=np.arange(64, dtype=np.int32),
                chunks=(8,), compression="gzip",
            )
