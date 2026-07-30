import sys

import h5py
import numpy as np


with h5py.File(sys.argv[1], "r") as handle:
    np.testing.assert_equal(handle["grid/Nt"][()], np.int32(42))
    np.testing.assert_allclose(
        handle["grid/matrix"][()],
        np.array([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], dtype=np.float64),
    )
    np.testing.assert_equal(handle["rw_added"][()], np.int32(73))
