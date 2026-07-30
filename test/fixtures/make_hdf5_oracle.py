import sys

import h5py
import numpy as np


with h5py.File(sys.argv[1], "w", libver="latest") as handle:
    grid = handle.create_group("grid")
    grid.create_dataset("Nt", data=np.int32(42))
    grid.create_dataset(
        "x_values",
        data=np.array([1.25, -2.5, 4.75], dtype=np.float32),
    )
    grid.create_dataset("label", data=np.bytes_("stellarator"))
    matrix = grid.create_dataset(
        "matrix",
        data=np.array([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], dtype=np.float64),
    )
    matrix.attrs["lbounds"] = np.array([-2, 5], dtype=np.int32)
    matrix.attrs["ubounds"] = np.array([0, 6], dtype=np.int32)
    integer_ranks = handle.create_group("integer_ranks")
    integer_ranks.create_dataset(
        "int_matrix",
        data=np.arange(1, 7, dtype=np.int32).reshape(2, 3),
    )
    integer_ranks.create_dataset(
        "int_cube",
        data=np.arange(1, 13, dtype=np.int32).reshape(2, 2, 3),
    )
    real_ranks = handle.create_group("real_ranks")
    real_ranks.create_dataset(
        "rank4",
        data=np.arange(1, 13, dtype=np.float64).reshape(2, 1, 2, 3),
    )
    real_ranks.create_dataset(
        "real_cube",
        data=np.arange(1, 13, dtype=np.float64).reshape(2, 2, 3),
    )
    real_ranks.create_dataset(
        "rank5",
        data=np.arange(1, 9, dtype=np.float64).reshape(2, 1, 2, 1, 2),
    )
    continued = handle.create_group("continued")
    for index in range(5):
        continued.create_dataset(f"value_{index}", data=np.int32(index + 10))
