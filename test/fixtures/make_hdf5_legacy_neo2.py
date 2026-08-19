import sys

import h5py
import numpy as np


with h5py.File(sys.argv[1], "w", libver="earliest") as handle:
    radial_points = handle.create_dataset(
        "num_radial_pts", data=np.array([3], dtype=np.int32)
    )
    handle.create_dataset("num_species", data=np.array([2], dtype=np.int32))

    boozer_s = handle.create_dataset(
        "boozer_s", data=np.array([0.0, 0.25, 1.0], dtype=np.float64)
    )
    boozer_s.attrs["unit"] = "dimensionless"

    profile = handle.create_dataset(
        "T_prof",
        data=np.array([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], dtype=np.float64),
    )
    profile.attrs["unit"] = "erg"

    radial_points.attrs["description"] = "legacy one-element scalar"
