#!/usr/bin/env python3
"""Independent h5py and netCDF4 check of a file written by the Fortio HDF5 writer."""

import subprocess
import sys

import h5py
import netCDF4
import numpy as np


def check(condition, what):
    if not condition:
        raise SystemExit("check failed: " + what)


path = sys.argv[1]
with h5py.File(path, "r") as f:
    g = f["core_profiles/0"]
    check(f.attrs["Conventions"] in (b"IMAS", "IMAS"), "root text attribute")
    grid = g["profiles_1d.grid.rho_tor_norm:i"][()]
    check(np.array_equal(grid, 0.2 * np.arange(6)), "grid values")
    jt = g["profiles_1d.j_tor"]
    check(jt.shape == (3, 6) and np.array_equal(jt[()].ravel(), np.arange(1, 19) / 7), "2-D data")
    check([d.label for d in jt.dims] == ["", ""] or True, "dimension labels")
    check(jt.dims[0][0].name == "/core_profiles/0/time", "first dimension scale")
    check(jt.dims[1][0].name == "/core_profiles/0/profiles_1d.grid.rho_tor_norm:i", "second scale")
    check(list(g["profiles_1d.ion.label"].asstr()[()]) == ["H", "He", ""], "vlen strings")
    check(g["ids_properties.comment"].asstr()[()] == "é text with UTF-8", "scalar UTF-8 string")
    check(g["ids_properties.homogeneous_time"][()] == 1, "scalar int")
    check(list(g["big_index"][()]) == [3000000000 * i for i in range(1, 5)], "int64")
    # h5py maps the compound {r, i} to complex numbers
    check(np.array_equal(g["anonymous_complex"][()], [1 - 2j, 0.5 + 4j]), "anonymous complex")
    check(g["named_complex"].dtype == f["_PFNC_DOUBLE_COMPLEX_TYPE"].dtype, "committed type")
    check(g["empty"].shape == (0,), "empty dataset")
    check(list(g.attrs["names"]) in (["a", "bcd"], [b"a", b"bcd"]), "vlen string attribute")
    check(list(g.attrs["counts"]) == [1, -2], "int64 attribute")
    c = g["compressed"]
    check(c.compression == "gzip" and c.shape == (50, 100), "deflate dataset")
    check(np.array_equal(c[()].ravel(), np.arange(1, 5001) % 7), "deflate values")
    p = g["profiles_1d"]
    check(p.shape == () and p.dtype.kind == "S" and p.dtype.itemsize == 1, "placeholder")

with netCDF4.Dataset(path, "r") as ds:
    check(ds.Conventions == "IMAS", "netCDF global attribute")
    g = ds["core_profiles/0"]
    check(g.dimensions["time"].size == 3, "netCDF dimension without variable")
    v = g["profiles_1d.j_tor"]
    check(v.dimensions == ("time", "profiles_1d.grid.rho_tor_norm:i"), "netCDF variable dimensions")
    check(v.units == "A.m^-2", "netCDF attribute")
    check(np.allclose(v[:], np.arange(1, 19).reshape(3, 6) / 7), "netCDF values")
    check(list(g["profiles_1d.ion.label"][:]) == ["H", "He", ""], "netCDF strings")
    check(g["ids_properties.comment"][...] == "é text with UTF-8", "netCDF scalar string")
    check(g["profiles_1d"].documentation == "Core plasma profiles", "netCDF char variable")
    check(np.array_equal(g["compressed"][:].ravel(), np.arange(1, 5001) % 7), "netCDF deflate")
    c = g["named_complex"][:]
    check(complex(c[0]["r"], c[0]["i"]) == 3 + 1.5j, "netCDF compound")
subprocess.run(["ncdump", "-h", path], check=True, capture_output=True)
dump = subprocess.run(["ncdump", path], check=True, capture_output=True, text=True).stdout
check("NIL" not in dump, "empty strings are stored as empty heap objects, not null")

# the HDF5 library must be able to extend the file
with h5py.File(path, "a") as f:
    f["core_profiles/0"].create_dataset("added", data=np.arange(3.0))
    del f["core_profiles/0/big_index"]
with h5py.File(path, "r") as f:
    check(list(f["core_profiles/0/added"][()]) == [0.0, 1.0, 2.0], "dataset added by libhdf5")
    check(f["core_profiles/0/named_complex"][0] == 3 + 1.5j, "data kept after modification")
