#!/usr/bin/env python3
"""Independent h5py oracle for the general Fortio HDF5 reader.

generate DIR: write test files covering storage layouts, chunk indexes, datatypes,
link and attribute storage in the oldest and newest file formats.
verify DIR: compare the Fortran dumps DIR/*.dump with the h5py view of each file.
"""

from pathlib import Path
import sys

import h5py
import numpy as np


def fill(f, libver):
    rng = np.random.default_rng(3)
    f.create_dataset("contiguous", data=rng.standard_normal((7, 5)))
    f.create_dataset("scalar", data=np.float64(2.5))
    f.create_dataset("compact_i16", data=np.arange(6, dtype="<i2").reshape(2, 3),
                     dcpl=compact_plist())
    for name, dt in [("u8", "u1"), ("i8", "i1"), ("u16", ">u2"), ("i32", ">i4"),
                     ("u32", "<u4"), ("i64", "<i8"), ("f32", ">f4")]:
        f.create_dataset("types/" + name, data=(np.arange(-3, 9) * 7).astype(dt)
                         if dt[-2] in "if" else (np.arange(12) * 13).astype(dt))
    f.create_dataset("types/fixed_str", data=np.array([b"alpha", b"be", b""], dtype="S6"))
    f.create_dataset("types/vlen_str", data=np.array(["one", "", "drei é"], dtype=object),
                     dtype=h5py.string_dtype())
    f.create_dataset("types/complex", data=np.arange(5) * (1 + 2j))
    ct = np.dtype([("r", "<f8"), ("i", "<f8")])
    f["committed"] = ct
    f.create_dataset("types/committed_complex", data=np.array([(1.5, -2.0), (3.0, 4.25)], ct),
                     dtype=f["committed"])
    f.create_dataset("types/enum", data=np.array([0, 1, 1, 0], "i1"),
                     dtype=h5py.enum_dtype({"OFF": 0, "ON": 1}, basetype="i1"))
    f.create_dataset("empty", data=h5py.Empty("f8"))
    # chunked: partial edge chunks, filters, every chunk index
    data = rng.standard_normal((37, 23))
    f.create_dataset("chunk/plain", data=data, chunks=(8, 5))
    f.create_dataset("chunk/filtered", data=data, chunks=(8, 5), compression="gzip",
                     shuffle=True, fletcher32=True)
    f.create_dataset("chunk/single", data=data, chunks=(37, 23))
    f.create_dataset("chunk/single_filtered", data=data, chunks=(37, 23), compression="gzip")
    big = np.arange(64 * 40, dtype="f8").reshape(64, 40)
    f.create_dataset("chunk/paged", data=big, chunks=(1, 2))
    f.create_dataset("chunk/paged_filtered", data=big, chunks=(1, 2), compression="gzip")
    grow = np.arange(300 * 6, dtype="i4").reshape(300, 6)
    f.create_dataset("chunk/extensible", data=grow, chunks=(1, 6), maxshape=(None, 6))
    f.create_dataset("chunk/extensible_last", data=grow.T.copy(), chunks=(6, 1),
                     maxshape=(6, None), compression="gzip")
    f.create_dataset("chunk/btree2", data=data, chunks=(4, 4), maxshape=(None, None))
    f.create_dataset("chunk/btree2_filtered", data=data, chunks=(4, 4), maxshape=(None, None),
                     compression="gzip")
    f.create_dataset("chunk/sparse", shape=(30, 30), dtype="f8", chunks=(10, 10), fillvalue=-1.0)
    f["chunk/sparse"][12:15, 3] = 7.0
    implicit(f, data)
    # groups and attributes
    g = f.create_group("many_links")
    for i in range(40):
        g.create_dataset(f"d{i:02d}", data=np.int32(i))
    a = f.create_group("attrs")
    a.attrs["number"] = 3.25
    a.attrs["ints"] = np.arange(4, dtype=">i2")
    a.attrs["text"] = "fixed text"
    a.attrs["vtext"] = np.array(["x", "yz"], dtype=h5py.string_dtype())
    d = f.create_group("dense_attrs")
    for i in range(30):
        d.attrs[f"a{i:02d}"] = float(i) / 4


def compact_plist():
    plist = h5py.h5p.create(h5py.h5p.DATASET_CREATE)
    plist.set_layout(h5py.h5d.COMPACT)
    return plist


def implicit(f, data):
    if f.libver[1] in ("earliest", "v108"):
        return
    plist = h5py.h5p.create(h5py.h5p.DATASET_CREATE)
    plist.set_chunk((8, 5))
    plist.set_alloc_time(h5py.h5d.ALLOC_TIME_EARLY)
    space = h5py.h5s.create_simple(data.shape)
    dsid = h5py.h5d.create(f.id, b"chunk/implicit", h5py.h5t.IEEE_F64LE, space, dcpl=plist)
    dsid.write(h5py.h5s.ALL, h5py.h5s.ALL, np.ascontiguousarray(data))


def text(v):
    if isinstance(v, bytes):
        v = v.decode("utf-8")
    return v.replace("\\", "\\\\").replace("\n", "\\n")


def blocks_of(path):
    out = {}

    def values(arr):
        arr = np.asarray(arr)
        dt = arr.dtype
        if dt.kind in "SUO":
            return ["S " + text(v) for v in arr.ravel()]
        if dt.names is not None:
            return [f"C {float(v[dt.names[0]])!r} {float(v[dt.names[1]])!r}" for v in arr.ravel()]
        if dt.kind == "c":
            return [f"C {float(v.real)!r} {float(v.imag)!r}" for v in arr.ravel()]
        return [f"N {float(v)!r}" for v in arr.ravel()]

    def visit(name, obj):
        if isinstance(obj, h5py.Dataset):
            if obj.shape is None:
                out["D /" + name + " null"] = []
            else:
                out["D /" + name + " " + " ".join(str(n) for n in obj.shape)] = values(obj[()])
        elif isinstance(obj, h5py.Group):
            out["G /" + name] = []
        else:
            return
        for key in obj.attrs.keys():
            out["A /" + name + " " + key] = values(obj.attrs[key])

    with h5py.File(path, "r") as f:
        f.visititems(visit)
    return out


def parse_dump(path):
    out, current = {}, None
    for line in Path(path).read_text("utf-8").splitlines():
        if line[:2] in ("D ", "G ", "A "):
            current = line
            out[current] = []
        elif line[:2] in ("N ", "C "):
            out[current].append(line[:2] + " ".join(repr(float(x)) for x in line[2:].split()))
        else:
            out[current].append(line)
    return out


command, directory = sys.argv[1:]
directory = Path(directory)
if command == "generate":
    directory.mkdir(parents=True, exist_ok=True)
    for libver in ("earliest", "latest"):
        with h5py.File(directory / f"{libver}.h5", "w", libver=(libver, "latest")) as f:
            fill(f, libver)
    (directory / "files.txt").write_text("earliest.h5\nlatest.h5\n")
elif command == "verify":
    for name in ("earliest.h5", "latest.h5"):
        expected = blocks_of(directory / name)
        received = parse_dump(directory / (name + ".dump"))
        for key in sorted(set(expected) | set(received)):
            if key not in received:
                raise SystemExit(f"{name}: fortio misses {key}")
            if key not in expected:
                raise SystemExit(f"{name}: fortio reports unknown {key}")
            if received[key] != expected[key]:
                bad = next(i for i, (a, b) in enumerate(zip(received[key] + [None] * 9,
                                                            expected[key] + [None] * 9)) if a != b)
                raise SystemExit(f"{name} {key} value {bad}: fortio {received[key][bad:bad+2]} "
                                 f"h5py {expected[key][bad:bad+2]} "
                                 f"({len(received[key])} vs {len(expected[key])} values)")
else:
    raise SystemExit(f"unknown command: {command}")
