---
title: Compatibility matrix
---

# Compatibility matrix

Fortio implements the behavior exercised by pinned revisions of NEO-2,
NEO-RT, libneo, KAMEL, SIMPLE, MEPHIT, and Rabe. Unsupported operations return
an error; they must not silently degrade the file.

## Storage formats

| Capability | Read | Write | Notes |
|---|:---:|:---:|---|
| NetCDF CDF-1 | yes | yes | Classic data model |
| NetCDF CDF-2 | yes | yes | 64-bit offsets |
| NetCDF-4/HDF5 | yes | yes | Required HDF5 subset |
| HDF5 contiguous datasets | yes | yes | Required numeric and text types |
| HDF5 superblock 0 input | yes | no | Legacy object headers and symbol-table groups |
| Legacy one-element integer scalars | yes | no | Rank-1 length-one datasets read as scalars |
| HDF5 single-chunk datasets | yes | yes | Shuffle and Deflate supported |
| General HDF5 chunk indexes | no | no | Multi-chunk indexes are not implemented |
| CDF-5 | no | no | No downstream requirement |

## NetCDF adapter

The `netcdf` module provides the required overloads of:

- file creation, opening, closing, and define/data-mode transitions;
- dimension, variable, group, and attribute definition and inquiry;
- whole-variable and required hyperslab `nf90_get_var`/`nf90_put_var`;
- `nf90_def_var_deflate` for the SIMPLE whole-variable chunk layout;
- error codes and `nf90_strerror`.

The adapter is source-compatible only for those overloads. It is not a
replacement for the complete netcdf-fortran package.

## HDF5 adapters and native engine

The ITP `hdf5_tools` surface supports the required typed `h5_add` and `h5_get`
operations, groups, attributes, Fortran bounds, hyperslabs, unlimited
datasets, append, copy, replacement, and deletion. MEPHIT's required
`h5ltget_dataset_info_f` call is supplied by the `h5lt` module.

Legacy superblock-0 files use version-1 object headers and symbol-table
groups. Fortio reads their numeric contiguous datasets and ignores unsupported
variable-length attributes such as the `unit` attributes produced by the
supplier NEO-2 files. The values and shapes remain available to the reader.

The underlying HDF5 engine additionally exposes object description, child
listing, existence checks, 64-bit integer arrays, and the complex array ranks
used downstream. Complex values use the compound representation expected by
the ITP files.

## Deliberately unsupported

- full HDF5 or netcdf-fortran API compatibility;
- arbitrary HDF5 compound and variable-length types;
- external, soft, and virtual links;
- arbitrary chunk indexes and plugin filters;
- MPI-I/O and parallel HDF5;
- ZIP64, encrypted ZIP, and multi-disk ZIP;
- CDF-5, CSV, Parquet, Zstd, LZO, and LZ4.
