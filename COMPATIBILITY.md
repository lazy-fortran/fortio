# Compatibility contract

Fortio supports only behavior required by pinned NEO-2, NEO-RT, libneo, KAMEL,
SIMPLE, MEPHIT, and Rabe revisions. Unsupported format features return an
explicit error.

## NetCDF

The compatibility module is named `netcdf`. The target surface includes
`nf90_open`, `nf90_create`, `nf90_close`, dimension/variable/group definition
and inquiry, attributes, `nf90_get_var`, `nf90_put_var`, define/data mode
transitions, and error reporting. Required storage formats are CDF-1, CDF-2,
and NetCDF-4. NetCDF-4 output supports SIMPLE's whole-variable chunk with
shuffle and zlib deflate; the compatibility entry point is
`nf90_def_var_deflate`.

## HDF5

The compatibility boundary is the ITP `hdf5_tools` API rather than the full
HDF5 Fortran API. It includes typed `h5_add`/`h5_get`, groups, attributes,
Fortran bounds, hyperslabs, unlimited datasets, append, copy, and delete.
MEPHIT's sole direct HDF5-Lite call is provided by the `h5lt` compatibility
module as `h5ltget_dataset_info_f`.
Input fixtures used by the pinned codes are contiguous. Legacy HDF5 input with
superblock version 0, version-1 object headers, symbol-table groups, and
contiguous datasets is also readable. Legacy one-element rank-1 integer
datasets are accepted by scalar reads. Unsupported variable-length attributes
are skipped while the containing datasets remain readable. Filtered input accepts
the single-chunk NetCDF-4 layout emitted by Fortio and by the system
NetCDF/HDF5 oracle. Multi-chunk indexes are deliberately not implemented.

## Explicitly deferred

CDF-5, MPI-IO, parallel HDF5, external and virtual links, arbitrary compound
types, plugin filters, CSV, Parquet, and Zstd are outside the current contract.
