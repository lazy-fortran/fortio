# Compatibility contract

Fortio supports only behavior required by pinned NEO-2, libneo, and KAMEL
revisions. Unsupported format features return an explicit error.

## NetCDF

The compatibility module is named `netcdf`. The target surface includes
`nf90_open`, `nf90_create`, `nf90_close`, dimension/variable/group definition
and inquiry, attributes, `nf90_get_var`, `nf90_put_var`, define/data mode
transitions, and error reporting. Required storage formats are CDF-1, CDF-2,
and NetCDF-4.

## HDF5

The compatibility boundary is the ITP `hdf5_tools` API rather than the full
HDF5 Fortran API. It includes typed `h5_add`/`h5_get`, groups, attributes,
Fortran bounds, hyperslabs, unlimited datasets, append, copy, and delete.

## Explicitly deferred

CDF-5, MPI-IO, parallel HDF5, external and virtual links, arbitrary compound
types, plugin filters, CSV, Parquet, and Zstd are outside the current contract.
