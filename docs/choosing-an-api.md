---
title: Choosing an API
---

# Choosing an API

Fortio has three public surfaces. Choose one surface for each layer of an
application rather than mixing handles between them.

| Surface | Use it for | Do not use it for |
|---|---|---|
| `fortio` | New code, format detection, typed HDF5/NetCDF reads, compression, ZIP output | Emulating every HDF5 or NetCDF feature |
| `netcdf` | Compiling existing `nf90_*` call sites while migrating | New domain abstractions |
| `hdf5_tools` | Compiling the ITP helper calls used by existing applications | The general HDF5 Fortran API |

NetCDF-4 is not a second file engine. The `netcdf` adapter translates named
dimensions, coordinate variables, and attributes into HDF5 objects and then
uses the same HDF5 reader and writer as `hdf5_tools`.

```text
fortio native API        netcdf adapter        hdf5_tools adapter
         \                    |                      /
          +------------ HDF5 engine ---------------+
                         |
              byte I/O, shuffle, Deflate
```

Classic CDF-1 and CDF-2 files use the classic NetCDF engine instead of HDF5.

## Native read

Use an explicit kind and check every status:

```fortran
use, intrinsic :: iso_fortran_env, only: real64
use fortio, only: fortio_file_t, fortio_status_t

type(fortio_file_t) :: file
type(fortio_status_t) :: status
real(real64), allocatable :: values(:)

call file%open("input.nc", status)
if (.not. status%ok()) error stop status%message
call file%read("potential", values, status)
if (.not. status%ok()) error stop status%message
call file%close(status)
if (.not. status%ok()) error stop status%message
```

The native file API intentionally exposes only operations already required by
downstream codes. Use the migration adapters for write operations that do not
yet have a native spelling.

## Compression and ZIP output

Fortio's Deflate implementation is shared by NetCDF-4, HDF5, and ZIP output.
It does not call zlib.

```fortran
use, intrinsic :: iso_fortran_env, only: int8
use fortio, only: fortio_status_t, zip_writer_t

type(zip_writer_t) :: archive
type(fortio_status_t) :: status
integer(int8) :: payload(3) = [1_int8, 2_int8, 3_int8]

call archive%open("result.zip", status)
if (.not. status%ok()) error stop status%message
call archive%add("values.bin", payload, status)
if (.not. status%ok()) error stop status%message
call archive%close(status)
if (.not. status%ok()) error stop status%message
```

The current writer emits ZIP32 archives. ZIP64, encryption, and multi-disk
archives are outside the supported boundary.
