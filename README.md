# Fortio

Fortio is an MIT-licensed Fortran library for the NetCDF and HDF5 subset used
by NEO-2, NEO-RT, libneo, KAMEL, SIMPLE, MEPHIT, and Rabe. It reads and writes
the required formats without linking NetCDF, HDF5, zlib, or another compression
library.

Fortio provides:

- a typed native API for new code;
- the required `nf90_*`, `hdf5_tools`, and HDF5-Lite migration adapters;
- CDF-1, CDF-2, and the required NetCDF-4/HDF5 layouts;
- an internal Deflate codec shared by HDF5, NetCDF-4, and ZIP output;
- thread-safe independent handles and serialized same-path updates;
- fpm and installed or fetched CMake consumption.

The native HDF5 engine is shared by NetCDF-4 and direct HDF5 access. Fortio
does not implement unrelated parts of the full HDF5 or NetCDF APIs.

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

Documentation: <https://lazy-fortran.github.io/fortio/>

- [Choose the native or migration API](docs/choosing-an-api.md)
- [Install with fpm or CMake](docs/installation.md)
- [Review the exact compatibility boundary](docs/compatibility.md)
- [Understand the thread-safety contract](docs/thread-safety.md)
- [Run the enforced performance comparisons](docs/performance.md)

System NetCDF/HDF5 tools and Python readers are independent test oracles.
System NetCDF/HDF5 libraries are used only by optional native performance
comparisons.
