# fortio

Dependency-free scientific file I/O for modern Fortran.

Fortio is an MIT-licensed implementation of the NetCDF and HDF5 file-format
subset used by NEO-2, libneo, and KAMEL. System NetCDF and HDF5 installations
are test oracles, not production dependencies.

The implementation is intentionally incremental. The current source tree
contains the native typed file API, byte-order-safe binary primitives, and
the classic NetCDF reader. NetCDF-4/HDF5 writing and the complete downstream
compatibility surface are tracked in `COMPATIBILITY.md`.

```fortran
use fortio, only: fortio_file_t, fortio_status_t

type(fortio_file_t) :: file
type(fortio_status_t) :: status
real, allocatable :: values(:)

call file%open("input.nc", status)
call file%read("values", values, status)
call file%close(status)
```

## Consuming fortio

With fpm:

```toml
[dependencies]
fortio = { git = "https://github.com/lazy-fortran/fortio.git" }
```

With an installed CMake package:

```cmake
find_package(fortio CONFIG REQUIRED)
target_link_libraries(my_program PRIVATE fortio::fortio)
```

Or directly from GitHub:

```cmake
include(FetchContent)
FetchContent_Declare(fortio
  GIT_REPOSITORY https://github.com/lazy-fortran/fortio.git
  GIT_TAG main)
FetchContent_MakeAvailable(fortio)
target_link_libraries(my_program PRIVATE fortio::fortio)
```
