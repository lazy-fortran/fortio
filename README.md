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

## Performance comparison

The optional benchmark builds the same dense NetCDF round-trip source once
against fortio and once against the system NetCDF Fortran library. The system
library is a benchmark dependency only.

```sh
cmake -S . -B build-benchmark \
  -DCMAKE_BUILD_TYPE=Release -DFORTIO_BUILD_BENCHMARKS=ON
cmake --build build-benchmark -j
python benchmark/compare.py \
  build-benchmark/benchmark_fortio_netcdf \
  build-benchmark/benchmark_native_netcdf --enforce
python benchmark/compare.py \
  build-benchmark/benchmark_fortio_hdf5 \
  build-benchmark/benchmark_native_hdf5 --enforce
```

The comparison uses medians and verifies identical result checksums.
