# fortio

Dependency-free scientific file I/O for modern Fortran.

Fortio is an MIT-licensed implementation of the NetCDF and HDF5 file-format
subset used by NEO-2, libneo, and KAMEL. System NetCDF and HDF5 installations
are test oracles, not production dependencies.

The current implementation includes the native typed file API,
byte-order-safe binary primitives, CDF-1/CDF-2 and NetCDF-4 access, and the
small HDF5 writer/reader subset required by the downstream codes. It also
provides their `nf90_*` and ITP `hdf5_tools` compatibility surfaces. The exact
boundary and deliberately unsupported features are recorded in
`COMPATIBILITY.md`.

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

The optional benchmarks build equivalent supported dense-array round trips
against Fortio and against the system NetCDF and HDF5 Fortran libraries. They
alternate execution order, compare median timings, verify identical
checksums, and fail with `--enforce` if Fortio is slower. The system libraries
are benchmark and test-oracle dependencies only.

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
