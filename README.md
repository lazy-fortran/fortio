# fortio

Small scientific file I/O for modern Fortran without NetCDF or HDF5 libraries.

Fortio is an MIT-licensed implementation of the NetCDF and HDF5 file-format
subset used by NEO-2, libneo, KAMEL, and SIMPLE. System NetCDF and HDF5
installations are test oracles, not production dependencies.

The only format-code dependency is zlib, used for the shuffle/deflate
compression enabled by SIMPLE orbit output. Both fpm and the installed CMake
package link it transitively.

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

## Thread-safety contract

Independent handles may be used concurrently. OpenMP readers use separate
file state, and write sessions targeting the same HDF5 path are serialized so
that updates are not lost. The compatibility handle tables and diagnostics are
race-checked in CI with ThreadSanitizer.

The legacy public `h5overwrite` switch is process configuration: set it before
starting a parallel region and do not mutate it while I/O calls are active.
This contract does not imply MPI-IO or parallel-HDF5 format support.

The synchronization is always enabled, including in the Release build used by
the native-library performance comparisons below.

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
  build-benchmark/benchmark_fortio_netcdf4_deflate \
  build-benchmark/benchmark_native_netcdf4_deflate --enforce
python benchmark/compare.py \
  build-benchmark/benchmark_fortio_hdf5 \
  build-benchmark/benchmark_native_hdf5 --enforce
python benchmark/compare.py \
  build-benchmark/benchmark_fortio_hdf5_append \
  build-benchmark/benchmark_native_hdf5_append --enforce
python benchmark/compare_threaded.py \
  build-benchmark/benchmark_fortio_netcdf4_threaded --enforce
```

The compressed comparison reproduces SIMPLE's particle-row orbit writes with
shuffle and deflate level 4. The append comparison reproduces KAMEL's
six-field unlimited time-step matrix with a close/reopen cycle per append.
The threaded gate compares independent compressed writes using one and two
OpenMP threads, and fails if synchronization removes the throughput benefit.
All comparisons use alternating-order medians and verify stable checksums.
