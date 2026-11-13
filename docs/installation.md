---
title: Installation
---

# Installation

Fortio requires Fortran and C compilers and a POSIX threads implementation.
CMake builds require CMake 3.20 or newer.
It does not require NetCDF, HDF5, zlib, Zstd, LZO, or LZ4 in production.

## Build from source

For a library build without the test or benchmark dependencies:

```sh
git clone https://github.com/lazy-fortran/fortio.git
cd fortio
cmake -S . -B build -DFORTIO_BUILD_TESTING=OFF
cmake --build build --parallel
cmake --install build --prefix "$HOME/.local"
```

Use the same Fortran compiler for Fortio and its consumers; Fortran module
files are compiler-specific.

## fpm

Pin a released tag:

```toml
[dependencies]
fortio = { git = "https://github.com/lazy-fortran/fortio.git", tag = "v0.2.0" }
```

With GNU Fortran, applications that call Fortio concurrently must compile the
library with `-frecursive`, for example by passing `--flag "-frecursive"` to fpm.
This gives each call independent automatic local storage and permits concurrent
entry. Fortio itself needs no OpenMP dependency; applications using OpenMP must
also enable it for their own parallel code. The CMake target sets `-frecursive`
for GNU Fortran automatically.

## Installed CMake package

```cmake
find_package(fortio 0.1 CONFIG REQUIRED)
target_link_libraries(my_program PRIVATE fortio::fortio)
```

## CMake FetchContent

```cmake
include(FetchContent)
FetchContent_Declare(
  fortio
  GIT_REPOSITORY https://github.com/lazy-fortran/fortio.git
  GIT_TAG v0.2.0
  GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(fortio)
target_link_libraries(my_program PRIVATE fortio::fortio)
```

Applications that need reproducible builds should pin the release tag or its
commit, not `main`.

The fpm and FetchContent examples use the published v0.2.0 release.
Documentation in a source checkout describes that checkout, which may include
changes since the release.

## Tests and benchmarks

Production builds need no format libraries. Tests require an OpenMP-capable
Fortran compiler, NetCDF command-line tools (`ncgen` and `ncdump`), the HDF5
tool `h5dump`, and Python with h5py. They use these independent readers and
writers as behavioral oracles. Native comparison libraries are needed only
when benchmarks are enabled.

```sh
cmake -S . -B build -DFORTIO_BUILD_TESTING=ON
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

Set `FORTIO_BUILD_BENCHMARKS=ON` to build comparisons against system NetCDF and
HDF5. See [Performance](performance.html) for the enforced workloads.
