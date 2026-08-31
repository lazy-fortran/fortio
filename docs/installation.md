---
title: Installation
---

# Installation

Fortio requires Fortran and C compilers and a POSIX threads implementation.
It does not require NetCDF, HDF5, zlib, Zstd, LZO, or LZ4 in production.

## fpm

Pin a released tag:

```toml
[dependencies]
fortio = { git = "https://github.com/lazy-fortran/fortio.git", tag = "v0.3.0" }
```

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
  GIT_TAG v0.3.0
  GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(fortio)
target_link_libraries(my_program PRIVATE fortio::fortio)
```

Applications that need reproducible builds should pin the release tag or its
commit, not `main`.

## Tests and benchmarks

Production builds need no format libraries. Tests use independent command-line
and Python readers as behavioral oracles. Native comparison libraries are
needed only when benchmarks are enabled.

```sh
cmake -S . -B build -DFORTIO_BUILD_TESTING=ON
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

Set `FORTIO_BUILD_BENCHMARKS=ON` to build comparisons against system NetCDF and
HDF5. See [Performance](performance.html) for the enforced workloads.
