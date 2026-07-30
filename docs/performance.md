---
title: Performance
---

# Performance

Fortio gates the supported production-shaped workloads against the native
NetCDF and HDF5 libraries:

| Workload | Downstream shape | Gate |
|---|---|---|
| Classic NetCDF dense round trip | Shared scientific arrays | Fortio median must not exceed native median |
| Compressed NetCDF-4 | SIMPLE particle-row orbit output, shuffle + Deflate level 4 | Fortio median must not exceed native median |
| HDF5 dense round trip | Typed contiguous datasets | Fortio median must not exceed native median |
| HDF5 append | KAMEL six-field unlimited time-step matrix, close/reopen per append | Fortio median must not exceed native median |
| Concurrent compressed output | Independent OpenMP writers | Two threads must retain a throughput benefit |

Each comparison alternates execution order and checks stable payload checksums.
The system libraries are benchmark oracles, not production dependencies.

```sh
cmake -S . -B build-benchmark \
  -DCMAKE_BUILD_TYPE=Release \
  -DFORTIO_BUILD_TESTING=OFF \
  -DFORTIO_BUILD_BENCHMARKS=ON
cmake --build build-benchmark --parallel
python3 benchmark/compare.py \
  build-benchmark/benchmark_fortio_netcdf \
  build-benchmark/benchmark_native_netcdf --enforce
```
