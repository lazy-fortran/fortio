---
title: Module overview
---

# Module overview

Start new applications with `fortio`; use the adapters when migrating existing
call sites. The generated API reference documents individual procedures.

| Module | Purpose |
|---|---|
| `fortio` | Native file handles, typed reads, status values, compression and ZIP output |
| `fortio_status` | Error codes and diagnostic messages |
| `netcdf` | Supported `nf90_*` source compatibility |
| `hdf5_tools` | Typed helper interface used by the plasma-code consumers |
| `h5lt` | Supported HDF5-Lite dataset inquiry |
| `fortio_netcdf_classic`, `fortio_netcdf_writer` | Classic NetCDF storage |
| `fortio_hdf5_reader`, `fortio_hdf5_writer` | Shared HDF5 and NetCDF-4 storage |
| `fortio_deflate`, `fortio_compression` | Compression and decompression |
| `fortio_zip` | ZIP32 archive output |
| `fortio_bytes`, `fortio_checksum`, `fortio_posix` | Byte conversion, checksums and platform file access |

Internal storage modules support the public interfaces; their presence does
not extend the format [compatibility boundary](compatibility.html).
