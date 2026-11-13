---
title: Verification and limitations
---

# Verification and limitations

The native test suite checks NetCDF and HDF5 reads and writes against files
created or read by the system NetCDF tools and Python h5py. Python's zlib
and ZIP readers check compression and archive output independently. Separate
tests cover metadata, hyperslabs, legacy HDF5 layouts, deferred writes and
concurrent access. The [downstream suite](downstream-testing.html) checks
pinned applications with their ordinary build and test commands.

The Windows test job covers checksums, byte conversion and the platform file
backend with GNU Fortran in MSYS2. It does not run the complete format or
downstream test suites on Windows. ThreadSanitizer checks the concurrent
OpenMP cases on Linux.

These tests cover the [documented subset](compatibility.html), not every
NetCDF or HDF5 feature. General multi-chunk HDF5 indexes, arbitrary compound
and variable-length types, MPI-I/O and parallel HDF5 remain unsupported.
Format support outside that boundary is not implied by a successful file open.
The [performance comparisons](performance.html) apply to their specified
workloads; they do not establish a speed advantage for every file or machine.
