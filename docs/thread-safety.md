---
title: Thread safety
---

# Thread safety

Independent handles may be used concurrently. Compatibility handle tables and
diagnostics are synchronized. HDF5 write sessions targeting the same path are
serialized so that close/reopen updates cannot overwrite one another.

The `hdf5_tools` adapter retains a closed writer's in-memory file image for
later `h5_open_rw` calls on the same path in the same process. This avoids
reading and copying the complete file for every update; do not mix such a
sequence with an external writer, since the retained image is authoritative.

Synchronization is always enabled, including release builds. It is tested with
OpenMP callers under ThreadSanitizer and benchmarked with one and two threads.
Fortio does not require applications to compile with OpenMP.

The public `h5overwrite` variable belongs to the `hdf5_tools` migration
surface. Treat it as process configuration: set it before entering a parallel
region and never modify it while I/O calls are active.

This contract does not provide MPI-I/O, parallel-HDF5 collective operations,
or concurrent mutation through the same handle.
