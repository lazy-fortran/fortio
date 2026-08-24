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

Applications that perform many same-process updates to one output file may set
the public `h5_defer_close` flag before opening it. In that mode `h5_close`
retains the in-memory image and `h5_deinit` writes the final image once. The
file is not guaranteed to reflect intermediate updates, and external writers
must not touch the path until `h5_deinit` completes.

This contract does not provide MPI-I/O, parallel-HDF5 collective operations,
or concurrent mutation through the same handle.
