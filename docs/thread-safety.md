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
retains writer state and `h5_deinit` writes the final metadata image. The file
is not guaranteed to reflect intermediate metadata updates, and external
writers must not touch the path until `h5_deinit` completes.

For large new outputs, `h5_stream_write = .true.` can be enabled together with
deferred close. Dataset payloads are written directly as they are added, so
the deferred state contains metadata rather than a second in-memory copy of
the raw data. Streaming output is unfiltered and is intended for a fresh or
explicitly truncated file; attempts to enable a deflate filter in this mode
are rejected.

This contract does not provide MPI-I/O, parallel-HDF5 collective operations,
or concurrent mutation through the same handle.
