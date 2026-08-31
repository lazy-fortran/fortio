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
the public `h5_defer_close` flag before opening it. The flag is retained for
source compatibility. For streaming writers, `h5_close` always writes a
complete metadata checkpoint and closes the descriptor, matching the normal
HDF5 close durability contract without forcing an `fsync` on every append. For the
non-streaming path, the flag still defers the potentially multi-gigabyte image
write until `h5_deinit`; the writer state remains available for a same-process
`h5_open_rw` without rereading the complete file. External writers must still
not touch the path while a write session is active.

For large new outputs, `h5_stream_write = .true.` can be enabled together with
deferred close. Dataset payloads are written directly as they are added, so the
retained state contains metadata rather than a second in-memory copy of the raw
data. The writer emits an initial valid checkpoint and checkpoints again at
every `h5_close`; an abort therefore leaves the last committed image readable,
although data added after that checkpoint may be absent. Streaming output is
unfiltered and is intended for a fresh or explicitly truncated file; attempts
to enable a deflate filter in this mode are rejected.

This contract does not provide MPI-I/O, parallel-HDF5 collective operations,
or concurrent mutation through the same handle.
