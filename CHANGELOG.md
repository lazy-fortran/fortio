# Changelog

## Unreleased
- Read NetCDF-4 character variables through `nf90_get_var`.
- Write object headers whose messages exceed 65535 bytes in total with the
  wider chunk-size field, and read message sizes and counts of 32768 and
  above as unsigned; such headers were corrupt and unreadable before.
- Reject a NetCDF-4 attribute that does not fit one header message instead of
  writing a corrupt file.
- Read dense HDF5 groups whose link index is a B-tree of depth one or more
  and whose fractal heap has more than one row. Links held in internal B-tree
  nodes were skipped, so NetCDF-4 files with more than about 50 variables
  reported existing variables as not found, and larger ones failed to open.

## 0.3.0 (2026-11-14)
- Add citation metadata, a module overview and a description of verification
  coverage and limitations.
- Consolidate the compatibility documentation and document source builds.

The [release history](https://github.com/lazy-fortran/fortio/releases)
records published versions.
