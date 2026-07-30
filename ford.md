project: Fortio
project_github: https://github.com/lazy-fortran/fortio
project_website: https://lazy-fortran.github.io/fortio/
summary: Dependency-light scientific file I/O for Fortran
author: Fortio contributors
license: MIT
src_dir: src
page_dir: docs
output_dir: site
display: public
         protected
source: false
graph: false
search: false
print_creation_date: false
warn: false
hide_undoc: true

# Fortio

Fortio reads and writes the NetCDF and HDF5 subset used by the ITP plasma
codes without linking the NetCDF, HDF5, or zlib libraries. It provides a small
native API plus migration adapters for the `nf90_*` and `hdf5_tools` calls used
downstream.

Start with [Choosing an API](page/choosing-an-api.html), then follow the
[installation guide](page/installation.html). The
[compatibility matrix](page/compatibility.html) defines the supported boundary.
