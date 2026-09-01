---
title: Development
---

# Development

Keep generated files out of the source tree. CMake builds, FORD output,
Fortran modules, archives, benchmark files, and test fixtures belong in a
build or temporary directory and are ignored by git.

Before submitting a change:

```sh
fo
cmake -S . -B build-cmake -DFORTIO_BUILD_TESTING=ON
cmake --build build-cmake --parallel
ctest --test-dir build-cmake --output-on-failure
ford --output_dir /tmp/fortio-site ford.md
python3 test/check_docs.py /tmp/fortio-site
git status --short
```

Tests must compare behavior with an independent reader, writer, specification
fixture, or mathematical oracle. A test that merely restates Fortio's own
internal representation is not sufficient.

Run an ordinary pinned consumer suite against the current checkout with:

```sh
python3 ci/test_downstreams.py libneo
```

The harness neither modifies the consumer nor changes its dependency pin.

Pages are built in a runner temporary directory. Pull requests validate the
site without deploying it; pushes to `main` deploy the generated artifact.
