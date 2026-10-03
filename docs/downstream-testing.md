---
title: Downstream testing
---

# Downstream testing

Fortio owns its downstream compatibility tests. `ci/downstreams.toml` pins an
ordinary consumer revision and records that consumer's normal configure,
build, and test commands. `ci/test_downstreams.py` clones that revision,
injects the local Fortio checkout with CMake's standard
`FETCHCONTENT_SOURCE_DIR_FORTIO` override, and runs those commands.

Consumers do not need a Fortio-specific workflow, input, branch, or test path.
Their dependency pins remain unchanged during candidate testing. The harness
is independent of GitHub Actions: the CI workflow merely installs the listed
packages and invokes the same Python command a developer can run on any
machine or CI provider.

## Adding a consumer

1. Add a pinned consumer table to `ci/downstreams.toml`.
2. Record the packages and commands needed for its ordinary test suite.
3. Add its name to the CI matrix when it should gate every Fortio change.
4. Run `python3 ci/test_downstreams.py <name>` locally.

Use `--packages` to print the Debian packages recorded for a consumer and
`--workspace <path>` to retain its checkout and build tree for diagnosis. The
harness prints the resolved consumer commit before building it.

The manifest does not replace package manifests or lock files. Consumers keep
pinning reviewed Fortio commits in the usual way. Candidate testing changes no
pin; after a Fortio pull request is squash-merged, each affected consumer needs
only one update to the unique commit on `main`.
