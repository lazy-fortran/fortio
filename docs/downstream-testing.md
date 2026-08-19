---
title: Downstream testing
---

# Downstream testing

Fortio keeps a row-oriented consumer registry in `ci/downstreams`. Each row
names a repository, its dispatchable workflow, the candidate-ref input, and
the owner of the compatibility gate.

The direct Fortio gate runs on same-repository pull requests. It dispatches
the registered consumer's workflow with the pull request commit as
`fortio_ref`, then waits for that workflow's own conclusion. A failed blocking
consumer fails the Fortio gate. Fork pull requests are skipped because a
candidate SHA from a fork is not available from the upstream repository under
the downstream's normal fetch permissions.

NEO-2, NEO-RT, SIMPLE, MEPHIT, KAMEL, and rabe are recorded as delegated
consumers. They reach Fortio through libneo, which owns candidate injection
and its reverse-dependency gate. rabe's direct `fortio_ref` gate becomes
blocking after its protected downstream change is reviewed and merged.

## Adding a consumer

1. Add one row to `ci/downstreams`.
2. Make the consumer's default-branch workflow accept `workflow_dispatch` and
   a full commit SHA input for a direct consumer.
3. Make the build system use that input only for the test run. Keep the normal
   default pinned to a reviewed commit.
4. Run `python3 ci/check_downstreams.py` and the consumer's fast test locally.
5. Add the consumer's dependency pin update to the same release change or to a
   tracked downstream pull request. Record the resulting commit in the release
   report.

The `RELEASE_BOT_TOKEN` secret needs Actions write permission on direct
consumer repositories and read permission for their workflow runs. The gate
uses a concurrency group so a newer pull request revision cancels obsolete
downstream runs. A downstream workflow must make its candidate ref part of the
build command and must keep the ref in its build log.

The gate also supports manual and release-time verification. Run
`gh workflow run downstream-gate.yml -f fortio_ref=<commit>`; if `fortio_ref`
is omitted, the selected Fortio branch commit is tested.

The registry does not replace package manifests or lock files. CMake users
should pin `FetchContent` Git dependencies to full commit IDs. FPM users should
pin the same release commit in `fpm.toml`. A released Fortio commit is promoted
only after the direct gate is green and the affected downstream pins are
updated.

This workflow uses GitHub's `workflow_dispatch` event and the Actions API's
explicit repository permissions. The design follows CMake's guidance to use a
commit hash for `FetchContent` Git content and GitHub's guidance for explicit
workflow permissions and reproducible action references. Fortio pins actions
to full commit IDs; Dependabot proposes weekly GitHub Actions updates so those
immutable references do not silently become stale.
