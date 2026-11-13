# Contributing

Describe the unsupported behavior or incorrect result in an issue before a
large change. Include a small reproducer, the compiler and the Fortio version.
Keep changes within the [compatibility boundary](docs/compatibility.md), or
explain why an application needs it extended.

Use the documented [build and test commands](docs/installation.md#tests-and-benchmarks).
Behavioral changes need a test against an independent reader, writer,
specification fixture or mathematical oracle. Keep build products and generated
test files outside the source tree. The [development guide](docs/development.md)
also describes the documentation and downstream checks.

Contributions are licensed under the repository's MIT license.
