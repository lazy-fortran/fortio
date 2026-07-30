#!/usr/bin/env python3
"""Compare Fortio with a system library on one supported I/O workload."""

import argparse
import re
import statistics
import subprocess
import tempfile
from pathlib import Path


RESULT = re.compile(r"seconds=\s*([0-9.Ee+-]+)\s+checksum=\s*([0-9.Ee+-]+)")


def measure_once(executable: Path, path: Path) -> tuple[float, float]:
    output = subprocess.check_output([executable, path], text=True)
    match = RESULT.search(output)
    if match is None:
        raise RuntimeError(f"unrecognized benchmark output: {output}")
    return float(match.group(1)), float(match.group(2))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("fortio", type=Path)
    parser.add_argument("native", type=Path)
    parser.add_argument("--samples", type=int, default=7)
    parser.add_argument(
        "--enforce",
        action="store_true",
        help="fail unless fortio's median is no slower than system NetCDF",
    )
    args = parser.parse_args()

    with tempfile.TemporaryDirectory(prefix="fortio-benchmark-") as temporary:
        directory = Path(temporary)
        executables = [args.fortio.resolve(), args.native.resolve()]
        timings = [[], []]
        checksums = [None, None]
        for sample in range(args.samples):
            order = [0, 1] if sample % 2 == 0 else [1, 0]
            for index in order:
                elapsed, checksum = measure_once(
                    executables[index],
                    directory / f"{executables[index].name}-{sample}.dat",
                )
                timings[index].append(elapsed)
                if checksums[index] is not None and checksum != checksums[index]:
                    raise RuntimeError("benchmark checksums are inconsistent")
                checksums[index] = checksum
        fortio_seconds, native_seconds = map(statistics.median, timings)
    if checksums[0] != checksums[1]:
        raise RuntimeError("fortio and native-library checksums differ")

    ratio = fortio_seconds / native_seconds
    print(f"fortio median: {fortio_seconds:.6f} s")
    print(f"native median: {native_seconds:.6f} s")
    print(f"fortio/native: {ratio:.3f}")
    if args.enforce and ratio > 1.0:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
