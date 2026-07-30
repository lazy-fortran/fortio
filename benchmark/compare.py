#!/usr/bin/env python3
"""Compare fortio with system NetCDF on the supported dense-array workload."""

import argparse
import re
import statistics
import subprocess
import tempfile
from pathlib import Path


RESULT = re.compile(r"seconds=\s*([0-9.Ee+-]+)\s+checksum=\s*([0-9.Ee+-]+)")


def measure(executable: Path, directory: Path, samples: int) -> tuple[float, float]:
    timings = []
    checksum = None
    for sample in range(samples):
        output = subprocess.check_output(
            [executable, directory / f"{executable.name}-{sample}.nc"], text=True
        )
        match = RESULT.search(output)
        if match is None:
            raise RuntimeError(f"unrecognized benchmark output: {output}")
        timings.append(float(match.group(1)))
        current_checksum = float(match.group(2))
        if checksum is not None and current_checksum != checksum:
            raise RuntimeError("benchmark checksums are inconsistent")
        checksum = current_checksum
    return statistics.median(timings), checksum


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
        fortio_seconds, fortio_checksum = measure(
            args.fortio.resolve(), directory, args.samples
        )
        native_seconds, native_checksum = measure(
            args.native.resolve(), directory, args.samples
        )
    if fortio_checksum != native_checksum:
        raise RuntimeError("fortio and native NetCDF checksums differ")

    ratio = fortio_seconds / native_seconds
    print(f"fortio median: {fortio_seconds:.6f} s")
    print(f"native median: {native_seconds:.6f} s")
    print(f"fortio/native: {ratio:.3f}")
    if args.enforce and ratio > 1.0:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
