#!/usr/bin/env python3
"""Ensure thread safety does not serialize independent compressed writes."""

import argparse
import os
import re
import statistics
import subprocess
import tempfile
from pathlib import Path


RESULT = re.compile(r"seconds=\s*([0-9.Ee+-]+)\s+checksum=\s*([0-9.Ee+-]+)")


def measure(executable: Path, prefix: Path, threads: int) -> tuple[float, float]:
    environment = os.environ.copy()
    environment["OMP_NUM_THREADS"] = str(threads)
    output = subprocess.check_output(
        [executable, prefix], env=environment, text=True
    )
    match = RESULT.search(output)
    if match is None:
        raise RuntimeError(f"unrecognized benchmark output: {output}")
    return float(match.group(1)), float(match.group(2))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("executable", type=Path)
    parser.add_argument("--samples", type=int, default=7)
    parser.add_argument("--threads", type=int, default=2)
    parser.add_argument("--enforce", action="store_true")
    args = parser.parse_args()

    sequential: list[float] = []
    threaded: list[float] = []
    checksum = None
    with tempfile.TemporaryDirectory(prefix="fortio-thread-benchmark-") as temporary:
        directory = Path(temporary)
        executable = args.executable.resolve()
        for sample in range(args.samples):
            order = [1, args.threads] if sample % 2 == 0 else [args.threads, 1]
            for threads in order:
                elapsed, current_checksum = measure(
                    executable, directory / f"run-{sample}-{threads}", threads
                )
                if checksum is not None and current_checksum != checksum:
                    raise RuntimeError("thread benchmark checksums are inconsistent")
                checksum = current_checksum
                (sequential if threads == 1 else threaded).append(elapsed)

    sequential_seconds = statistics.median(sequential)
    threaded_seconds = statistics.median(threaded)
    ratio = threaded_seconds / sequential_seconds
    print(f"sequential median: {sequential_seconds:.6f} s")
    print(f"{args.threads}-thread median: {threaded_seconds:.6f} s")
    print(f"threaded/sequential: {ratio:.3f}")
    if args.enforce and ratio > 1.0:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
