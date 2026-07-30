#!/usr/bin/env python3
"""Verify a Fortio ZIP archive with Python's independent ZIP reader."""

from pathlib import Path
import sys
import zipfile


path = Path(sys.argv[1])
expected = {
    "hello.txt": b"Fortio ZIP\n",
    "empty.bin": b"",
    "bytes.bin": bytes([0, 1, 2, 126, 255]),
    "nested/source.bin": bytes([0, 1, 2, 126, 255]),
}

with zipfile.ZipFile(path) as archive:
    if archive.testzip() is not None:
        raise SystemExit("ZIP CRC verification failed")
    if set(archive.namelist()) != set(expected):
        raise SystemExit(f"ZIP names differ: {archive.namelist()}")
    for name, contents in expected.items():
        if archive.read(name) != contents:
            raise SystemExit(f"ZIP contents differ: {name}")
        if archive.getinfo(name).compress_type != zipfile.ZIP_DEFLATED:
            raise SystemExit(f"ZIP entry is not deflated: {name}")
