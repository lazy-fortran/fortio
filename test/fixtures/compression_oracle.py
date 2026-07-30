#!/usr/bin/env python3
"""Independent zlib oracle for Fortio compression tests."""

from pathlib import Path
import sys
import zlib


def payload() -> bytes:
    return b"The quick brown fox jumps over the lazy dog. " * 10_000


command, compressed_path, raw_path = sys.argv[1:]
compressed = Path(compressed_path)
raw = Path(raw_path)

if command == "generate":
    data = payload()
    encoded = zlib.compress(data, level=6)
    if (encoded[2] >> 1) & 3 != 2:
        raise SystemExit("oracle did not produce a dynamic-Huffman block")
    compressed.write_bytes(encoded)
    raw.write_bytes(data)
elif command == "verify":
    expected = raw.read_bytes()
    if zlib.decompress(compressed.read_bytes()) != expected:
        raise SystemExit("Fortio zlib stream differs")
else:
    raise SystemExit(f"unknown command: {command}")
