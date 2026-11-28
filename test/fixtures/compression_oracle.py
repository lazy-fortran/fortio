#!/usr/bin/env python3
"""Independent zlib oracle for Fortio compression tests."""

from pathlib import Path
import random
import sys
import zlib


def payload() -> bytes:
    return b"The quick brown fox jumps over the lazy dog. " * 10_000


CASES = 60


def case_data(k: int) -> bytes:
    """Random contents of case k: sizes up to 200 kB, alphabets of 1 to 256 bytes."""
    rng = random.Random(k)
    n = rng.choice([0, 1, 2, 7, 300, 5000, 70000, 200000])
    alphabet = rng.choice([1, 2, 16, 256])
    high = rng.random() < 0.5
    return bytes((255 - rng.randrange(alphabet)) if high else rng.randrange(alphabet)
                 for _ in range(n))


command, compressed_path, raw_path = sys.argv[1:]
compressed = Path(compressed_path)
raw = Path(raw_path)

if command == "generate-set":
    # streams of every block type: stored (level 0), fixed, Huffman-only, RLE, dynamic
    strategies = [zlib.Z_DEFAULT_STRATEGY, zlib.Z_FILTERED, zlib.Z_HUFFMAN_ONLY,
                  zlib.Z_RLE, zlib.Z_FIXED]
    compressed.mkdir(parents=True, exist_ok=True)
    for k in range(1, CASES + 1):
        data = case_data(k)
        rng = random.Random(1000 + k)
        encoder = zlib.compressobj(rng.randint(0, 9), zlib.DEFLATED, 15, 9,
                                   strategies[k % len(strategies)])
        (compressed / f"case-{k}.raw").write_bytes(data)
        (compressed / f"case-{k}.z").write_bytes(encoder.compress(data) + encoder.flush())
elif command == "verify-set":
    for k in range(1, CASES + 1):
        data = (compressed / f"case-{k}.raw").read_bytes()
        if zlib.decompress((compressed / f"case-{k}.fortio").read_bytes()) != data:
            raise SystemExit(f"Fortio zlib stream of case {k} differs")
elif command == "generate":
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
