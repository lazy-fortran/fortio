#!/usr/bin/env python3
"""Validate the row-oriented downstream compatibility registry."""

from pathlib import Path
import re
import sys


REGISTRY = Path(__file__).with_name("downstreams")
REPOSITORY = re.compile(r"^[^/\s]+/[^/\s]+$")
WORKFLOW = re.compile(r"^[^\s]+\.ya?ml$")
MODES = {"direct", "delegated"}
BOOLEANS = {"yes", "no"}


def rows():
    for number, raw in enumerate(REGISTRY.read_text(encoding="utf-8").splitlines(), 1):
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        fields = line.split()
        if len(fields) != 6:
            raise ValueError(f"{REGISTRY}:{number}: expected six columns")
        repo, workflow, mode, input_name, blocking, full = fields
        if not REPOSITORY.fullmatch(repo):
            raise ValueError(f"{REGISTRY}:{number}: invalid repository {repo!r}")
        if not WORKFLOW.fullmatch(workflow):
            raise ValueError(f"{REGISTRY}:{number}: invalid workflow {workflow!r}")
        if mode not in MODES:
            raise ValueError(f"{REGISTRY}:{number}: invalid mode {mode!r}")
        if mode == "direct" and input_name == "-":
            raise ValueError(f"{REGISTRY}:{number}: direct users need an input")
        if mode == "delegated" and input_name == "-" and blocking == "yes":
            raise ValueError(f"{REGISTRY}:{number}: delegated row cannot block")
        if blocking not in BOOLEANS or full not in BOOLEANS:
            raise ValueError(f"{REGISTRY}:{number}: blocking/full must be yes or no")
        yield mode


def main():
    modes = list(rows())
    if modes.count("direct") == 0:
        raise ValueError(f"{REGISTRY}: registry must contain a direct consumer")
    print(f"validated {len(modes)} downstream registry rows")


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError) as error:
        print(error, file=sys.stderr)
        raise SystemExit(1)
