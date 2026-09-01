#!/usr/bin/env python3
"""Run ordinary consumer test suites against this Fortio checkout."""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import tomllib


ROOT = Path(__file__).resolve().parents[1]
MANIFEST = Path(__file__).with_name("downstreams.toml")
REQUIRED_FIELDS = {
    "name", "repository", "revision", "packages", "configure", "cmake_cache", "commands"
}


def load_consumers() -> dict[str, dict[str, object]]:
    data = tomllib.loads(MANIFEST.read_text(encoding="utf-8"))
    consumers: dict[str, dict[str, object]] = {}
    for index, consumer in enumerate(data.get("consumer", []), 1):
        missing = REQUIRED_FIELDS - consumer.keys()
        if missing:
            raise ValueError(f"consumer {index} is missing {sorted(missing)}")
        name = consumer["name"]
        if not isinstance(name, str) or not name.isidentifier():
            raise ValueError(f"consumer {index} has invalid name {name!r}")
        if name in consumers:
            raise ValueError(f"duplicate consumer name {name!r}")
        revision = consumer["revision"]
        if not isinstance(revision, str) or len(revision) != 40:
            raise ValueError(f"consumer {name!r} must pin a full commit SHA")
        packages = consumer["packages"]
        if not isinstance(packages, list) or not all(
            isinstance(package, str) and package for package in packages
        ):
            raise ValueError(f"consumer {name!r} packages must be non-empty strings")
        python_packages = consumer.get("python_packages", [])
        if not isinstance(python_packages, list) or not all(
            isinstance(package, str) and package for package in python_packages
        ):
            raise ValueError(
                f"consumer {name!r} python_packages must be non-empty strings"
            )
        format_command(consumer["configure"], {})
        for command in consumer["commands"]:
            format_command(command, {})
        consumers[name] = consumer
    if not consumers:
        raise ValueError("downstream manifest is empty")
    return consumers


def format_command(command: list[str], paths: dict[str, str]) -> list[str]:
    if not command or not all(isinstance(argument, str) for argument in command):
        raise ValueError("each downstream command must be a non-empty string array")
    if not paths:
        return command
    try:
        return [argument.format_map(paths) for argument in command]
    except KeyError as error:
        raise ValueError(f"unknown command placeholder {error.args[0]!r}") from error


def run_command(
    raw_command: list[str], paths: dict[str, str], checkout: Path, environment: dict[str, str]
) -> None:
    command = format_command(raw_command, paths)
    print("+ " + " ".join(command), flush=True)
    subprocess.run(command, cwd=checkout, env=environment, check=True)


def verify_candidate(cache_template: str, paths: dict[str, str]) -> None:
    cache = Path(cache_template.format_map(paths))
    expected = f"FETCHCONTENT_SOURCE_DIR_FORTIO:PATH={ROOT}"
    if expected not in cache.read_text(encoding="utf-8"):
        raise ValueError(f"{cache} does not select the Fortio candidate {ROOT}")


def run_consumer(consumer: dict[str, object], workspace: Path) -> None:
    name = str(consumer["name"])
    checkout = workspace / name
    build = workspace / f"{name}-build"
    if not (checkout / ".git").is_dir():
        checkout.mkdir(parents=True, exist_ok=True)
        subprocess.run(["git", "-C", str(checkout), "init"], check=True)
        subprocess.run(
            ["git", "-C", str(checkout), "remote", "add", "origin", str(consumer["repository"])],
            check=True,
        )
    subprocess.run(
        ["git", "-C", str(checkout), "fetch", "--depth=1", "origin", str(consumer["revision"])],
        check=True,
    )
    subprocess.run(
        ["git", "-C", str(checkout), "checkout", "--detach", "FETCH_HEAD"],
        check=True,
    )
    resolved = subprocess.check_output(
        ["git", "-C", str(checkout), "rev-parse", "HEAD"], text=True
    ).strip()
    print(f"testing {name} at {resolved} against Fortio {ROOT}", flush=True)

    paths = {
        "build": str(build),
        "checkout": str(checkout),
        "fortio": str(ROOT),
        "workspace": str(workspace),
    }
    environment = os.environ.copy()
    environment["PIP_NO_CACHE_DIR"] = "1"
    python_source = checkout / "python"
    if python_source.is_dir():
        previous_pythonpath = environment.get("PYTHONPATH", "")
        environment["PYTHONPATH"] = f"{python_source}:{build}"
        if previous_pythonpath:
            environment["PYTHONPATH"] += f":{previous_pythonpath}"
    python_packages = consumer.get("python_packages", [])
    if python_packages:
        virtualenv = workspace / f"{name}-venv"
        subprocess.run(["python3", "-m", "venv", str(virtualenv)], check=True)
        python = virtualenv / "bin" / "python"
        environment["PATH"] = f"{virtualenv / 'bin'}:{environment['PATH']}"
        constraints = checkout / "constraints" / "ci-python.txt"
        install = [str(python), "-m", "pip", "install"]
        if constraints.is_file():
            install.extend(["-c", str(constraints)])
        subprocess.run(install + list(python_packages), cwd=checkout, check=True)

    run_command(consumer["configure"], paths, checkout, environment)
    verify_candidate(str(consumer["cmake_cache"]), paths)
    for raw_command in consumer["commands"]:
        run_command(raw_command, paths, checkout, environment)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("consumers", nargs="*", help="consumer names; default: all")
    parser.add_argument("--list", action="store_true", help="list registered consumers")
    parser.add_argument("--packages", action="store_true", help="print required Debian packages")
    parser.add_argument("--workspace", type=Path, help="retain checkouts and builds here")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    consumers = load_consumers()
    selected_names = args.consumers or list(consumers)
    unknown = sorted(set(selected_names) - consumers.keys())
    if unknown:
        raise ValueError(f"unknown consumers: {', '.join(unknown)}")
    if args.list:
        print("\n".join(selected_names))
        return 0
    if args.packages:
        packages = {
            package
            for name in selected_names
            for package in consumers[name]["packages"]
        }
        print(" ".join(sorted(packages)))
        return 0

    if args.workspace:
        workspace = args.workspace.resolve()
        workspace.mkdir(parents=True, exist_ok=True)
        for name in selected_names:
            run_consumer(consumers[name], workspace)
        return 0

    with tempfile.TemporaryDirectory(prefix="fortio-downstreams-") as temporary:
        workspace = Path(temporary)
        for name in selected_names:
            run_consumer(consumers[name], workspace)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, subprocess.CalledProcessError, TypeError, ValueError) as error:
        print(f"downstream test failed: {error}", file=sys.stderr)
        raise SystemExit(1)
