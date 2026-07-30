#!/usr/bin/env python3
"""Reject broken local links and missing required documentation pages."""

from __future__ import annotations

import re
import sys
from pathlib import Path
from urllib.parse import unquote, urlsplit


site = Path(sys.argv[1]).resolve()
required = {
    "index.html",
    "page/choosing-an-api.html",
    "page/compatibility.html",
    "page/installation.html",
    "page/performance.html",
    "page/thread-safety.html",
}
missing = sorted(path for path in required if not (site / path).is_file())
if missing:
    raise SystemExit("missing generated pages: " + ", ".join(missing))

broken: list[str] = []
for document in site.rglob("*.html"):
    text = document.read_text(encoding="utf-8")
    for target in re.findall(r'''(?:href|src)=["']([^"'#]+)''', text):
        parsed = urlsplit(target)
        if parsed.scheme or parsed.netloc or target.startswith(("mailto:", "data:")):
            continue
        resolved = (document.parent / unquote(parsed.path)).resolve()
        if not resolved.exists():
            broken.append(f"{document.relative_to(site)} -> {target}")

if broken:
    raise SystemExit("broken generated links:\n" + "\n".join(sorted(set(broken))))
