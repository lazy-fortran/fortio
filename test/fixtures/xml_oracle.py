#!/usr/bin/env python3
"""Independent expat oracle for the Fortio XML reader."""

from pathlib import Path
import random
import sys
import xml.parsers.expat


def document() -> str:
    rng = random.Random(7)
    lines = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        "<!DOCTYPE root [ <!ELEMENT root ANY> <!-- a > inside --> ]>",
        "<!-- leading comment -->",
        '<root version="4.1.1" quote=\'say "hi"\' esc="a&lt;b&gt;c&amp;d&quot;e&apos;f">',
    ]
    for i in range(200):
        depth = rng.randint(1, 4)
        attrs = " ".join(f'a{j}="v{rng.randint(0, 999)}&#x{rng.choice([rng.randint(32, 0xD7FF), rng.randint(0xE000, 0xFFFD), rng.randint(0x10000, 0x10FFFF)]):X};"'
                         for j in range(rng.randint(0, 3)))
        lines.append(f"<field name=\"f{i}\" {attrs}>" * depth)
        lines.append(rng.choice(["text &amp; more", "  ", "<![CDATA[raw <&> ]]>", "x&#65;y",
                                 "<?pi ignored?>", "<empty/>", "<e k='multi\n\tline'/>"]))
        lines.append("</field>" * depth)
    lines.append("<tail>\u00e9\u4e2d\U0001F600</tail>\r\n<crlf>a\r\nb</crlf>")
    lines.append("</root>")
    return "\r\n".join(lines)


def escape(text: str) -> str:
    return text.replace("\\", "\\\\").replace("\n", "\\n").replace("\t", "\\t")


def events(data: bytes) -> list[str]:
    out: list[str] = []
    pending: list[str] = []

    def flush() -> None:
        if pending:
            out.append("T " + escape("".join(pending)))
            pending.clear()

    def start(name, attrs):
        flush()
        out.append("S " + name)
        for k, v in zip(attrs[::2], attrs[1::2]):
            out.append("A " + k + "=" + escape(v))

    def end(name):
        flush()
        out.append("E " + name)

    parser = xml.parsers.expat.ParserCreate()
    parser.ordered_attributes = True
    parser.StartElementHandler = start
    parser.EndElementHandler = end
    parser.CharacterDataHandler = pending.append
    parser.Parse(data, True)
    flush()
    return out


command, doc_path, dump_path = sys.argv[1:]
if command == "generate":
    Path(doc_path).write_bytes(document().encode("utf-8"))
elif command == "verify":
    expected = events(Path(doc_path).read_bytes())
    received = Path(dump_path).read_bytes().decode("utf-8").splitlines()
    if received != expected:
        for i, (a, b) in enumerate(zip(received, expected)):
            if a != b:
                raise SystemExit(f"event {i}: fortio {a!r} expat {b!r}")
        raise SystemExit(f"{len(received)} events from fortio, {len(expected)} from expat")
else:
    raise SystemExit(f"unknown command: {command}")
