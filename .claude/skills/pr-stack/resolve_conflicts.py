#!/usr/bin/env python3
"""Replaces the conflict blocks in a file, in order, with the given resolutions.

Usage: resolve_conflicts.py <file> <resolutions.json>

<resolutions.json> is a list with one entry per conflict block: "ours", "theirs", or the literal text to put there
(ending in a newline). It fails without changing the file if the counts differ. A diff3-style block's `|||||||` base
section is dropped.
"""
import json
import re
import sys

path, resolutions = sys.argv[1], json.load(open(sys.argv[2], encoding="utf-8"))
text = open(path, encoding="utf-8").read()
blocks = list(re.finditer(
    r"^<<<<<<< [^\n]*\n(.*?)(?:^\|\|\|\|\|\|\|[^\n]*\n.*?)?^=======\n(.*?)^>>>>>>> [^\n]*\n", text, re.S | re.M
))
if len(blocks) != len(resolutions):
    sys.exit(f"{path}: {len(blocks)} conflicts, but {len(resolutions)} resolutions")
out, last = [], 0
for block, resolution in zip(blocks, resolutions):
    out.append(text[last:block.start()])
    out.append({"ours": block.group(1), "theirs": block.group(2)}.get(resolution, resolution))
    last = block.end()
out.append(text[last:])
open(path, "w", encoding="utf-8").write("".join(out))
