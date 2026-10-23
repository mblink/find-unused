#!/usr/bin/env python3
"""Shows which PR in a stack introduced the lines a change touches, to decide which PR to fold it into.

Usage: blame_layers.py <layers file> <change base> <change head> [paths...]

<layers file> lists the stack's branches bottom up, one per line, starting with the branch the stack is based on
(e.g. `main`). For each hunk of `git diff <change base> <change head>`, it prints the layers that last touched the lines
the hunk replaces, from blaming <change base>.
"""
import collections
import re
import subprocess
import sys


def git(*args):
    return subprocess.run(["git", *args], capture_output=True, text=True, check=True).stdout


layers_file, base, head, *paths = sys.argv[1:]
layers = [line.strip() for line in open(layers_file, encoding="utf-8") if line.strip()]
layer_of = {}
for below, layer in zip(layers, layers[1:]):
    for commit in git("rev-list", f"{below}..{layer}").split():
        layer_of[commit] = layer

for path in git("diff", "--name-only", base, head, "--", *paths).split():
    try:
        blame = git("blame", "--line-porcelain", base, "--", path)
    except subprocess.CalledProcessError:
        print(f"== {path}: new file")
        continue
    owners = [layer_of.get(m.group(1), layers[0]) for m in re.finditer(r"^([0-9a-f]{40}) \d+ \d+", blame, re.M)]
    print(f"== {path}")
    diff = git("diff", "-U0", base, head, "--", path)
    for hunk in re.finditer(r"^@@ -(\d+)(?:,(\d+))? \+\d+(?:,\d+)? @@(.*)$", diff, re.M):
        start, count = int(hunk.group(1)), int(hunk.group(2) or 1)
        touched = owners[start - 1:start - 1 + count] if count else owners[start - 1:start]
        print(f"  -{start},{count}: {dict(collections.Counter(touched))} {hunk.group(3).strip()[:60]}")
