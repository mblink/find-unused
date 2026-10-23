#!/usr/bin/env python3
"""Totals the CPU samples in `jfr print --events jdk.ExecutionSample --stack-depth 64 run.jfr` output.

Usage: python3 summarize_jfr.py samples.txt

Prints the methods most often at the top of a stack (self time), then the find-unused and tasty-query methods most often
anywhere in it (inclusive time), as percentages of all samples.
"""
import collections
import re
import sys

events = open(sys.argv[1]).read().split("jdk.ExecutionSample {")[1:]
self_counts, total_counts = collections.Counter(), collections.Counter()
for event in events:
    frames = re.findall(r"^\s+([\w.$<>]+)\(", event, re.M)
    if frames:
        self_counts[frames[0]] += 1
        total_counts.update(set(frames))

n = len(events)
print(f"{n} samples\n\nself:")
for f, c in self_counts.most_common(15):
    print(f"{c * 100 / n:5.1f}% {f}")
print("\ninclusive:")
for f, c in [x for x in total_counts.most_common(1000) if x[0].startswith(("bl.unused", "tastyquery"))][:40]:
    print(f"{c * 100 / n:5.1f}% {f}")
