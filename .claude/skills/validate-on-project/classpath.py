#!/usr/bin/env python3
"""Prints the union of the `Test / fullClasspath` that sbt last exported for each project of a build, one per line.

Usage: classpath.py <build root> [--scala <version>]

Compile the build's tests first. sbt 1 writes each project's to
`<project>/target/streams/test/fullClasspath/_global/streams/export`, separated by the path separator. sbt 2 writes it
to `target/out/jvm/<scala version>/<project>/streams/test/fullClasspath/_global/streams/export` as `List(...)`, with
`${OUT}`, `${BASE}` and `${CSR_CACHE}` for the build's output directory, the build root and coursier's cache, and a
content hash after each jar it built. Entries that don't exist are reported on stderr. sbt 2 keeps the output of each
Scala version the build was compiled with, and combining them puts two copies of each class on the classpath, so when
there's more than one it says so; pass `--scala <version>` to keep one.
"""
import os
import re
import sys

EXPORT = os.path.join("streams", "test", "fullClasspath", "_global", "streams", "export")


def coursier_cache():
    if os.environ.get("COURSIER_CACHE"):
        return os.environ["COURSIER_CACHE"]
    if sys.platform == "darwin":
        return os.path.expanduser("~/Library/Caches/Coursier/v1")
    return os.path.expanduser("~/.cache/coursier/v1")


def entries(line, root):
    line = line.strip()
    if not line.startswith("List("):
        return [e for e in line.split(os.pathsep) if e]
    placeholders = {"${OUT}": os.path.join(root, "target", "out"), "${BASE}": root, "${CSR_CACHE}": coursier_cache()}
    result = []
    for e in line[len("List("):-1].split(", "):
        e = e.split(">", 1)[0]
        for k, v in placeholders.items():
            e = e.replace(k, v)
        result.append(e)
    return result


def scala_version(path):
    match = re.search(r"[/\\]jvm[/\\]scala-([^/\\]+)[/\\]", path)
    return match.group(1) if match else None


def main():
    root = os.path.abspath(sys.argv[1])
    only = sys.argv[3] if len(sys.argv) > 3 and sys.argv[2] == "--scala" else None
    exports = []
    for d, dirs, files in os.walk(root):
        dirs[:] = sorted(x for x in dirs if not x.startswith(".") and x != "node_modules")
        if d.endswith(os.path.dirname(EXPORT)) and "export" in files:
            exports.append(os.path.join(d, "export"))
    versions = sorted({v for v in map(scala_version, exports) if v})
    if only:
        exports = [e for e in exports if scala_version(e) in (None, only)]
    elif len(versions) > 1:
        print(f"exports for Scala {', '.join(versions)}; pass --scala <version> to keep one", file=sys.stderr)
    if not exports:
        sys.exit(f"no exported Test / fullClasspath under {root}; compile the build's tests first")
    seen = set()
    for path in exports:
        with open(path, encoding="utf-8") as f:
            lines = f.read().splitlines()
        for e in entries(lines[-1] if lines else "", root):
            if e not in seen:
                seen.add(e)
                if not os.path.exists(e):
                    print(f"missing: {e}", file=sys.stderr)
                print(e)


if __name__ == "__main__":
    main()
