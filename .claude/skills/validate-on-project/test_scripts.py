#!/usr/bin/env python3
"""Checks `classpath.py`, `table2tsv.sh` and `pr-stack/resolve_conflicts.py` on small inputs. CI runs
it; run it after changing them."""
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))


def run(script, *args, cwd):
    return subprocess.run(
        [*(["bash"] if script.endswith(".sh") else [sys.executable]), os.path.join(HERE, script), *args], cwd=cwd, capture_output=True, text=True, check=True
    ).stdout


def check(name, actual, expected):
    if actual != expected:
        sys.exit(f"{name}: expected\n{expected!r}\nbut got\n{actual!r}")


TABLE = """Found 2 unused terms

┌────────────────────────┬───────────────────┐
│ Instance               │ Location          │
├────────────────────────┼───────────────────┤
│ ex.Foo.bar(baz = ...)  │ Foo.scala:2:15    │
├────────────────────────┼───────────────────┤
│ ex.Foo.unused          │ Foo.scala:4:7     │
└────────────────────────┴───────────────────┘
"""

with tempfile.TemporaryDirectory() as d:
    with open(os.path.join(d, "all.txt"), "w", encoding="utf-8") as f:
        f.write(TABLE)
    check("table2tsv.sh", run("table2tsv.sh", "all.txt", cwd=d), "ex.Foo.bar(baz = ...)\tFoo.scala:2:15\nex.Foo.unused\tFoo.scala:4:7\n")

# sbt 1 and sbt 2 export layouts, each project's classpath on the last line
with tempfile.TemporaryDirectory() as d:
    export = os.path.join("streams", "test", "fullClasspath", "_global", "streams", "export")
    sbt1 = os.path.join(d, "a", "target", export)
    sbt2 = os.path.join(d, "target", "out", "jvm", "scala-3.10.0", "b", export)
    for path, line in [
        (sbt1, os.pathsep.join(["/x/a-classes", "/x/lib.jar"])),
        (sbt2, "List(${OUT}/jvm/scala-3.10.0/b/b_3-1.0.jar>sha256-0f/12, ${CSR_CACHE}/lib.jar, /x/lib.jar)"),
    ]:
        os.makedirs(os.path.dirname(path))
        with open(path, "w", encoding="utf-8") as f:
            f.write("ignored first line\n" + line + "\n")
    stale = os.path.join(d, "target", "out", "jvm", "scala-3.8.4", "b", export)
    os.makedirs(os.path.dirname(stale))
    with open(stale, "w", encoding="utf-8") as f:
        f.write("List(/old/lib.jar)\n")
    env_cache = os.environ.get("COURSIER_CACHE")
    os.environ["COURSIER_CACHE"] = "/cache"
    try:
        check(
            "classpath.py",
            run("classpath.py", d, "--scala", "3.10.0", cwd=d).splitlines(),
            ["/x/a-classes", "/x/lib.jar", os.path.join(d, "target", "out", "jvm", "scala-3.10.0", "b", "b_3-1.0.jar"), "/cache/lib.jar"],
        )
    finally:
        if env_cache is None:
            del os.environ["COURSIER_CACHE"]
        else:
            os.environ["COURSIER_CACHE"] = env_cache

# Conflict blocks are replaced in order, by side or by text
with tempfile.TemporaryDirectory() as d:
    path = os.path.join(d, "f")
    with open(path, "w", encoding="utf-8") as f:
        f.write("a\n<<<<<<< ours\nx\n=======\ny\n>>>>>>> theirs\nb\n<<<<<<< ours\n1\n=======\n2\n>>>>>>> theirs\n")
    with open(os.path.join(d, "r.json"), "w", encoding="utf-8") as f:
        f.write('["theirs", "three\\n"]')
    run(os.path.join("..", "pr-stack", "resolve_conflicts.py"), path, os.path.join(d, "r.json"), cwd=d)
    with open(path, encoding="utf-8") as f:
        check("resolve_conflicts.py", f.read(), "a\ny\nb\nthree\n")
    # diff3 style: the base section isn't part of either side
    with open(path, "w", encoding="utf-8") as f:
        f.write("<<<<<<< ours\nx\n||||||| base\nb\n=======\ny\n>>>>>>> theirs\n")
    with open(os.path.join(d, "r.json"), "w", encoding="utf-8") as f:
        f.write('["ours"]')
    run(os.path.join("..", "pr-stack", "resolve_conflicts.py"), path, os.path.join(d, "r.json"), cwd=d)
    with open(path, encoding="utf-8") as f:
        check("resolve_conflicts.py with diff3 markers", f.read(), "x\n")

# `remove-unused-code` bundles a copy of these so users can copy that one folder into their own project
for name in ("CompareBytecode.scala", "RemoveDefaults.scala", "table2tsv.sh"):
    with open(os.path.join(HERE, name), "rb") as a, open(os.path.join(HERE, "..", "remove-unused-code", name), "rb") as b:
        check(f"remove-unused-code/{name} matches validate-on-project/{name}", b.read() == a.read(), True)

print("ok")
