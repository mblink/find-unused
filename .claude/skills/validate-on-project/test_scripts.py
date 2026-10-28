#!/usr/bin/env python3
"""Checks `classpath.py`, `remove_defaults.py`, `table2tsv.py` and `pr-stack/resolve_conflicts.py` on small inputs. CI runs
it; run it after changing them."""
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))


def run(script, *args, cwd):
    return subprocess.run(
        [sys.executable, os.path.join(HERE, script), *args], cwd=cwd, capture_output=True, text=True, check=True
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

# Each default is reported at its parameter, and each case is a line of its own
SOURCE = """object Foo {
  def bar(a: Int, baz: Int = 1, c: Int): Int = a
  def braces(x: String = s"${Map("k" -> "}").size} }", y: Int): Int = y
  def unused = 1
  def tripleQuoted(x: String = \"\"\"a ${"}"} b\"\"\" /* trailing */, y: Int): Int = y
  def comments(x: Int = 2 // the default
    , y: Int): Int = y
  def nested(x: List[Int] = List(1, (2), 3), y: Int): Int = y
  def operators(f: (Int, Int) => Boolean = _ >= _, y: Int): Int = y
}
"""

EXPECTED = """object Foo {
  def bar(a: Int, baz: Int, c: Int): Int = a
  def braces(x: String, y: Int): Int = y
  def unused = 1
  def tripleQuoted(x: String /* trailing */, y: Int): Int = y
  def comments(x: Int // the default
    , y: Int): Int = y
  def nested(x: List[Int], y: Int): Int = y
  def operators(f: (Int, Int) => Boolean, y: Int): Int = y
}
"""

FINDINGS = [
    ("ex.Foo.bar(baz = ...)", "Foo.scala:2:19"),
    ("ex.Foo.braces(x = ...)", "Foo.scala:3:14"),
    ("ex.Foo.unused", "Foo.scala:4:7"),
    ("ex.Foo.tripleQuoted(x = ...)", "Foo.scala:5:20"),
    ("ex.Foo.comments(x = ...)", "Foo.scala:6:16"),
    ("ex.Foo.nested(x = ...)", "Foo.scala:8:14"),
    ("ex.Foo.operators(f = ...)", "Foo.scala:9:17"),
    # Without a location, so it's skipped
    ("ex.Foo.elsewhere(x = ...)", ""),
]

with tempfile.TemporaryDirectory() as d:
    with open(os.path.join(d, "all.txt"), "w", encoding="utf-8") as f:
        f.write(TABLE)
    check("table2tsv.py", run("table2tsv.py", "all.txt", cwd=d), "ex.Foo.bar(baz = ...)\tFoo.scala:2:15\nex.Foo.unused\tFoo.scala:4:7\n")

    # Windows line endings must survive the rewrite
    with open(os.path.join(d, "Foo.scala"), "w", encoding="utf-8", newline="") as f:
        f.write(SOURCE.replace("\n", "\r\n"))
    with open(os.path.join(d, "all.tsv"), "w", encoding="utf-8") as f:
        f.writelines(f"{n}\t{l}\n" for n, l in FINDINGS)

    dry = run("remove_defaults.py", "all.tsv", cwd=d)
    check("remove_defaults.py without --apply", dry.count("removing"), 6)
    check("remove_defaults.py skipped", "SKIPPED" in dry, False)
    with open(os.path.join(d, "Foo.scala"), encoding="utf-8", newline="") as f:
        check("remove_defaults.py without --apply changes nothing", f.read(), SOURCE.replace("\n", "\r\n"))

    run("remove_defaults.py", "all.tsv", "--apply", cwd=d)
    with open(os.path.join(d, "Foo.scala"), encoding="utf-8", newline="") as f:
        check("remove_defaults.py --apply", f.read(), EXPECTED.replace("\n", "\r\n"))

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

# `remove-unused-code` bundles copies of these so users can copy that one folder into their own project
for name in ("CompareBytecode.scala", "table2tsv.py", "remove_defaults.py"):
    with open(os.path.join(HERE, name), "rb") as a, open(os.path.join(HERE, "..", "remove-unused-code", name), "rb") as b:
        check(f"remove-unused-code/{name} matches validate-on-project/{name}", b.read() == a.read(), True)

print("ok")
