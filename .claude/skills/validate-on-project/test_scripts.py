#!/usr/bin/env python3
"""Checks `remove_defaults.py` and `table2tsv.py` on small inputs. CI runs it; run it after changing either script."""
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

print("ok")
