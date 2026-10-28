#!/usr/bin/env bash
# Checks the skills' scripts (`Classpath.scala`, `RemoveDefaults.scala`, `table2tsv.sh`, `benchmark/SummarizeJfr.scala`
# and `pr-stack/`'s `BlameLayers.scala` and `ResolveConflicts.scala`) on small inputs. Needs scala-cli and git. CI runs
# it; run it after changing them.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
skills=$(dirname "$here")
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# scala-cli writes `.scala-build/` next to the script it runs, so run copies
mkdir "$tmp/scripts"
cp "$here"/*.scala "$skills"/benchmark/*.scala "$skills"/pr-stack/*.scala "$tmp/scripts/"
run() { local script=$1; shift; scala-cli run --power "$tmp/scripts/$script" -- "$@" ; }

check() {
  if [[ "$2" != "$3" ]]; then
    printf '%s: expected\n%s\nbut got\n%s\n' "$1" "$2" "$3" >&2
    exit 1
  fi
}

crlf() { awk '{ printf "%s\r\n", $0 }'; }

# table2tsv.sh
cat > "$tmp/all.txt" <<'TABLE'
Found 2 unused terms

┌────────────────────────┬───────────────────┐
│ Instance               │ Location          │
├────────────────────────┼───────────────────┤
│ ex.Foo.bar(baz = ...)  │ Foo.scala:2:15    │
├────────────────────────┼───────────────────┤
│ ex.Foo.unused          │ Foo.scala:4:7     │
└────────────────────────┴───────────────────┘
TABLE
check table2tsv.sh $'ex.Foo.bar(baz = ...)\tFoo.scala:2:15\nex.Foo.unused\tFoo.scala:4:7' "$(bash "$here/table2tsv.sh" "$tmp/all.txt")"

# RemoveDefaults.scala: each default is reported at its parameter, and each case is a line of its own. Windows line
# endings must survive the rewrite.
mkdir "$tmp/defaults"
cat > "$tmp/defaults/source" <<'SOURCE'
object Foo {
  def bar(a: Int, baz: Int = 1, c: Int): Int = a
  def braces(x: String = s"${Map("k" -> "}").size} }", y: Int): Int = y
  def unused = 1
  def tripleQuoted(x: String = """a ${"}"} b""" /* trailing */, y: Int): Int = y
  def comments(x: Int = 2 // the default
    , y: Int): Int = y
  def nested(x: List[Int] = List(1, (2), 3), y: Int): Int = y
  def operators(f: (Int, Int) => Boolean = _ >= _, y: Int): Int = y
}
SOURCE
cat > "$tmp/defaults/expected" <<'EXPECTED'
object Foo {
  def bar(a: Int, baz: Int, c: Int): Int = a
  def braces(x: String, y: Int): Int = y
  def unused = 1
  def tripleQuoted(x: String /* trailing */, y: Int): Int = y
  def comments(x: Int // the default
    , y: Int): Int = y
  def nested(x: List[Int], y: Int): Int = y
  def operators(f: (Int, Int) => Boolean, y: Int): Int = y
}
EXPECTED
crlf < "$tmp/defaults/source" > "$tmp/defaults/Foo.scala"
crlf < "$tmp/defaults/expected" > "$tmp/defaults/Foo.expected"
printf '%s\t%s\n' \
  'ex.Foo.bar(baz = ...)' 'Foo.scala:2:19' \
  'ex.Foo.braces(x = ...)' 'Foo.scala:3:14' \
  'ex.Foo.unused' 'Foo.scala:4:7' \
  'ex.Foo.tripleQuoted(x = ...)' 'Foo.scala:5:20' \
  'ex.Foo.comments(x = ...)' 'Foo.scala:6:16' \
  'ex.Foo.nested(x = ...)' 'Foo.scala:8:14' \
  'ex.Foo.operators(f = ...)' 'Foo.scala:9:17' \
  'ex.Foo.elsewhere(x = ...)' '' > "$tmp/defaults/all.tsv"
( cd "$tmp/defaults"
  dry=$(run RemoveDefaults.scala all.tsv)
  check "RemoveDefaults.scala without --apply" 6 "$(grep -c removing <<< "$dry")"
  check "RemoveDefaults.scala skipped" 0 "$(grep -c SKIPPED <<< "$dry" || true)"
  cmp -s Foo.scala <(crlf < source) || { echo "RemoveDefaults.scala without --apply changed the file" >&2; exit 1; }
  run RemoveDefaults.scala all.tsv --apply > /dev/null
  cmp -s Foo.scala Foo.expected || { echo "RemoveDefaults.scala --apply: unexpected result" >&2; exit 1; } )

# Classpath.scala: sbt 1 and sbt 2 export layouts, each project's classpath on the last line
export_dir=streams/test/fullClasspath/_global/streams
mkdir -p "$tmp/cp/a/target/$export_dir" "$tmp/cp/target/out/jvm/scala-3.10.0/b/$export_dir" "$tmp/cp/target/out/jvm/scala-3.8.4/b/$export_dir"
printf 'ignored first line\n/x/a-classes:/x/lib.jar\n' > "$tmp/cp/a/target/$export_dir/export"
printf 'ignored first line\nList(${OUT}/jvm/scala-3.10.0/b/b_3-1.0.jar>sha256-0f/12, ${CSR_CACHE}/lib.jar, /x/lib.jar)\n' \
  > "$tmp/cp/target/out/jvm/scala-3.10.0/b/$export_dir/export"
printf 'List(/old/lib.jar)\n' > "$tmp/cp/target/out/jvm/scala-3.8.4/b/$export_dir/export"
# (the script's own view of coursier's cache can't be changed here, since scala-cli uses it too)
if [[ -n "${COURSIER_CACHE:-}" ]]; then cache=$COURSIER_CACHE
elif [[ "$(uname)" == Darwin ]]; then cache=$HOME/Library/Caches/Coursier/v1
else cache=$HOME/.cache/coursier/v1; fi
check Classpath.scala "$(printf '%s\n' /x/a-classes /x/lib.jar "$tmp/cp/target/out/jvm/scala-3.10.0/b/b_3-1.0.jar" "$cache/lib.jar")" \
  "$(run Classpath.scala "$tmp/cp" --scala 3.10.0 2>/dev/null)"

# ResolveConflicts.scala: conflict blocks are replaced in order, by side or by text
printf 'a\n<<<<<<< ours\nx\n=======\ny\n>>>>>>> theirs\nb\n<<<<<<< ours\n1\n=======\n2\n>>>>>>> theirs\n' > "$tmp/f"
printf '["theirs", "three\\n"]' > "$tmp/r.json"
run ResolveConflicts.scala "$tmp/f" "$tmp/r.json"
check ResolveConflicts.scala $'a\ny\nb\nthree' "$(cat "$tmp/f")"
# diff3 style: the base section isn't part of either side
printf '<<<<<<< ours\nx\n||||||| base\nb\n=======\ny\n>>>>>>> theirs\n' > "$tmp/f"
printf '["ours"]' > "$tmp/r.json"
run ResolveConflicts.scala "$tmp/f" "$tmp/r.json"
check "ResolveConflicts.scala with diff3 markers" x "$(cat "$tmp/f")"

# SummarizeJfr.scala
cat > "$tmp/samples.txt" <<'SAMPLES'
jdk.ExecutionSample {
  stackTrace = [
    bl.unused.Foo.bar(int) line: 1
    tastyquery.Baz.qux() line: 2
  ]
}
jdk.ExecutionSample {
  stackTrace = [
    bl.unused.Foo.bar(int) line: 1
  ]
}
SAMPLES
check SummarizeJfr.scala $'2 samples\n\nself:\n100.0% bl.unused.Foo.bar\n\ninclusive:\n100.0% bl.unused.Foo.bar\n 50.0% tastyquery.Baz.qux' \
  "$(run SummarizeJfr.scala "$tmp/samples.txt")"

# BlameLayers.scala: `main` has the file, `layer1` changes its fourth line, and the change under test edits that and the first
mkdir "$tmp/repo"
( cd "$tmp/repo"
  g() { git -c user.name=t -c user.email=t@t "$@"; }
  g init -q -b main
  printf 'a\nb\nc\nd\n' > f; g add f; g commit -q -m base
  g checkout -q -b layer1; printf 'a\nb\nc\nd1\n' > f; g commit -q -am layer1
  g checkout -q -b change; printf 'a2\nb\nc\nd2\n' > f; g commit -q -am change
  printf 'main\nlayer1\n' > ../layers
  check BlameLayers.scala $'== f\n  -1,1: {\'main\': 1}\n  -4,1: {\'layer1\': 1}' \
    "$(run BlameLayers.scala ../layers layer1 change | sed 's/} .*$/}/')" )

# `remove-unused-code` bundles copies of these so users can copy that one folder into their own project
for name in CompareBytecode.scala RemoveDefaults.scala table2tsv.sh; do
  cmp -s "$here/$name" "$skills/remove-unused-code/$name" || { echo "remove-unused-code/$name differs from validate-on-project/$name" >&2; exit 1; }
done

echo ok
