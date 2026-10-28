#!/usr/bin/env bash
# Usage: capture.sh <test> [sbt project, default plugin3]
# Runs a scripted test with each `$ must-mirror <expected> <actual>` line replaced by a copy of <actual> to
# $CAPTURE_DIR/<test>/<expected file name> (default: a temporary directory), restores the test script, and diffs every
# captured file against the test's expected one. Run it from the repository root.
set -uo pipefail
test=$1
project=${2:-plugin3}
dir=plugin/src/sbt-test/find-unused/$test
[ -f "$dir/test" ] || { echo "no scripted test at $dir" >&2; exit 2; }
out=${CAPTURE_DIR:-$(mktemp -d)}/$test
rm -rf "$out" && mkdir -p "$out"
cp "$dir/test" "$out.test.bak"
trap 'cp "$out.test.bak" "$dir/test"' EXIT
sed -E "s#^\\\$ must-mirror ([^ ]+) ([^ ]+)\$#\$ exec cp \\2 $out/\\1#" "$out.test.bak" | sed -E "s#$out/output/#$out/#" > "$dir/test"
sbt -batch "$project/scripted find-unused/$test" > "$out.log" 2>&1
echo "scripted exit=$? (log: $out.log)"
grep -E '^\[error\] x |Cause of test' "$out.log" | head -3
for f in "$out"/*; do
  [ -e "$f" ] || { echo "nothing captured"; break; }
  n=$(basename "$f"); echo "== $n"
  diff "$dir/output/$n" "$f" && echo same
done
