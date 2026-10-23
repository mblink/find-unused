#!/usr/bin/env bash
# Usage: compare_builds.sh <inputs dir> <out dir> <before.jar> <after.jar> [modes...]
#
# Runs two builds of the CLI (each `sbt cli3/assembly`, copied out of `cli/target/jvm-3/`) on an already compiled
# project and diffs their output in each mode, exiting 1 if any differ. <inputs dir> holds:
#   classpath.txt   the project's classpath, one entry per line (`classpath.py`)
#   packages.txt    the packages to analyze, one per line
#   exclusions.txt  optional: one `--exclusion` argument per line (the exclusions replay script's `--args`)
#   records.txt     optional: the compiler plugin's record directories, one per line
# Modes: all, transitive, givens, explicits, and with exclusions.txt, excluded and excluded-transitive (the default is
# every one that applies). The output is the plugin's, except that positions are relative to the build root, as
# TASTy records them, since the plugin passes `--root-directory`. A run that writes no output, e.g. because the CLI
# threw (see its `.log`), counts as a failure. JOBS runs at once (default 3), each with HEAP (default 8g).
set -u
inputs=$1; out=$2; before=$3; after=$4; shift 4
jobs=${JOBS:-3}; heap=${HEAP:-8g}

common=(); excl=()
# `|| [ -n "$l" ]` keeps a last line without a newline
while read -r l || [ -n "$l" ]; do [ -n "$l" ] && common+=(-p "$l"); done < "$inputs/packages.txt"
while read -r l || [ -n "$l" ]; do [ -n "$l" ] && common+=(-c "$l"); done < "$inputs/classpath.txt"
[ -f "$inputs/records.txt" ] && while read -r l || [ -n "$l" ]; do [ -n "$l" ] && common+=(--inline-given-records "$l"); done < "$inputs/records.txt"
[ -f "$inputs/exclusions.txt" ] && while read -r l || [ -n "$l" ]; do [ -n "$l" ] && excl+=(-e "$l"); done < "$inputs/exclusions.txt"

modes=("$@")
if [ $# -eq 0 ]; then
  modes=(all transitive givens explicits)
  [ ${#excl[@]} -gt 0 ] && modes+=(excluded excluded-transitive)
fi

run() {
  local jar=$1 dir=$2 mode=$3 cmd
  case $mode in
    all) cmd=(all) ;;
    transitive) cmd=(all --transitive) ;;
    givens | explicits) cmd=("$mode") ;;
    excluded) cmd=(all ${excl[@]+"${excl[@]}"}) ;;
    excluded-transitive) cmd=(all --transitive ${excl[@]+"${excl[@]}"}) ;;
    *) echo "unknown mode $mode" >&2; return 1 ;;
  esac
  rm -f "$dir/$mode.txt"
  java "-Xmx$heap" -cp "$jar" bl.unused.FindUnusedCli "${cmd[@]}" "${common[@]}" -o "$dir/$mode.txt" > "$dir/$mode.log" 2>&1
}

mkdir -p "$out/before" "$out/after"
for mode in "${modes[@]}"; do
  for side in before after; do
    jar=$before; [ $side = after ] && jar=$after
    run "$jar" "$out/$side" "$mode" &
    while [ "$(jobs -r | wc -l)" -ge "$jobs" ]; do sleep 1; done
  done
done
wait

status=0
for mode in "${modes[@]}"; do
  missing=""
  for side in before after; do [ -f "$out/$side/$mode.txt" ] || missing="$missing $side"; done
  if [ -n "$missing" ]; then
    echo "FAILED: $mode, no output from$missing (see $out/<side>/$mode.log)"; status=1
  elif cmp -s "$out/before/$mode.txt" "$out/after/$mode.txt"; then
    echo "same: $mode ($(head -1 "$out/after/$mode.txt"))"
  else
    echo "DIFFERENT: $mode"; diff "$out/before/$mode.txt" "$out/after/$mode.txt" | head -40; status=1
  fi
done
exit $status
