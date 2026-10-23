#!/usr/bin/env bash
# Usage: apply_layer.sh <old base> <old branch> <new base> <new branch>
#
# Starts <new branch> at <new base> and applies the whole change of one stacked PR, everything between <old base> and
# <old branch>, with a 3-way merge, leaving conflict markers where the new base differs. Lists the conflicted files.
# Resolve them, make the layer what it should be, `sbt test`, and commit it as one commit.
set -u
git checkout -q -B "$4" "$3" || exit 1
patch=$(mktemp)
git diff --binary "$1" "$2" > "$patch"
git apply -3 --whitespace=nowarn "$patch" 2>&1 | grep -vE "^Applied patch .* cleanly|^Falling back|^Performing three-way"
rm -f "$patch"
echo "--- conflicts:"
git diff --name-only --diff-filter=U
