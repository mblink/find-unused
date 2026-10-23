---
name: scripted-test
description: Add or update a find-unused scripted test and regenerate its expected output. Use when adding a test case for a new TASTy/bytecode pattern, when a change legitimately alters a test's rendered table, or when a scripted test fails with "File contents are different".
---

# Scripted tests

All behavior is tested by sbt scripted tests in `plugin/src/sbt-test/find-unused/<name>/`. See the "Testing model"
section of `CLAUDE.md` for what each file does.

## Adding a test

1. Copy the closest existing test. Single-project tests (`inline`, `summonFrom`) keep sources in `src/`;
   multi-project tests (`givens`, `summon`) use `a/` and `b/` with `b.dependsOn(a)`. Use a multi-project build when the
   test needs a macro, since a macro must be compiled before the code that expands it.
2. Keep `project/plugins.sbt` and the `findUnused*` settings in `build.sbt` exactly as in the other tests. A new test
   directory needs its own copy of `project/plugins.sbt`; without it, loading the build fails with
   `Not found: findUnusedUseLocalClasspath`.
3. Put sources in package `bl.unused`, which is what `findUnusedPackages` analyzes.
4. Include a control case: something that should still be reported. A test where everything is used would also pass
   if the analysis were disabled.
5. If the test expects findings, the script uses `-> findUnusedGivens` (or `Explicits`/`All`), because finding unused
   code exits 1. A test can run several analyses in turn, e.g. `> set Global / findUnusedTransitive := true` between
   two `findUnusedAll`s compared with `expected.txt` and `expected-transitive.txt`. To change files between runs in a
   way that works under both sbt versions, whose `target` layouts differ, define a task in the test's `build.sbt` that
   computes the path from a setting, as the `compiler-plugin` test's `deleteRecord` does.
6. If the output depends on how the compiler pickles something, run the test on every Scala version in
   `scriptedScalaVersions` (`-Dscripted.scala.version=<v>`): they must give the same output, since CI compares all of
   them with one `expected.txt`.

## Capturing the actual output

Scripted copies each test to a temporary directory and deletes it afterwards, so `output/actual.txt` never shows up in
the source tree. To capture it, temporarily add an `exec cp` line to the test's `test` script before `must-mirror`,
copying to an absolute path in your scratchpad:

```
-> findUnusedGivens
$ exec cp output/actual.txt /absolute/scratch/path/actual.txt
$ must-mirror output/expected.txt output/actual.txt
```

Run it (an empty `output/expected.txt` is fine the first time; the test fails but the copy still happens). If it fails
on the first line with "Command succeeded but failure was expected", nothing was reported, so there's no table to
capture; switch to `>` if that's the intended result.

```bash
sbt 'plugin3/scripted find-unused/<name>'
```

Read the captured file, check that every row is what you expect, then copy it to `output/expected.txt`, and remove the
`exec cp` line again. Copy the file rather than retyping the table, since `must-mirror` compares bytes exactly.

## Checking that the test exercises the change

Run the new test with the change temporarily removed, to confirm it fails without it:

```bash
# -u so new files are stashed too; name the entry so the pop can't take an older one
git stash push -u -m exercise-check -- core cli plugin/src/main build.sbt
sbt 'plugin3/scripted find-unused/<name>'
git stash list | grep -q exercise-check && git stash pop
```

This only works while the change is uncommitted. If `git stash push` reports nothing to stash, the change is already
committed or lives somewhere else; check out the base commit's versions of those files instead.

## Before finishing

- Run the test under both sbt versions: `sbt 'plugin2_12/scripted find-unused/<name>' 'plugin3/scripted find-unused/<name>'`.
- For anything that depends on how the compiler encodes code (bytecode names, TASTy shapes), also run the older Scala
  versions CI uses: `sbt -Dscripted.scala.version=3.8.4 'plugin3/scripted find-unused/<name>'` (and `3.9.0`; the full
  list is `scriptedScalaVersions` in `build.sbt`).
- The full local check is `sbt 'test; scripted'`.
