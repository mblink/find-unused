---
name: benchmark
description: Measure find-unused's performance and the effect of an analysis change on a large real classpath - bytecode scan time, givens analysis time, and how many findings a change adds or removes. Use when asked how fast find-unused is, before and after changing Bytecode.scala or the traversal, or to check a change against a real project.
---

# Benchmarking

`Bench.scala` (next to this file) runs the core directly, without the plugin or CLI. It prints the bytecode scan time
(three runs: the first is what a real CLI run pays, cold), the tasty-query `Context` setup time, and the number of
unused givens with and without the bytecode scan. `sbt test` compiles it against `core` (the `skills3` project), so
a change to core internals that breaks it fails the build. It also writes `rescued-by-bytecode.tsv` (givens only the scan
counts as used) and `unused-givens.tsv` to an output directory. `$SCRATCH` below is any scratch directory outside the
repo.

## 1. Build the core and export its classpath

```bash
sbt "core3/compile; export core3/Runtime/fullClasspath" 2>&1 | grep -v '^\[' | tail -1 | tr ':' '\n' > "$SCRATCH/core-cp.txt"
```

## 2. Get a classpath to analyze, one entry per line

- Published Scala 3 artifacts (stable and reproducible), e.g. the Scala 3 compiler, analyzing package `dotty`:
  `cs fetch org.scala-lang:scala3-compiler_3:3.10.0 > "$SCRATCH/dotty-cp.txt"`.
  Avoid cats as a full-analysis corpus: tasty-query throws `InvalidProgramStructureException` on it, independent of
  find-unused, though the bytecode scan alone works.
- A real sbt project that's already compiled: sbt writes each project's `Test / fullClasspath` to
  `<project>/target/streams/test/fullClasspath/_global/streams/export`. Union the last line of those files (split on
  `:`). Only combine projects compiled with the same Scala version, since mixing versions puts two copies of each
  class on the classpath. Reading another repo's build output is read-only, but don't run sbt in someone else's
  working tree to produce it.

## 3. Run it from a scratch copy

scala-cli writes `.scala-build/` and `.bsp/` next to its inputs, so copy the harness out of the repo first:

```bash
cp .claude/skills/benchmark/Bench.scala "$SCRATCH/"
cd "$SCRATCH" && scala-cli run --server=false -S 3.10.0 --java-opt -Xmx6g \
  --extra-jars "$(paste -sd: core-cp.txt)" Bench.scala -- dotty dotty-cp.txt "$SCRATCH"
```

The first argument is a comma-separated package list, as in `findUnusedPackages`.

## Reading the results

- Compare the unused counts, not the two analysis timings: the first analysis also warms up the JIT.
- To measure a change, run once on the base commit and once with the change, and diff `unused-givens.tsv`.
- `Bytecode.references` takes an optional `threads` argument, to compare pool sizes. On a large closed-source
  application (about ten sbt projects, roughly 10k classes and 250 jars; October 2026, ASM 9.11.1, JDK 25, 18 cores,
  OS file cache warm), cold scans took ~830 ms on 1 thread, ~665 ms on 2, ~585 ms on 3 (the default cap), ~560 ms on
  4, and ~515 ms on 9. A first run with a cold OS file cache took ~1.7 s on 1 thread. Listing the entries of all of
  its jars takes ~40 ms, so nearly all of the scan is reading and parsing class files.
- The givens analysis itself took ~3–7 s on that application and ~4–5 s on the Scala 3 compiler (`dotty`,
  5,058 classes), depending on JIT warm-up.
