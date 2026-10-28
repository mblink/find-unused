---
name: find-unused
description: Set up and use find-unused, a dead code detector for Scala 3, in a project - install the sbt plugin or CLI, configure packages and exclusions, run it, interpret the findings, and safely delete what it reports. Use when asked to find or remove unused/dead Scala code, to add find-unused to a build or CI, or to explain or fix a find-unused finding.
---

# Using find-unused in a Scala 3 project

find-unused reports code nothing uses: classes, objects, methods, `val`s, `given`s, default values and auxiliary
constructors. It reads the TASTy files the compiler emits, plus the bytecode for what TASTy can't show, so it analyzes
the whole codebase at once and can tell that a *public* member is never called, which `-Wunused` can't. The project's
[README](https://github.com/mblink/find-unused#readme) is the reference for every option; this is the workflow.

## Requirements

- The code under analysis is Scala 3 and compiled. find-unused reads compiler output, so it never sees stale source.
- sbt 1.9+ or sbt 2 for the plugin, or Java for the standalone CLI.
- One Scala version across the build. A build that cross-compiles several versions isn't supported yet.

## 1. Install

sbt plugin, in `project/plugins.sbt` (it isn't on Maven Central, so the resolver is required; take the version from the
[latest release](https://github.com/mblink/find-unused/releases/latest)):

```scala
resolvers += "bondlink-maven-repo" at "https://maven.bondlink-cdn.com"
addSbtPlugin("bondlink" % "find-unused" % "x.y.z")
```

Then in `build.sbt`, name the **top-level packages** to analyze, the ones the project owns. Dependencies are read from
the classpath but never reported:

```scala
Global / findUnusedPackages := Seq("com.example")
```

Without sbt, download `find-unused.jar` from the release and pass `--package` and `--classpath` (the full classpath of
the project and its dependencies) to it; see "Standalone CLI" in the README.

## 2. Run it

```
sbt findUnusedAll
```

| Task | Reports |
|---|---|
| `findUnusedAll` | everything; also lists exclusions and `@unused` annotations that aren't needed. Start here |
| `findUnusedExplicits` | non-implicit code only |
| `findUnusedGivens` (`findUnusedImplicits`) | `given`/`implicit`/`using` code only |

It prints a table of symbol and `file:line:col`, and exits non-zero when it finds anything, so it can gate CI. Set
`Global / findUnusedOutputFile := Some(file("find-unused.txt"))` to write the table to a file.

Add `Global / findUnusedTransitive := true` to also report code that's only used by unused code (an unused object whose
members call each other, a recursive method nothing else calls). It needs `findUnusedAll`. Roots are `main`/`@main`
methods, `App`/`IOApp` objects, classes under `src/test/`, and excluded or `@unused` code, so anything else reached
only by reflection or configuration needs an exclusion first.

## 3. Triage every finding before deleting

A finding is evidence, not proof. For each one, in this order:

1. **Search for it by name** across the repo, including non-Scala files (config, `reference.conf`, routes, SQL,
   Java sources, build files) and reflection (`Class.forName`, framework wiring).
2. **Decide which kind it is:**
   - *Genuinely dead*: delete it, then compile and run the tests.
   - *Used in a way find-unused can't see*: add an exclusion (below), with a comment saying why.
   - *Public API of a library*: exclude it, or narrow `findUnusedPackages` to the internal packages.
   - *A false positive*: check the known issues below; if it isn't one, it's a bug worth reporting with a minimal
     example.
3. **Delete one logical group at a time**, recompile, and rerun find-unused. Removing code often makes more code
   unused, so repeat until it's quiet. Transitive mode finds these chains in one run.

Never bulk-delete the whole list. Don't remove an unused default value's *parameter*: only the default is unused, and
removing the default is safe because a caller relying on it fails to compile.

## 4. Exclusions

```scala
Global / findUnusedExclusions := Seq(
  FindUnusedExclusion(src = ".*/src_managed/.*".r),                      // generated code
  FindUnusedExclusion(sym = """^com\.example\.MyClass\.(foo|bar)$""".r), // used by reflection
)
```

`src` matches the file path and `sym` the fully qualified symbol; both are regexes. Good reasons to exclude: generated
sources, code looked up by name at runtime, entry points a framework calls (servlets, Lambda handlers, JSON-RPC
methods), no-argument constructors a serialization or DI framework needs (`sym=\.<init>$`), and defaults a reflection
library fills in (`sym=...\(\w+ = \.\.\.\)$`). `findUnusedAll` lists exclusions that no longer match anything, so
prune them as code is deleted. Prefer an exclusion over `@unused` for code that's *used*; `@unused` also silences the
compiler's own warning and is for parameters that are intentionally ignored.

## 5. Known limitations (check before calling something a false positive)

- A `given` declared inside a method body that's only summoned by an `inline` method or macro is reported, because it
  compiles to a local variable.
- An `inline given` of a type class that some `inline` method summons is never reported, even if unused. Enable
  `Global / findUnusedCompilerPlugin := true` for exact results; it records the `inline given`s the compiler expands.
- A `transparent inline` method that reduces to a constant can be reported although it is used.
- Reflection, `Class.forName`, and framework-instantiated classes need exclusions.

## 6. CI

Run `sbt findUnusedAll` as a build step; a finding fails the job. Start by excluding what exists today so the check
passes, then tighten as the exclusions are removed. To adopt incrementally, run it on the main branch first and only
fail on new findings after the initial cleanup.

## When something looks wrong

- *Nothing is reported on a project that surely has dead code*: `findUnusedPackages` doesn't match the real package
  names, or the code isn't compiled. Run with `Global / findUnusedDebug := true`.
- *The build fails in a multi-Scala-version setup*: not supported yet; run against a single-version build.
- *A crash or a clearly wrong result*: reproduce with the standalone CLI and `--debug`, and open an issue at
  https://github.com/mblink/find-unused/issues with a minimal example.
