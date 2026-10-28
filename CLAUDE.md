# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Scala 3 tool that finds unused code by reading the TASTy files the compiler emits and walking them with
[tasty-query](https://github.com/scalacenter/tasty-query), supplemented by a scan of the compiled bytecode for
`given`s, default values and `object`s that TASTy can't see being used. It ships as an sbt plugin and a standalone CLI
jar, with an optional compiler plugin for exact `inline given` results.
`README.md` documents user-facing setup, options, exclusions, default values, auxiliary constructors, transitive mode,
and known limitations (unused `inline given`s of type classes that inline methods summon, `transparent inline` methods
that reduce to a constant, single-Scala-version builds) — read it before changing behavior that's documented
there.

## Modules

Projects are built with sbt-projectmatrix, so the sbt project ids carry a Scala-version suffix:

| id | directory | notes |
|---|---|---|
| `core3` | `core/` | The analysis engine. Depends on tasty-query, ASM, cats, pprint. |
| `cli3` | `cli/` | `bl.unused.FindUnusedCli` main class, mainargs parsing, table rendering. Assembled into one jar (CI names it `find-unused.jar`). |
| `plugin2_12` / `plugin3` | `plugin/` | The sbt plugin. `2_12` targets sbt 1.9.0, `3` targets sbt 2.0.0. |
| `skills3` | `.claude/skills/` | Compiles the skills' scala-cli scripts against `core`, so `sbt test` catches them breaking. Not published. |
| `compiler-plugin3` | `compiler-plugin/` | The optional compiler plugin that records the `inline given`s inlining expands. Built with the oldest supported Scala version, so it runs in newer compilers. |

## Commands

```bash
sbt test                                  # compiles everything (there are no unit tests)
sbt scripted                              # runs all plugin integration tests, both sbt versions
sbt 'plugin3/scripted find-unused/all'    # a single scripted test under sbt 2
sbt 'plugin2_12/scripted find-unused/*'   # all scripted tests under sbt 1
sbt cli3/assembly                         # builds the standalone jar into cli/target/jvm-3/
sbt compiler-plugin3/package              # builds the compiler plugin jar into compiler-plugin/target/jvm-3/
sbt githubWorkflowGenerate                # regenerate .github/workflows/ after editing build.sbt
```

CI runs `sbt 'test; scripted'` and `bash .claude/skills/validate-on-project/test_scripts.sh` (which needs scala-cli), so those are the full
local check. CI also runs scripted against older Scala
versions (`scriptedScalaVersions` in `build.sbt`); reproduce one locally with
`sbt -Dscripted.scala.version=3.8.4 'plugin3/scripted find-unused/<name>'`.
A scripted test failing in CI under sbt 2 with `file referenced by the build does not exist:
.../scala3-sbt-bridge-<version>.jar` (and no `actual.txt`) is a compiler-bridge cache flake, not a regression; rerun the job.

`.github/workflows/*.yml` are **generated** from the `githubWorkflow*` settings in `build.sbt` by sbt-github-actions.
Never hand-edit them; change `build.sbt` and regenerate, then commit both.

To develop against a local tasty-query checkout at `~/tasty-query`, run sbt with `TASTY_QUERY_DEVELOPMENT=1` —
`build.sbt` then swaps the published dependency for a `ProjectRef`.

## Testing model

There are no unit tests. All behavior is covered by sbt scripted tests in `plugin/src/sbt-test/find-unused/<name>/`.
Each one is a miniature sbt build containing real Scala sources with known unused code, plus:

- `test` — the script. `> task` expects success, `-> task` expects failure. Finding unused code exits 1, so tests
  that expect findings use `->`.
- `build.sbt` — always sets `findUnusedUseLocalClasspath := true` (run the just-built `core`/`cli` classes instead of
  fetching a published CLI), `findUnusedUseRootDirectory := false` (keep positions relative so expected output is
  stable), and `findUnusedOutputFile` pointing at `output/actual.txt`.
- `output/expected.txt` — the exact rendered table, compared via scripted's `must-mirror`. Scripted runs each test in a
  temporary copy that it deletes afterwards, so `output/actual.txt` never appears in the source tree; the
  `scripted-test` skill shows how to capture it when output changes legitimately.

To check a change against a real codebase, or before deleting code find-unused reports, follow the
`validate-on-project` skill; to delete what find-unused reports in a project that uses it, follow `remove-unused-code`. To change a PR that others are stacked on, follow the `pr-stack` skill.

Adding a case for a new TASTy pattern means adding a directory here, not a unit test. `scalaVersion` comes from
`sys.props("scala.version")` and the plugin version from `sys.props("plugin.version")`, both injected by
`scriptedLaunchOpts`.

## Core architecture

The analysis is a single fold over symbols producing a `References` value, then a set difference.

**Entry point.** `FindUnused.Runner` (`core/.../FindUnused.scala`) initializes a tasty-query `Context` from the JDK
modules plus the supplied classpath, resolves the requested packages, builds an `Env`, and folds
`Symbols.references` over each package. Three runners differ only by a symbol predicate: `explicits` (non-given),
`givens`, `all`.

**`EnvR[A]`** (`Env.scala`) is the effect type: an opaque `ReaderT[Id, Env, A]`. `Env` carries the debug flag, root
directory, `isInPackages`, `symbolIsValid` — the combined package + explicit/given filter — `bytecodeCallers`, see
below, and `enclosing`, changed for a subtree with `local`, and `seen`, the mutable set of symbols already visited,
used for cycle-breaking: `Symbols.references` returns empty for any symbol `EnvR.markSeen` says it has already visited.
There is a `Monoid[EnvR[A]]` that runs its left side first, which is why almost all traversal is written as `|+|` and
`foldMap`. The traversal produces `Refs`, not `References`: combining
two only builds a tree, and `Refs.build`, the only code that merges results, flattens it into one `References` at the
end, since merging every field at every tree was a fifth of the run on a large codebase. Every `References` field
defaults to empty, so a partial result sets only what it knows, e.g. `References(usedFrom = ...)`. Code that has to
look inside a partial result (`removeUsed`, `addUsedProxy`, reading `used`) builds just that piece and wraps the result
in `Refs(...)` again.

**`References`** (`References.scala`) accumulates `defined`, `used`, and `usedProxy: Map[Sym, Set[Sym]]`.
`usedProxy` means "if the key is used, treat the values as used too" and is the mechanism behind most of the
special-casing: constructor params ↔ param accessors, overridden methods and their parameters, SAM lambda params ↔
the abstract method's params, and case classes vs. their synthetic members. `computeUnused` finds what's `reachable`
from what's referenced through the proxy map and subtracts it from `defined`. What's recorded as a definition is
exactly what `Symbols.isDefinition` accepts. `Sym` is a lightweight snapshot of a tasty-query `Symbol` (a unique id,
display name, formatted position) with a global memo cache.

**Attribution and transitive mode.** `Env.enclosing` is the stack of definitions around the tree being traversed
(pushed by `Trees.within`, only for symbols `Symbols.isDefinition` accepts, including around a class's declarations).
`References.used` records a reference in `usedFrom` under the innermost one, or in `used` when there's none;
`References.root` is for symbols that are used no matter what references them (annotations), and
`attributeBytecodeCallers` and `markPossiblySummonedInlineGivens` add their roots to `used` directly. `computeUnused`
counts every reference, as the tool always has. `computeTransitivelyUnused` (`--transitive`, meant for the `all` runner,
since the others only see one kind of code) marks from roots instead: `used`, `entryPoints` (classes in `src/test/` by
their TASTy source path, `main` methods and ones annotated `@main` or mainargs's `@main`, whose macro-generated callers
TASTy doesn't show, objects inheriting `main`), exclusions, then follows `usedFrom`, `usedProxy`, `owners` (a used
definition's enclosing definition is used, except for `ownerIndependent` ones: parameters, the fields they declare, and
definitions local to a method), `externalOverrides` (used when their owner is) and `internalOverrides` (used when their
owner and something they override are both used, so an implementation in a class that's never instantiated doesn't keep
the class alive).
`Symbols.allOverriddenSymbols` returns what a member overrides in every parent, not just the first match. It ignores
`importRefs` (imports outside any definition; one inside a definition, e.g. an `inline` method importing `given`s for
its expansion, is an ordinary reference), and only returns the outermost unused definitions.
`unusedWithoutExclusions` is what either mode would report with no exclusions, including what's nested in it, and the
CLI's `all` command lists the exclusions that match none of it. That's exact in default mode, where exclusions only
filter the result, and conservative in transitive mode, where they're also roots. In default mode,
`computeUnusedExcluding` also keeps the definitions enclosing excluded code, which transitive mode gets from `owners`.

**Traversal.** `Symbols.references` dispatches on symbol kind and recurses into declarations; `Trees.references`
exhaustively matches every tasty-query `Tree` case; `Types.symbols` walks a `Type` to the symbols it mentions.
`Annotations.checkForUnused` marks a symbol used when it carries `@unused` or a `@nowarn` whose `msg` filter mentions
"unused"/"never used", and records it in `unusedAnnotations` with the symbols the annotation covers, so the CLI's `all`
command can list the annotations `References.unnecessaryUnusedAnnotations` finds: those whose symbols are all
referenced, not counting the annotation's own roots, what overrides make used, references from inside what the
annotation covers (found through `owners`), or the references find-unused infers for `transparent inline` expansions and
quote type healing (`References.inferred`, recorded in `inferredRefs`), which the compiler may not count.
`isCheckedForNeed` leaves out `export` forwarders, which copy their annotations, and class parameters and their fields,
since the compiler doesn't count a class passing its own field to its constructor as a use. A `msg` filter starting
`unused value` is about discarded values, so it doesn't make anything used.

**Bytecode.** TASTy is pickled before the compiler's `inlining` phase, so implicit searches that happen while
expanding non-`transparent` inline methods and macros (`summonFrom`, `summonAll`, `Expr.summon`, `summonInline` inside
an inline method) leave no trace in TASTy — scala/scala3#22701 — and neither do macros that read default values or
refer to `object`s, like enumeratum's `findValues`. The
bytecode is generated afterwards and does contain those references. `Bytecode.references` (`Bytecode.scala`) reads
every class file under the analyzed packages, from both class directories and jars, with ASM, recording which member
names are referenced on which classes and each class's supertypes. It also keeps the class and method each reference
is made from, and the enclosing method of each anonymous or local class. `References.defined` records the callers
`Env.bytecodeCallers` returns (only for givens, default getters, and `object`s, through
`BytecodeReferences.moduleCallers`: reads of `MODULE$`, or of the accessor of one nested in a class), and
`References.attributeBytecodeCallers` turns
each caller into a reference from the definition it belongs to (dropping known compiler suffixes like
`$lzyINIT1$$anonfun$1`, following anonymous classes to their enclosing method, and falling back to the class for
constructors and lifted local methods like `helper$1`), or into a root when it can't. Things to keep in mind when
changing it:

- References the compiler generates for a member itself (static forwarders, which are static methods in the mirror
  class, bridges, `lazy val` initializers, getters, trait `foo$` implementation methods, an `object` reading its own
  `MODULE$`) are dropped by `isSelfReference`; otherwise every given looks used. So is the `ordinal` method of a sealed
  class's `Mirror.Sum` companion, which compares with each child `object`.
- The JVM records the receiver's static type as a reference's owner, so `BytecodeReferences.callers` matches a
  member against references on any subclass of its declaring class.
- A private member accessed from another class (e.g. an anonymous class generated by derivation) is compiled with an
  expanded name like `bl$Foo$$$bar`, so `callers` checks that form too.
- Field writes don't count, only reads and calls.
- The scan doesn't need tasty-query, so `FindUnused` starts it on its own thread before loading the classpath, and
  waits for it the first time a given, a default getter or an `object` is analyzed. Class files are read and parsed in
  parallel on at most 3 threads (and at most half the processors), alongside the analysis's own, to leave the user's
  machine responsive; more threads barely help. Jars with matching classes are opened through a `Using.Manager` so they
  stay open until all of their entries are read; others are closed right away. A package nested in another analyzed
  package is read once.
- Local `given`s compile to local variables, so the scan can't see them.
- It can't help with `inline given`s: their bodies are copied to each summoning site, and cross-file inlining leaves
  no line-number or source-map trace either. `InlineSummons` covers them conservatively instead: it walks the bodies
  of the inline methods the analyzed code calls (collected in `References.inlineCalls`) and the macro implementations
  they splice in (but not the quotes those build), collects the classes those bodies summon or pass as type arguments
  to inline methods, along with the type arguments the analyzed code passes to inline methods
  (`References.inlineTypeArgs`). `FindUnused.markPossiblySummonedInlineGivens` marks any `inline given` of one of those
  type classes, or of a subclass of one, used, and walks the bodies of those until no more are found. Universal
  classes like `Any` and `Object` don't count, and when there are too many methods to walk, every `inline given` is
  considered used. Libraries compiled with Scala 3.3 pickle splices and quotes as calls to
  `scala.quoted.runtime.Expr.splice`/`nestedSplice`/`quote` rather than `Splice`/`Quote` trees, so the walk handles
  both.
- The exact alternative is the compiler plugin (`compiler-plugin/`, `FindUnusedCompilerPlugin`), which runs after the
  `inlining` phase, where an `Inlined` tree's `call` is the `inline given` it expands, and `ctx.owner` the definition
  expanding it. It writes one record per top-level class, named after its TASTy file, using `Symbols.name`'s format for
  both. `InlineGivenRecords` only returns them when every analyzed top-level class with TASTy has one, since a class
  compiled without the plugin can expand any `inline given` (it skips a record in `<classes>-find-unused` when
  `<classes>` doesn't have the class's TASTy anymore, since the compiler never deletes records), and
  `FindUnused.markRecordedInlineGivens` then uses them
  in place of `InlineSummons`, attributing each expansion to the innermost analyzed definition around it. `Inlined`
  trees from `transparent inline` calls are trimmed before pickling, in memory too, so it can't help with those.

Before adding a TASTy workaround for a missing reference, check whether the reference exists in TASTy at all — the
`diagnose-false-positive` skill covers how.

**Quote type healing.** The same pickling boundary hides uses of `scala.quoted.Type` evidence in macro code: the
`staging` phase rewrites a method type parameter `A` mentioned in a quote, a splice's result type, a quote pattern, or
a `Type.of[...]` call (including the ones the compiler synthesizes for `Expr.summon[F[A]]` and `TypeRepr.of[F[A]]`) to
use the method's `Type[A]` parameter, seeing through local type aliases. `Trees.quotedTypeParamReferences` marks those
parameters used. It doesn't look inside splices, whose code runs at compile time and isn't healed, or into nested
quotes, which are handled on their own. It only handles method type parameters; a class-level `Type` context bound is
a field, which the bytecode shows being read.

**`transparent inline` expansions.** `transparent inline` calls are expanded before pickling, so TASTy only has the
expansion, wrapped in an `Inlined` tree whose `caller` is the top-level class that defines the method. The root of the
expansion keeps the position of a tree in the method's definition (its body, or the `${ ... }` splice for a macro), so
`Trees.transparentInlineReferences` marks used the `transparent inline` method anywhere in that class's tree (including
local classes) with a tree at exactly that position, and the classes and objects enclosing it, since the expansion can
drop the receiver. It compares offsets only, since the recorded source file is the call site's for macro expansions and
the outer method's for constants. A nested call that reduces to a constant keeps the inner method's offsets, possibly
from another file, so when the root is a `Literal`, `constantChain` also marks the methods in the class whose bodies are
calls (`rootCalls`) leading to a method with a tree there; it can't tell those apart, so it marks them all. Call-site
code inside an expansion is wrapped in `Inlined` trees with no `caller`.

**Exports.** One `export` selector can generate several symbols — exporting an implicit class generates an implicit
conversion (a given), a companion forwarder, and a type alias — so `References.fromSymbol` propagates use between an
export's same-named symbols for anything in the analyzed packages, even when the used symbol itself belongs to the
other analysis (explicits vs. givens). That's what `Env.isInPackages` is for.

**Suppression rules worth knowing before touching the traversal**, because each exists to prevent a false negative:
module vals are skipped, and so is a nested one's initializer, `new Inner$()` (they'd make every `object` look used),
but an `object` overrides what its module val does, e.g. an abstract `val` it implements; the compiler's `@Child`
annotations on a sealed class and its `Mirror.Sum` companion's synthetic `ordinal` (`Symbols.isSumMirrorOrdinal`) don't
count as uses of its children; a `ClassDef`'s own `Template` self-reference is removed; synthetic case-class members
don't count as definitions and don't mark their class used unless they're themselves used; primary constructors
(`Symbols.isPrimaryConstructor`; auxiliary ones are reported), the class the compiler generates for a `@main` method
(`Symbols.isMainWrapper`), wildcard/destructuring params, and refinement classes are skipped. Default getters
(`foo$default$2`, and `<init>$default$1` in the companion) *are* definitions, reported via `Symbols.displayName` as
`foo(bar = ...)` at the parameter, but not when their method or class is reported or not analyzed (`defaultValueOf`),
and not for `export` forwarders or local methods (`Symbols.isReportedDefaultGetter`); their own parameters and case
classes' synthetic `copy$default$N` aren't. A default is excluded along with its method
(`References.excludingDefaultsOf`), and the CLI splits an exclusion filter on its first `=` only, since these names
contain one.

**tasty-query workarounds.** `Symbols.scala` contains several deliberate fallbacks, each documented in place:
`getFromIdent`/`getFromSelect` catch throwing `.symbol` calls and re-resolve by name through the class linearization;
`getSelectOwner` reads a private field by Java reflection; `nextOverriddenSymbol` falls back to matching an override by
its parameters' erased types (its signature, as the compiler does) because tasty-query's version misses some cases, like
parameters written with an `export`ed alias of a generic class; comparing the types themselves with `matches` accepts
overloads, since it's true for any two value types. `signatureWithoutErasedParams` strips `erased` params so call sites
match definitions. `core/src/main/scala/tastyquery/SymbolsOps.scala` lives in the `tastyquery` package specifically to
reach package-private API (flags, signatures, a type's class). `.claude/skills/diagnose-false-positive/tasty-query.md`
records what causes each workaround and the upstream fix that would remove it. Prefer adding a narrow, commented
workaround in this style over changing the general traversal.

## Plugin architecture

`FindUnusedPlugin` never runs the analysis in-process. It collects classpaths, then forks a JVM running
`bl.unused.FindUnusedCli` with `--package`/`--classpath`/`--exclusion` arguments.

Classpath collection is the subtle part. When no project cross-builds, `findUnusedProjectClasspathTask` just joins
every project's `findUnusedFullTestClasspath`. When one does, `findUnusedStoreAllProjectClasspaths`, a *command* (not
a task), iterates every `crossScalaVersions` of every project, issuing `++<version>` switches and accumulating each
project's `Test / fullClasspath` into session state under `findUnusedClasspathKey`, and the task drives it with
`runCommand` and reads the resulting attribute. `++` reapplies every project's settings, which races tasks running
alongside, which is why it's avoided when possible (the `no-cross-switch` test checks that). This is also why the tool
currently requires one Scala version across the build.

The CLI comes either from `BuildInfo.cliClasspath` (when `findUnusedUseLocalClasspath := true`, used by scripted
tests) or from a coursier fetch of `bondlink:find-unused-cli_3:<BuildInfo.version>`. `findUnusedCompilerPlugin` adds the
compiler plugin to every Scala 3 project the same way: `-Xplugin:` with `BuildInfo.compilerPluginJar`, or a
`compilerPlugin` dependency, whose `-Xplugin:` sbt adds even when a build replaces `scalacOptions`. So the compiler
plugin writes next to the class directory, `<classes>-find-unused`, unless given `-P:find-unused:out:<dir>`, and
`findUnusedInlineGivenRecords` computes the same directories to pass to the CLI as `--inline-given-records`. They're
outside the class directories, since sbt 2 puts jars on the classpath (`exportJars`) and the records shouldn't be
published.

`plugin/src/main/scala/` must compile under **both** Scala 2.12/sbt 1 and Scala 3/sbt 2 — keep it Scala-2-compatible
syntax. Version-specific pieces go in `FindUnusedPluginCompat` under `plugin/src/main/scala-2/` and
`plugin/src/main/scala-3/` (currently the `cacheLevel` annotation, `coursierDependency`, and `fileConverter`-based
classpath extraction).

The `*DebugConfig` tasks emit a VS Code `launch.json` for debugging the CLI against a real project; see the
Debugging section of `README.md`.

## Conventions

- Scala 3 style throughout `core`/`cli`: `given`/`using`, `.*` imports, cats syntax imports listed individually
  (`cats.syntax.foldable.*`), heavy use of `foldMap`/`|+|` rather than explicit recursion where possible.
- Profile before optimizing: run the CLI with `-XX:StartFlightRecording=filename=run.jfr,settings=profile` and read
  `jfr print --events jdk.ExecutionSample` (the `benchmark` skill has a script that totals the samples). A lookup that
  scans a whole collection per symbol, like `moduleCallers` once did, shows up as most of the run on a large codebase.
- Never use `hashCode` to identify a symbol or type. tasty-query doesn't override `hashCode` or `equals`, so these are
  identity hash codes, which collide: analyzing a large codebase visits ~200k symbols, and a collision made the
  traversal silently skip a symbol. Key collections by the objects themselves, or by `Sym.id`.
- sbt-tpolecat is enabled, so warnings are errors in CI. Unused imports and non-exhaustive matches will fail the build.
- `Trees.references` is an exhaustive match over all tasty-query trees; a tasty-query upgrade that adds a tree type
  surfaces there first.
- Releases are cut from `v*` git tags (CI builds the jar, attests it, and drafts a GitHub release). `ThisBuild /
  version` in `build.sbt` is the snapshot version; artifacts publish to the `bondlink-maven-repo` S3 bucket.
