---
name: validate-on-project
description: Validate a find-unused change against a real Scala 3 project, and safely remove the unused code it reports. Use after changing the analysis, before trusting a drop in findings, when a project wants to remove its find-unused exclusions, or when deleting code find-unused reports.
---

# Validating find-unused on a real project

Scripted tests prove a pattern works; a real codebase shows what's still wrong. A finding that goes away can mean the
analysis got better *or* that it now hides real dead code, so every change in output needs evidence.

## 1. Run an unreleased find-unused in the project

```bash
sbt publishLocal                     # here: publishes core, cli and both plugins as the current -SNAPSHOT
```

In the project (a worktree of it, not someone's working tree), point `project/plugins.sbt` at the snapshot version.
The plugin resolves the CLI from local Ivy (among other repositories), so nothing else is needed. Add a
local-only `.sbt` file (named so it sorts after `build.sbt`, e.g. `zz-find-unused-local.sbt`) and don't commit it:

```scala
Global / findUnusedOutputFile := Some(file("/absolute/scratch/all.txt"))
Global / findUnusedExclusions := Seq() // see everything; replay the real exclusions offline (step 2)
```

Write each run to its own file (`all.txt`, `all-transitive.txt`). Run the task the project's CI runs, and then with
`Global / findUnusedTransitive := true` too: transitive mode finds
dead code that references itself or is only used by other dead code, which the default mode can't. `findUnusedAll`
validates givens and explicits together, so it reports different things than `findUnusedGivens` and
`findUnusedExplicits` run separately. Convert the rendered table:

```bash
python3 .claude/skills/validate-on-project/table2tsv.py all.txt > all.tsv   # name<TAB>location, one per line
```

## 2. Replay the project's exclusions

`ExclusionsPrelude.scala` reimplements the plugin's `FindUnusedExclusion` factories and the CLI's matching rules.
Build a script from it plus the project's exclusion block copied verbatim from `build.sbt` (rename
`Global / findUnusedExclusions ++= Seq(` to `val exclusions: Seq[FindUnusedExclusion] = Seq(`, and include any helper
it uses), then `scala-cli run` it with `all.tsv`. For each exclusion it prints
how many findings it matches; an exclusion that matches nothing is stale or no longer needed.

To compare before and after a find-unused change on the same compiled project, run the analysis in-process with the
`benchmark` skill's harness against both builds of `core` and diff the lists. That harness only runs the givens
analysis without `--transitive`; for anything else, publish each build of find-unused locally in turn and diff the
plugin's output files. Check that nothing is *newly* reported, and explain every finding that disappears.

## 3. Before removing anything find-unused reports, prove it's unused

Never delete on find-unused's word alone. For each finding:

1. **Look for references in the compiled code.** Search every class file in the project for references to the
   member by name, including the expanded form of private names (`pkg$Owner$$$name`). References from the
   member's own forwarders and initializers (`Owner.name` calling `Owner$.name`, `name$`, `name$lzyINIT1`) aren't
   uses.
2. **Delete it and compile everything** (`Test/compile` for all projects). If compilation fails, it's used: restore
   it and treat it as a find-unused bug (see `diagnose-false-positive`).
3. **Check behavior didn't change.** A given that's never selected can still affect implicit search: removing it can
   unshadow another given, flip a `NotGiven`, or change a `summonFrom` branch. Compiling doesn't catch that, but the
   bytecode does, because implicit resolution is compiled into every use site. Before editing, clone the project's
   class directories (`cp -c -R` clones them on macOS; use `cp -R` elsewhere); after recompiling, copy
   `CompareBytecode.scala` to a scratch directory (scala-cli writes `.scala-build/` next to it) and run it on the two
   copies. Changes should
   only appear in classes compiled from the files you edited (removed members, `<clinit>` VarHandle setup for removed
   lazy vals, `$deserializeLambda$`). The script already ignores debug info and compiler-generated numbering
   (`$anonfun$10`, `$lzy3`, `$$anon$4`). Any change in another file means resolution changed somewhere: restore and
   investigate — but first rule out nondeterministic compilation. Some macros produce different bytecode on every
   compile (e.g. by iterating over an unordered collection of symbols). Force just the suspect files to recompile
   without a semantic change (append a comment, which changes the source hash but not the API), and compare against
   the build from before: if they differ too, that file's differences are noise. Another harmless one: removing a
   trait's last field can make the compiler skip calls to that trait's `$init$` in classes that mix it in, and the
   trait's compiled `$init$` may just `return`. Check the target's body before treating it as a behavior change.

   Incremental compilation adds noise of its own: what a file compiles to can depend on which other files were
   compiled in the same run, especially when it expands `inline` methods from them. Seen so far: dead stores of
   `GETSTATIC Foo$.MODULE$` into locals (inline proxies) appearing or disappearing, `List(a, b)` compiled as a `::`
   chain or as `List.apply`, and calls to empty trait `$init$` methods. When changes like these show up in files
   you didn't edit, compare two *clean* builds instead (`sbt clean Test/compile` with the code before the edit,
   snapshot, then the same after it), which compile every module's files together.
4. **Intentionally unreferenced code stays, annotated.** For example, a pair of identical givens can exist only to
   make implicit search ambiguous, so that summoning an unsupported instance fails to compile. Deleting them
   compiles but silently changes behavior. Mark them with `@unused` or
   `@nowarn("msg=unused")` (find-unused honors both) with a comment, rather than adding an exclusion.

Removing code can leave unused imports, which `-Werror` builds reject; clean those up and recompile.

A default value reported as unused (`Foo.bar(baz = ...)`, located at the parameter) is removed by deleting just its `
= ...`; `python3 .claude/skills/validate-on-project/remove_defaults.py all.tsv` prints each removal, and `--apply` makes
them once you've reviewed the list. Use the output of a run with the project's exclusions and without `--transitive`:
without the exclusions it would strip defaults they keep on purpose, e.g. ones a macro reads, and in transitive mode, a
default that only unused code relies on is reported too. Every caller already passes the argument, so the compile and bytecode checks above apply unchanged: only
the default getters (`bar$default$2`, `$lessinit$greater$default$1`) and their trait forwarders in implementing
classes should disappear.

`test_scripts.py` checks `remove_defaults.py` and `table2tsv.py` on small inputs, and CI runs it; add a case to it when
fixing either. `sbt test` compiles the Scala scripts here and in `benchmark` against `core` (the `skills3` project),
with a stub for the `exclusions` that `ExclusionsPrelude.scala` expects.

## Known gaps found this way

- **`inline given`s resolved by an implicit search after TASTy is written** (`summonFrom`/`summonInline` inside
  library derivation) leave no trace in TASTy or bytecode. For example, circe's `derives ConfiguredDecoder` can
  resolve the decoder for an opaque type's field through an `inline given` that just returns the underlying type's
  decoder. `InlineSummons` treats such an `inline given` as used if its type class is summoned by an inline method
  the code calls, which trades this false positive for not reporting unused `inline given`s of those type classes.
  The exact fix would need a compiler plugin running after the `inlining` phase, where `Inlined` trees still carry
  the full call.
- **Members found by name in macros** (e.g. `TypeRepr.typeSymbol.typeMember("Out")`) aren't referenced in TASTy or
  bytecode.
- **Wildcard exports** (`export cats.syntax.parallel.*`) report every unused forwarder; that's correct, but the fix
  is `name as _` selectors or an exclusion.
