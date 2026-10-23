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

Work in a worktree of the project, or in an export of it, not in someone's working tree. To export, run
`git archive <ref> | tar -x -C <scratch dir>`, then `git init` and commit there if the build reads git state. An
export lacks what the project generates outside sbt, e.g. assets a Node build writes for templates, so generate those
the way the project's own docs or skills say before compiling. Point `project/plugins.sbt` at the snapshot version.
The plugin resolves the CLI from local Ivy (among other repositories), so nothing else is needed. Add a
local-only `.sbt` file (named so it sorts after `build.sbt`, e.g. `zz-find-unused-local.sbt`) and don't commit it:

```scala
Global / findUnusedOutputFile := Some(file("/absolute/scratch/all.txt"))
Global / findUnusedExclusions := Seq() // see everything; replay the real exclusions offline (step 2)
```

Don't run `publishLocal` while scripted tests run in another checkout of find-unused: scripted publishes the plugin
locally under the same snapshot version and its tests resolve it from there, so the two overwrite each other and a
test fails for no reason (or the project picks up another layer's build).

On an sbt 2 project, a plain `sbt <command>` hands it to a background server for that directory, which keeps the
build it loaded and can miss an edit to `project/plugins.sbt` or a new `.sbt` file, so results silently come from the
wrong find-unused. Run each step in its own process with `sbt --server -batch "<a>; <b>"`, passing several commands as
one `;`-separated argument (sbt 2 joins separate arguments into one command, so `set ... findUnusedAll` breaks), and
stop a server you left running in the project (`project/target/active.json` names its socket). Settings only the
snapshot has, like `Global / findUnusedTransitive`, go in the local `.sbt` file rather than `set`; if the build loads,
the snapshot is the plugin it's using.

With `findUnusedOutputFile` set, the CLI's `[find-unused] ...` progress lines are lost (the output file replaces
them), so run once with `Global / findUnusedOutputFile := None` when you need them, e.g. to see whether the compiler
plugin's records were used or find-unused fell back to checking `inline given`s conservatively.

To try the compiler plugin, add `Global / findUnusedCompilerPlugin := true` and recompile everything (`clean`) first;
the records are only used when every analyzed class has one. Compare its results with a run without it: anything it
newly reports is an `inline given` the conservative check kept, which still has to be proven unused.

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
how many findings it matches; an exclusion that matches nothing is stale or no longer needed. `findUnusedAll` also lists
the exclusions that don't exclude anything after its results, which is the quicker check. Run with `--args` instead of
a file, it prints each exclusion as the CLI's `--exclusion` argument, for `compare_builds.sh`.

In transitive mode the replay overcounts what's left unexcluded: an exclusion also keeps what the excluded code uses,
which a filter can't reproduce, so code that only excluded code uses (e.g. the overrides of a member that's only used
from outside the analysis) shows up as matching no exclusion. Only a run with the real exclusions tells you what's
reported.

To compare before and after a find-unused change on the same compiled project, run both builds of the CLI on it with
`compare_builds.sh`. It diffs their output in every mode (default and transitive, `all`, `givens` and `explicits`, with
and without the exclusions) without `publishLocal` or sbt, and takes a minute or two on a large project, running three
analyses at once with 8 GB each (set `JOBS` and `HEAP` to change that). Build each side with `sbt cli3/assembly` and
copy `cli/target/jvm-3/find-unused-cli-assembly-*.jar` away before switching commits. Its inputs are files in one
directory:

- `classpath.txt`, from `classpath.py <build root>` (sbt 1 or 2, after the project's tests are compiled), adding
  `--scala <version>` if it says the build has output for more than one Scala version
- `packages.txt`, with `findUnusedPackages`
- `exclusions.txt`, from the replay script's `--args`
- `records.txt`, with the `<classes>-find-unused` directories, if the project was compiled with the compiler plugin

Its output is the plugin's, but with positions relative to the build root. Check that nothing is
*newly* reported, and explain every finding that disappears. When a change is meant to report a
new kind of code, the reverse applies: prove every new finding is unused (step 3), and treat each one that isn't as a
bug to fix before going on. Rerun after each fix and diff against the previous run, so a fix that removes more than
the false positives it targets shows up.

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

`test_scripts.py` checks `classpath.py`, `remove_defaults.py`, `table2tsv.py` and `pr-stack`'s `resolve_conflicts.py` on
small inputs, and CI runs it; add a case to it when fixing one. `sbt test` compiles the Scala scripts here and in
`benchmark` against `core` (the `skills3` project), with a stub for the `exclusions` that `ExclusionsPrelude.scala`
expects.

## Known gaps found this way

- **`inline given`s resolved by an implicit search after TASTy is written** (`summonFrom`/`summonInline` inside
  library derivation) leave no trace in TASTy or bytecode. For example, circe's `derives ConfiguredDecoder` can
  resolve the decoder for an opaque type's field through an `inline given` that just returns the underlying type's
  decoder. `InlineSummons` treats such an `inline given` as used if its type class is summoned by an inline method
  the code calls, which trades this false positive for not reporting unused `inline given`s of those type classes.
  Enabling the compiler plugin (`findUnusedCompilerPlugin := true`, then recompiling everything) makes it exact;
  if find-unused says a class wasn't compiled with it, it's still checking conservatively.
- **Members found by name in macros** (e.g. `TypeRepr.typeSymbol.typeMember("Out")`) aren't referenced in TASTy or
  bytecode.
- **Wildcard exports** (`export cats.syntax.parallel.*`) report every unused forwarder; that's correct, but the fix
  is `name as _` selectors or an exclusion.
- **Removing `@nowarn`s the compiler's `-Wunused:nowarn` lists.** It's mostly right, but it has listed a `@nowarn` on a
  class's `using` parameter that the compiler warns about once it's gone, and removing some `@nowarn`s can make it list
  more that they hid. Remove what it lists, recompile without `-Werror` to collect both kinds of warning, put back the
  ones whose removal exposed one, and repeat until nothing changes. Then run find-unused: the compiler doesn't warn
  about every class parameter find-unused reports, so some of those need `@unused` instead, which `-Wunused:nowarn`
  doesn't check. find-unused's own list of annotations that aren't needed is narrower; it skips class parameters.
