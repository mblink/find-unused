---
name: diagnose-false-positive
description: Work out why find-unused reports something as unused when it is used (or the reverse), and where a fix belongs - the TASTy traversal, the bytecode scan, or the README's known issues. Use when a user reports a wrong finding, when an exclusion exists only to hide a false positive, or before adding a tasty-query workaround.
---

# Diagnosing a wrong finding

find-unused only knows about a reference if it appears in TASTy (walked by `Trees`/`Symbols`/`Types`) or, for `given`s,
default values and `object`s, in bytecode (`Bytecode.scala`), or for `inline given`s, in the compiler plugin's records.
So the first question is always: where does the reference actually exist? The opposite question matters just as much
for a missed finding: what in TASTy or bytecode counts as a use that isn't one? Compiler-generated code is the usual
culprit, like the `@Child` annotations on a sealed class and its `Mirror.Sum` companion's `ordinal`, which made every
child of a sealed class look used.

## 1. Reproduce it small

Write the smallest program that shows the problem, in a scratch directory outside the repo. If the use goes through a
macro, put the macro in a separate file (or project), since it has to be compiled before the code that expands it.

Run the locally built CLI on it directly, which is much faster than a scripted test while iterating. After
`sbt cli3/compile`, `sbt 'export cli3/Runtime/fullClasspath'` prints the classpath to run it with:

```bash
lib=$(cs fetch --classpath org.scala-lang:scala3-library_3:3.10.0)
cs launch -q scala3-compiler:3.10.0 -M dotty.tools.dotc.Main -- -classpath "$lib" -d out $(find src -name '*.scala')
java -cp "$cli_cp" bl.unused.FindUnusedCli all -p <pkg> -c "$PWD/out" $(tr ':' '\n' <<<"$lib" | sed 's/^/-c /') --no-color
```

Compile from inside the scratch directory with relative paths, so positions in the output stay short. To check which
part of a fix matters, disable that part, recompile, and rerun: a case that's still right without it doesn't test it.

## 2. Compare what the compiler pickles with what it generates

TASTy is written at the end of the `pickler` phase. Anything the compiler does later is invisible in TASTy: expanding
non-`transparent` inline methods and macros (`inlining`), healing types in quotes (`staging`), and the implicit
searches those trigger. Print the trees on both sides of that boundary:

```bash
scala-cli compile --server=false -S 3.10.0 src -d out -O -color:never -O -Vprint:pickler,inlining,staging
```

- The reference is in the `pickler` output: it's in TASTy, so the bug is in the traversal. Find the tree or symbol case
  that drops it in `Trees.references`/`Symbols.references`, using the debug output below.
- The reference only shows up after `pickler`: TASTy can't see it. Check the bytecode next.

## 3. Check the bytecode

```bash
javap -c -p 'out/<pkg>/<Class>$.class' | grep -E "Method|Field|InvokeDynamic"
```

Look for `invokevirtual`/`invokestatic`/`getstatic` on the member, e.g. `Method bl/unused/Foo$.derived$Decoder`.
Remember that the static forwarder in the companion/mirror class (`Foo.derived$Decoder` calling
`Foo$.derived$Decoder`) is the compiler's own reference, not a use.

- The reference is there and the symbol is a `given` or a default getter: the bytecode scan should already catch it.
  Check that `Bytecode.memberKey` computes the right owner class and member name for the symbol, and that
  `isSelfReference` isn't dropping it.
- The reference is there but the symbol is something else: the bytecode scan is deliberately limited to givens,
  default getters and `object`s (see `FindUnused`), because compiler-generated code such as case-class `equals`/`_1`
  reads every field. Widening it needs a way to tell those apart, like `isSelfReference` and the `Mirror.Sum` `ordinal`
  check do for objects.
- The reference is in neither: first check the workarounds for what the compiler expands before or after pickling,
  `Trees.transparentInlineReferences` for `transparent inline` methods, and `InlineSummons` or the compiler plugin for
  `inline given`s. To see what a phase after `inlining` sees, a throwaway compiler plugin with a `transformInlined` that
  prints `tree.call` and `ctx.owner` takes a few minutes (that's how the compiler plugin's approach was checked); note
  that `transparent inline` calls are trimmed to their top-level class before pickling, in memory too. What's left
  has nothing to recover, short of reimplementing the compiler's step. Document it under
  "Known issues" in `README.md`, pin the current behavior in a scripted test, and suggest an exclusion.

## Debug output

Set `Global / findUnusedDebug := true` in the build being analyzed (or pass `--debug` to the CLI) to print every
symbol and tree visited, plus `used in bytecode: <name>` for each given, default getter or object the bytecode scan
found. To find what references a symbol, search that output for the symbol's `TermRef`/`TypeRef` and read upwards to
the enclosing `*********** ... Symbol:` line. Libraries can pickle differently from the code being analyzed: those
compiled with Scala 3.3 have `scala.quoted.runtime.Expr.splice` calls instead of `Splice` trees. The
`findUnused*DebugConfig` tasks generate a VS Code launch config for stepping through the CLI; see "Debugging" in
`README.md`.

## Fixing it

Add a scripted test (see the `scripted-test` skill) that fails before the fix, with a control case that must still be
reported. Prefer a narrow, commented workaround in the style of the existing ones in `Symbols.scala` over changing the
general traversal, and run `sbt 'test; scripted'`.
