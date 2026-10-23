---
name: diagnose-false-positive
description: Work out why find-unused reports something as unused when it is used (or the reverse), and where a fix belongs - the TASTy traversal, the bytecode scan, or the README's known issues. Use when a user reports a wrong finding, when an exclusion exists only to hide a false positive, or before adding a tasty-query workaround.
---

# Diagnosing a wrong finding

find-unused only knows about a reference if it appears in TASTy (walked by `Trees`/`Symbols`/`Types`) or, for `given`s
and default values, in bytecode (`Bytecode.scala`). So the first question is always: where does the reference actually
exist?

## 1. Reproduce it small

Write the smallest program that shows the problem, in a scratch directory outside the repo. If the use goes through a
macro, put the macro in a separate file (or project), since it has to be compiled before the code that expands it.

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
- The reference is there but the symbol is something else: the bytecode scan is deliberately limited to givens and
  default getters (see `FindUnused`), because compiler-generated code such as case-class `equals`/`_1` reads every
  field. Widening it needs a way to tell those apart.
- The reference is in neither: first check the workarounds for what the compiler expands before or after pickling,
  `Trees.transparentInlineReferences` for `transparent inline` methods and `InlineSummons` for `inline given`s. What's
  left (a nested `transparent inline` call that reduces to a constant, an `inline given` of a type class nothing
  summons where TASTy shows it) has nothing to recover, short of reimplementing the compiler's step. Document it under
  "Known issues" in `README.md`, pin the current behavior in a scripted test, and suggest an exclusion.

## Debug output

Set `Global / findUnusedDebug := true` in the build being analyzed (or pass `--debug` to the CLI) to print every
symbol and tree visited, plus `used in bytecode: <name>` for each given or default getter the bytecode scan found. The
`findUnused*DebugConfig` tasks generate a VS Code launch config for stepping through the CLI; see "Debugging" in
`README.md`.

## Fixing it

Add a scripted test (see the `scripted-test` skill) that fails before the fix, with a control case that must still be
reported. Prefer a narrow, commented workaround in the style of the existing ones in `Symbols.scala` over changing the
general traversal, and run `sbt 'test; scripted'`.
