# tasty-query gaps behind find-unused's workarounds

What was measured on tasty-query 1.10.0 (October 2026) by counting how often each workaround in `Symbols.scala`
fires, on the Scala 3 compiler and on a large private application, and reproducing each cause in a few lines. Before
relying on any of this, check the tasty-query version in `build.sbt` and whether a fix below has been released: each
one lets find-unused delete code.

## Symbol resolution (`getFromIdent`, `getFromSelect`, `getSelectOwner`, `signatureWithoutErasedParams`)

`Ident#symbol` and `Select#symbol` threw `MemberNotFoundException` thousands of times on the application and once on
the compiler. tasty-query matches the signed name the compiler pickled against each candidate's *declared* signature
only (`ClassSymbol.getDecl`/`findMember`), and the compiler pickles something else in these cases:

- **A `TERMREF` carries the signature as seen from its prefix.** An unqualified call from an `object` to an inherited
  generic method, like twirl's `_display_(x: T)` in a template object, or a given a companion inherits from a generic
  parent, is pickled with the instantiated types, e.g. `(java.lang.String)` for `(x: T)` with `T = String`. Calls from a
  class are `SELECTin` with the declared signature and resolve. Fix: in `findMember`, when the exact lookup fails, compare
  `Signature.fromType(decl.typeAsSeenFrom(prefix))` too.
- **`erased` parameters aren't in the compiler's signatures,** including parameters whose type extends
  `compiletime.Erased`, but `TermSymbol.signature` keeps them. Fix: mark their types with `@ErasedParam` in method
  types, as the compiler does, and leave such parameters out in `Signature.fromType`; dropping them only from
  `TermSymbol.signature`, as `signatureWithoutErasedParams` does here, makes method signatures disagree with refinement
  signatures. That also gives tasty-query a public `isErased`, replacing the `flags` reflection in `SymbolsOps`.
- **A `Unit` result reached through an opaque alias (outside its scope), an abstract type member, a type parameter or
  a singleton type erases to `BoxedUnit`,** but tasty-query keeps it as `Unit`. The compiler only keeps `Unit` through
  aliases (`isRef(defn.UnitClass)`), judged by the reference's info as seen from its prefix, so a refinement's
  `type Elem = Unit` or an exported alias still counts. Fix: in `Erasure`, keep `keepUnit` only through a reference
  whose `optAliasedType` is defined, which is computed from its bounds as seen from its prefix and so includes an
  opaque alias inside its scope.
- **A Java annotation's constructor is pickled without a signature** (scala/scala3#19951), and `getDecl` refuses an
  unsigned name for a method. Resolving it to tasty-query's only constructor isn't a fix: that's a synthesized `<init>()`
  applied to one argument per element, and the annotation tree's type becomes `Unit`. It needs an annotation
  constructor with one parameter per element, as the compiler synthesizes (`addAnnotationConstructor`). It's rare: once
  on the Scala 3 compiler.
- **Match types:** reducing one can throw an `AssertionError` from `provablyDisjoint` (a class `TypeRef` against a union
  or intersection), a member of a value typed with a match type is looked up on its bound instead of its reduction, and
  `compiletime.ops.int.S[n]` patterns and a `TupleN` scrutinee against a `*:` pattern don't reduce at all (named-tuple
  field selections, HList `Drop[L, 2]`). The first two are small fixes, along with the compiler's rule that `Nothing` is
  disjoint from everything; the last needs pattern-matching rules tasty-query doesn't have.

With the first three fixed, `getFromIdent`'s fallback never fired on either codebase; `getFromSelect`'s fallbacks are
still needed for the match types that don't reduce.

## Override matching (`nextOverriddenSymbol`, `allOverriddenSymbols`)

tasty-query's `nextOverriddenSymbol` misses an override whose parameter is written with an `export`ed alias of a
generic class (`export Lib.Box`; `def f(b: predef.Box[A])` overriding `def f(b: Lib.Box[A])`): it reads the exported
alias as `AbstractType(TypeAlias(...))` instead of `TypeAlias(...)` (`TreeUnpickler.makeTypeMemberDefinition`), so
`dealias` and `=:=` don't see through it. A few overrides through match types are missed too. Once the export case is
fixed, the fallback in `nextOverriddenSymbol` and the chain in `allOverriddenSymbols` add nothing to tasty-query's
`allOverriddenSymbols` on either codebase, apart from the match-type cases.

Don't compare parameter types with `Type.matches` to tell overrides apart: it's true for any two value types (it's the
member-matching relation, where a `val` overrides a `val` of any type), so it takes overloads for overrides. Compare the
erased parameter types as seen from the subclass, which is what the compiler's signatures do. tasty-query erases a
match type it can't reduce to its bound, which may not be what the compiler erases it to, so such a parameter can't
tell an override from an overload either way; the fallback lets it match anything.

## Package-private API (`SymbolsOps`)

- `ClassSymbol.isRefinementClass`: the public equivalent is `sym.name == tpnme.RefinedClassMagic`, which is what
  tasty-query derives it from.
- `Type.classSymbol` (`instanceClass`): can be rebuilt from public API, but making it public upstream is a one-line,
  binary-compatible change.
- `Signature.fromType` (`erasedParamsAsSeenFrom`) and `Symbol.flags` (`isErased`): no public equivalent.
- `Select`'s `selectOwner`, read by reflection in `getSelectOwner`: no public equivalent; only needed for the match
  types tasty-query can't reduce once the fixes above are in.

## Expected, not bugs

- `TypeTree#toType` throws `Unexpected non-type PackageRef(...)` for the qualifier of a package-qualified type, e.g.
  `java.lang.StringBuilder`, whose prefix is a package. Match on `toPrefix` instead of catching it if it matters.
- `Member foo not found in PackageRef()` when a library's inline code refers to an optional dependency that isn't on
  the classpath.
