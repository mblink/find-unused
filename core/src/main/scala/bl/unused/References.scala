package bl.unused

import cats.Monoid
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import scala.collection.mutable
import scala.util.chaining.*
import tastyquery.Contexts.Context
import tastyquery.Symbols.*
import tastyquery.Types.Type

sealed case class Sym private (id: Int, name: String, pos: Option[String])
object Sym {
  // A unique id per symbol. `Symbol` doesn't override `hashCode`, so different symbols can share an identity hash code
  private val ids = mutable.Map.empty[Symbol, Int]
  def id(sym: Symbol): Int = ids.synchronized(ids.getOrElseUpdate(sym, ids.size))

  private val cache = mutable.Map.empty[Symbol, Sym]

  private def cached(sym: Symbol)(compute: => EnvR[Sym]): EnvR[Sym] =
    cache.synchronized(cache.get(sym).fold(compute.map(s => cache.put(sym, s).pipe(_ => s)))(_.pure[EnvR]))

  def fromSymbol(sym: Symbol)(using ctx: Context): EnvR[Sym] =
    cached(sym)(EnvR.noSeenSymbols(env => new Sym(
      id = id(sym),
      name = Symbols.displayName(sym),
      // A default value is reported at its parameter, rather than in the middle of its expression
      pos = Symbols.defaultGetterParam(sym).flatMap(_._2.tree).orElse(sym.tree)
        .map(t => Positions.format(env.rootDirectory, t.pos)),
    )))
}

case class References(
  defined: Set[Sym],
  used: Set[Sym],
  // If the symbol represented by the map key is used, also mark the symbols represented by the map value as used
  usedProxy: Map[Sym, Set[Sym]],
  // The non-`transparent` inline methods the analyzed code calls, and the `inline given`s it defines -- see `InlineSummons`
  inlineCalls: Set[TermSymbol] = Set.empty,
  inlineGivens: Map[Sym, TermSymbol] = Map.empty,
  // The type arguments the analyzed code passes to non-`transparent` inline methods, which can pass them on to a search
  inlineTypeArgs: Set[Type] = Set.empty,
  // References made inside a definition, keyed by that definition. `used` holds references made outside any
  // definition, and symbols that are used no matter what references them.
  usedFrom: Map[Sym, Set[Sym]] = Map.empty,
  // Each definition's enclosing definition, which is used if the definition is
  owners: Map[Sym, Sym] = Map.empty,
  // Definitions whose use doesn't make their enclosing definition used: parameters and the fields they declare, which
  // are part of their method's or class's signature, and definitions local to a method, which only it can use
  ownerIndependent: Set[Sym] = Set.empty,
  // Definitions that are used without being referenced, like `main` methods and test classes
  entryPoints: Set[Sym] = Set.empty,
  // Definitions that override something outside the analyzed code, so they can be called by it
  externalOverrides: Set[Sym] = Set.empty,
  // Definitions that override something in the analyzed code, mapped to what they override
  internalOverrides: Map[Sym, Set[Sym]] = Map.empty,
  // Default getters, mapped to their method, or their class for a constructor's. A default isn't reported separately
  // when its method is.
  defaultValueOf: Map[Sym, Sym] = Map.empty,
  // Symbols referenced by `import`s outside any definition, which don't use anything themselves
  importRefs: Set[Sym] = Set.empty,
  // `given`s referenced in bytecode, with the classes and methods that reference them, and the JVM names of
  // definitions, which `attributeBytecodeCallers` uses to turn those references into references from definitions
  bytecodeCallers: Map[Sym, Set[Bytecode.Caller]] = Map.empty,
  jvmMembers: Map[(String, String), Set[Sym]] = Map.empty,
  jvmClasses: Map[String, Sym] = Map.empty,
) {
  final def removeUsed(sym: Symbol): References = {
    val c = Sym.id(sym)
    copy(used = used.filterNot(_.id === c), usedFrom = usedFrom.view.mapValues(_.filterNot(_.id === c)).toMap)
  }

  final def addUsedProxy(m: Map[Symbol, Set[Symbol]])(using ctx: Context): EnvR[References] =
    m.toList.filter(_._2.nonEmpty).foldMapM((s, ss) =>
      (Sym.fromSymbol(s), ss.toList.traverse(Sym.fromSymbol)).mapN((s, ss) => Map(s -> ss.toSet))
    ).map(m => copy(usedProxy = usedProxy |+| m))

  final def addUsedProxy(ifUsed: Symbol, thenUsed: Set[Symbol])(using ctx: Context): EnvR[References] =
    addUsedProxy(Map(ifUsed -> thenUsed))

  /**
   * Attributes the bytecode references in `bytecodeCallers` to the definitions that make them, so they only count in
   * transitive mode if those definitions are used, e.g. the derived instance of a type that's only used by the derived
   * instance of an unused type is unused too.
   *
   * A reference from a method is attributed to the member it belongs to, found by dropping the suffixes the compiler
   * adds to the names of the methods it generates (`foo$lzyINIT1$$anonfun$1` is part of `foo`). One from an anonymous or
   * local class is attributed to the method that defines it (`outers`), and one from anywhere else in a class, like its
   * constructor or a local method lifted out of the method that defines it (`helper$1`), to the class. A reference
   * that can't be attributed counts no matter what.
   */
  final def attributeBytecodeCallers(outers: Map[String, (String, Option[String])]): References = {
    def candidates(name: String): List[String] =
      References.generatedSuffix.findFirstMatchIn(name) match {
        case Some(m) => name :: candidates(name.take(m.start))
        case None if name.isEmpty || References.liftedLocal.findFirstIn(name).nonEmpty => Nil
        case None => List(name)
      }

    def resolve(cls: String, method: Option[String], depth: Int): Set[Sym] = {
      val fromMethod = method.filterNot(m => m == "<init>" || m == "<clinit>").fold(Set.empty[Sym]) { m =>
        // A private member's expanded name, `bl$Foo$$$bar`, ends with its real name
        val unexpanded = Option(m.lastIndexOf("$$$")).filter(_ >= 0).map(i => m.drop(i + 3))
        (candidates(m) ++ unexpanded.toList.flatMap(candidates)).iterator
          .map(n => jvmMembers.getOrElse((cls, n), Set.empty))
          .find(_.nonEmpty)
          .getOrElse(Set.empty)
      }
      if (fromMethod.nonEmpty) fromMethod
      else outers.get(cls).filter(_ => depth < 20).map((o, om) => resolve(o, om, depth + 1)).filter(_.nonEmpty)
        .orElse(jvmClasses.get(cls).map(Set(_)))
        .getOrElse(Set.empty)
    }

    val (attributed, roots) = bytecodeCallers.toList.foldLeft((usedFrom, used)) { case ((from, roots), (sym, callers)) =>
      val sources = callers.toList.map((cls, m) => resolve(cls, Some(m), 0))
      if (sources.exists(_.isEmpty)) (from, roots + sym)
      else (sources.flatten.foldLeft(from)((acc, s) => acc |+| Map(s -> Set(sym))), roots)
    }
    copy(usedFrom = attributed, used = roots, bytecodeCallers = Map.empty)
  }

  // A default value isn't reported when its method is, or when its method isn't analyzed, e.g. a `given`'s default when
  // only explicits are
  private def isDefaultOfUnreported(unused: Set[Sym])(s: Sym): Boolean =
    defaultValueOf.get(s).exists(m => unused.contains(m) || !defined.contains(m))

  /** `isExcluded`, extended to the default values of excluded methods */
  final def excludingDefaultsOf(isExcluded: Sym => Boolean): Sym => Boolean =
    s => isExcluded(s) || defaultValueOf.get(s).exists(isExcluded)

  final lazy val computeUnused: Set[Sym] = {
    // An entry point is used, and so is what it's in, e.g. the object a `main` method is in
    def withOwners(s: Sym): List[Sym] = s :: owners.get(s).toList.flatMap(withOwners)
    val referenced = used ++ usedFrom.valuesIterator.flatten ++ externalOverrides ++ internalOverrides.keys ++ importRefs ++
      entryPoints.flatMap(withOwners)
    val allUsed = References.reachable(referenced)((s, _) => usedProxy.getOrElse(s, Set.empty))
    val unused = defined.filterNot(allUsed.contains)
    unused.filterNot(isDefaultOfUnreported(unused))
  }

  /**
   * Like `computeUnused`, but a reference only counts if it's made by a definition that's used itself, so code that's
   * only used by unused code -- including by itself, e.g. a recursive method -- is unused too.
   *
   * Starts from the symbols that are used no matter what references them (`used`), `entryPoints`, and the excluded
   * symbols, whose references must keep what they use alive, then follows references, `usedProxy` and `owners`. An
   * override is used when its owner is and, if it overrides something in the analyzed code, when that's used too: a
   * `toString` in a class that's never instantiated is unused, and so is an implementation of a used trait method in a
   * class that's never instantiated.
   * `import`s are ignored, since anything they make available is referenced directly where it's used.
   *
   * Only the outermost unused definitions are returned: when an object is unused, so is everything in it.
   */
  final def computeTransitivelyUnused(isExcluded: Sym => Boolean): Set[Sym] = {
    val overridesByOwner = (externalOverrides ++ internalOverrides.keys).groupBy(owners.get)
    val overridesByBase = internalOverrides.toList.flatMap((o, bases) => bases.map(_ -> o)).groupMap(_._1)(_._2)
    // An override without an owner that only overrides external code is used no matter what, and one that overrides
    // analyzed code is reached through what it overrides
    val roots = used ++ entryPoints ++ defined.filter(isExcluded) ++
      overridesByOwner.getOrElse(None, Set.empty).filterNot(internalOverrides.contains)

    val alive = References.reachable(roots) { (s, isAlive) =>
      def overrideIsAlive(o: Sym): Boolean =
        owners.get(o).forall(isAlive) && internalOverrides.get(o).forall(_.exists(isAlive))
      usedFrom.get(s).iterator.flatten ++
        usedProxy.get(s).iterator.flatten ++
        owners.get(s).filterNot(_ => ownerIndependent.contains(s)) ++
        (overridesByOwner.get(Some(s)).iterator.flatten ++ overridesByBase.get(s).iterator.flatten).filter(overrideIsAlive)
    }
    val unused = defined.filterNot(s => alive.contains(s) || isExcluded(s))
    unused.filterNot(s => owners.get(s).exists(unused.contains) || isDefaultOfUnreported(unused)(s))
  }
}

object References {
  /**
   * Everything reachable from `roots`, following `next` from each symbol reached. `next` also gets whether a symbol has
   * been reached so far, for edges that depend on more than one symbol.
   */
  private def reachable(roots: IterableOnce[Sym])(next: (Sym, Sym => Boolean) => IterableOnce[Sym]): Set[Sym] = {
    val reached = mutable.Set.empty[Sym]
    val queue = mutable.Queue.empty[Sym]
    def reach(s: Sym): Unit = if (reached.add(s)) queue += s
    roots.iterator.foreach(reach)
    while (queue.nonEmpty) next(queue.dequeue(), reached.contains).iterator.foreach(reach)
    reached.toSet
  }

  // The suffixes the compiler adds to the names of methods it generates for a member
  private val generatedSuffix = """(?:\$\$anonfun\$\d+|\$lzyINIT\d+|\$adapted|\$default\$\d+|\$)$""".r
  // The suffix of a local method lifted out of the method it's defined in
  private val liftedLocal = """\$\d+$""".r

  given monoid: Monoid[References] =
    Monoid.instance(
      References(Set.empty, Set.empty, Map.empty),
      (x, y) => References(
        x.defined ++ y.defined,
        x.used ++ y.used,
        x.usedProxy |+| y.usedProxy,
        x.inlineCalls ++ y.inlineCalls,
        x.inlineGivens ++ y.inlineGivens,
        x.inlineTypeArgs ++ y.inlineTypeArgs,
        x.usedFrom |+| y.usedFrom,
        x.owners ++ y.owners,
        x.ownerIndependent ++ y.ownerIndependent,
        x.entryPoints ++ y.entryPoints,
        x.externalOverrides ++ y.externalOverrides,
        x.internalOverrides |+| y.internalOverrides,
        x.defaultValueOf ++ y.defaultValueOf,
        x.importRefs ++ y.importRefs,
        x.bytecodeCallers |+| y.bytecodeCallers,
        x.jvmMembers |+| y.jvmMembers,
        x.jvmClasses ++ y.jvmClasses,
      ),
    )

  lazy val empty: EnvR[References] = monoid.empty.pure[EnvR]

  // A definition, if it's one the analysis reports on (`Symbols.isDefinition`)
  val defined = (sym: Symbol) => (ctx: Context) ?=>
    EnvR.env.flatMap(env =>
      if (!Symbols.isDefinition(env, sym)) empty
      else (for {
        defSym <- Sym.fromSymbol(sym)
        // An overriding symbol is used whenever a symbol it overrides is, since it can be called through a parent type,
        // and always if it overrides something outside the analyzed code
        overrides <- sym match {
          case t: TermOrTypeSymbol =>
            Symbols.allOverriddenSymbols(t) match {
              case Nil => empty
              case bases if bases.forall(Symbols.isDefinition(env, _)) =>
                bases.traverse(Sym.fromSymbol).map(bs => monoid.empty.copy(internalOverrides = Map(defSym -> bs.toSet)))
              case _ => monoid.empty.copy(externalOverrides = Set(defSym)).pure[EnvR]
            }
          case _ => empty
        }
        // If a symbol is referenced in bytecode, consider it used
        // This accounts for `given`s summoned by inline methods and macros, and default values read by macros, which
        // TASTy doesn't record
        callers = env.bytecodeCallers(sym)
        usedInBytecode =
          if (callers.nonEmpty) {
            if (env.debug) println(s"*********** used in bytecode: ${Symbols.name(sym)}")
            monoid.empty.copy(bytecodeCallers = Map(defSym -> callers))
          } else monoid.empty
        jvmNames = sym match {
          case c: ClassSymbol => monoid.empty.copy(jvmClasses = Bytecode.internalClassName(c).map(_ -> defSym).toMap)
          case t: TermSymbol => monoid.empty.copy(jvmMembers = Bytecode.memberKey(t).map(_ -> Set(defSym)).toMap)
          case _ => monoid.empty
        }
        // The innermost enclosing definition, which for a member of an anonymous class is the method that creates it
        owners <- env.enclosing.headOption
          .fold(Map.empty[Sym, Sym].pure[EnvR])(o => Sym.fromSymbol(o).map(os => Map(defSym -> os)))
        // A parameter's owner is its method, even a constructor's, whose class is the parameter's enclosing definition
        ownerIndependent =
          if ((env.enclosing.headOption ++ Option(sym.owner)).exists { case m: TermSymbol => m.isMethod; case _ => false } ||
            Some(sym).collect { case t: TermSymbol => t.isParamAccessor }.getOrElse(false)) Set(defSym)
          else Set.empty[Sym]
        inlineGivens = sym match {
          case t: TermSymbol if t.isInline && Symbols.isGiven(t) => Map(defSym -> t)
          case _ => Map.empty
        }
        defaultValueOf <- Symbols.defaultGetterParam(sym)
          .fold(Map.empty[Sym, Sym].pure[EnvR])((m, _) => Sym.fromSymbol(m).map(ms => Map(defSym -> ms)))
        entryPoints = if (Symbols.isEntryPoint(sym)) Set(defSym) else Set.empty[Sym]
        refs = References(
          Set(defSym),
          Set.empty,
          Map.empty,
          inlineGivens = inlineGivens,
          owners = owners,
          ownerIndependent = ownerIndependent,
          entryPoints = entryPoints,
          defaultValueOf = defaultValueOf,
        )
      } yield refs |+| overrides |+| usedInBytecode |+| jvmNames) |+| Annotations.checkForUnused(sym)
    )

  // A reference made by code, attributed to the innermost enclosing definition, if any
  val used = (sym: Symbol) => (ctx: Context) ?=>
    EnvR.env.flatMap(env =>
      env.enclosing.headOption.fold(root(sym))(src =>
        Sym.fromSymbol(src).flatMap(s => Sym.fromSymbol(sym).map(t => monoid.empty.copy(usedFrom = Map(s -> Set(t)))))
      )
    )

  // A symbol that's used no matter what references it
  val root = (sym: Symbol) => (ctx: Context) ?=>
    Sym.fromSymbol(sym).map(s => References(Set.empty, Set(s), Map.empty))

  def inlineCalls(syms: Set[Symbol]): EnvR[References] =
    monoid.empty.copy(inlineCalls = syms.collect { case t: TermSymbol if t.isInline && !t.isTransparentInline => t }).pure[EnvR]

  def usedProxy(m: Map[Symbol, Set[Symbol]])(using ctx: Context): EnvR[References] = empty.flatMap(_.addUsedProxy(m))
  def usedProxy(ifUsed: Symbol, thenUsed: Set[Symbol])(using ctx: Context): EnvR[References] =
    empty.flatMap(_.addUsedProxy(ifUsed, thenUsed))

  def fromSymbol(
    sym: Symbol,
    mk: Symbol => Context ?=> EnvR[References],
    skipExportCheck: Boolean = false,
  )(using ctx: Context): EnvR[References] =
    EnvR.env.flatMap { env =>
      (if (env.symbolIsValid(sym))
        sym match {
          case _: PackageSymbol => empty

          case t: TermSymbol =>
            mk(t) |+|
              // If a TermSymbol is used and it has a corresponding module class, consider the module class used too
              t.moduleClass.fold(empty)(mk(_))

          case _ => mk(sym)
        }
      else
        empty) |+|
        // This doesn't depend on whether `sym` itself is valid, because the symbols an export generates can be split
        // between the explicits and givens analyses. For example, exporting an implicit class generates an implicit
        // conversion, which is a given, along with a companion and a type, which aren't.
        (if (skipExportCheck || !env.isInPackages(sym)) References.empty else sym match {
        /*
        If sym is an exported term:

          1. Look up the matching term symbol in the Context and consider the result used
            - This might be a bug in tasty-query -- some symbols are different `Symbol` objects when found in an
              `Ident` in `treeRefs` vs. when found as a `TermSymbol` in `symRefs`
          2. Look up the matching type symbol in the Context and consider the result used
            - This covers cases where an export is only used as a value, not also as a type
        */
        case t: TermSymbol if t.isExport =>
          Symbols.allMatchingSymbols(t).foldMap(fromSymbol(_, mk, true))

        /*
        If sym is a type member and there's a matching term symbol that's an exported term, consider the term used

        This covers cases where an export is only used as a type and the companion object isn't used
        */
        case t: TypeMemberSymbol =>
          Symbols.allMatchingSymbols(t).collect { case t: TermSymbol if t.isExport => t }.foldMap(fromSymbol(_, mk, true))

        case _ => empty
      })
    }
}
