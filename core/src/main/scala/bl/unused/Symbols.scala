package bl.unused

import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Signatures.{ParamSig, Signature}
import tastyquery.Symbols.*
import tastyquery.SymbolsOps.*

object Symbols {
  def isSynthetic(sym: Symbol): Boolean =
    Some(sym).collect { case t: TermOrTypeSymbol => t.isSynthetic }.getOrElse(false)

  def isParamOfSyntheticMethod(sym: Symbol): Boolean =
    Option(sym.owner).collect { case t: TermSymbol if t.isMethod => t.isSynthetic }.getOrElse(false)

  def syntheticMemberOfCaseClass(sym: Symbol)(using ctx: Context): Option[ClassSymbol] =
    if (isSynthetic(sym))
      Option(sym.owner).flatMap {
        case c: ClassSymbol => if (c.isCaseClass) Some(c) else c.companionClass.filter(_.isCaseClass)
        case _ => None
      }
    else
      None

  def isSyntheticMemberOfCaseClass(cls: ClassSymbol, sym: Symbol)(using ctx: Context): Boolean =
    syntheticMemberOfCaseClass(sym).fold(false)(_ == cls)

  def isConstructor(sym: Symbol): Boolean = sym.name == nme.Constructor

  /**
   * Whether `sym` is a class's primary constructor, which is used whenever its class is. Auxiliary constructors
   * (`def this(...)`) are reported like methods. Without a tree to tell them apart, every constructor counts as primary.
   */
  def isPrimaryConstructor(sym: Symbol): Boolean =
    isConstructor(sym) && Option(sym.owner).collect { case c: ClassSymbol => c }.flatMap(_.tree).forall(_.rhs.constr.symbol == sym)

  def constructorParamToParamAccessors(ctorParam: Symbol)(using ctx: Context): Set[Symbol] =
    Option(ctorParam.owner)
      .collect { case t: TermSymbol if Symbols.isPrimaryConstructor(t) => t } // t is the class constructor
      .flatMap(t => Option(t.owner))
      .collect { case c: ClassSymbol => c } // c is the class
      .fold(Set())(c => c.getDecl(ctorParam.name).toSet ++ c.getMember(ctorParam.name).toSet) // getDecl/getMember find the param accessors

  def paramAccessorToConstructorParam(paramAccessor: Symbol)(using ctx: Context): Option[Symbol] =
    Option(paramAccessor)
      .collect { case t: TermSymbol if t.isParamAccessor => t }
      .flatMap(t => Option(t.owner))
      .collect { case c: ClassSymbol => c } // c is the parent class
      .flatMap(_.declarations.find(Symbols.isPrimaryConstructor))
      .collect { case t: TermSymbol => t.tree } // t is the class constructor
      .flatten
      .collect { case d: DefDef => d.paramLists.flatMap(_.fold(identity, _ => Nil)) } // d is the constructor tree
      .flatMap(_.collectFirst { case v if v.name == paramAccessor.name => v.symbol }) // v is a constructor param

  /** A method that computes a parameter's default value, e.g. `foo$default$2` for the second parameter of `foo` */
  def isDefaultGetter(sym: Symbol): Boolean =
    sym.name match {
      case _: DefaultGetterName => true
      case _ => false
    }

  def isGiven(sym: Symbol): Boolean =
    Some(sym).collect { case t: TermSymbol => t.isGivenOrUsing || t.isImplicit }.getOrElse(false)

  def isInPackage(pkgSyms: Set[PackageSymbol])(sym: Symbol): Boolean =
    sym match {
      case p: PackageSymbol => pkgSyms.contains(p) || Option(p.owner).exists(isInPackage(pkgSyms))
      case _ => Option(sym.owner).exists(isInPackage(pkgSyms))
    }

  def name(sym: Symbol): String =
    (Option(sym.owner) match {
      case Some(owner: PackageSymbol) => s"${owner.displayFullName}.${sym.name}"
      case Some(owner: TermOrTypeSymbol) => s"${name(owner)}.${sym.name}"
      case None => sym.displayFullName
    }).replace("$.", ".").stripSuffix("$")

  /**
   * The parameter whose default value a default getter computes, with its method, or its class for a constructor
   * parameter
   */
  def defaultGetterParam(sym: Symbol)(using ctx: Context): Option[(TermOrTypeSymbol, TermSymbol)] =
    (sym.name, Option(sym.owner)) match {
      case (DefaultGetterName(method, index), Some(owner: ClassSymbol)) =>
        // A constructor's default getters are in its class's companion
        val methods: List[(TermOrTypeSymbol, TermSymbol)] =
          if (method == nme.Constructor)
            // A primary constructor's defaults belong to its class, and an auxiliary constructor's to itself
            owner.companionClass.toList.flatMap(c => c.getAllOverloadedDecls(method).map(d => (if (isPrimaryConstructor(d)) c else d) -> d))
          else owner.getAllOverloadedDecls(method).map(m => m -> m)
        // Only one overload of a method can have default values
        methods.iterator
          .map((m, decl) => (m, decl.paramSymss.flatMap(_.left.toOption.toList.flatten).lift(index)))
          .collectFirst { case (m, Some(p)) if p.isParamWithDefault => (m, p) }
      case _ => None
    }

  /**
   * Whether `sym` is a default getter that's reported: not the forwarder an `export` generates for one, which can't
   * be removed on its own, not one of a local method, which has no name to report it under, and not one of a local
   * class, whose JVM name the bytecode scan can't predict, so it couldn't see a macro reading it.
   */
  def isReportedDefaultGetter(sym: Symbol)(using ctx: Context): Boolean =
    isDefaultGetter(sym) && !Some(sym).collect { case t: TermSymbol => t.isExport }.getOrElse(false) &&
      defaultGetterParam(sym).nonEmpty && Bytecode.memberKey(sym).nonEmpty

  /**
   * The name a symbol is reported under. That's `name`, except for default getters: the default value of `bar` in
   * `def foo(bar: Int = 1)` is `foo(bar = ...)`, and in `class Foo(bar: Int = 1)` it's `Foo(bar = ...)`.
   */
  def displayName(sym: Symbol)(using ctx: Context): String =
    defaultGetterParam(sym).fold(name(sym))((method, param) => s"${name(method)}(${param.name} = ...)")

  private val declarationsByNameCache = Memo[Symbol, Map[String, List[Symbol]]]()

  // An owner's declarations by name, since this is looked up for every type member and export
  private def declarationsByName(owner: Symbol, declarations: => List[Symbol]): Map[String, List[Symbol]] =
    declarationsByNameCache(owner)(declarations.groupBy(_.name.toString))

  def allMatchingSymbols(sym: Symbol)(using ctx: Context): List[Symbol] =
    Option(sym.owner).fold(Nil) {
      case c: ClassSymbol => declarationsByName(c, c.declarations).getOrElse(sym.name.toString, Nil)
      case p: PackageSymbol => declarationsByName(p, p.declarations).getOrElse(sym.name.toString, Nil)
      // I don't think TermSymbols have children...?
      case _: (TermSymbol | ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol) => Nil
    }.filterNot(_ == sym)

  def withStaticOwners(sym: Symbol): Set[Symbol] = {
    @annotation.tailrec
    def go(acc: Set[Symbol], sym: Symbol): Set[Symbol] =
      Either.catchNonFatal(Option(sym.owner)).toOption.flatten match {
        case Some(s: (ClassSymbol | PackageSymbol)) => go(acc + s, s)
        case Some(s: (TermSymbol | ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol)) => go(acc, s)
        case None => acc
      }

    go(Set.empty, sym) + sym
  }

  /** Remove `erased` parameters from a `TermSymbol`'s signature.'
    *
    * This is used below in `getFromClasses` and is necessary because calls to methods with `erased` params may end up with
    * a `SignedName` where the `signature` does not include the `erased` params, causing the signature of the method call
    * to not match the signature of the method definition.
    */
  private def signatureWithoutErasedParams(sym: TermSymbol)(using ctx: Context): Signature = {
    val erasedParamIndices = sym.paramSymss.flatMap(_.merge).zipWithIndex.collect { case (t: TermSymbol, i) if t.isErased => i }.toSet

    if (erasedParamIndices.isEmpty) sym.signature
    else sym.signature.copy(paramsSig = sym.signature.paramsSig.foldLeft((0, List.empty[ParamSig])) {
      case ((accIdx, accRes), p @ ParamSig.TypeLen(len)) => (accIdx + len, accRes :+ p)
      case ((accIdx, accRes), p @ ParamSig.Term(_)) => (accIdx + 1, if (erasedParamIndices.contains(accIdx)) accRes else accRes :+ p)
    }._2)
  }

  /** Find all symbols matching `name` in this `klass` and its parent classes */
  def getFromClasses(klass: ClassSymbol, name: Name)(using ctx: Context): Set[Symbol] = {
    name match {
      /** If `name` is a `SignedName`, look for:
        *   1. A member by `name`
        *   2. A member by the `underlying` name
        *   3. Any declarations with `name` as their name
        *   4. Any declarations with `underlying` as their name and `signature` as their signature (excluding erased params)
        */
      case SignedName(underlying, signature, _) =>
        klass.linearization
          .flatMap(c =>
            c.getMember(name).toList ++
              c.getMember(underlying).toList ++
              c.declarations.filter(d =>
                d.name == name || (d.name == underlying && (d match {
                  case t: TermSymbol => signature == t.signature || signature == signatureWithoutErasedParams(t)
                  case _ => true
                }))
              )
          )
          .toSet

      /** Otherwise look for a member by `name` and any declarations with `name` as their name */
      case _ =>
        klass.linearization
          .flatMap(c => c.getMember(name).toList ++ c.declarations.filter(_.name == name))
          .toSet
    }
  }


  /**
   * Find all symbols matching the given `Ident`
   *
   * tasty-query (1.10.0) throws on `Ident#symbol` when it can't match the signature the compiler pickled for a call to
   * the method's: the compiler uses the method's signature as seen from the prefix, e.g. for an inherited generic method
   * called from an `object` (twirl's `_display_` in a template) or a given inherited by a companion, leaves out `erased`
   * parameters, and erases a `Unit` result reached through an opaque alias or a type parameter to `BoxedUnit`. In that
   * case we fall back to looking at `Ident#referenceType`.
   * If that's a `TermRef`, we look at its `prefix` in an attempt to identify the `ClassSymbol` that owns the `Ident`.
   * If we find a `ClassSymbol`, we look in it and its parent classes for all symbols with the same name as the `Ident`.
   */
  def getFromIdent(ident: Ident)(using ctx: Context): Set[Symbol] =
    Either.catchNonFatal(Set(ident.symbol)).getOrElse(
      ident.referenceType match {
        case t: TermRef =>
          t.prefix match {
            case t: ThisType => getFromClasses(t.cls, ident.name)
            // If a `TermRef` is a module val (the singleton instance of an `object`),
            // look at the corresponding module class (the `object` itself)
            case t: TermRef if t.symbol.isModuleVal => t.symbol.moduleClass.fold(Set.empty)(getFromClasses(_, ident.name))
            case _ => Set.empty
          }

        case _ => Set.empty
      }
    )

  private lazy val selectOwnerField = {
    val f = classOf[Select].getDeclaredField("selectOwner")
    f.setAccessible(true)
    f
  }

  private def getSelectOwner(sel: Select): Option[TypeRef] =
    selectOwnerField.get(sel) match {
      case o: Option[?] => o.collect { case t: TypeRef => t }
      case _ => None
    }

  /**
   * Find all symbols matching the given `Select` by looking at the `selectOwner`
   *
   * Unfortunately `selectOwner` is private so we need to use java reflection to get it -- see `getSelectOwner` above.
   *
   * If there is a `selectOwner`, we attempt to get its symbol, check if it's a `ClassSymbol`, and then use that
   * `ClassSymbol` to find symbols with names matching the name of the `Select`.
   */
  private def getFromSelectOwner(sel: Select)(using ctx: Context): Set[Symbol] =
    getSelectOwner(sel)
      .flatMap(_.optSymbol)
      .collect { case c: ClassSymbol => c }
      .fold(Set.empty)(getFromClasses(_, sel.name))

  /**
   * Find all symbols matching the given `Select` by checking if its `qualifier` is an `Apply`
   *
   * If it is an `Apply`, we get the result type of the method call it represents, attempt to get the symbol of
   * that type, check if it's a `ClassSymbol`, and then use that `ClassSymbol` to find symbols with names matching
   * the name of the `Select`.
   */
  private def getFromSelectApply(sel: Select)(using ctx: Context): Set[Symbol] =
    Some(sel.qualifier)
      .collect { case a: Apply => a.methodType.resultType }
      .flatMap {
        case t: TypeRef => t.optSymbol
        case a: AppliedType =>
          Some(a.tycon).flatMap {
            case t: TypeRef => t.optSymbol
            case _ => None
          }
        case _ => None
      }
      .collect { case c: ClassSymbol => c }
      .fold(Set.empty)(getFromClasses(_, sel.name))

  /**
   * Find all symbols matching the given `Select`
   *
   * tasty-query throws on `Select#symbol` for the reasons `getFromIdent` lists, mostly when resolving the qualifier's type,
   * for members of a match type it doesn't reduce, and when reducing a match type crashes, in which case we fall back to
   * two approaches:
   *
   *   1. Checking `Select#selectOwner` -- see `getFromSelectOwner` above
   *   2. Checking if the `qualifier` of the `Select` is an `Apply -- see `getFromSelectApply` above
   */
  def getFromSelect(sel: Select)(using ctx: Context): Set[Symbol] =
    Either.catchNonFatal(Set(sel.symbol)).getOrElse(
      Set(
        getFromSelectOwner,
        getFromSelectApply,
      ).flatMap(f => Either.catchNonFatal(f(sel)).toOption.getOrElse(Set.empty))
    )

  /** The symbols an `Ident` or `Select` refers to, or none for another tree or one that can't be resolved */
  def fromTree(tree: Tree)(using ctx: Context): Set[Symbol] =
    Either.catchNonFatal(tree match {
      case i: Ident => getFromIdent(i)
      case s: Select => getFromSelect(s)
      case _ => Set.empty
    }).getOrElse(Set.empty)

  /**
   * The first symbol `sym` overrides, if any. tasty-query's `nextOverriddenSymbol` misses some overrides, like ones whose
   * parameter types are written with an `export`ed alias of a generic class, which `Type.matches` doesn't see through.
   * So when it finds nothing, this looks for a non-private method of the same name in a parent class whose parameters
   * erase to the same types as `sym`'s, both seen from `sym`'s class, which is how the compiler's signatures tell an
   * override from an overload. A parameter whose erasure tasty-query can't tell matches any, and a method whose
   * parameters can't be erased at all, e.g. because a type isn't on the classpath, matches none.
   */
  def nextOverriddenSymbol(sym: TermOrTypeSymbol)(using ctx: Context): Option[TermOrTypeSymbol] =
    sym.nextOverriddenSymbol.orElse(sym match {
      case t: TermSymbol if t.owner.isClass && !isConstructor(t) && !t.isPrivate =>
        val site = t.owner.asClass.thisType
        def erasedParams(m: TermSymbol) = Either.catchNonFatal(m.erasedParamsAsSeenFrom(site)).toOption
        // Only erased if there's a candidate, which most methods don't have
        lazy val params = erasedParams(t)
        t.owner.asClass.linearization.drop(1).iterator
          .flatMap(_.getAllOverloadedDecls(t.name).filterNot(_.isPrivate))
          .find(c => (params, erasedParams(c)) match {
            case (Some(ps), Some(cs)) =>
              ps.sizeCompare(cs) == 0 && ps.lazyZip(cs).forall((p, c) => p.isEmpty || c.isEmpty || p == c)
            case _ => false
          })
      case _ => None
    })

  /**
   * Everything `sym` overrides. Following `nextOverriddenSymbol` only finds one member per class it reaches, so the
   * members `sym` overrides in each of its class's parents are added too: `close()` in a class that extends both
   * `AutoCloseable` and a trait declaring `close()` overrides both.
   */
  def allOverriddenSymbols(sym: TermOrTypeSymbol)(using ctx: Context): List[TermOrTypeSymbol] =
    allOverriddenSymbolsCache(sym)(computeAllOverriddenSymbols(sym))

  // Memoized, since it's asked for each method and once more for each of its parameters
  private val allOverriddenSymbolsCache = Memo[TermOrTypeSymbol, List[TermOrTypeSymbol]]()

  private def computeAllOverriddenSymbols(sym: TermOrTypeSymbol)(using ctx: Context): List[TermOrTypeSymbol] = {
    @annotation.tailrec
    def go(acc: List[TermOrTypeSymbol], overriddenSyms: List[Symbol], addHead: Boolean): List[TermOrTypeSymbol] =
      overriddenSyms match {
        case Nil => acc
        case (s: TermOrTypeSymbol) :: rest => go(if (addHead) s :: acc else acc, nextOverriddenSymbol(s).toList ++ rest, true)
        case _ :: rest => go(acc, rest, true)
      }

    val inParents = sym match {
      case t: TermSymbol if Option(t.owner).exists(_.isClass) && !isConstructor(t) && !t.isPrivate =>
        Either.catchNonFatal(t.allOverriddenSymbols.toList).getOrElse(Nil)
      case _ => Nil
    }
    (go(Nil, List(sym), false) ++ inParents).distinct
  }

  /**
   * If the given `Symbol` is a parameter of a method that overrides at least one other,
   * get the method and the methods it overrides in all of its parent classes.
   */
  def getMethodAndOverridesFromParam(sym: Symbol)(using ctx: Context): Set[Symbol] =
    Option(sym.owner).collect { case t: TermSymbol => (t :: allOverriddenSymbols(t)).toSet[Symbol] }.getOrElse(Set())

  /**
   * If the given `Symbol` is a parameter of a method that overrides at least one other,
   * get the equivalent parameters from the methods in the parent classes.
   */
  def getSuperParamsFromParam(sym: Symbol)(using ctx: Context): Set[Symbol] =
    Option(sym.owner).flatMap {
      case t: TermSymbol => Some(allOverriddenSymbols(t).collect { case t: TermSymbol => t }).filter(_.nonEmpty).map((t, _))
      case _ => None
    }.fold(Set()) { (t, overrides) =>
      lazy val superParams = overrides.toList.map(_.paramSymss.flatMap(_.merge))
      t.paramSymss.flatMap(_.merge).zipWithIndex.find(_._1 == sym).fold(Set())((_, i) =>
        superParams.flatMap(_.lift(i)).toSet[Symbol]
      )
    }

  // A wildcard (`_`) or tuple-destructuring parameter
  private def isIgnoredParam(sym: Symbol): Boolean =
    sym.name match {
      case UniqueName(SimpleName(""), "_$" | "$", _) => true
      case _ => false
    }

  // The class of a refinement type, like `{ def foo: Int }`
  private def isRefinementClass(sym: Symbol): Boolean = sym.name == tpnme.RefinedClassMagic

  private val mainName = termName("main")
  private val testSource = "(^|/)src/test/".r

  private val ordinalName = termName("ordinal")

  /**
   * The `ordinal` method the compiler adds to a sealed class's companion to make it the class's `Mirror.Sum`. It tests
   * for each child, which doesn't make them used.
   */
  def isSumMirrorOrdinal(sym: Symbol)(using ctx: Context): Boolean =
    sym.name == ordinalName && isSynthetic(sym) && (Option(sym.owner) match {
      case Some(c: ClassSymbol) =>
        c.isModuleClass && Either.catchNonFatal(c.linearization.exists(name(_) == "scala.deriving.Mirror.Sum")).getOrElse(false)
      case _ => false
    })

  /** The class the compiler generates to run a `@main` method, whose `main` method is synthetic */
  def isMainWrapper(sym: Symbol)(using ctx: Context): Boolean =
    sym match {
      case c: ClassSymbol => Either.catchNonFatal(c.getAllOverloadedDecls(mainName).exists(_.isSynthetic)).getOrElse(false)
      case _ => false
    }

  private val mainAnnotationsCache = Memo[Context, List[ClassSymbol]]()

  // The annotations that make a method a command-line entry point: Scala's `@main`, and mainargs's, which generates the
  // code that calls the method in a macro expansion, so the call isn't in TASTy. Looked up once per run, since
  // `isEntryPoint` asks for every method.
  private def mainAnnotations(using ctx: Context): List[ClassSymbol] =
    mainAnnotationsCache(ctx)(
      List("scala.main", "mainargs.main").flatMap(n => Either.catchNonFatal(ctx.findTopLevelClass(n)).toOption)
    )

  private def isMainMethod(t: TermSymbol)(using ctx: Context): Boolean =
    t.isMethod && (t.name == mainName || mainAnnotations.exists(t.hasAnnotation))

  /**
   * Definitions that are used without being referenced: test classes, `main` methods and their parameters, and objects
   * that inherit one
   */
  def isEntryPoint(sym: Symbol)(using ctx: Context): Boolean =
    sym match {
      case c: ClassSymbol =>
        // The source path recorded in TASTy, which doesn't include the directory the project is in
        c.tree.exists(t => testSource.findFirstIn(t.pos.sourceFile.path).nonEmpty) ||
          Either.catchNonFatal(c.linearization.drop(1).exists(_.getAllOverloadedDecls(mainName).nonEmpty)).getOrElse(false)
      case t: TermSymbol =>
        isMainMethod(t) || Option(t.owner).exists {
          case m: TermSymbol => isMainMethod(m) && m.paramSymss.exists(_.merge.contains(t))
          case _ => false
        }
      case _ => false
    }

  /**
   * Whether `sym` is one of the definitions the analysis reports on. Not compiler-generated code, primary constructors
   * (used whenever their class is), the class that runs a `@main` method, module vals (an `object` is reported as its
   * class), wildcard parameters, the parameters of default getters (which repeat their method's), refinement classes and
   * their members, or the default getters `isReportedDefaultGetter` leaves out.
   */
  def isDefinition(env: Env, sym: Symbol)(using ctx: Context): Boolean =
    env.symbolIsValid(sym) && !isSynthetic(sym) && !isParamOfSyntheticMethod(sym) && !isPrimaryConstructor(sym) &&
      !isMainWrapper(sym) && (!isDefaultGetter(sym) || isReportedDefaultGetter(sym)) && (sym match {
        case t: TermSymbol => !t.isModuleVal && !isIgnoredParam(t) && !Option(t.owner).exists(isDefaultGetter)
        case c: ClassSymbol => !isRefinementClass(c)
        case t: TypeMemberSymbol => !Option(t.owner).exists(isRefinementClass)
        case _ => false
      })

  def references(sym: Symbol)(using ctx: Context): EnvR[Refs] =
    EnvR.markSeen(sym).flatMap(seen =>
      if (seen) References.empty
      else
        for {
          debug <- EnvR.debug
          res <- sym match {
            case s if syntheticMemberOfCaseClass(s).nonEmpty =>
              if (debug) println(s"*********** ${s.getClass.getSimpleName} (synthetic case class member): ${name(s)}")
              References.empty

            // Don't analyze module vals, they cause objects to always appear used
            case t: TermSymbol if t.isModuleVal =>
              if (debug) println(s"*********** TermSymbol (module val): ${name(t)}")
              References.empty

            case t: TermSymbol if isIgnoredParam(t) =>
              if (debug) println(s"*********** TermSymbol (ignored param): ${name(t)}")
              References.empty

            case t: TermSymbol =>
              if (debug) println(s"*********** TermSymbol: ${name(t)}")
              References.fromSymbol(t, References.defined) |+|
                References.usedProxy(getMethodAndOverridesFromParam(t).map((_, Set(t))).toMap) |+|
                References.usedProxy(t, getSuperParamsFromParam(t)) |+|
                References.usedProxy(constructorParamToParamAccessors(t).map((_, Set(t))).toMap)

            case p: PackageSymbol =>
              if (debug) println(s"*********** PackageSymbol: ${name(p)}")
              p.declarations.foldMap(references)

            case c: ClassSymbol =>
              if (debug) println(s"*********** ClassSymbol: ${name(c)}, decls: ${c.declarations}")
              References.fromSymbol(c, References.defined) |+| c.tree.fold(References.empty)(Trees.references) |+|
                Trees.within(c)(c.declarations.foldMap(references))

            case t: TypeMemberSymbol =>
              if (debug) println(s"*********** TypeMemberSymbol: ${name(t)}")
              References.fromSymbol(t, References.defined) |+|
                Trees.within(t)(t.typeDef match {
                  case TypeMemberDefinition.TypeAlias(tpe) =>
                    Types.symbols(tpe).toList.foldMap(References.fromSymbol(_, References.used))
                  case TypeMemberDefinition.AbstractType(bounds) =>
                    Types.typeBoundsTypes(bounds).flatMap(Types.symbols).foldMap(References.fromSymbol(_, References.used))
                  case TypeMemberDefinition.OpaqueTypeAlias(bounds, tpe) =>
                    (tpe :: Types.typeBoundsTypes(bounds)).flatMap(Types.symbols).foldMap(References.fromSymbol(_, References.used))
                })

            case _: (ClassTypeParamSymbol | LocalTypeParamSymbol) => References.empty
          }
        } yield res
    )
}
