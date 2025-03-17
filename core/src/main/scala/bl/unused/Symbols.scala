package bl.unused

import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Symbols.*
import tastyquery.SymbolsOps.*

object Symbols {
  def isSynthetic(sym: Symbol): Boolean =
    Some(sym).collect { case t: TermOrTypeSymbol => t.isSynthetic }.getOrElse(false)

  def syntheticMemberOfCaseClass(sym: Symbol)(using ctx: Context): Option[ClassSymbol] =
    if (isSynthetic(sym))
      Option(sym.owner).flatMap {
        case c: ClassSymbol => if (c.isCaseClass) Some(c) else c.companionClass.filter(_.isCaseClass)
        case _ => None
      }
    else
      None

  def isConstructor(sym: Symbol): Boolean = sym.name == nme.Constructor

  def isDefaultParam(sym: Symbol): Boolean =
    Some(sym).collect { case t: TermSymbol => t.isParamWithDefault }.getOrElse(false)

  def isGiven(sym: Symbol): Boolean =
    Some(sym).collect { case t: TermSymbol => t.isGivenOrUsing || t.isImplicit }.getOrElse(false)

  def isValidDefinition(sym: Symbol): Boolean =
    !isSynthetic(sym) && !isConstructor(sym) && !isDefaultParam(sym)

  def name(sym: Symbol): String =
    (Option(sym.owner) match {
      case Some(owner: PackageSymbol) => s"${owner.displayFullName}.${sym.name}"
      case Some(owner: TermOrTypeSymbol) => s"${name(owner)}.${sym.name}"
      case None => sym.displayFullName
    }).replace("$.", ".")

  private def matchingSymbol[N, A](
    name: String => N,
    getMember: (ClassSymbol, N) => Option[A],
    getDecl: (PackageSymbol, N) => Option[A],
  ): Symbol => Option[A] =
    sym => Option(sym.owner).flatMap {
      case c: ClassSymbol => getMember(c, name(sym.name.toString))
      case p: PackageSymbol => getDecl(p, name(sym.name.toString))
      // I don't think TermSymbols have children...?
      case _: (TermSymbol | ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol) => None
    }.filterNot(_ == sym)

  def matchingTermSymbol(using ctx: Context): Symbol => Option[TermSymbol] =
    matchingSymbol(termName, _.getMember(_), _.getDecl(_).collect { case t: TermSymbol => t })

  def matchingTypeSymbol(using ctx: Context): Symbol => Option[TypeSymbol] =
    matchingSymbol(typeName, _.getMember(_), _.getDecl(_))

  def debugDetails(message: String, sym: Symbol, indent: Int)(using ctx: Context): String = {
    val spaces = " " * indent
    val termSym = Some(sym).collect { case t: TermSymbol => t }
    s"""
    |$message -- ${sym.getClass} ${name(sym)}
    |  class: ${sym.getClass}
    |  toString: $sym
    |  name: ${name(sym)}
    |  raw name: ${sym.name}
    |  raw name pprint: ${Debug.printer(sym.name)}
    |  hashCode: ${sym.hashCode}
    |  isExport: ${termSym.fold(false)(_.isExport)}
    |  isSynthetic: ${isSynthetic(sym)}
    |  isConstructor: ${isConstructor(sym)}
    |  isDefaultParam: ${isDefaultParam(sym)}
    |  isGiven: ${isGiven(sym)}
    |  isParamAccessor: ${termSym.fold(false)(_.isParamAccessor)}
    |  nextOverriddenSymbol: ${termSym.flatMap(nextOverriddenSymbol).fold("None")(s => "\n" ++ debugDetails("nextOverriddenSymbol", s, indent + 2))}
    |""".stripMargin.replace("\n", s"\n$spaces")
  }

  def debug(message: String, sym: Symbol, onlyIf: String => Boolean)(using ctx: Context): Unit =
    if (onlyIf(name(sym)))
      println(s"""
        |********************************** $message
        |${debugDetails("sym", sym, 0)}
        |${debugDetails("owner", sym.owner, 2)}
        |${matchingTermSymbol(sym).fold("")(debugDetails("matchingTermSym", _, 0))}
        |**********************************
        |""".stripMargin)

  def withStaticOwners(sym: Symbol)(using ctx: Context): Set[Symbol] = {
    @annotation.tailrec
    def go(acc: Set[Symbol], sym: Symbol): Set[Symbol] =
      Either.catchNonFatal(Option(sym.owner)).toOption.flatten match {
        case Some(s: (ClassSymbol | PackageSymbol)) => go(acc + s, s)
        case Some(s: (TermSymbol | ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol)) => go(acc, s)
        case None => acc
      }

    go(Set.empty, sym) + sym
  }

  /** Find all symbols matching `name` in this `klass` and its parent classes */
  def getFromClasses(klass: ClassSymbol, name: Name)(using ctx: Context): Set[Symbol] =
    klass.linearization
      .flatMap(c => c.getMember(name).toList ++ c.declarations.filter(_.name == name))
      .toSet

  /** Find all symbols matching the given `Ident`
   *
   * tasty-query sometimes throws an error on `Ident#symbol`, in which case we fall back to looking at `Ident#referenceType`.
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

  /** Find all symbols matching the given `Select` by looking at the `selectOwner`
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

  /** Find all symbols matching the given `Select` by checking if its `qualifier` is an `Apply`
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

  /** Find all symbols matching the given `Select`
   *
   * tasty-query sometimes throws an error on `Select#symbol`, in which case we fall back to two approaches:
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

  /** Get the first symbol the given `TermOrTypeSymbol` overrides, if any
   *
   * tasty-query has a built-in `TermOrTypeSymbol#nextOverriddenSymbol` method, but it sometimes returns `None` unexpectedly
   * We first try calling that, and then perform our fallback logic.
   *
   * If the given `sym` meets all of these criteria:
   *
   *   1. It belongs to a `ClassSymbol`
   *   2. It is not a constructor method
   *   3. It is not private
   *   4. It is a method
   *
   * then we check the parent classes of the symbol's owner. For each one, we get method declarations with names
   * matching the name of the symbol, and look for the first one where both of the following are true:
   *
   *   1. The method's parameter types match the parameter types of the method represented by `sym`
   *   2. The method's result type matches the result type of the method represented by `sym`
   *
   * This logic is cobbled together from a handful of methods in tasty-query.
   *
   * TermOrTypeSymbol#nextOverriddenSymbol, which calls TermOrTypeSymbol#allOverriddenSymbols,
   * which calls TermOrTypeSymbol#overriddenSymbol, which calls TermOrTypeSymbol#matchingDecl.
   *
   * For TermSymbols, matchingDecl performs a check against the parent class similar to what we do, but it uses
   * `candidateType.matches(targetType)`, which returns `false` for some overridden methods.
   *
   * This is the ultimate problem we're working around. For some reason, both the parameter and result types match,
   * but the overall type of the methods don't match.
   */
  def nextOverriddenSymbol(sym: TermOrTypeSymbol)(using ctx: Context): Option[TermOrTypeSymbol] = {
    def listsMatch[A](l1: List[A], l2: List[A], cmp: (A, A) => Boolean): Boolean =
      if (l1.length == l2.length) l1.zip(l2).forall(cmp.tupled)
      else false

    def typesMatch(t1: TypeOrMethodic, t2: TypeOrMethodic): Boolean =
      (t1, t2) match {
        case (m1: MethodType, m2: MethodType) =>
          val paramsMatch = listsMatch(m1.paramTypes, m2.paramTypes, typesMatch)
          val resultMatch = typesMatch(m1.resultType, m2.resultType)

          paramsMatch && resultMatch

        case (p1: PolyType, p2: PolyType) =>
          val paramsMatch = listsMatch(p1.paramTypeBounds, p2.paramTypeBounds, _.isSameBounds(_))
          val resultMatch = typesMatch(p1.resultType, p2.resultType)

          paramsMatch && resultMatch

        case _ => t1.matches(t2)
      }

    sym.nextOverriddenSymbol.orElse(
      sym match {
        case t: TermSymbol if t.owner.isClass && !isConstructor(t) && !t.isPrivate =>
          val ownerClass = t.owner.asClass
          val ownerType = ownerClass.thisType
          val targetType = t.typeAsSeenFrom(ownerType)

          ownerClass.linearization.drop(1).collectFirst(Function.unlift { parentClass =>
            val candidates = parentClass.getAllOverloadedDecls(t.name).filterNot(_.isPrivate)

            candidates.find { candidate =>
              typesMatch(candidate.typeAsSeenFrom(ownerType), targetType)
            }
          })

        case _ =>
          None
      }
    )
  }

  // Does the given Symbol or its owner have a DefaultGetterName as its name?
  // It seems like `def`s with default params are duplicated as `DefDef`s with a `name` that's a `DefaultGetterParam`
  private object DefaultParam {
    private def isDefaultName(name: Name): Boolean =
      name match {
        case _: DefaultGetterName => true
        case _ => false
      }

    def unapply(sym: Symbol): Boolean =
      isDefaultName(sym.name) || Option(sym.owner).fold(false)(s => isDefaultName(s.name))
  }

  private object IgnoredParam {
    def unapply(sym: Symbol): Boolean =
      sym match {
        case t: TermSymbol =>
          t.name match {
            // Wildcard, i.e. `_`
            case UniqueName(SimpleName(""), "_$", _) => true
            // Tuple destructuring
            case UniqueName(SimpleName(""), "$", _) => true
            case _ => false
          }
        case _ => false
      }
  }

  private object AbstractMember {
    def unapply(sym: Symbol): Boolean = Some(sym).collect { case t: TermSymbol => t.isAbstractMember }.getOrElse(false)
  }

  private object AbstractMethodParam {
    def unapply(sym: Symbol): Boolean = Option(sym.owner).fold(false)(AbstractMember.unapply)
  }

  private object OverrideMethodParam {
    def unapply(sym: Symbol)(using ctx: Context): Boolean =
      Option(sym.owner)
        .collect { case t: TermSymbol => t }
        .flatMap(Symbols.nextOverriddenSymbol)
        .nonEmpty
  }

  private object RefinementClass {
    def unapply(sym: Symbol): Boolean = Some(sym).collect { case c: ClassSymbol => c.isRefinementClass }.getOrElse(false)
  }

  def references(sym: Symbol)(using ctx: Context): EnvR[References] =
    EnvR.hasSeenSymbol(sym).flatMap(seen =>
      if (seen) References.empty
      else
        for {
          _ <- EnvR.addSeenSymbol(sym)
          debug <- EnvR.debug
          res <- sym match {
            case s if syntheticMemberOfCaseClass(s).nonEmpty =>
              if (debug) println(s"*********** ${s.getClass.getSimpleName} (synthetic case class member): ${name(s)}")
              References.empty

            // Don't analyze module vals, they cause objects to always appear used
            case t: TermSymbol if t.isModuleVal =>
              if (debug) println(s"*********** TermSymbol (module val): ${name(t)}")
              References.empty

            case t @ IgnoredParam() =>
              if (debug) println(s"*********** TermSymbol (ignored param): ${name(t)}")
              References.empty

            case t: TermSymbol =>
              if (debug) println(s"*********** TermSymbol: ${name(t)}")
              // Don't count default params as definitions
              (t match {
                case DefaultParam() => References.empty
                case _ => References.fromSymbol(t, References.defined)
              }) |+|
                (t match {
                  // Consider abstract members, params of abstract methods, and params of override methods used
                  case AbstractMember() | AbstractMethodParam() | OverrideMethodParam() =>
                    References.fromSymbol(t, References.used)
                  case _ =>
                    References.empty
                }) |+|
                t.tree.fold(References.empty)(Trees.references)

            case p: PackageSymbol =>
              if (debug) println(s"*********** PackageSymbol: ${name(p)}")
              p.declarations.foldMap(references)

            case c: ClassSymbol =>
              if (debug) println(s"*********** ClassSymbol: ${name(c)}, decls: ${c.declarations}")
              (c match {
                case RefinementClass() => References.empty
                case _ => References.fromSymbol(c, References.defined)
              }) |+| c.tree.fold(References.empty)(Trees.references) |+| c.declarations.foldMap(references)

            case t: TypeMemberSymbol =>
              if (debug) println(s"*********** TypeMemberSymbol: ${name(t)}")
              (Option(t.owner) match {
                case Some(RefinementClass()) => References.empty
                case _ => References.fromSymbol(t, References.defined)
              }) |+|
                (t.typeDef match {
                  case TypeMemberDefinition.TypeAlias(tpe) =>
                    Types.symbols(tpe).toList.foldMap(References.fromSymbol(_, References.used))
                  case TypeMemberDefinition.AbstractType(bounds) =>
                    Types.typeBoundsTypes(bounds).flatMap(Types.symbols).foldMap(References.fromSymbol(_, References.used))
                  case TypeMemberDefinition.OpaqueTypeAlias(bounds, tpe) =>
                    Types.typeBoundsTypes(bounds).flatMap(Types.symbols).foldMap(References.fromSymbol(_, References.used)) |+|
                      Types.symbols(tpe).toList.foldMap(References.fromSymbol(_, References.used))
                })

            case _: (ClassTypeParamSymbol | LocalTypeParamSymbol) => References.empty
          }
        } yield res
    )
}
