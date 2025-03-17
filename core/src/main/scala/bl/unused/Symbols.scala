package bl.unused

import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Symbols.*

object Symbols {
  def isSynthetic(sym: Symbol): Boolean =
    sym match {
      case s: TermOrTypeSymbol => s.isSynthetic
      case _ => false
    }

  def isConstructor(sym: Symbol): Boolean = sym.name == nme.Constructor

  def isDefaultParam(sym: Symbol): Boolean =
    sym match {
      case t: TermSymbol => t.isParamWithDefault
      case _ => false
    }

  def isGiven(sym: Symbol): Boolean =
    sym match {
      case t: TermSymbol => t.isGivenOrUsing || t.isImplicit
      case _ => false
    }

  def defaultIsValid(sym: Symbol): Boolean =
    !isSynthetic(sym) && !isConstructor(sym) && !isDefaultParam(sym)

  def name(sym: Symbol): String =
    (Option(sym.owner) match {
      case Some(owner: PackageSymbol) => s"${owner.displayFullName}.${sym.name}"
      case Some(owner: TermOrTypeSymbol) => s"${name(owner)}.${sym.name}"
      case None => sym.displayFullName
    }).replace("$.", ".")

  def matchingTermSym(sym: Symbol)(using ctx: Context): Option[TermSymbol] =
    Option(sym.owner).flatMap {
      case c: ClassSymbol => c.getMember(termName(sym.name.toString))
      case p: PackageSymbol => p.getDecl(termName(sym.name.toString)).collect { case t: TermSymbol => t }
      // I don't think TermSymbols have children...?
      case _: (TermSymbol | ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol) => None
    }.filterNot(_ == sym)

  def debugDetails(message: String, sym: Symbol, indent: Int): String = {
    val spaces = " " * indent
    s"""
    |$message symbol -- ${sym.getClass} ${name(sym)}
    |  class: ${sym.getClass}
    |  toString: $sym
    |  name: ${name(sym)}
    |  hashCode: ${sym.hashCode}
    |  isExport: ${Some(sym).collect { case t: TermSymbol => t.isExport }.getOrElse(false)}
    |  isSynthetic: ${isSynthetic(sym)}
    |  isConstructor: ${isConstructor(sym)}
    |  isDefaultParam: ${isDefaultParam(sym)}
    |  isGiven: ${isGiven(sym)}
    |  isParamAccessor: ${Some(sym).collect { case t: TermSymbol => t.isParamAccessor }.getOrElse(false)}
    |""".stripMargin.replace("\n", s"\n$spaces")
  }

  def debug(message: String, sym: Symbol, onlyIf: String => Boolean)(using ctx: Context): Unit =
    if (onlyIf(name(sym)))
      println(s"""
        |********************************** $message
        |${debugDetails("sym", sym, 0)}
        |${debugDetails("owner", sym.owner, 2)}
        |${matchingTermSym(sym).fold("")(debugDetails("matchingTermSym", _, 0))}
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
  def nextOverriddenSymbol(sym: TermOrTypeSymbol)(using ctx: Context): Option[TermOrTypeSymbol] =
    sym.nextOverriddenSymbol.orElse(
      sym match {
        case t: TermSymbol if t.owner.isClass && !isConstructor(t) && !t.isPrivate =>
          val ownerClass = t.owner.asClass
          val ownerType = ownerClass.thisType

          t.typeAsSeenFrom(ownerType) match {
            case targetType: MethodType =>
              ownerClass.linearization.drop(1).collectFirst(Function.unlift { parentClass =>
                val candidates = parentClass.getAllOverloadedDecls(t.name).filterNot(_.isPrivate)

                candidates.find { candidate =>
                  candidate.typeAsSeenFrom(ownerType) match {
                    case candidateType: MethodType =>
                      val paramsMatch = (candidateType.paramTypes, targetType.paramTypes) match {
                        case (candidateParams, targetParams) if candidateParams.length == targetParams.length =>
                          candidateParams.zip(targetParams).forall(_.matches(_))
                        case _ =>
                          false
                      }
                      val resultMatch = candidateType.resultType.matches(targetType.resultType)

                      paramsMatch && resultMatch

                    case _ => false
                  }
                }
              })

            case _ => None
          }

        case _ =>
          None
      }
    )

  def references(sym: Symbol)(using ctx: Context): EnvR[References] =
    EnvR.hasSeenSymbol(sym).flatMap(seen =>
      if (seen) References.empty
      else
        for {
          _ <- EnvR.addSeenSymbol(sym)
          debug <- EnvR.debug
          res <- Annotations.checkForUnused(sym) |+| (sym match {
            // Don't analyze module vals, they cause objects to always appear used
            case t: TermSymbol if t.isModuleVal =>
              if (debug) println(s"*********** TermSymbol (module val): ${name(t)}")
              References.empty

            case t: TermSymbol =>
              if (debug) println(s"*********** TermSymbol: ${name(t)}")
              // Don't count default params as definitions
              (t.name match {
                case _: DefaultGetterName => References.empty
                case _ => References.fromSymbol(t, References.defined)
              }) |+|
                // Consider abstract members used
                (if (t.isAbstractMember) References.fromSymbol(t, References.used) else References.empty) |+|
                t.tree.fold(References.empty)(Trees.references)

            case p: PackageSymbol =>
              if (debug) println(s"*********** PackageSymbol: ${name(p)}")
              p.declarations.foldMap(references)

            case c: ClassSymbol =>
              if (debug) println(s"*********** ClassSymbol: ${name(c)}, decls: ${c.declarations}")
              (c.name match {
                case tpnme.RefinedClassMagic => References.empty
                case _ => References.fromSymbol(c, References.defined)
              }) |+| c.tree.fold(References.empty)(Trees.references) |+| c.declarations.foldMap(references)

            case _: (ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol) => References.empty
          })
        } yield res
    )
}
