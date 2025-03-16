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
  val isSynthetic: Symbol => Boolean = {
    case s: TermOrTypeSymbol => s.isSynthetic
    case _ => false
  }

  val isConstructor: Symbol => Boolean = _.name == nme.Constructor

  val isDefaultParam: Symbol => Boolean = {
    case t: TermSymbol => t.isParamWithDefault
    case _ => false
  }

  val isGiven: Symbol => Boolean = {
    case s: TermSymbol => s.isGivenOrUsing || s.isImplicit
    case _ => false
  }

  val defaultIsValidChecks = List(isSynthetic, isConstructor, isDefaultParam)

  val defaultIsValid: Symbol => Boolean = s => defaultIsValidChecks.forall(f => !f(s))

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
    |""".stripMargin.split("\n").map(spaces ++ _).mkString("\n")
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

  /** Find all symbols matching `name` in this `klass` and it's parent classes */
  def getFromClasses(klass: ClassSymbol, name: Name)(using ctx: Context): Set[Symbol] =
    klass.linearization
      .flatMap(c => c.getMember(name).toList ++ c.declarations.filter(_.name == name))
      .toSet

  /** Find all symbols matching the given `ident` */
  def getFromIdent(ident: Ident)(using ctx: Context): Set[Symbol] =
    Either.catchNonFatal(Set(ident.symbol)).getOrElse(
      ident.referenceType match {
        case t: TermRef =>
          t.prefix match {
            case t: ThisType => getFromClasses(t.cls, ident.name)
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

  def getSelectOwner(sel: Select): Option[TypeRef] =
    selectOwnerField.get(sel) match {
      case o: Option[?] => o.collect { case t: TypeRef => t }
      case _ => None
    }

  def getFromSelectOwner(sel: Select)(using ctx: Context): Set[Symbol] =
    getSelectOwner(sel)
      .flatMap(_.optSymbol)
      .collect { case c: ClassSymbol => c }
      .fold(Set.empty)(Symbols.getFromClasses(_, sel.name))

  def getFromSelectApply(sel: Select)(using ctx: Context): Set[Symbol] =
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
      .fold(Set.empty)(Symbols.getFromClasses(_, sel.name))

  def getFromSelect(sel: Select)(using ctx: Context): Set[Symbol] =
    Either.catchNonFatal(Set(sel.symbol)).getOrElse(
      Set(
        getFromSelectOwner,
        getFromSelectApply,
      ).flatMap(f => Either.catchNonFatal(f(sel)).toOption.getOrElse(Set.empty))
    )

  def nextOverriddenSymbol(sym: TermOrTypeSymbol)(using ctx: Context): Option[TermOrTypeSymbol] =
    sym.nextOverriddenSymbol.orElse(
      sym match {
        case t: TermSymbol if t.owner.isClass && !Symbols.isConstructor(t) && !t.isPrivate =>
          val ownerClass = t.owner.asClass
          val overrides = ownerClass.linearization.drop(1).iterator.flatMap { inClass =>
            val candidates = inClass.getAllOverloadedDecls(t.name).filterNot(_.isPrivate)
            val site = ownerClass.thisType
            t.typeAsSeenFrom(site) match {
              case targetType: MethodType =>
                candidates.find { candidate =>
                  candidate.typeAsSeenFrom(site) match {
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

              case _ => None
            }
          }
          if (overrides.hasNext) Some(overrides.next) else None

        case _ =>
          None
      }
    )

  def references(sym: Symbol)(using ctx: Context): EnvR[References] =
    EnvR { env =>
      if (env.seenSymbols.contains(sym.hashCode)) References.empty.run(env)
      else {
        val updEnv = env.copy(seenSymbols = env.seenSymbols + sym.hashCode)

        (Annotations.checkForUnused(sym) |+| (sym match {
          // Don't analyze module vals, they cause objects to always appear used
          case t: TermSymbol if t.isModuleVal =>
            if (updEnv.debug) println(s"*********** TermSymbol (module val): ${Symbols.name(t)}")
            References.empty

          case t: TermSymbol =>
            if (updEnv.debug) println(s"*********** TermSymbol: ${Symbols.name(t)}")
            // Don't count default params as definitions
            (t.name match {
              case _: DefaultGetterName => References.empty
              case _ => References.fromSymbol(t, References.defined)
            }) |+|
              // Consider abstract members used
              (if (t.isAbstractMember) References.fromSymbol(t, References.used) else References.empty) |+|
              t.tree.fold(References.empty)(Trees.references)

          case p: PackageSymbol =>
            if (updEnv.debug) println(s"*********** PackageSymbol: ${Symbols.name(p)}")
            p.declarations.foldMap(references)

          case c: ClassSymbol =>
            if (updEnv.debug) println(s"*********** ClassSymbol: ${Symbols.name(c)}, decls: ${c.declarations}")
            (c.name match {
              case tpnme.RefinedClassMagic => References.empty
              case _ => References.fromSymbol(c, References.defined)
            }) |+| c.tree.fold(References.empty)(Trees.references) |+| c.declarations.foldMap(references)

          case _: (ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol) => References.empty
        })).run(updEnv)
      }
    }
}
