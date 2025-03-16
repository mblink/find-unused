package bondlink

import cats.{Id, Monoid}
import cats.data.{Kleisli, Reader}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import java.net.URI
import java.nio.file.{Files, FileSystems, Path}
import scala.jdk.CollectionConverters.*
import tastyquery.Annotations.*
import tastyquery.Constants.*
import tastyquery.Contexts.*
import tastyquery.jdk.ClasspathLoaders
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

object FindUnused {
  private val isSynthetic: Symbol => Boolean = {
    case s: TermOrTypeSymbol => s.isSynthetic
    case _ => false
  }

  private val isConstructor: Symbol => Boolean = _.name == nme.Constructor

  private val isDefaultParam: Symbol => Boolean = {
    case t: TermSymbol => t.isParamWithDefault
    case _ => false
  }

  private val defaultFilterSymbolChecks = List(isSynthetic, isConstructor, isDefaultParam)

  private val defaultFilterSymbol: Symbol => Boolean = s => defaultFilterSymbolChecks.forall(f => !f(s))

  private val isGiven: Symbol => Boolean = {
    case s: TermSymbol => s.isGivenOrUsing || s.isImplicit
    case _ => false
  }

  class Runner(filterSymbol: Symbol => Boolean) {
    final def apply(debug: Boolean, rootDirectory: Option[Path], packages: Seq[String], classpath: Seq[Path]): References = {
      println("[find-unused] Initializing classpath")

      val javaModules = Files.list(FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules")).iterator.asScala.toList

      // `implicit val` instead of `given` so it's initialized eagerly for logging/timing purposes
      implicit val ctx: Context = Context.initialize(ClasspathLoaders.read(javaModules ::: classpath.toList))

      val env = Env(debug, rootDirectory, packages, Set.empty, s => defaultFilterSymbol(s) && filterSymbol(s))

      packages.foldMap { p =>
        println(s"[find-unused] Analyzing package $p")
        symRefs(ctx.findPackage(p)).run(env)
      }
    }
  }

  object explicits extends Runner(s => !isGiven(s))
  object givens extends Runner(isGiven)
  object all extends Runner(_ => true)

  case class Env(
    debug: Boolean,
    rootDirectory: Option[Path],
    packages: Seq[String],
    seenSymbols: Set[Int],
    filterSymbol: Symbol => Boolean,
  )

  type EnvR[A] = Reader[Env, A]

  private def formatPos(rootDirectory: Option[Path], pos: SourcePosition): String =
    rootDirectory.fold("")(_.toString ++ "/") ++ (
      if (pos.hasLineColumnInformation) s"${pos.sourceFile}:${pos.pointLine + 1}:${pos.pointColumn + 1}"
      else pos.toString
    )

  private def symName(sym: Symbol): String =
    (Option(sym.owner) match {
      case Some(owner: PackageSymbol) => s"${owner.displayFullName}.${sym.name}"
      case Some(owner: TermOrTypeSymbol) => s"${symName(owner)}.${sym.name}"
      case None => sym.displayFullName
    }).replace("$.", ".")

  def getSymFromClasses(klass: ClassSymbol, name: Name)(using Context): Set[Symbol] =
    klass.linearization
      .flatMap(c => c.getMember(name).toList ++ c.declarations.filter(_.name == name))
      .toSet

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

  def getSelectSymFromSelectOwner(sel: Select)(using Context): Set[Symbol] =
    getSelectOwner(sel)
      .flatMap(_.optSymbol)
      .collect { case c: ClassSymbol => c }
      .fold(Set.empty)(getSymFromClasses(_, sel.name))

  def getSelectSymsFromApply(sel: Select)(using Context): Set[Symbol] =
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
      .fold(Set.empty)(getSymFromClasses(_, sel.name))

  def getSelectSyms(sel: Select)(using Context): Set[Symbol] =
    Set(
      getSelectSymFromSelectOwner,
      getSelectSymsFromApply,
    ).flatMap(f => Either.catchNonFatal(f(sel)).toOption.getOrElse(Set.empty))

  def getTermReferenceTreeSyms(tree: TermReferenceTree)(using Context): Set[Symbol] =
    Either.catchNonFatal(Set(tree.symbol)).valueOr(e => tree match {
      case i: Ident =>
        i.referenceType match {
          case t: TermRef =>
            t.prefix match {
              case t: ThisType =>
                getSymFromClasses(t.cls, i.name)

              case t: TermRef if t.symbol.isModuleVal =>
                t.symbol.moduleClass.fold(Set.empty)(c => getSymFromClasses(c, i.name))

              case _ => Set.empty
            }

          case _ => Set.empty
        }

      case s: Select =>
        getSelectSyms(s)
    })

  private def symAndStaticOwners(sym: Symbol)(using Context): Set[Symbol] = {
    @annotation.tailrec
    def go(acc: Set[Symbol], sym: Symbol): Set[Symbol] =
      Either.catchNonFatal(Option(sym.owner)).toOption.flatten match {
        case Some(s: (ClassSymbol | PackageSymbol)) => go(acc + s, s)
        case Some(s: (TermSymbol | ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol)) => go(acc, s)
        case None => acc
      }

    go(Set.empty, sym) + sym
  }

  def nextOverriddenSymbol(sym: TermOrTypeSymbol)(using Context): Option[TermOrTypeSymbol] =
    sym.nextOverriddenSymbol.orElse(
      sym match {
        case t: TermSymbol if t.owner.isClass && !isConstructor(t) && !t.isPrivate =>
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

  private def lookupMatchingTermSym(sym: Symbol)(using Context): Option[TermSymbol] =
    Option(sym.owner).flatMap {
      case c: ClassSymbol => c.getMember(termName(sym.name.toString))
      case p: PackageSymbol => p.getDecl(termName(sym.name.toString)).collect { case t: TermSymbol => t }
      // I don't think TermSymbols have children...?
      case _: (TermSymbol | ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol) => None
    }.filterNot(_ == sym)

  private def debugSymDetails(prefix: String, sym: Symbol, indent: Int): String = {
    val spaces = " " * indent
    s"""
    |$prefix symbol -- ${sym.getClass} ${symName(sym)}
    |  class: ${sym.getClass}
    |  toString: $sym
    |  name: ${symName(sym)}
    |  hashCode: ${sym.hashCode}
    |  isExport: ${Some(sym).collect { case t: TermSymbol => t.isExport }.getOrElse(false)}
    |  isSynthetic: ${isSynthetic(sym)}
    |  isConstructor: ${isConstructor(sym)}
    |  isDefaultParam: ${isDefaultParam(sym)}
    |  isGiven: ${isGiven(sym)}
    |  isParamAccessor: ${Some(sym).collect { case t: TermSymbol => t.isParamAccessor }.getOrElse(false)}
    |""".stripMargin.split("\n").map(spaces ++ _).mkString("\n")
  }

  @annotation.nowarn("msg=unused")
  private def debugSym(prefix: String, sym: Symbol, onlyIf: String => Boolean)(using Context): Unit = {
    val name = symName(sym)
    if (onlyIf(symName(sym)))
      println(s"""
        |********************************** $prefix
        |${debugSymDetails("sym", sym, 0)}
        |${debugSymDetails("owner", sym.owner, 2)}
        |${lookupMatchingTermSym(sym).fold("")(debugSymDetails("matchingTermSym", _, 0))}
        |**********************************
        |""".stripMargin)
  }

  case class References(
    defined: Map[Int, (String, Option[String])],
    used: Set[Int],
  )

  object References {
    given monoid: Monoid[References] =
      Monoid.instance(References(Map.empty, Set.empty), (x, y) => References(x.defined ++ y.defined, x.used ++ y.used))

    lazy val empty: EnvR[References] = Reader(_ => monoid.empty)

    def defined(sym: Symbol)(using Context): EnvR[References] =
      Reader { env =>
        References(
          defined = Map(sym.hashCode -> (symName(sym), sym.tree.map(t => formatPos(env.rootDirectory, t.pos)))),
          used = sym match {
            // If a symbol overrides another, consider it used
            // This accounts for cases where a term is used in a parent class, but not in the subclass
            case t: TermOrTypeSymbol => nextOverriddenSymbol(t).fold(Set.empty)(_ => Set(t.hashCode))
            case _ => Set.empty
          },
        )
      }

    def used(sym: Symbol)(using Context): EnvR[References] =
      References(Map.empty, Set(sym.hashCode)).pure[EnvR]

    def fromSymbol(sym: Symbol, mk: Symbol => EnvR[References])(using Context): EnvR[References] = {
      Kleisli.ask[Id, Env].flatMap { env =>
        if (env.filterSymbol(sym))
          sym match {
            case _: PackageSymbol => empty

            case t: TermSymbol =>
              mk(t) |+|
                // If a TermSymbol is used and it has a corresponding module class, consider the module class used too
                t.moduleClass.fold(empty)(mk)

            case _ => mk(sym)
          }
        else
          empty
      } |+| (sym match {
        /*
        If sym is an exported term, look up the symbol in the Context and consider the result used

        This might be a bug in tasty-query -- some symbols have different `hashCode`s when found in an
        `Ident` in `treeRefs` vs. when found as a `TermSymbol` in `symRefs`
        */
        case t: TermSymbol if t.isExport => lookupMatchingTermSym(t).fold(empty)(fromSymbol(_, mk))

        /*
        If sym is a type member and there's a matching term symbol that's an exported term, consider the term used

        This covers cases where an export is only used as a type and the companion object isn't used
        */
        case t: TypeMemberSymbol => lookupMatchingTermSym(t).filter(_.isExport).fold(empty)(fromSymbol(_, mk))

        case _ => empty
      })
    }
  }

  private def typeBoundsTypes(bounds: TypeBounds): List[Type] = List(bounds.low, bounds.high)

  private def typeSymbols(tpe: Type)(using Context): Set[Symbol] = {
    @annotation.tailrec
    def go(acc: Set[Symbol], seen: Set[Int], types: List[Type]): Set[Symbol] =
      types.filterNot(t => seen.contains(t.hashCode)) match {
        case Nil => acc

        case (t: TypeRef) :: rest => go(acc ++ t.optSymbol, seen + t.hashCode, rest)

        case (t: TermRef) :: rest => go(acc + t.symbol, seen + t.hashCode, rest)

        // TODO - change to this? go(acc, t.underlying :: rest)
        case (t: ThisType) :: rest => go(acc ++ t.tref.optSymbol, seen + t.hashCode, rest)

        case (t: AppliedType) :: rest =>
          go(
            acc,
            seen + t.hashCode,
            t.tycon ::
              t.args.flatMap {
                case t: Type => List(t)
                case w: WildcardTypeArg => typeBoundsTypes(w.bounds)
              } :::
              rest,
          )

        case (t: TypeParamRef) :: rest => go(acc, seen + t.hashCode, typeBoundsTypes(t.bounds) ::: rest)

        case (t: TypeRefinement) :: rest => go(acc, seen + t.hashCode, t.parent :: typeBoundsTypes(t.refinedBounds) ::: rest)

        case (t: TermRefinement) :: rest =>
          go(
            acc,
            seen + t.hashCode,
            t.parent ::
              (t.refinedType match {
                case t: Type => List(t)
                case _: MethodicType => Nil
              }) :::
              rest
          )

        case (t: MatchType) :: rest =>
          go(
            acc,
            seen + t.hashCode,
            t.bound ::
              t.scrutinee ::
              t.cases.flatMap(c => c.pattern :: c.result :: c.paramTypeBounds.flatMap(typeBoundsTypes)) :::
              rest
          )

        case (t: TypeLambda) :: rest =>
          go(
            acc,
            seen + t.hashCode,
            t.paramTypeBounds.flatMap(typeBoundsTypes) :::
              t.resultType ::
              rest
          )

        case (t: AndType) :: rest => go(acc, seen + t.hashCode, t.first :: t.second :: rest)

        case (t: OrType) :: rest => go(acc, seen + t.hashCode, t.first :: t.second :: rest)

        case (t: (
          AnnotatedType
          | ByNameType
          | ConstantType
          | FlexibleType
          | RecThis
          | RecType
          | RepeatedType
          | SkolemType
          | SuperType
          | TermParamRef
        )) :: rest =>
          go(acc, seen + t.hashCode, t.underlying :: rest)

        case (t: (AnyKindType | CustomTransientGroundType | NothingType)) :: rest => go(acc, seen + t.hashCode, rest)
      }

    go(Set.empty, Set.empty, List(tpe))
  }

  def typeRefs(tpe: Type)(using Context): EnvR[References] =
    typeSymbols(tpe).toList.foldMap(References.fromSymbol(_, References.used))

  def prefixTypeRefs(prefix: NonEmptyPrefix)(using Context): EnvR[References] =
    prefix match {
      case t: Type => typeRefs(t)
      case _: PackageRef => References.empty
    }

  private def treeRefsO(tree: Option[Tree])(using Context): EnvR[References] = tree.fold(References.empty)(treeRefs)

  private def treeRefsL(trees: List[Tree])(using Context): EnvR[References] = trees.foldMap(treeRefs)

  // We have to handle `Template`s specially when the owner is a `ClassDef`
  // Objects (which are `ClassDef`s) contain a reference to themselves in their `Template`s,
  // causing them to always appear used
  def templateRefs(tpl: Template, klass: Option[ClassDef])(using Context): EnvR[References] = {
    val Template(constr, parents, self, body) = tpl
    val selfRefs0 = treeRefsO(self)
    val selfRefs = klass match {
      case Some(c) => selfRefs0.map(r => r.copy(used = r.used - c.symbol.hashCode))
      case None => selfRefs0
    }

    treeRefs(constr) |+|
      treeRefsL(parents) |+|
      selfRefs |+|
      treeRefsL(body)
  }

  def treeRefs(tree: Tree)(using Context): EnvR[References] =
    Kleisli.ask[Id, Env].flatMap { env =>
      if (env.debug) println(s"************* tree: ${pprint(tree)}")
      tree match {
        case Alternative(trees) => treeRefsL(trees)
        case AnnotatedTypeTree(tpt, annotation) => treeRefs(tpt) |+| treeRefs(annotation)
        case AppliedTypeTree(tycon, args) => treeRefs(tycon) |+| treeRefsL(args)
        case Apply(fun, args) => treeRefs(fun) |+| treeRefsL(args)
        case Assign(lhs, rhs) => treeRefs(lhs) |+| treeRefs(rhs)
        case Bind(_, body, _ /* symbol */) => treeRefs(body)
        case Block(stats, expr) => treeRefsL(stats) |+| treeRefs(expr)
        case ByNameTypeTree(result) => treeRefs(result)
        case CaseDef(pattern, guard, body) => treeRefs(pattern) |+| treeRefsO(guard) |+| treeRefs(body)
        case c @ ClassDef(_, rhs, _ /* symbol */) => templateRefs(rhs, Some(c))
        case d @ DefDef(_, paramLists, resultTpt, rhs, _ /* symbol */) =>
          paramLists.foldMap(e => treeRefsL(e.merge)) |+| treeRefs(resultTpt) |+| treeRefsO(rhs)
        case ExplicitTypeBoundsTree(low, high) => treeRefs(low) |+| treeRefs(high)
        case Export(expr, selectors) => treeRefs(expr) |+| treeRefsL(selectors)
        case ExprPattern(expr) => treeRefs(expr)
        case i @ Ident(name) =>
          getTermReferenceTreeSyms(i).toList.foldMap(
            symAndStaticOwners(_).toList.foldMap(References.fromSymbol(_, References.used))
          )
        case If(cond, thenPart, elsePart) => treeRefs(cond) |+| treeRefs(thenPart) |+| treeRefs(elsePart)
        case Import(expr, selectors) => treeRefs(expr) |+| treeRefsL(selectors)
        case ImportIdent(_) => References.empty
        case ImportSelector(imported, renamed, bound) => treeRefs(imported) |+| treeRefsO(renamed) |+| treeRefsO(bound)
        case InferredTypeBoundsTree(_) => References.empty
        case Inlined(expr, caller, bindings) => treeRefs(expr) |+| treeRefsO(caller) |+| treeRefsL(bindings)
        case i @ InlinedTypeTree(caller, expansion) => prefixTypeRefs(i.toPrefix) |+| treeRefsO(caller) |+| treeRefs(expansion)
        case InlineIf(cond, thenPart, elsePart) => treeRefs(cond) |+| treeRefs(thenPart) |+| treeRefs(elsePart)
        case InlineMatch(selector, cases) => treeRefsO(selector) |+| treeRefsL(cases)
        case Lambda(meth, tpt) => treeRefs(meth) |+| treeRefsO(tpt)
        case Literal(_) => References.empty
        case Match(selector, cases) => treeRefs(selector) |+| treeRefsL(cases)
        case MatchTypeTree(bound, selector, cases) => treeRefs(bound) |+| treeRefs(selector) |+| treeRefsL(cases)
        case NamedArg(_, arg) => treeRefs(arg)
        case NamedTypeBoundsTree(_, _) => References.empty
        case New(tpt) => treeRefs(tpt)
        case OpaqueTypeAliasDefinitionTree(bounds, alias) => treeRefs(bounds) |+| treeRefs(alias)
        case PackageDef(pid, stats) => symRefs(pid) |+| treeRefsL(stats)
        case PolyTypeDefinitionTree(tparams, body) => treeRefsL(tparams) |+| treeRefs(body)
        case Quote(body, _) => treeRefs(body)
        case QuotePattern(bindings, body, quotes, _) => treeRefsL(bindings) |+| treeRefs(body.merge) |+| treeRefs(quotes)
        case RefinedTypeTree(underlying, refinements, refinedCls) =>
          treeRefs(underlying) |+| treeRefsL(refinements) |+| symRefs(refinedCls)
        case Return(expr, from) => treeRefsO(expr) |+| symRefs(from)
        case s @ Select(qualifier, name) =>
          getTermReferenceTreeSyms(s).toList.foldMap(References.fromSymbol(_, References.used)) |+| treeRefs(qualifier)
        case SelectOuter(qualifier, _) => treeRefs(qualifier) // TODO - will this ever be a given?
        case s @ SelectTypeTree(qualifier, _) => typeRefs(s.toType) |+| treeRefs(qualifier)
        case SelfDef(_, tpt) => treeRefs(tpt)
        case SeqLiteral(elems, elemstpt) => treeRefsL(elems) |+| treeRefs(elemstpt)
        case SingletonTypeTree(ref) => treeRefs(ref)
        case Splice(expr, _) => treeRefs(expr)
        case SplicePattern(pattern, targs, args, _) => treeRefs(pattern) |+| treeRefsL(targs) |+| treeRefsL(args)
        case Super(qual, mix) => treeRefs(qual) |+| treeRefsO(mix)
        case t @ Template(_, _, _, _) => templateRefs(t, None)
        case t @ TermRefTypeTree(qualifier, _) => prefixTypeRefs(t.toPrefix) |+| treeRefs(qualifier)
        case This(qualifier) => treeRefs(qualifier)
        case Throw(expr) => treeRefs(expr)
        case Try(expr, cases, finalizer) => treeRefs(expr) |+| treeRefsL(cases) |+| treeRefsO(finalizer)
        case TypeAliasDefinitionTree(alias) => treeRefs(alias)
        case TypeApply(fun, args) => treeRefs(fun) |+| treeRefsL(args)
        case TypeBindingsTree(bindings, body) => treeRefsL(bindings) |+| treeRefs(body)
        case TypeCaseDef(pattern, body) => treeRefs(pattern) |+| treeRefs(body)
        case Typed(expr, tpt) => treeRefs(expr) |+| treeRefs(tpt)
        case t @ TypeIdent(_) => typeRefs(t.toType)
        case TypeLambdaTree(tparams, body) => treeRefsL(tparams) |+| treeRefs(body)
        case TypeMember(_, rhs, _) => treeRefs(rhs)
        case TypeParam(_, bounds, _) => treeRefs(bounds)
        case TypeTest(body, tpt) => treeRefs(body) |+| treeRefs(tpt)
        case TypeTreeBind(_, body, _) => treeRefs(body)
        case t @ TypeWrapper(_) => prefixTypeRefs(t.toPrefix)
        // TODO - should implicits be added to References#used?
        case Unapply(fun, implicits, patterns) => treeRefs(fun) |+| treeRefsL(implicits) |+| treeRefsL(patterns)
        case ValDef(_, tpt, rhs, symbol) => treeRefs(tpt) |+| treeRefsO(rhs) |+| symAnnotationRefs(symbol)
        case While(cond, body) => treeRefs(cond) |+| treeRefs(body)
        case WildcardPattern(_) => References.empty
        case WildcardTypeArgTree(bounds) => treeRefs(bounds)
      }
    }

  object UnusedAnnotation {
    private class UnusedAnnotationSignature(name: String) {
      final def unapply(tree: Tree): Option[List[TermTree]] =
        tree match {
          case Apply(Select(New(_), SignedName(_, Signature(_, sigName), _)), args) if name == sigName.toString => Some(args)
          case _ => None
        }
    }
    private object NowarnAnnotationSignature extends UnusedAnnotationSignature("scala.annotation.nowarn")
    private object UnusedAnnotationSignature extends UnusedAnnotationSignature("scala.annotation.unused")

    private object UnusedMsgFilter {
      private val checks = Set("unused", "never used")

      def unapply(tree: TermTree): Boolean =
        tree match {
          case Literal(Constant(s: String)) =>
            s.split('&').exists(_.split('=') match {
              case Array("msg", msg) => checks.exists(msg.toLowerCase.contains)
              case _ => false
            })
          case _ => false
        }
    }

    def is(a: Annotation)(using Context): Boolean =
      a.tree match {
        case UnusedAnnotationSignature(Nil) => true
        case NowarnAnnotationSignature(List(UnusedMsgFilter())) => true
        case NowarnAnnotationSignature(List(ref: TermReferenceTree)) =>
          Either.catchNonFatal(ref.symbol).toOption
            .collect { case t: TermSymbol => t }
            .flatMap(_.tree)
            .collect {
              case v: ValDef => v.rhs
              case d: DefDef => d.rhs
            }
            .flatten
            .collect { case UnusedMsgFilter() => true }
            .getOrElse(false)

        case _ => false
      }
  }

  private def ctorParamToParamAccessor(ctorParam: Symbol)(using Context): Option[Symbol] =
    Option(ctorParam.owner)
      .collect { case t: TermSymbol if isConstructor(t) => t } // t is the class constructor
      .flatMap(t => Option(t.owner))
      .collect { case c: ClassSymbol => c } // c is the class
      .flatMap(_.getMember(ctorParam.name)) // the result of getMember is the param accessor

  private def paramAccessorToCtorParam(paramAccessor: Symbol)(using Context): Option[Symbol] =
    Option(paramAccessor)
      .collect { case t: TermSymbol if t.isParamAccessor => t }
      .flatMap(t => Option(t.owner))
      .collect { case c: ClassSymbol => c } // c is the parent class
      .flatMap(_.declarations.find(isConstructor))
      .collect { case t: TermSymbol => t.tree } // t is the class constructor
      .flatten
      .collect { case d: DefDef => d.paramLists.flatMap(_.fold(identity, _ => Nil)) } // d is the constructor tree
      .flatMap(_.collectFirst { case v if v.name == paramAccessor.name => v.symbol }) // v is a constructor param

  def symAnnotationRefs(sym: Symbol)(using Context): EnvR[References] = {
    // If the symbol is a class constructor parameter, attempt to find the corresponding param accessor in the class
    val paramAccessorSym = ctorParamToParamAccessor(sym)
    // If the symbol is a param accessor, attempt to find the corresponding constructor param in the class
    val ctorParamSym = paramAccessorToCtorParam(sym)

    // Check all relevant symbols for unused annotations
    val allSyms = (Set(sym) ++ paramAccessorSym ++ ctorParamSym).toList
    val hasUnusedAnnotation = allSyms.exists(_.annotations.exists(UnusedAnnotation.is))

    allSyms.foldMap(_.annotations.foldMap(a => treeRefs(a.tree))) |+| (
      // If any relevant symbol has an unused annotation, consider them all used
      if (hasUnusedAnnotation) allSyms.foldMap(References.fromSymbol(_, References.used))
      else References.empty
    )
  }

  def symRefs(sym: Symbol)(using Context): EnvR[References] =
    Reader { env =>
      if (env.seenSymbols.contains(sym.hashCode)) References.empty.run(env)
      else {
        val updEnv = env.copy(seenSymbols = env.seenSymbols + sym.hashCode)

        (symAnnotationRefs(sym) |+| (sym match {
          // Don't analyze module vals, they cause objects to always appear used
          case t: TermSymbol if t.isModuleVal =>
            if (updEnv.debug) println(s"*********** TermSymbol (module val): ${symName(t)}")
            References.empty

          case t: TermSymbol =>
            if (updEnv.debug) println(s"*********** TermSymbol: ${symName(t)}")
            // Don't count default params as definitions
            (t.name match {
              case _: DefaultGetterName => References.empty
              case _ => References.fromSymbol(t, References.defined)
            }) |+|
              // Consider abstract members used
              (if (t.isAbstractMember) References.fromSymbol(t, References.used) else References.empty) |+|
              t.tree.fold(References.empty)(treeRefs)

          case p: PackageSymbol =>
            if (updEnv.debug) println(s"*********** PackageSymbol: ${symName(p)}")
            p.declarations.foldMap(symRefs)

          case c: ClassSymbol =>
            if (updEnv.debug) println(s"*********** ClassSymbol: ${symName(c)}, decls: ${c.declarations}")
            (c.name match {
              case tpnme.RefinedClassMagic => References.empty
              case _ => References.fromSymbol(c, References.defined)
            }) |+| c.tree.fold(References.empty)(treeRefs) |+| c.declarations.foldMap(symRefs)

          case _: (ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol) => References.empty
        })).run(updEnv)
      }
    }
}
