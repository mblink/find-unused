package bondlink

import cats.{Id, Monoid}
import cats.data.{Kleisli, Reader}
import cats.syntax.all.*
import java.net.URI
import java.nio.file.{FileSystems, Path}
import tastyquery.Contexts.Context
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.jdk.ClasspathLoaders

object FindUnusedGivens {
  def all(pkgs: Seq[String], classpath: Seq[Path]): Givens = {
    given ctx: Context = Context.initialize(ClasspathLoaders.read(
      FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules", "java.base") :: classpath.toList
    ))

    pkgs.foldMap(p => symGivens(ctx.findPackage(p))).run(Env(false, Set.empty))
  }

  case class Env(log: Boolean, seenSymbols: Set[Int])

  case class Givens(
    defined: Map[Int, (String, Option[String])],
    used: Set[Int],
  )

  object Givens {
    given monoid: Monoid[Givens] =
      Monoid.instance(Givens(Map.empty, Set.empty), (x, y) => Givens(x.defined ++ y.defined, x.used ++ y.used))

    lazy val empty: Givens = monoid.empty

    private def symName(sym: Symbol): String =
      Option(sym.owner) match {
        case Some(owner: PackageSymbol) => s"${owner.displayFullName}.${sym.name}"
        case Some(owner: TermOrTypeSymbol) => s"${symName(owner)}.${sym.name}"
        case None => sym.displayFullName
      }

    // TODO - sourceFile is relative, can we get an absolute path?
    private def formatPos(pos: SourcePosition): String =
      if (pos.hasLineColumnInformation) s"${pos.sourceFile}:${pos.pointLine + 1}:${pos.pointColumn + 1}"
      else pos.toString

    def defined(sym: TermSymbol)(using ctx: Context): Givens =
      Givens(
        defined = Map(sym.hashCode -> (symName(sym), sym.tree.map(t => formatPos(t.pos)))),
        // If a symbol overrides another, consider it used
        // This accounts for cases where a `given` is used in a parent class, but not in the subclass
        used = sym.nextOverriddenSymbol.fold(Set.empty)(_ => Set(sym.hashCode)),
      )

    def used(sym: Symbol): Givens =
      Givens(Map.empty, Set(sym.hashCode))

    def fromTermSymbol(sym: TermSymbol, mk: TermSymbol => Givens): Givens =
      if (sym.isGivenOrUsing || sym.isImplicit) mk(sym) else empty

    def fromTermOrPackageSymbol(sym: => TermSymbol | PackageSymbol, mk: TermSymbol => Givens): Givens =
      Either.catchNonFatal(sym match {
        case t: TermSymbol => fromTermSymbol(t, mk)
        case _: PackageSymbol => empty
      }).getOrElse(empty)
  }

  type ResF[A] = Reader[Env, A]
  type Res = ResF[Givens]

  private lazy val empty: Res = Givens.empty.pure[ResF]

  private def treeGivensO(tree: Option[Tree])(using ctx: Context): Res = tree.fold(empty)(treeGivens)

  private def treeGivensL(trees: List[Tree])(using ctx: Context): Res = trees.foldMap(treeGivens)

  def treeGivens(tree: Tree)(using ctx: Context): Res =
    Kleisli.ask[Id, Env].flatMap { env =>
      if (env.log) println(s"************* tree: ${pprint(tree.toString)}")
      tree match {
        case Alternative(trees) => treeGivensL(trees)
        case AnnotatedTypeTree(tpt, annotation) => treeGivens(tpt) |+| treeGivens(annotation)
        case AppliedTypeTree(tycon, args) => treeGivens(tycon) |+| treeGivensL(args)
        case Apply(fun, args) => treeGivens(fun) |+| treeGivensL(args)
        case Assign(lhs, rhs) => treeGivens(lhs) |+| treeGivens(rhs)
        case Bind(_, body, _ /* symbol */) => treeGivens(body)/* |+| symGivens(symbol)*/ // TODO - is symGivens right?
        case Block(stats, expr) => treeGivensL(stats) |+| treeGivens(expr)
        case ByNameTypeTree(result) => treeGivens(result)
        case CaseDef(pattern, guard, body) => treeGivens(pattern) |+| treeGivensO(guard) |+| treeGivens(body)
        case ClassDef(_, rhs, symbol) => treeGivens(rhs) |+| symGivens(symbol)
        case DefDef(_, paramLists, resultTpt, rhs, _ /* symbol */) =>
          paramLists.foldMap(e => treeGivensL(e.merge)) |+|
            treeGivens(resultTpt) |+|
            treeGivensO(rhs)/* |+|
            symGivens(symbol)*/ // TODO - is symGivens right?
        case ExplicitTypeBoundsTree(low, high) => treeGivens(low) |+| treeGivens(high)
        case Export(expr, selectors) => treeGivens(expr) |+| treeGivensL(selectors)
        case ExprPattern(expr) => treeGivens(expr)
        case i @ Ident(_) => Givens.fromTermOrPackageSymbol(i.symbol, Givens.used).pure[ResF]
        case If(cond, thenPart, elsePart) => treeGivens(cond) |+| treeGivens(thenPart) |+| treeGivens(elsePart)
        case Import(expr, selectors) => treeGivens(expr) |+| treeGivensL(selectors)
        case ImportIdent(_) => empty
        case ImportSelector(imported, renamed, bound) => treeGivens(imported) |+| treeGivensO(renamed) |+| treeGivensO(bound)
        case InferredTypeBoundsTree(_) => empty
        case Inlined(expr, caller, bindings) => treeGivens(expr) |+| treeGivensO(caller) |+| treeGivensL(bindings)
        case InlinedTypeTree(caller, expansion) => treeGivensO(caller) |+| treeGivens(expansion)
        case InlineIf(cond, thenPart, elsePart) => treeGivens(cond) |+| treeGivens(thenPart) |+| treeGivens(elsePart)
        case InlineMatch(selector, cases) => treeGivensO(selector) |+| treeGivensL(cases)
        case Lambda(meth, tpt) => treeGivens(meth) |+| treeGivensO(tpt)
        case Literal(_) => empty
        case Match(selector, cases) => treeGivens(selector) |+| treeGivensL(cases)
        case MatchTypeTree(bound, selector, cases) => treeGivens(bound) |+| treeGivens(selector) |+| treeGivensL(cases)
        case NamedArg(_, arg) => treeGivens(arg)
        case NamedTypeBoundsTree(_, _) => empty
        case New(tpt) => treeGivens(tpt)
        case OpaqueTypeAliasDefinitionTree(bounds, alias) => treeGivens(bounds) |+| treeGivens(alias)
        case PackageDef(pid, stats) => symGivens(pid) |+| treeGivensL(stats)
        case PolyTypeDefinitionTree(tparams, body) => treeGivensL(tparams) |+| treeGivens(body)
        case Quote(body, _) => treeGivens(body)
        case QuotePattern(bindings, body, quotes, _) => treeGivensL(bindings) |+| treeGivens(body.merge) |+| treeGivens(quotes)
        case RefinedTypeTree(underlying, refinements, refinedCls) =>
          treeGivens(underlying) |+| treeGivensL(refinements) |+| symGivens(refinedCls)
        case Return(expr, from) => treeGivensO(expr) |+| symGivens(from)
        case s @ Select(qualifier, _) => Givens.fromTermOrPackageSymbol(s.symbol, Givens.used).pure[ResF] |+| treeGivens(qualifier)
        case SelectOuter(qualifier, _) => treeGivens(qualifier) // TODO - will this ever be a given?
        case SelectTypeTree(qualifier, _) => treeGivens(qualifier)
        case SelfDef(_, tpt) => treeGivens(tpt)
        case SeqLiteral(elems, elemstpt) => treeGivensL(elems) |+| treeGivens(elemstpt)
        case SingletonTypeTree(ref) => treeGivens(ref)
        case Splice(expr, _) => treeGivens(expr)
        case SplicePattern(pattern, targs, args, _) => treeGivens(pattern) |+| treeGivensL(targs) |+| treeGivensL(args)
        case Super(qual, mix) => treeGivens(qual) |+| treeGivensO(mix)
        case Template(constr, parents, self, body) => treeGivens(constr) |+| treeGivensL(parents) |+| treeGivensO(self) |+| treeGivensL(body)
        case TermRefTypeTree(qualifier, _) => treeGivens(qualifier)
        case This(qualifier) => treeGivens(qualifier)
        case Throw(expr) => treeGivens(expr)
        case Try(expr, cases, finalizer) => treeGivens(expr) |+| treeGivensL(cases) |+| treeGivensO(finalizer)
        case TypeAliasDefinitionTree(alias) => treeGivens(alias)
        case TypeApply(fun, args) => treeGivens(fun) |+| treeGivensL(args)
        case TypeBindingsTree(bindings, body) => treeGivensL(bindings) |+| treeGivens(body)
        case TypeCaseDef(pattern, body) => treeGivens(pattern) |+| treeGivens(body)
        case Typed(expr, tpt) => treeGivens(expr) |+| treeGivens(tpt)
        case TypeIdent(_) => empty
        case TypeLambdaTree(tparams, body) => treeGivensL(tparams) |+| treeGivens(body)
        case TypeMember(_, rhs, _) => treeGivens(rhs)
        case TypeParam(_, bounds, _) => treeGivens(bounds)
        case TypeTest(body, tpt) => treeGivens(body) |+| treeGivens(tpt)
        case TypeTreeBind(_, body, _) => treeGivens(body)
        case TypeWrapper(_) => empty
        // TODO - should implicits be added to Givens#used?
        case Unapply(fun, implicits, patterns) => treeGivens(fun) |+| treeGivensL(implicits) |+| treeGivensL(patterns)
        case ValDef(_, tpt, rhs, _ /* symbol */) =>
          treeGivens(tpt) |+|
            treeGivensO(rhs)/* |+|
            symGivens(symbol)*/ // TODO - is symGivens right?
        case While(cond, body) => treeGivens(cond) |+| treeGivens(body)
        case WildcardPattern(_) => empty
        case WildcardTypeArgTree(bounds) => treeGivens(bounds)
      }
    }

  def symGivens(sym: Symbol)(using ctx: Context): Res =
    Reader { env =>
      if (env.seenSymbols.contains(sym.hashCode)) empty.run(env)
      else {
        val updEnv = env.copy(seenSymbols = env.seenSymbols + sym.hashCode)
        sym match {
          case t: TermSymbol =>
            if (updEnv.log) println(s"*********** TermSymbol ${t.name}")
            (Givens.fromTermSymbol(t, Givens.defined).pure[ResF] |+| t.tree.fold(empty)(treeGivens)).run(updEnv)
          case p: PackageSymbol =>
            if (updEnv.log) println(s"*********** PackageSymbol ${p.name}")
            p.declarations.foldMap(symGivens).run(updEnv)
          case c: ClassSymbol =>
            if (updEnv.log) println(s"*********** ClassSymbol ${c.name}")
            (c.tree.fold(empty)(treeGivens) |+| c.declarations.foldMap(symGivens)).run(updEnv)
          case _: (ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol) => empty.run(updEnv)
        }
      }
    }
}
