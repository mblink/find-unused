package bondlink

// import cats.Eval
import cats.Monoid
import cats.syntax.all.*
import java.net.URI
import java.nio.file.FileSystems
// import scala.util.Try
import tastyquery.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.jdk.ClasspathLoaders

object Example {
  trait Thing[A]

  object Thing {
    given thingO[A: Thing]: Thing[Option[A]] = new Thing[Option[A]] {}
  }

  case class Foo()
  object Foo {
    given thing: Thing[Foo] = new Thing[Foo] {}

    // given show: cats.Show[Foo] = cats.Show.fromToString

  }

  // def takesShow[A](a: A)(using s: cats.Show[A]): String = s.show(a)

  def takesThing[A](using t: Thing[A]): Thing[A] = t

  // val test = takesShow(Foo())

  val testThing = takesThing[Option[Foo]]
}

object FindUnusedGivens {
  val jars = List(
    os.Path("/Users/matt/tasty-query-unused/target/scala-3.5.2/tasty-query-unused_3-0.1.0-SNAPSHOT.jar"),
    os.Path("/Users/matt/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.5.2/scala3-library_3-3.5.2.jar"),
    os.Path("/Users/matt/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/tasty-query_3/1.4.0/tasty-query_3-1.4.0.jar"),
    os.Path("/Users/matt/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/os-lib_3/0.11.3/os-lib_3-0.11.3.jar"),
    os.Path("/Users/matt/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-core_3/2.12.0/cats-core_3-2.12.0.jar"),
    os.Path("/Users/matt/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.14/scala-library-2.13.14.jar"),
    os.Path("/Users/matt/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/geny_3/1.1.1/geny_3-1.1.1.jar"),
    os.Path("/Users/matt/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_3/2.12.0/cats-kernel_3-2.12.0.jar"),
  ) // ++ os.walk(os.root / "example" / "proj" / "target" / "docker" / "stage").filter(_.ext == "jar")

  // val jars = os.Path("/Users/matt/tasty-query-unused/target/scala-3.5.2/tasty-query-unused_3-0.1.0-SNAPSHOT.jar") +:
  //   os.walk(os.root / "example" / "proj" / "target" / "docker" / "stage").filter(_.ext == "jar")
  val paths = FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules", "java.base") :: jars.map(_.toNIO).toList
  val cp = ClasspathLoaders.read(paths)
  given ctx: Contexts.Context = Contexts.Context.initialize(cp)

  case class Givens(
    defined: Set[Symbol],
    used: Set[Symbol],
  )

  object Givens {
    given monoid: Monoid[Givens] =
      Monoid.instance(Givens(Set.empty, Set.empty), (x, y) => Givens(x.defined ++ y.defined, x.used ++ y.used))

    lazy val empty: Givens = monoid.empty

    def defined(sym: Symbol): Givens = Givens(Set(sym), Set.empty)
    def used(sym: Symbol): Givens = Givens(Set.empty, Set(sym))

    def fromTermSymbol(sym: TermSymbol, mk: Symbol => Givens): Givens =
      if (sym.isGivenOrUsing || sym.isImplicit) mk(sym) else Givens.empty
  }

  private def treeGivensO(tree: Option[Tree]): Givens = tree.fold(Givens.empty)(treeGivens)

  private def treeGivensL(trees: List[Tree]): Givens = trees.foldMap(treeGivens)

  def treeGivens(tree: Tree): Givens =
    tree match {
      case Alternative(trees) => treeGivensL(trees)
      case AnnotatedTypeTree(tpt, annotation) => treeGivens(tpt) |+| treeGivens(annotation)
      case AppliedTypeTree(tycon, args) => treeGivens(tycon) |+| treeGivensL(args)
      case Apply(fun, args) => treeGivens(fun) |+| treeGivensL(args)
      case Assign(lhs, rhs) => treeGivens(lhs) |+| treeGivens(rhs)
      case Bind(_, body, symbol) => treeGivens(body)/* |+| symGivens(symbol)*/ // TODO - is symGivens right?
      case Block(stats, expr) => treeGivensL(stats) |+| treeGivens(expr)
      case ByNameTypeTree(result) => treeGivens(result)
      case CaseDef(pattern, guard, body) => treeGivens(pattern) |+| treeGivensO(guard) |+| treeGivens(body)
      case ClassDef(_, rhs, symbol) => treeGivens(rhs) |+| symGivens(symbol)
      case DefDef(_, paramLists, resultTpt, rhs, symbol) =>
        paramLists.foldMap(e => treeGivensL(e.merge)) |+|
          treeGivens(resultTpt) |+|
          treeGivensO(rhs)/* |+|
          symGivens(symbol)*/ // TODO - is symGivens right?
      case ExplicitTypeBoundsTree(low, high) => treeGivens(low) |+| treeGivens(high)
      case Export(expr, selectors) => treeGivens(expr) |+| treeGivensL(selectors)
      case ExprPattern(expr) => treeGivens(expr)
      case i @ Ident(_) =>
        i.symbol match {
          case t: TermSymbol => Givens.fromTermSymbol(t, Givens.used)
          case _: PackageSymbol => Givens.empty
        }
      case If(cond, thenPart, elsePart) => treeGivens(cond) |+| treeGivens(thenPart) |+| treeGivens(elsePart)
      case Import(expr, selectors) => treeGivens(expr) |+| treeGivensL(selectors)
      case ImportIdent(_) => Givens.empty
      case ImportSelector(imported, renamed, bound) => treeGivens(imported) |+| treeGivensO(renamed) |+| treeGivensO(bound)
      case InferredTypeBoundsTree(_) => Givens.empty
      case Inlined(expr, caller, bindings) => treeGivens(expr) |+| treeGivensO(caller) |+| treeGivensL(bindings)
      case InlinedTypeTree(caller, expansion) => treeGivensO(caller) |+| treeGivens(expansion)
      case InlineIf(cond, thenPart, elsePart) => treeGivens(cond) |+| treeGivens(thenPart) |+| treeGivens(elsePart)
      case InlineMatch(selector, cases) => treeGivensO(selector) |+| treeGivensL(cases)
      case Lambda(meth, tpt) => treeGivens(meth) |+| treeGivensO(tpt)
      case Literal(_) => Givens.empty
      case Match(selector, cases) => treeGivens(selector) |+| treeGivensL(cases)
      case MatchTypeTree(bound, selector, cases) => treeGivens(bound) |+| treeGivens(selector) |+| treeGivensL(cases)
      case NamedArg(_, arg) => treeGivens(arg)
      case NamedTypeBoundsTree(_, _) => Givens.empty
      case New(tpt) => treeGivens(tpt)
      case OpaqueTypeAliasDefinitionTree(bounds, alias) => treeGivens(bounds) |+| treeGivens(alias)
      case PackageDef(pid, stats) => symGivens(pid) |+| treeGivensL(stats) // TODO - is symGivens right? probz not
      case PolyTypeDefinitionTree(tparams, body) => treeGivensL(tparams) |+| treeGivens(body)
      case Quote(body, _) => treeGivens(body)
      case QuotePattern(bindings, body, quotes, _) => treeGivensL(bindings) |+| treeGivens(body.merge) |+| treeGivens(quotes)
      case RefinedTypeTree(underlying, refinements, refinedCls) =>
        treeGivens(underlying) |+| treeGivensL(refinements) |+| symGivens(refinedCls) // TODO - is symGivens right?
      case Return(expr, from) => treeGivensO(expr) |+| symGivens(from) // TODO - is symGivens right?
      case Select(qualifier, _) => treeGivens(qualifier) // TODO - will this ever be a given?
      case SelectOuter(qualifier, _) => treeGivens(qualifier) // TODO - will this ever be a given?
      case SelectTypeTree(qualifier, _) => treeGivens(qualifier)
      case SelfDef(_, tpt) => treeGivens(tpt)
      case SeqLiteral(elems, elemstpt) => treeGivensL(elems) |+| treeGivens(elemstpt)
      case SingletonTypeTree(ref) => treeGivens(ref)
      case Splice(expr, _) => treeGivens(expr)
      case SplicePattern(pattern, targs, args, _) => treeGivens(pattern) |+| treeGivensL(targs) |+| treeGivensL(args)
      case Super(qual, mix) => treeGivens(qual) |+| treeGivensO(mix)
      case Template(constr, parents, self, body) => treeGivens(constr) |+| treeGivensL(parents) |+| treeGivensO(self) |+| treeGivensL(body)
      case TermRefTypeTree(qualifier, _) => treeGivens(qualifier) // TODO - will this ever be a given?
      case This(qualifier) => treeGivens(qualifier)
      case Throw(expr) => treeGivens(expr)
      case Try(expr, cases, finalizer) => treeGivens(expr) |+| treeGivensL(cases) |+| treeGivensO(finalizer)
      case TypeAliasDefinitionTree(alias) => treeGivens(alias)
      case TypeApply(fun, args) => treeGivens(fun) |+| treeGivensL(args)
      case TypeBindingsTree(bindings, body) => treeGivensL(bindings) |+| treeGivens(body)
      case TypeCaseDef(pattern, body) => treeGivens(pattern) |+| treeGivens(body)
      case Typed(expr, tpt) => treeGivens(expr) |+| treeGivens(tpt)
      case TypeIdent(_) => Givens.empty
      case TypeLambdaTree(tparams, body) => treeGivensL(tparams) |+| treeGivens(body)
      case TypeMember(_, rhs, _) => treeGivens(rhs)
      case TypeParam(_, bounds, _) => treeGivens(bounds)
      case TypeTest(body, tpt) => treeGivens(body) |+| treeGivens(tpt)
      case TypeTreeBind(_, body, _) => treeGivens(body)
      case TypeWrapper(_) => Givens.empty
      // TODO - should implicits be added to Givens#used?
      case Unapply(fun, implicits, patterns) => treeGivens(fun) |+| treeGivensL(implicits) |+| treeGivensL(patterns)
      case ValDef(_, tpt, rhs, symbol) =>
        treeGivens(tpt) |+|
          treeGivensO(rhs)/* |+|
          symGivens(symbol)*/ // TODO - is symGivens right?
      case While(cond, body) => treeGivens(cond) |+| treeGivens(body)
      case WildcardPattern(_) => Givens.empty
      case WildcardTypeArgTree(bounds) => treeGivens(bounds)
    }

  def symGivens(sym: Symbol): Givens =
    sym match {
      case t: TermSymbol => Givens.fromTermSymbol(t, Givens.defined) |+| t.tree.fold(Givens.empty)(treeGivens)
      case p: PackageSymbol => p.declarations.foldMap(symGivens)
      case c: ClassSymbol => c.declarations.foldMap(symGivens)
      case _: (ClassTypeParamSymbol | LocalTypeParamSymbol | TypeMemberSymbol) => Givens.empty
    }
}
/*
object FindUnused {
  val jars = os.Path("/Users/matt/tasty-query-unused/target/scala-3.4.3/tasty-query-unused_3-0.1.0-SNAPSHOT.jar") +:
    os.walk(os.root / "example" / "proj" / "target" / "docker" / "stage").filter(_.ext == "jar")
  val paths = FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules", "java.base") :: jars.map(_.toNIO).toList
  val cp = ClasspathLoaders.read(paths)
  given ctx: Contexts.Context = Contexts.Context.initialize(cp)

  val emptySet = Eval.now(Set.empty[Symbol])

  def typeBoundsSyms(bounds: TypeBounds): Eval[Set[Symbol]] =
    termTypeSyms(bounds.low) |+| termTypeSyms(bounds.high)

  def typeOrWildcardSyms(tpe: TypeOrWildcard): Eval[Set[Symbol]] =
    tpe match {
      case t: Type => termTypeSyms(t)
      case w: WildcardTypeArg => typeBoundsSyms(w.bounds)
    }

  def matchTypeCaseSyms(cse: MatchTypeCase): Eval[Set[Symbol]] =
    termTypeSyms(cse.pattern) |+| termTypeSyms(cse.result) |+| cse.paramTypeBounds.foldMap(typeBoundsSyms)

  def termTypeSyms(tpe: TermType): Eval[Set[Symbol]] =
    tpe match {
      case t: AndType => termTypeSyms(t.first) |+| termTypeSyms(t.second)
      case _: AnyKindType => emptySet
      case t: AppliedType => termTypeSyms(t.tycon) |+| t.args.foldMap(typeOrWildcardSyms)
      case t: AnnotatedType => termTypeSyms(t.typ).map(_ + t.annotation.symbol)
      case t: ByNameType => termTypeSyms(t.resultType)
      case _: ConstantType => emptySet
      case _: CustomTransientGroundType => emptySet
      case t: MatchType => termTypeSyms(t.bound) |+| termTypeSyms(t.scrutinee) |+| t.cases.foldMap(matchTypeCaseSyms)
      case t: MethodType => t.paramTypes.foldMap(termTypeSyms) |+| termTypeSyms(t.resultType)
      case _: NothingType => emptySet
      case t: OrType => termTypeSyms(t.first) |+| termTypeSyms(t.second)
      case t: PackageRef => Eval.now(Set(t.symbol))
      case t: PolyType => termTypeSyms(t.resultType) |+| t.paramTypeBounds.foldMap(typeBoundsSyms)
      case t: RecThis => termTypeSyms(t.binder)
      case t: RecType => termTypeSyms(t.parent)
      case t: RepeatedType => termTypeSyms(t.elemType)
      case t: SkolemType => termTypeSyms(t.tpe)
      case t: SuperType => termTypeSyms(t.underlying)
      case t: TermParamRef => termTypeSyms(t.underlying)
      case t: TermRef => termTypeSyms(t.underlying)
      case t: TermRefinement => termTypeSyms(t.parent) |+| termTypeSyms(t.refinedType)
      case t: ThisType => termTypeSyms(t.underlying)
      case t: TypeLambda => termTypeSyms(t.resultType) |+| t.paramTypeBounds.foldMap(typeBoundsSyms)
      case t: TypeParamRef => typeBoundsSyms(t.bounds)
      case t: TypeRef => t.optSymbol.fold(emptySet)(s => Eval.now(Set(s))) |+| Try(typeBoundsSyms(t.bounds)).getOrElse(emptySet)
      case t: TypeRefinement => termTypeSyms(t.parent) |+| typeBoundsSyms(t.refinedBounds)
    }

  @annotation.nowarn("msg=exhaustive")
  def treeReferencedSyms(tree: Tree): Eval[Set[Symbol]] =
    tree match {
      case PackageDef(pid, stats) => stats.foldMap(treeReferencedSyms).map(_ + pid)
      case ImportIdent(_) => emptySet
      case ImportSelector(_, _, _) => emptySet
      case Import(_, _) => emptySet
      case Export(_, _) => emptySet
      case ClassDef(_, tpl, _) => treeReferencedSyms(tpl)
      case TypeMember(_, rhs, _) => treeReferencedSyms(rhs)
      case TypeParam(_, bounds, _) => treeReferencedSyms(bounds)
      case Template(constr, parents, self, body) =>
        treeReferencedSyms(constr) |+|
          parents.foldMap(treeReferencedSyms) |+|
          self.fold(emptySet)(treeReferencedSyms) |+|
          body.foldMap(treeReferencedSyms)
      case ValDef(_, tpt, rhs, _) => treeReferencedSyms(tpt) |+| rhs.fold(emptySet)(treeReferencedSyms)
      case SelfDef(_, tpt) => treeReferencedSyms(tpt)
      case DefDef(_, paramLists, resultTpt, rhs, _) =>
        paramLists.foldMap(_.fold(identity, identity).foldMap(treeReferencedSyms)) |+|
          treeReferencedSyms(resultTpt) |+|
          rhs.fold(emptySet)(treeReferencedSyms)
      case i @ Ident(_) => termTypeSyms(i.tpe)
      // case Select(qualifier, _) =>
      //   qualifier.tpe
    }

  def symReferencedSyms(sym: Symbol): Eval[Set[Symbol]] =
    sym.tree.fold(emptySet)(treeReferencedSyms)

  // def findUnusedMethods(sym: Symbol): List[Symbol] =
  //   sym match {
  //     case p: PackageSymbol => p.declarations.flatMap(findUnusedMethods)
  //     case t: TermSymbol => t.tree.fold(Nil)()
  //   }

  def main(args: Array[String]): Unit =
    ()
}
*/
