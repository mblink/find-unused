package bl.unused

import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

object Trees {
  def referencesO(tree: Option[Tree])(using ctx: Context): EnvR[References] = tree.fold(References.empty)(references)

  def referencesL(trees: List[Tree])(using ctx: Context): EnvR[References] = trees.foldMap(references)

  // We have to handle `Template`s specially when the owner is a `ClassDef`
  // Objects (which are `ClassDef`s) contain a reference to themselves in their `Template`s,
  // causing them to always appear used
  def templateReferences(tpl: Template, klass: Option[ClassDef])(using ctx: Context): EnvR[References] = {
    val Template(constr, parents, self, body) = tpl
    val selfRefs0 = referencesO(self)
    val selfRefs = klass match {
      case Some(c) => selfRefs0.map(r => r.copy(used = r.used - c.symbol.hashCode))
      case None => selfRefs0
    }

    references(constr) |+|
      referencesL(parents) |+|
      selfRefs |+|
      referencesL(body)
  }

  def references(tree: Tree)(using ctx: Context): EnvR[References] =
    EnvR.debug.flatMap { debug =>
      if (debug) println(s"************* tree: ${Debug.printer(tree)}")
      tree match {
        case Alternative(trees) => referencesL(trees)
        case AnnotatedTypeTree(tpt, annotation) => references(tpt) |+| references(annotation)
        case AppliedTypeTree(tycon, args) => references(tycon) |+| referencesL(args)
        case Apply(fun, args) => references(fun) |+| referencesL(args)
        case Assign(lhs, rhs) => references(lhs) |+| references(rhs)
        case Bind(_, body, symbol) => Symbols.references(symbol) |+| references(body)
        case Block(stats, expr) => referencesL(stats) |+| references(expr)
        case ByNameTypeTree(result) => references(result)
        case CaseDef(pattern, guard, body) => references(pattern) |+| referencesO(guard) |+| references(body)
        case c @ ClassDef(_, rhs, symbol) => Symbols.references(symbol) |+| templateReferences(rhs, Some(c))
        case DefDef(_, paramLists, resultTpt, rhs, symbol) =>
          Symbols.references(symbol) |+|
            paramLists.foldMap(e => referencesL(e.merge)) |+|
            references(resultTpt) |+|
            referencesO(rhs)
        case ExplicitTypeBoundsTree(low, high) => references(low) |+| references(high)
        case Export(expr, selectors) => references(expr) |+| referencesL(selectors)
        case ExprPattern(expr) => references(expr)
        case i @ Ident(_) =>
          Symbols.getFromIdent(i).toList.foldMap(
            Symbols.withStaticOwners(_).toList.foldMap(References.fromSymbol(_, References.used))
          )
        case If(cond, thenPart, elsePart) => references(cond) |+| references(thenPart) |+| references(elsePart)
        case Import(expr, selectors) => references(expr) |+| referencesL(selectors)
        case ImportIdent(_) => References.empty
        case ImportSelector(imported, renamed, bound) => references(imported) |+| referencesO(renamed) |+| referencesO(bound)
        case InferredTypeBoundsTree(_) => References.empty
        case Inlined(expr, caller, bindings) => references(expr) |+| referencesO(caller) |+| referencesL(bindings)
        case i @ InlinedTypeTree(caller, expansion) => Types.prefixReferences(i.toPrefix) |+| referencesO(caller) |+| references(expansion)
        case InlineIf(cond, thenPart, elsePart) => references(cond) |+| references(thenPart) |+| references(elsePart)
        case InlineMatch(selector, cases) => referencesO(selector) |+| referencesL(cases)
        case Lambda(meth, tpt) => references(meth) |+| referencesO(tpt)
        case Literal(_) => References.empty
        case Match(selector, cases) => references(selector) |+| referencesL(cases)
        case MatchTypeTree(bound, selector, cases) => references(bound) |+| references(selector) |+| referencesL(cases)
        case NamedArg(_, arg) => references(arg)
        case NamedTypeBoundsTree(_, _) => References.empty
        case New(tpt) => references(tpt)
        case OpaqueTypeAliasDefinitionTree(bounds, alias) => references(bounds) |+| references(alias)
        case PackageDef(pid, stats) => Symbols.references(pid) |+| referencesL(stats)
        case PolyTypeDefinitionTree(tparams, body) => referencesL(tparams) |+| references(body)
        case Quote(body, _) => references(body)
        case QuotePattern(bindings, body, quotes, _) => referencesL(bindings) |+| references(body.merge) |+| references(quotes)
        case RefinedTypeTree(underlying, refinements, refinedCls) =>
          references(underlying) |+| referencesL(refinements) |+| Symbols.references(refinedCls)
        case Return(expr, from) => referencesO(expr) |+| Symbols.references(from)
        case s @ Select(qualifier, _) =>
          Symbols.getFromSelect(s).toList.foldMap(References.fromSymbol(_, References.used)) |+| references(qualifier)
        case SelectOuter(qualifier, _) => references(qualifier) // TODO - will this ever be a given?
        case s @ SelectTypeTree(qualifier, _) => Types.references(s.toType) |+| references(qualifier)
        case SelfDef(_, tpt) => references(tpt)
        case SeqLiteral(elems, elemstpt) => referencesL(elems) |+| references(elemstpt)
        case SingletonTypeTree(ref) => references(ref)
        case Splice(expr, _) => references(expr)
        case SplicePattern(pattern, targs, args, _) => references(pattern) |+| referencesL(targs) |+| referencesL(args)
        case Super(qual, mix) => references(qual) |+| referencesO(mix)
        case t @ Template(_, _, _, _) => templateReferences(t, None)
        case t @ TermRefTypeTree(qualifier, _) => Types.prefixReferences(t.toPrefix) |+| references(qualifier)
        case This(qualifier) => references(qualifier)
        case Throw(expr) => references(expr)
        case Try(expr, cases, finalizer) => references(expr) |+| referencesL(cases) |+| referencesO(finalizer)
        case TypeAliasDefinitionTree(alias) => references(alias)
        case TypeApply(fun, args) => references(fun) |+| referencesL(args)
        case TypeBindingsTree(bindings, body) => referencesL(bindings) |+| references(body)
        case TypeCaseDef(pattern, body) => references(pattern) |+| references(body)
        case Typed(expr, tpt) => references(expr) |+| references(tpt)
        case t @ TypeIdent(_) => Types.references(t.toType)
        case TypeLambdaTree(tparams, body) => referencesL(tparams) |+| references(body)
        case TypeMember(_, rhs, _) => references(rhs)
        case TypeParam(_, bounds, _) => references(bounds)
        case TypeTest(body, tpt) => references(body) |+| references(tpt)
        case TypeTreeBind(_, body, _) => references(body)
        case t @ TypeWrapper(_) => Types.prefixReferences(t.toPrefix)
        // TODO - should implicits be added to References#used?
        case Unapply(fun, implicits, patterns) => references(fun) |+| referencesL(implicits) |+| referencesL(patterns)
        case ValDef(_, tpt, rhs, symbol) => Symbols.references(symbol) |+| references(tpt) |+| referencesO(rhs)
        case While(cond, body) => references(cond) |+| references(body)
        case WildcardPattern(_) => References.empty
        case WildcardTypeArgTree(bounds) => references(bounds)
      }
    }
}
