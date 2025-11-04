package bl.unused

import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Trees.*

object Trees {
  def referencesO(tree: Option[Tree])(using ctx: Context): EnvR[References] = tree.fold(References.empty)(references)

  def referencesL(trees: List[Tree])(using ctx: Context): EnvR[References] = trees.foldMap(references)

  // We have to handle `Template`s specially when the owner is a `ClassDef`
  // Objects (which are `ClassDef`s) contain a reference to themselves in their `Template`s,
  // causing them to always appear used
  def templateReferences(tpl: Template, klass: Option[ClassDef])(using ctx: Context): EnvR[References] = {
    val Template(constr, parents, self, body) = tpl
    val selfRefs0 = referencesO(self)
    val selfRefs = klass.fold(selfRefs0)(c => selfRefs0.map(_.removeUsed(c.symbol)))

    references(constr) |+|
      referencesL(parents) |+|
      selfRefs |+|
      referencesL(body)
  }

  /**
   * If this symbol is a synthetic member of a case class or its companion object, don't consider the case class used
   * unless this symbol is used.
   *
   * This is necessary to verify whether a case class is truly used. Without this check, the synthetic members
   * always make it look like the class is used, since they refer to the class type in their signatures.
   */
  private def removeCaseClassIfSynthetic(sym: Symbol)(refs: EnvR[References])(using ctx: Context): EnvR[References] =
    Symbols.syntheticMemberOfCaseClass(sym) match {
      case Some(cls) =>
        EnvR((env, syms) =>
          refs.run(env.copy(symbolIsValid = s => env.symbolIsValid(s) && !Symbols.isSyntheticMemberOfCaseClass(cls, s)), syms)
        ).flatMap(_.removeUsed(cls).addUsedProxy(sym, Set(cls)))
      case None => refs
    }

  /**
   * If this `Lambda` represents an implementation of a trait/class using single abstract method (SAM) syntax, we consider
   * the parameters of the method in the trait/class used when the parameters of the lambda are used.
   */
  private def singleAbstractMethodReferences(l: Lambda)(using ctx: Context): EnvR[References] = {
    val thisParamsO = Some(l.meth.symbol).collect { case t: TermSymbol => t.paramSymss.flatMap(_.merge) }
    val superParamsO = Either.catchNonFatal(l.samClassSymbol).toOption
      .map(_.declarations.collect { case t: TermSymbol if t.isMethod && t.isAbstractMember && !Symbols.isConstructor(t) => t })
      .collect { case t :: Nil => t.paramSymss.flatMap(_.merge) }

    (thisParamsO, superParamsO).tupled.fold(References.empty)((thisParams, superParams) =>
      References.usedProxy(thisParams.lazyZip(superParams).map((t, s) => (t, Set(s))).toMap)
    )
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
        case DefDef(name, paramLists, resultTpt, rhs, symbol) =>
          removeCaseClassIfSynthetic(symbol)(
            Symbols.references(symbol) |+|
              paramLists.foldMap(e => referencesL(e.merge)) |+|
              references(resultTpt) |+|
              referencesO(rhs)
          )
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
        case l @ Lambda(meth, tpt) => references(meth) |+| referencesO(tpt) |+| singleAbstractMethodReferences(l)
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
        case ValDef(_, tpt, rhs, symbol) =>
          removeCaseClassIfSynthetic(symbol)(Symbols.references(symbol) |+| references(tpt) |+| referencesO(rhs))
        case While(cond, body) => references(cond) |+| references(body)
        case WildcardPattern(_) => References.empty
        case WildcardTypeArgTree(bounds) => references(bounds)
      }
    }
}
