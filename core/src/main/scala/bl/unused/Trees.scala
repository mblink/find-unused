package bl.unused

import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import scala.collection.mutable
import tastyquery.Contexts.*
import tastyquery.Names.{termName, SignedName, TermName}
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*
import tastyquery.Types.*

object Trees {
  def referencesO(tree: Option[Tree])(using ctx: Context): EnvR[References] = tree.fold(References.empty)(references)

  def referencesL(trees: List[Tree])(using ctx: Context): EnvR[References] = trees.foldMap(references)

  /** Attributes the references found by `refs` to `sym`, if it's one of the definitions the analysis reports on */
  def within(sym: Symbol)(refs: EnvR[References])(using ctx: Context): EnvR[References] =
    EnvR((env, syms) =>
      refs.run(if (Symbols.isDefinition(env, sym)) env.copy(enclosing = sym :: env.enclosing) else env, syms)
    )

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

  private def isQuotedTypeOf(tpe: TypeOrMethodic, tparam: LocalTypeParamSymbol)(using ctx: Context): Boolean =
    tpe match {
      case a: AppliedType =>
        (a.tycon, a.args) match {
          case (tycon: TypeRef, List(arg: TypeRef)) =>
            tycon.optSymbol.exists(_.displayFullName == "scala.quoted.Type") && arg.optSymbol.contains(tparam)
          case _ => false
        }
      case _ => false
    }

  private val ofName = termName("of")

  private def unsigned(name: TermName): TermName =
    name match {
      case s: SignedName => s.underlying
      case n => n
    }

  // Checks the name before resolving the symbol, since this runs for every `TypeApply`
  private def isTypeOf(fun: TermTree)(using ctx: Context): Boolean =
    (fun match {
      case i: Ident => i.name == ofName
      case s: Select => unsigned(s.name) == ofName
      case _ => false
    }) && Symbols.fromTree(fun).exists(Symbols.name(_) == "scala.quoted.Type.of")

  /**
   * If these trees or types, which are part of a quote, refer to a method's type parameters, we consider the method's
   * `scala.quoted.Type` parameters for those type parameters used, whether they're context bounds (`[A: Type]`) or
   * explicit `using` parameters.
   *
   * The compiler heals type references in quotes with an implicit search for a `Type`, but it does so in the `staging`
   * phase, after TASTy is pickled. For example, `'{ List.empty[A] }` becomes `'{ List.empty[evidence$1.Underlying] }`,
   * and `Type.of[A]` -- including the ones the compiler synthesizes for `Expr.summon[F[A]]` -- becomes `evidence$1`.
   * The TASTy only refers to `A`, so without this the `Type` would always appear unused.
   *
   * A splice's result type is healed too, but the code inside a splice runs at compile time and isn't healed, and a
   * nested quote is handled when `references` reaches it.
   *
   * The match is approximate: only a parameter of exactly `Type[A]` counts, so evidence like `Type[List[A]]` that healing
   * could use is still reported, and a local `given Type[A]` that healing would pick instead doesn't stop the
   * parameter from counting.
   */
  private def quotedTypeParamReferences(trees: List[Tree], types: List[Type])(using ctx: Context): EnvR[References] = {
    val typeParams = mutable.Set.empty[LocalTypeParamSymbol]
    val localAliases = mutable.Set.empty[TypeMemberSymbol]
    // Resolving a type's symbols can throw in tasty-query, which shouldn't stop the analysis
    def addTypeParams(tpe: Type): Unit =
      Either.catchNonFatal(Types.symbols(tpe)).getOrElse(Set.empty).foreach {
        case s: LocalTypeParamSymbol => typeParams += s
        // Healing sees through local type aliases: `type L = List[A]; '{ Nil: L }` needs a `Type[A]`
        case t: TypeMemberSymbol if Option(t.owner).exists(_.isInstanceOf[TermSymbol]) && localAliases.add(t) =>
          t.typeDef match {
            case TypeMemberDefinition.TypeAlias(alias) => addTypeParams(alias)
            case _ => ()
          }
        case _ => ()
      }

    types.foreach(addTypeParams)
    new TreeTraverser {
      override def traverse(tree: Tree): Unit =
        tree match {
          case s: Splice => addTypeParams(s.spliceType)
          case _: Quote => ()
          case t: TypeTree =>
            Either.catchNonFatal(t.toType).foreach(addTypeParams)
            super.traverse(tree)
          case _ => super.traverse(tree)
        }
    }.traverse(trees)

    typeParams.toList.flatMap(tparam =>
      Option(tparam.owner).collect { case m: TermSymbol => m }.toList
        .flatMap(_.paramSymss.flatMap(_.merge))
        .collect { case p: TermSymbol if isQuotedTypeOf(p.declaredType, tparam) => p }
    ).foldMap(References.fromSymbol(_, References.used))
  }

  private val transparentInlineDefsCache = Memo[ClassSymbol, List[TermSymbol]]()

  // The `transparent inline` methods defined anywhere in a class's tree, including in the classes, objects and methods
  // in it. Without a tree, neither they nor their trees can be found.
  private def transparentInlineDefs(cls: ClassSymbol): List[TermSymbol] =
    transparentInlineDefsCache(cls) {
      val defs = mutable.ListBuffer.empty[TermSymbol]
      cls.tree.foreach(new TreeTraverser {
        override def traverse(tree: Tree): Unit = {
          tree match {
            case d: DefDef if d.symbol.isTransparentInline => defs += d.symbol
            case _ => ()
          }
          super.traverse(tree)
        }
      }.traverse(_))
      defs.toList
    }

  private val definitionSpansCache = Memo[TermSymbol, Set[(Int, Int)]]()

  // The offsets of every tree in a method's definition
  private def definitionSpans(m: TermSymbol): Set[(Int, Int)] =
    definitionSpansCache(m) {
      val spans = mutable.Set.empty[(Int, Int)]
      m.tree.foreach(new TreeTraverser {
        override def traverse(tree: Tree): Unit = {
          if (!tree.pos.isUnknown) spans += ((tree.pos.startOffset, tree.pos.endOffset))
          super.traverse(tree)
        }
      }.traverse(_))
      spans.toSet
    }

  private val rootCallsCache = Memo[TermSymbol, List[TermSymbol]]()

  // The `transparent inline` methods that a method's body is a call to, like `foo` for `def bar() = foo(41)`
  private def rootCalls(m: TermSymbol)(using ctx: Context): List[TermSymbol] =
    rootCallsCache(m) {
      @annotation.tailrec
      def callee(tree: Tree): Set[Symbol] =
        tree match {
          case Typed(expr, _) => callee(expr)
          case Block(Nil, expr) => callee(expr)
          case Apply(fun, _) => callee(fun)
          case TypeApply(fun, _) => callee(fun)
          case _ => Symbols.fromTree(tree)
        }

      m.tree.collect { case d: DefDef => d.rhs }.flatten.toList
        .flatMap(callee)
        .collect { case t: TermSymbol if t.isTransparentInline && t != m => t }
    }

  /**
   * The methods on a chain of `transparent inline` methods whose bodies are calls to the next one, starting from `m`,
   * that ends with one with a tree at `pos`, not counting `m`. When the last one reduces to a constant, the expansion
   * of a call to `m` is just that constant, positioned in the last one's definition.
   */
  private def constantChain(m: TermSymbol, pos: SourcePosition)(using ctx: Context): List[TermSymbol] = {
    // Only the offsets, since the source file recorded for the constant is the outermost method's
    def matches(t: TermSymbol): Boolean = definitionSpans(t).contains((pos.startOffset, pos.endOffset))

    def go(t: TermSymbol, seen: Set[TermSymbol]): Option[List[TermSymbol]] =
      if (matches(t)) Some(List(t))
      else rootCalls(t).iterator.filterNot(seen.contains).flatMap(n => go(n, seen + n).map(t :: _)).nextOption()

    rootCalls(m).iterator.flatMap(n => go(n, Set(m, n))).nextOption().getOrElse(Nil)
  }

  // Calling a method takes an instance of its class, and of the classes around that, or a reference to the `object`s
  private def enclosingClasses(m: TermSymbol): List[ClassSymbol] = {
    @annotation.tailrec
    def go(acc: List[ClassSymbol], s: Symbol): List[ClassSymbol] =
      Option(s.owner) match {
        case Some(c: ClassSymbol) => go(c :: acc, c)
        case _ => acc
      }
    go(Nil, m)
  }

  /**
   * If this `Inlined` tree is the expansion of a `transparent inline` method, we consider the method used.
   *
   * `transparent inline` methods are expanded before TASTy is pickled, and the `Inlined` tree only records the
   * top-level class the method is defined in as its `caller`. The root of the expansion keeps the position of a tree in
   * the method's definition, though -- its body for a regular method, or the `${ ... }` splice for a macro -- so we look
   * for the `transparent inline` method in that class with a tree at exactly that position. Code from the call site,
   * like inline arguments, is nested in `Inlined` trees with no `caller`, so it doesn't interfere.
   *
   * Only the position's offsets are used, since the source file recorded for a macro expansion is the call site's. They
   * have to match exactly, because the root of a nested `transparent inline` call that reduces to a constant keeps the
   * position of a tree in the inner method's definition, which can be in another file. So when the root is a constant,
   * the methods in the class whose bodies are calls that lead to a method with a tree there are considered used too.
   *
   * The classes and `object`s the method is in are considered used too, since the expansion doesn't have to refer to
   * the instance or `object` it was called on.
   */
  private def transparentInlineReferences(expr: TermTree, caller: TypeTree)(using ctx: Context): EnvR[References] =
    EnvR.env.flatMap { env =>
      val pos = expr.pos
      val defs = Some(caller.toType)
        .collect { case t: TypeRef => t.optSymbol }
        .flatten
        .collect { case c: ClassSymbol if !pos.isUnknown && env.isInPackages(c) => c }
        .toList
        .flatMap(transparentInlineDefs)
      val direct = defs.filter(definitionSpans(_).contains((pos.startOffset, pos.endOffset)))
      val viaConstant = expr match {
        case _: Literal =>
          defs.flatMap(d => constantChain(d, pos) match {
            case Nil => Nil
            case chain => d :: chain
          })
        case _ => Nil
      }
      // An expansion inside one of those classes doesn't make it used
      (direct ++ viaConstant).distinct.foldMap(m =>
        References.fromSymbol(m, References.used) |+|
          enclosingClasses(m).filterNot(env.enclosing.contains).foldMap(References.fromSymbol(_, References.used))
      )
    }

  // The type arguments of a call to a non-`transparent` inline method, which can pass them on to an implicit search
  private def inlineTypeArgs(fun: TermTree, args: List[TypeTree])(using ctx: Context): EnvR[References] =
    if (Symbols.fromTree(fun).exists {
      case t: TermSymbol => t.isInline && !t.isTransparentInline
      case _ => false
    }) References.empty.map(_.copy(inlineTypeArgs = args.flatMap(a => Either.catchNonFatal(a.toType).toOption).toSet))
    else References.empty

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
        case c @ ClassDef(_, rhs, symbol) => Symbols.references(symbol) |+| within(symbol)(templateReferences(rhs, Some(c)))
        case DefDef(_, _, _, _, symbol) if Symbols.isSumMirrorOrdinal(symbol) => Symbols.references(symbol)
        case DefDef(name, paramLists, resultTpt, rhs, symbol) =>
          removeCaseClassIfSynthetic(symbol)(
            Symbols.references(symbol) |+|
              within(symbol)(paramLists.foldMap(e => referencesL(e.merge)) |+| references(resultTpt) |+| referencesO(rhs))
          )
        case ExplicitTypeBoundsTree(low, high) => references(low) |+| references(high)
        case Export(expr, selectors) => references(expr) |+| referencesL(selectors)
        case ExprPattern(expr) => references(expr)
        case i @ Ident(_) =>
          val syms = Symbols.getFromIdent(i)
          syms.toList.foldMap(Symbols.withStaticOwners(_).toList.foldMap(References.fromSymbol(_, References.used))) |+|
            References.inlineCalls(syms)
        case If(cond, thenPart, elsePart) => references(cond) |+| references(thenPart) |+| references(elsePart)
        // An import at the top of a file isn't a use of anything, since what it imports is referenced where it's used,
        // but one in a definition is, e.g. an `inline` method can import `given`s for the code it expands to
        case Import(expr, selectors) =>
          EnvR.env.flatMap(env =>
            if (env.enclosing.nonEmpty) references(expr) |+| referencesL(selectors)
            else (references(expr) |+| referencesL(selectors)).map(r => References.monoid.empty.copy(importRefs = r.used))
          )
        case ImportIdent(_) => References.empty
        case ImportSelector(imported, renamed, bound) => references(imported) |+| referencesO(renamed) |+| referencesO(bound)
        case InferredTypeBoundsTree(_) => References.empty
        case Inlined(expr, caller, bindings) =>
          references(expr) |+| referencesO(caller) |+| referencesL(bindings) |+|
            caller.fold(References.empty)(transparentInlineReferences(expr, _))
        case i @ InlinedTypeTree(caller, expansion) => Types.prefixReferences(i.toPrefix) |+| referencesO(caller) |+| references(expansion)
        case InlineIf(cond, thenPart, elsePart) => references(cond) |+| references(thenPart) |+| references(elsePart)
        case InlineMatch(selector, cases) => referencesO(selector) |+| referencesL(cases)
        case l @ Lambda(meth, tpt) => references(meth) |+| referencesO(tpt) |+| singleAbstractMethodReferences(l)
        case Literal(_) => References.empty
        case Match(selector, cases) => references(selector) |+| referencesL(cases)
        case MatchTypeTree(bound, selector, cases) => references(bound) |+| references(selector) |+| referencesL(cases)
        case NamedArg(_, arg) => references(arg)
        case NamedPattern(_, body) => references(body)
        case NamedTypeBoundsTree(_, _) => References.empty
        case New(tpt) => references(tpt)
        case OpaqueTypeAliasDefinitionTree(bounds, alias) => references(bounds) |+| references(alias)
        case PackageDef(pid, stats) => Symbols.references(pid) |+| referencesL(stats)
        case PolyTypeDefinitionTree(tparams, body) => referencesL(tparams) |+| references(body)
        case Quote(body, bodyType) => references(body) |+| quotedTypeParamReferences(List(body), List(bodyType))
        case QuotePattern(bindings, body, quotes, patternType) =>
          referencesL(bindings) |+| references(body.merge) |+| references(quotes) |+|
            quotedTypeParamReferences(List(body.merge), List(patternType))
        case RefinedTypeTree(underlying, refinements, refinedCls) =>
          references(underlying) |+| referencesL(refinements) |+| Symbols.references(refinedCls)
        case Return(expr, from) => referencesO(expr) |+| Symbols.references(from)
        case s @ Select(qualifier, _) =>
          val syms = Symbols.getFromSelect(s)
          syms.toList.foldMap(References.fromSymbol(_, References.used)) |+| References.inlineCalls(syms) |+|
            references(qualifier)
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
        case TypeApply(fun, args) =>
          references(fun) |+| referencesL(args) |+| inlineTypeArgs(fun, args) |+|
            (if (isTypeOf(fun)) quotedTypeParamReferences(args, Nil) else References.empty)
        case TypeBindingsTree(bindings, body) => referencesL(bindings) |+| references(body)
        case TypeCaseDef(pattern, body) => references(pattern) |+| references(body)
        case Typed(expr, tpt) => references(expr) |+| references(tpt)
        case t @ TypeIdent(_) => Types.references(t.toType)
        case TypeLambdaTree(tparams, body) => referencesL(tparams) |+| references(body)
        case TypeMember(_, rhs, symbol) => within(symbol)(references(rhs))
        case TypeParam(_, bounds, _) => references(bounds)
        case TypeTest(body, tpt) => references(body) |+| references(tpt)
        case TypeTreeBind(_, body, _) => references(body)
        case t @ TypeWrapper(_) => Types.prefixReferences(t.toPrefix)
        // TODO - should implicits be added to References#used?
        case Unapply(fun, implicits, patterns) => references(fun) |+| referencesL(implicits) |+| referencesL(patterns)
        // An `object`'s module val is initialized with `new Foo$()`, which would make every nested `object` look used
        case ValDef(_, _, _, symbol) if symbol.isModuleVal => Symbols.references(symbol)
        case ValDef(_, tpt, rhs, symbol) =>
          removeCaseClassIfSynthetic(symbol)(Symbols.references(symbol) |+| within(symbol)(references(tpt) |+| referencesO(rhs)))
        case While(cond, body) => references(cond) |+| references(body)
        case WildcardPattern(_) => References.empty
        case WildcardTypeArgTree(bounds) => references(bounds)
      }
    }
}
