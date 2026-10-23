package bl.unused

import cats.syntax.either.*
import scala.collection.mutable
import tastyquery.Contexts.Context
import tastyquery.Symbols.*
import tastyquery.SymbolsOps.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*
import tastyquery.Types.*

/**
 * The classes that implicit searches performed after TASTy is pickled might look for.
 *
 * When a non-`transparent` inline method is expanded, `summonFrom`, `summonInline`, `summonAll` and macros'
 * `Expr.summon` search for givens after TASTy is written. A `given` they find usually still shows up in the bytecode
 * (see `Bytecode`), but an `inline given` is expanded where it's summoned and leaves no trace at all. We can't tell
 * which `inline given` a search found without reimplementing inlining, so instead we find the type classes those
 * searches look for, by walking the bodies of the inline methods the analyzed code calls and the macro
 * implementations they splice in, and by looking at the type arguments those calls pass. An `inline given` of one of
 * those type classes is considered possibly used.
 */
object InlineSummons {
  // Guards against walking an unreasonable amount of library code
  private val maxMethods = 50000

  private def isCompiletimeOrQuoted(sym: Symbol): Boolean = {
    val name = Symbols.name(sym)
    name.startsWith("scala.compiletime.") || name.startsWith("scala.quoted.")
  }

  // Library code that macro implementations call, which never performs implicit searches for the macro
  private def isPlatform(sym: Symbol): Boolean = {
    val name = Symbols.name(sym)
    name.startsWith("scala.") || name.startsWith("java.")
  }

  private def isInlineMethod(sym: Symbol): Boolean =
    sym match {
      case t: TermSymbol => t.isInline && t.isMethod
      case _ => false
    }

  // Every class extends these, so finding one of them, e.g. in `TypeRepr.of[Any]`, says nothing about a search
  private def universalClasses(using ctx: Context): Set[ClassSymbol] =
    Set(ctx.defn.AnyClass, ctx.defn.MatchableClass, ctx.defn.ObjectClass, ctx.defn.ProductClass) ++
      List("java.io.Serializable", "scala.Equals").flatMap(n => Either.catchNonFatal(ctx.findTopLevelClass(n)).toOption)

  // The classes a type mentions, including through a type alias it's an application of
  private def classesOf(tpe: Type)(using ctx: Context): Set[ClassSymbol] =
    Either.catchNonFatal((Types.symbols(tpe) ++ Types.symbols(tpe.dealias)).collect { case c: ClassSymbol => c })
      .getOrElse(Set.empty)

  /** The classes `types` mention, including through type aliases they're applications of, other than universal ones */
  def classes(types: Iterable[Type])(using ctx: Context): Set[ClassSymbol] =
    types.iterator.flatMap(classesOf).toSet -- universalClasses

  private def isQuotedRuntime(fun: Tree, method: String)(using ctx: Context): Boolean =
    Symbols.fromTree(fun).exists(s => Symbols.name(s) == s"scala.quoted.runtime.Expr.$method")

  /**
   * @param roots the inline methods called by the analyzed code
   * @return the classes they might summon, other than universal ones, or `None` when there are too many methods to walk,
   *         in which case any class might be summoned
   */
  def summonedClasses(roots: Set[TermSymbol])(using ctx: Context): Option[Set[ClassSymbol]] = {
    val found = mutable.Set.empty[ClassSymbol]
    val visited = mutable.Set.empty[(TermSymbol, Boolean)]
    val queue = mutable.Queue.empty[(TermSymbol, Boolean)]
    var truncated = false

    // `inMacro` means the method runs at compile time, as part of a macro, so any method it calls can summon too
    def enqueue(sym: TermSymbol, inMacro: Boolean): Unit =
      if (!visited.contains((sym, inMacro))) {
        if (visited.size >= maxMethods) truncated = true
        else {
          visited += ((sym, inMacro))
          queue += ((sym, inMacro))
        }
      }

    def addTypes(tree: Tree): Unit =
      new TreeTraverser {
        override def traverse(tree: Tree): Unit = {
          tree match {
            case t: TypeTree => Either.catchNonFatal(t.toType).foreach(found ++= classesOf(_))
            case _ => ()
          }
          super.traverse(tree)
        }
      }.traverse(tree)

    def walk(body: Tree, inMacro: Boolean): Unit =
      new TreeTraverser {
        private var macroCode = inMacro

        private def withMacroCode(value: Boolean)(traverseBody: => Unit): Unit = {
          val outer = macroCode
          macroCode = value
          traverseBody
          macroCode = outer
        }

        override def traverse(tree: Tree): Unit =
          tree match {
            // `summonInline[T]`, `summonAll[T]`, `Expr.summon[T]`, `Type.of[T]`, etc., and inline methods, which can
            // pass their type arguments on to those
            case TypeApply(fun, args) if Symbols.fromTree(fun).exists(s => isCompiletimeOrQuoted(s) || isInlineMethod(s)) =>
              args.foreach(addTypes)
              super.traverse(tree)

            // `summonFrom` is pickled as an `InlineMatch` without a selector
            case InlineMatch(None, cases) =>
              cases.foreach(c => addTypes(c.pattern))
              super.traverse(tree)

            // The code in a splice runs at compile time, but the code in a quote runs wherever the macro expands it
            case Splice(expr, _) => withMacroCode(true)(traverse(expr))
            case Quote(body, _) => withMacroCode(false)(traverse(body))

            // Scala 3.3, which most libraries are still compiled with, pickles splices and quotes as these calls
            case Apply(TypeApply(fun, _), List(expr)) if isQuotedRuntime(fun, "splice") =>
              withMacroCode(true)(traverse(expr))
            case Apply(Apply(TypeApply(fun, _), _), List(expr)) if isQuotedRuntime(fun, "nestedSplice") =>
              withMacroCode(true)(traverse(expr))
            case Apply(TypeApply(fun, _), List(body)) if isQuotedRuntime(fun, "quote") =>
              withMacroCode(false)(traverse(body))

            case _: (Ident | Select) =>
              Symbols.fromTree(tree).foreach {
                case t: TermSymbol if t.isInline => enqueue(t, macroCode)
                case t: TermSymbol if macroCode && t.isMethod && !isPlatform(t) => enqueue(t, true)
                case _ => ()
              }
              super.traverse(tree)

            case _ => super.traverse(tree)
          }
      }.traverse(body)

    // Sorted so that what's cut off when there are too many methods doesn't depend on hash codes
    roots.toList.sortBy(Symbols.name).foreach(enqueue(_, false))
    while (queue.nonEmpty) {
      val (sym, inMacro) = queue.dequeue()
      // A body tasty-query can't read is skipped, which can make an `inline given` it summons look unused, so say so
      Either.catchNonFatal(sym.tree.foreach {
        case d: DefDef => d.rhs.foreach(walk(_, inMacro))
        case v: ValDef => v.rhs.foreach(walk(_, inMacro))
        case _ => ()
      }).leftMap(e => println(s"[find-unused] Failed to read the body of ${Symbols.name(sym)}: ${e.getMessage}")): Unit
    }

    Option.unless(truncated)(found.toSet -- universalClasses)
  }

  private def resultType(tpe: TypeOrMethodic): TypeOrMethodic =
    tpe match {
      case m: MethodType => resultType(m.resultType)
      case p: PolyType => resultType(p.resultType)
      case t => t
    }

  /** Whether a search for one of the `summoned` classes, from `classes` or `summonedClasses`, might find `inlineGiven` */
  def mightFind(summoned: Set[ClassSymbol], inlineGiven: TermSymbol)(using ctx: Context): Boolean =
    // A search for `Decoder[A]` can find a given of a subclass, like `ConfiguredDecoder[A]`
    Either.catchNonFatal(resultType(inlineGiven.declaredType) match {
      case t: Type => t.dealias.instanceClass.toList.flatMap(c => Either.catchNonFatal(c.linearization).getOrElse(List(c)))
      case _ => Nil
    }).getOrElse(Nil).exists(summoned.contains)
}
