package bl.unused

import cats.syntax.either.*
import scala.collection.mutable
import tastyquery.Contexts.Context
import tastyquery.Symbols.*
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
 * implementations they splice in. An `inline given` of one of those type classes is considered possibly used.
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

  private def classes(tpe: Type)(using ctx: Context): Set[ClassSymbol] =
    Types.symbols(tpe).collect { case c: ClassSymbol => c }

  private def symbolOf(tree: Tree)(using ctx: Context): Set[Symbol] =
    tree match {
      case i: Ident => Either.catchNonFatal(Symbols.getFromIdent(i)).getOrElse(Set.empty)
      case s: Select => Either.catchNonFatal(Symbols.getFromSelect(s)).getOrElse(Set.empty)
      case _ => Set.empty
    }

  /** @param roots the inline methods called by the analyzed code */
  def summonedClasses(roots: Set[TermSymbol])(using ctx: Context): Set[ClassSymbol] = {
    val found = mutable.Set.empty[ClassSymbol]
    val visited = mutable.Set.empty[TermSymbol]
    val queue = mutable.Queue.empty[(TermSymbol, Boolean)]

    // `inMacro` means the method runs at compile time, as part of a macro, so any method it calls can summon too
    def enqueue(sym: TermSymbol, inMacro: Boolean): Unit =
      if (visited.size < maxMethods && !visited.contains(sym)) {
        visited += sym
        queue += ((sym, inMacro))
      }

    def addTypes(tree: Tree): Unit =
      new TreeTraverser {
        override def traverse(tree: Tree): Unit = {
          tree match {
            case t: TypeTree => Either.catchNonFatal(t.toType).foreach(found ++= classes(_))
            case _ => ()
          }
          super.traverse(tree)
        }
      }.traverse(tree)

    def walk(body: Tree, inMacro: Boolean): Unit =
      new TreeTraverser {
        private var macroCode = inMacro

        override def traverse(tree: Tree): Unit =
          tree match {
            // `summonInline[T]`, `summonAll[T]`, `Expr.summon[T]`, `Type.of[T]`, etc.
            case TypeApply(fun, args) if symbolOf(fun).exists(isCompiletimeOrQuoted) =>
              args.foreach(addTypes)
              super.traverse(tree)

            // `summonFrom` is pickled as an `InlineMatch` without a selector
            case InlineMatch(None, cases) =>
              cases.foreach(c => addTypes(c.pattern))
              super.traverse(tree)

            case Splice(expr, _) =>
              val outer = macroCode
              macroCode = true
              traverse(expr)
              macroCode = outer

            case _: (Ident | Select) =>
              symbolOf(tree).foreach {
                case t: TermSymbol if t.isInline => enqueue(t, macroCode)
                case t: TermSymbol if macroCode && t.isMethod && !isPlatform(t) => enqueue(t, true)
                case _ => ()
              }
              super.traverse(tree)

            case _ => super.traverse(tree)
          }
      }.traverse(body)

    roots.foreach(enqueue(_, false))
    while (queue.nonEmpty) {
      val (sym, inMacro) = queue.dequeue()
      Either.catchNonFatal(sym.tree).toOption.flatten.foreach {
        case d: DefDef => d.rhs.foreach(walk(_, inMacro))
        case _ => ()
      }
    }

    found.toSet
  }

  private def resultType(tpe: TypeOrMethodic): TypeOrMethodic =
    tpe match {
      case m: MethodType => resultType(m.resultType)
      case p: PolyType => resultType(p.resultType)
      case t => t
    }

  /** Whether an implicit search for one of `summoned` might find the given `inline given` */
  def mightFind(summoned: Set[ClassSymbol], inlineGiven: TermSymbol)(using ctx: Context): Boolean = {
    val typeClasses = Either.catchNonFatal(resultType(inlineGiven.declaredType)).toOption.toList.flatMap {
      case a: AppliedType =>
        a.tycon match {
          case t: TypeRef => t.optSymbol.toList.flatMap {
            case c: ClassSymbol => List(c)
            case _ => classes(a).toList
          }
          case _ => classes(a).toList
        }
      case t: Type => classes(t).toList
      case _ => Nil
    }
    // A search for `Decoder[A]` can find a given of a subclass, like `ConfiguredDecoder[A]`
    typeClasses.exists(c => Either.catchNonFatal(c.linearization).getOrElse(List(c)).exists(summoned.contains))
  }
}
