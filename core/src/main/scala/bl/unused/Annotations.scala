package bl.unused

import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import tastyquery.Annotations.*
import tastyquery.Constants.*
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.Symbols.*
import tastyquery.Trees.*

object Annotations {
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

  private def isUnusedAnnotation(a: Annotation)(using ctx: Context): Boolean =
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

  /**
   * Check the given Symbol for annotations that suppress unused warnings
   *
   * This includes two variants:
   *
   *   1. An annotation that's an instance of `scala.annotation.unused` annotation
   *   2. An annotation that's an instance of `scala.annotation.nowarn` with a `msg` filter that mentions
   *      "unused" or "never used"
   *
   * If a matching annotation is found, we consider the symbol used.
   */
  def checkForUnused(sym: Symbol)(using ctx: Context): EnvR[References] = {
    // If the symbol is a class constructor parameter, attempt to find the corresponding param accessor in the class
    val paramAccessorSyms = Symbols.constructorParamToParamAccessors(sym)
    // If the symbol is a param accessor, attempt to find the corresponding constructor param in the class
    val ctorParamSym = Symbols.paramAccessorToConstructorParam(sym)

    // Check all relevant symbols for unused annotations
    val allSyms = (Set(sym) ++ paramAccessorSyms ++ ctorParamSym).toList
    val hasUnusedAnnotation = allSyms.exists(_.annotations.exists(isUnusedAnnotation))

    allSyms.foldMap(_.annotations.foldMap(a => Trees.references(a.tree))) |+| (
      // If any relevant symbol has an unused annotation, consider them all used
      if (hasUnusedAnnotation) allSyms.foldMap(References.fromSymbol(_, References.used))
      else References.empty
    )
  }
}
