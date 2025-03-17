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

  private def ctorParamToParamAccessor(ctorParam: Symbol)(using ctx: Context): Option[Symbol] =
    Option(ctorParam.owner)
      .collect { case t: TermSymbol if Symbols.isConstructor(t) => t } // t is the class constructor
      .flatMap(t => Option(t.owner))
      .collect { case c: ClassSymbol => c } // c is the class
      .flatMap(_.getMember(ctorParam.name)) // the result of getMember is the param accessor

  private def paramAccessorToCtorParam(paramAccessor: Symbol)(using ctx: Context): Option[Symbol] =
    Option(paramAccessor)
      .collect { case t: TermSymbol if t.isParamAccessor => t }
      .flatMap(t => Option(t.owner))
      .collect { case c: ClassSymbol => c } // c is the parent class
      .flatMap(_.declarations.find(Symbols.isConstructor))
      .collect { case t: TermSymbol => t.tree } // t is the class constructor
      .flatten
      .collect { case d: DefDef => d.paramLists.flatMap(_.fold(identity, _ => Nil)) } // d is the constructor tree
      .flatMap(_.collectFirst { case v if v.name == paramAccessor.name => v.symbol }) // v is a constructor param

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
    val paramAccessorSym = ctorParamToParamAccessor(sym)
    // If the symbol is a param accessor, attempt to find the corresponding constructor param in the class
    val ctorParamSym = paramAccessorToCtorParam(sym)

    // Check all relevant symbols for unused annotations
    val allSyms = (Set(sym) ++ paramAccessorSym ++ ctorParamSym).toList
    val hasUnusedAnnotation = allSyms.exists(_.annotations.exists(isUnusedAnnotation))

    allSyms.foldMap(_.annotations.foldMap(a => Trees.references(a.tree))) |+| (
      // If any relevant symbol has an unused annotation, consider them all used
      if (hasUnusedAnnotation) allSyms.foldMap(References.fromSymbol(_, References.used))
      else References.empty
    )
  }
}
