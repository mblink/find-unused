package bl.unused

import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
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

    // "unused value of type ..." is the warning for a discarded value, which isn't about unused code
    private def isAboutUnusedCode(msg: String): Boolean =
      checks.exists(msg.contains) && !msg.startsWith("unused value")

    def unapply(tree: TermTree): Boolean =
      tree match {
        case Literal(Constant(s: String)) =>
          s.split('&').exists(_.split('=') match {
            case Array("msg", msg) => isAboutUnusedCode(msg.toLowerCase)
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

  // The compiler annotates a sealed class with each of its children, which doesn't make them used
  private def isChild(a: Annotation)(using ctx: Context): Boolean =
    Either.catchNonFatal(Symbols.name(a.symbol) == "scala.annotation.internal.Child").getOrElse(false)

  /**
   * Whether an annotation on `sym` can be listed as not needed. Not an `export` forwarder's, which is copied from what it
   * forwards to, and not a class parameter's or its field's: the compiler doesn't count a class passing its own field to
   * its constructor as a use, which find-unused can't tell apart, and its `-Wunused:nowarn` covers them.
   */
  private def isCheckedForNeed(sym: Symbol): Boolean =
    sym match {
      case t: TermSymbol =>
        !t.isExport && !t.isParamAccessor && (Option(t.owner) match {
          case Some(o: TermSymbol) => !o.isExport && !Symbols.isConstructor(o)
          case _ => true
        })
      case _ => true
    }

  /**
   * The references in the annotations of `sym` and the symbols that share its annotations: a constructor parameter and
   * its fields, and a default getter and its parameter.
   *
   * If one of them has an annotation that suppresses unused warnings -- `@unused`, or `@nowarn` with a `msg` filter that
   * mentions "unused" or "never used" -- they're all used, and recorded in `unusedAnnotations` so the annotation can be
   * listed if it isn't needed.
   */
  def checkForUnused(sym: Symbol)(using ctx: Context): EnvR[Refs] = {
    // If the symbol is a class constructor parameter, attempt to find the corresponding param accessor in the class
    val paramAccessorSyms = Symbols.constructorParamToParamAccessors(sym)
    // If the symbol is a param accessor, attempt to find the corresponding constructor param in the class
    val ctorParamSym = Symbols.paramAccessorToConstructorParam(sym)
    // If the symbol is a default getter, its parameter's annotations apply to it
    val defaultParamSym = Symbols.defaultGetterParam(sym).map(_._2)

    val allSyms = (Set(sym) ++ paramAccessorSyms ++ ctorParamSym ++ defaultParamSym).toList
    val annotated = allSyms.filter(_.annotations.exists(isUnusedAnnotation))

    allSyms.foldMap(s => Trees.within(s)(s.annotations.filterNot(isChild).foldMap(a => Trees.references(a.tree)))) |+| (
      if (annotated.isEmpty) References.empty
      else
        allSyms.foldMap(References.fromSymbol(_, References.root)) |+|
          (annotated.filter(isCheckedForNeed).traverse(Sym.fromSymbol), allSyms.traverse(Sym.fromSymbol)).mapN((as, covered) =>
            Refs(References(unusedAnnotations = as.map(_ -> covered.toSet).toMap))
          )
    )
  }
}
