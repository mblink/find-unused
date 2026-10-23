package bl.unused

import scala.compiletime.{summonFrom, summonInline}
import scala.quoted.*

// A type class per way an `inline given` can be summoned after pickling, so each is checked on its own
trait ViaInline[A]
trait ViaFrom[A]
trait ViaMacro[A]
trait ViaCallSite[A]
trait ViaWrapper[A]
trait Parent[A]
trait Child[A] extends Parent[A]
trait Elem[A]
trait Container[A]
trait NeverSummonedEither[A]

object InlineGivens {
  inline given viaInline: ViaInline[Int] = new ViaInline[Int] {}
  inline given viaFrom: ViaFrom[Int] = new ViaFrom[Int] {}
  inline given viaMacro: ViaMacro[Int] = new ViaMacro[Int] {}
  inline given viaCallSite: ViaCallSite[Int] = new ViaCallSite[Int] {}
  inline given viaWrapper: ViaWrapper[Int] = new ViaWrapper[Int] {}

  // A search for `Parent` can find a `Child`
  inline given child: Child[Int] = new Child[Int] {}

  // Only summoned when `container` is expanded
  inline given elem: Elem[Int] = new Elem[Int] {}
  inline given container: Container[Int] = {
    summonInline[Elem[Int]]
    new Container[Int] {}
  }

  // Never summoned, although a macro mentions `Any`, which every class extends
  inline given neverSummonedEither: NeverSummonedEither[Int] = new NeverSummonedEither[Int] {}
}

object InlineSummoners {
  inline def viaInline[A]: ViaInline[A] = summonInline[ViaInline[A]]

  inline def viaFrom[A]: ViaFrom[A] = summonFrom { case v: ViaFrom[A] => v }

  inline def viaMacro[A]: ViaMacro[A] = ${ viaMacroImpl[A] }

  def viaMacroImpl[A: Type](using q: Quotes): Expr[ViaMacro[A]] = {
    import q.reflect.*
    if (TypeRepr.of[A] =:= TypeRepr.of[Any]) report.errorAndAbort("No ViaMacro[Any]")
    Expr.summon[ViaMacro[A]].getOrElse(report.errorAndAbort("No ViaMacro"))
  }

  inline def wrapper[T]: T = summonInline[T]

  inline def parent[A]: Parent[A] = summonInline[Parent[A]]

  inline def container[A]: Container[A] = summonInline[Container[A]]
}
