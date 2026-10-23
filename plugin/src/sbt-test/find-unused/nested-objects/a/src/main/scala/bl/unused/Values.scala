package bl.unused

import scala.quoted.*

// Lists the `object`s extending a sealed type, which TASTy doesn't show, like enumeratum's `findValues`
object Values {
  inline def of[A]: List[A] = ${ ofImpl[A] }

  def ofImpl[A: Type](using q: Quotes): Expr[List[A]] = {
    import q.reflect.*
    Expr.ofList(TypeRepr.of[A].typeSymbol.children.filter(_.isTerm).map(c => Ref(c).asExprOf[A]))
  }
}
