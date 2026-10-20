package bl.unused

import scala.quoted.*

object Defaults {
  // Reads the default value of a case class's constructor parameter, like a JSON decoder filling in a missing field.
  // The reference to the default getter is only in the expansion, so only the bytecode shows it.
  inline def default[A](inline index: Int): Any = ${ defaultImpl[A]('index) }

  def defaultImpl[A: Type](index: Expr[Int])(using q: Quotes): Expr[Any] = {
    import q.reflect.*
    val companion = TypeRepr.of[A].typeSymbol.companionModule
    val getter = companion.methodMember("$lessinit$greater$default$" + (index.valueOrAbort + 1)).head
    Ref(companion).select(getter).asExpr
  }
}
