package bl.unused

import scala.quoted.*

object Defs {
  transparent inline def plain(): Int = 1

  transparent inline def withParam(inline x: Int): Int = x + 1

  transparent inline def callSiteOnly(inline x: Int): Int = x

  transparent inline def viaMacro: Any = ${ viaMacroImpl }
  def viaMacroImpl(using Quotes): Expr[Any] = '{ "macro" }

  transparent inline def viaQuotedArg[A]: Any = ${ viaQuotedArgImpl[A]('{ identity[A] }) }
  def viaQuotedArgImpl[A: Type](f: Expr[A => A])(using Quotes): Expr[Any] = '{ $f }

  object Nested {
    transparent inline def nested(): String = "nested"
  }

  transparent inline def unused(): Int = 2
}
