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

  // Only expands its own method, which doesn't make it used
  object SelfExpanding {
    transparent inline def one(): Int = 1
    def two(): Int = one() + 1
  }

  transparent inline def unused(): Int = 2

  transparent inline def compound(s: String): String = s + "!"

  // Its body is a call to another `transparent inline` method that doesn't reduce to a constant
  transparent inline def nestedCall(s: String): String = compound(s)

  transparent inline def constant(x: Int): Int = x + 1

  // Their bodies are calls that lead to `constant`, which reduces to a constant positioned in `constant`'s body
  transparent inline def constantCall(): Int = constant(41)
  transparent inline def deepConstantCall(): Int = constantCall()

  // Also leads to `constant`, so it can't be told apart from `constantCall` and is considered used
  transparent inline def unusedConstantCall(): Int = constant(1)

  // Leads to a method in another file
  transparent inline def otherConstantCall(): Int = Others.three

  def withLocalClass(): Int = {
    class Local { transparent inline def inLocal(): Int = 5 }
    new Local().inLocal()
  }
}

trait Helpers {
  transparent inline def inTrait(): Int = 3
}

object WithHelpers extends Helpers
