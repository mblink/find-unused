package bl.unused

import scala.compiletime.{summonFrom, summonInline}
import scala.quoted.*

object Summoners {
  inline def withSummonFrom[A]: Show[A] = summonFrom { case s: Show[A] => s }

  inline def withSummonInline[A]: Show[A] = summonInline[Show[A]]

  inline def withExprSummon[A]: Show[A] = ${ withExprSummonImpl[A] }

  def withExprSummonImpl[A: Type](using Quotes): Expr[Show[A]] =
    Expr.summon[Show[A]].getOrElse(quotes.reflect.report.errorAndAbort(s"No Show[${Type.show[A]}]"))

  inline def withQuotedSummonInline: Show[Byte] = ${ withQuotedSummonInlineImpl }

  def withQuotedSummonInlineImpl(using Quotes): Expr[Show[Byte]] = '{ summonInline[Show[Byte]] }
}
