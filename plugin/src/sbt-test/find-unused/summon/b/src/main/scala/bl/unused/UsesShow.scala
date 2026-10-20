package bl.unused

import scala.compiletime.summonAll

object UsesShow {
  val viaSummonAll = summonAll[Show[Boolean] *: EmptyTuple]
  val viaSummonFrom = Summoners.withSummonFrom[Char]
  val viaSummonInline = Summoners.withSummonInline[String]
  val viaExprSummon = Summoners.withExprSummon[Int]
  val viaQuotedSummonInline = Summoners.withQuotedSummonInline
  val viaTrait = Summoners.withSummonInline[Short]
  val viaTopLevel = Summoners.withSummonInline[Unit]
}
