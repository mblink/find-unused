package bl.unused

import scala.compiletime.summonAll

object UsesInlineGivens {
  import InlineGivens.given

  val viaInline = InlineSummoners.viaInline[Int]
  val viaFrom = InlineSummoners.viaFrom[Int]
  val viaMacro = InlineSummoners.viaMacro[Int]
  val viaCallSite = summonAll[ViaCallSite[Int] *: EmptyTuple]
  val viaWrapper = InlineSummoners.wrapper[ViaWrapper[Int]]
  val viaParent = InlineSummoners.parent[Int]
  val viaContainer = InlineSummoners.container[Int]
}
