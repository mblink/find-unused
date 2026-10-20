package bl.unused

trait Show[A] {
  def show(a: A): String
}

trait ShowInstances {
  given viaTrait: Show[Short] = _.toString
  given unusedViaTrait: Show[Float] = _.toString
}

object Show extends ShowInstances {
  given viaSummonAll: Show[Boolean] = _.toString
  given viaSummonFrom: Show[Char] = _.toString
  given viaSummonInline: Show[String] = identity(_)
  given viaExprSummon: Show[Int] = _.toString
  given viaQuotedSummonInline: Show[Byte] = _.toString
  given unused: Show[Double] = _.toString
}

given viaTopLevel: Show[Unit] = _ => "()"

given unusedTopLevel: Show[BigInt] = _.toString

object PrivateShows {
  private given viaPrivate: Show[BigDecimal] = _.toString
  private given unusedPrivate: Show[BigInt] = _.toString

  // Accessing a private member from another class, like this anonymous class, gives it an expanded name in bytecode
  val anon = new Object {
    def shown: Show[BigDecimal] = Summoners.withSummonInline[BigDecimal]
  }
}
