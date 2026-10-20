package bl.unused

object syntax {
  implicit final class IntOps(private val i: Int) extends AnyVal {
    def double: Int = i * 2
  }

  implicit final class UnusedOps(private val i: Int) extends AnyVal {
    def triple: Int = i * 3
  }
}

// Exporting an implicit class also exports its companion and its type, which are considered used along with the
// implicit conversion
object predef {
  export syntax.{IntOps, UnusedOps}
  export scala.concurrent.duration.DurationInt
}

object Use {
  import predef.*

  val doubled = 2.double
  val duration = 5.seconds
}
