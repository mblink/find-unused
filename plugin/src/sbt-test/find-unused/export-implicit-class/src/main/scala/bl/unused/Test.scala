package bl.unused

object syntax {
  implicit final class IntOps(private val i: Int) extends AnyVal {
    def double: Int = i * 2
  }

  implicit final class UnusedOps(private val i: Int) extends AnyVal {
    def triple: Int = i * 3
  }

  implicit final class TypeOnlyOps(private val i: Int) extends AnyVal {
    def quadruple: Int = i * 4
  }
}

// Exporting an implicit class also exports its companion and its type, which are considered used along with the
// implicit conversion
object predef {
  export syntax.{IntOps, TypeOnlyOps, UnusedOps}
  export scala.concurrent.duration.DurationInt
}

object Use {
  import predef.*

  val doubled = 2.double
  val duration = 5.seconds

  // Only the exported type is used, which also marks the exported conversion used
  def typeOnly(o: TypeOnlyOps): Int = o.quadruple
  val viaType = typeOnly(new syntax.TypeOnlyOps(1))
}
