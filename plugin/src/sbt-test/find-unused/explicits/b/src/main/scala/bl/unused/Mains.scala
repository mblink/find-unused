package bl.unused

// Entry points, and what they're in, are used without being referenced
object RunsMain {
  def main(args: Array[String]): Unit = println(args.length)
}

object RunsApp extends App {
  println("app")
}

// Nested entry points too
object Launchers {
  object NestedMain {
    def main(args: Array[String]): Unit = println(args.length)
  }

  object NestedApp extends App {
    println("nested app")
  }
}
