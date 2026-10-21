package bl.unused

// Entry points, and what they're in, are used without being referenced
object RunsMain {
  def main(args: Array[String]): Unit = println(args.length)
}

object RunsApp extends App {
  println("app")
}
