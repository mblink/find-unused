package bl.unused

class Conn(val host: String, val port: Int) {
  // Used by `Main`
  def this(host: String) = this(host, 80)

  // Never used
  def this(port: Int) = this("localhost", port)
}

object Main {
  @scala.annotation.unused // `main` is only an entry point in transitive mode
  def main(args: Array[String]): Unit = println((new Conn(args.head).port, new Conn("a", 1).host))
}
