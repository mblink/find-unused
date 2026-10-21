package bl.unused

class Conn(val host: String, val port: Int) {
  // Used by `Main`
  def this(host: String) = this(host, 80)

  // Never used, although its parameter has the same name as a field that's read
  def this(port: Int) = this("localhost", port)

  // Only used by `SecureConn`'s parent constructor call
  def this(host: String, secure: Boolean) = this(host, if (secure) 443 else 80)

  // Never used, although it calls a used constructor
  def this(secure: Boolean) = this("localhost", secure)
}

class SecureConn extends Conn("localhost", true)

// An auxiliary constructor's default that's always passed
class Sized(val n: Int) {
  def this(label: String, n: Int = 1) = this(label.length + n)
}

// Only the unused constructor is reported, not its default
class Rarely(val n: Int) {
  def this(d: Double = 1.0) = this(d.toInt)
}

object Main {
  @scala.annotation.unused // `main` is only an entry point in transitive mode
  def main(args: Array[String]): Unit =
    println((new Conn(args.head).port, new Conn("a", 1).host, new SecureConn().host, new Sized("x", 2).n, new Rarely(1).n, Pool.anon.size, Pool.fromCompanion.size))
}
