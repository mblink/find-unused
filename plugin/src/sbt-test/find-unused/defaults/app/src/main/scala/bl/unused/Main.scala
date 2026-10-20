package bl.unused

// `retries` uses its default, and `timeout` is always passed
case class Config(name: String, retries: Int = 3, timeout: Int = 30)

// Only a macro reads `verbose`'s default
case class Flags(name: String, verbose: Boolean = false)

// `host` uses its default, `port` is always passed
class Client(val host: String = "localhost", val port: Int = 80)

// Called through the trait, using the default
trait Greeter {
  def greet(name: String = "world"): String
}

class EnglishGreeter extends Greeter {
  def greet(name: String): String = s"hello $name"
}

// A field with a default is reported like any other when nothing reads it
class Holder(val read: Int = 1, val unread: Int = 2)

// Never used, so only the class and method are reported, not their default values
class NeverCreated(val value: Int = 1)

object NeverCalled {
  def run(times: Int = 1): Int = times
}

object Main {
  def add(a: Int, b: Int = 1): Int = a + b
  def neverDefaulted(a: Int, b: Int = 2): Int = a * b

  @scala.annotation.unused // `main` is only an entry point in transitive mode
  def main(args: Array[String]): Unit = {
    println(args.toList)
    val config = Config("x", timeout = 10)
    val client = new Client(port = 8080)
    val greeter: Greeter = new EnglishGreeter
    println((add(1), neverDefaulted(1, 3), config.copy(name = "y"), client.host, client.port, greeter.greet()))
    println((Flags("f", true), Defaults.default[Flags](1)))
    println(new Holder().read)
  }
}
