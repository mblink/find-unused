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

  // An entry point, so it's used without being referenced
  def main(args: Array[String]): Unit = {
    println(args.toList)
    val config = Config("x", timeout = 10)
    val client = new Client(port = 8080)
    val greeter: Greeter = new EnglishGreeter
    println((add(1), neverDefaulted(1, 3), config.copy(name = "y"), client.host, client.port, greeter.greet()))
    println((Flags("f", true), Defaults.default[Flags](1)))
    println(new Holder().read)
    println((new Box().size(2), (new Box(): Sized).size(3), Lists.curried(1)(5), Lists.padded("a")(3), Lists.centered("b")(4)))
    println((Impl.run(), Api.run(2), Local.process(List(1)), ExportedApi.go()))
  }
}

// Always passed, whether `size` is called through the trait or the class
trait Sized {
  def size(scale: Int = 1): Int
}

class Box extends Sized {
  def size(scale: Int): Int = scale
}

// Defaults after other parameter lists: `b` is always passed and `c` never is
object Lists {
  def curried(a: Int)(b: Int = 2)(using c: Int = 3): Int = a + b + c

  extension (s: String) {
    def padded(width: Int = 10): String = s.padTo(width, ' ')
    // Excluded in build.sbt
    def centered(width: Int = 10): String = s.padTo(width, ' ')
  }
}

// An export forwards the method's default too, which can't be removed on its own
object Impl {
  def run(times: Int = 1): Int = times
}

object Api {
  export Impl.run
}

// A local method's default isn't reported, and neither is a local class's, which a macro could read without the
// bytecode scan seeing it
object Local {
  def process(xs: List[Int]): Int = {
    def loop(rest: List[Int], acc: Int = 0): Int = rest.foldLeft(acc)(_ + _)
    case class Settings(start: Int, step: Int = 1)
    loop(xs, Settings(0, 2).start)
  }
}

// Only called through the export, using the default
object ExportedImpl {
  def go(times: Int = 1): Int = times
}

object ExportedApi {
  export ExportedImpl.go
}
