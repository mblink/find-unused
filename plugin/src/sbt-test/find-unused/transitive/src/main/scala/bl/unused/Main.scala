package bl.unused

// Entry points: a `main` method, an object that inherits one, and test classes (in src/test)
object Main {
  def main(args: Array[String]): Unit = println((Live.value, UsesShows.shown, LiveChainTop.top.show(None), Greeters.all.map(_.greet)))
}

object Script extends App {
  println(UsedByScript.value)
}

object Live {
  val value: Int = helper(1)

  def helper(i: Int): Int = i + 1
}

object UsedByScript {
  val value: Int = 2
}

// Only used through an import in an inline method, whose expansion summons its given after TASTy is pickled
object ImportedInInline {
  given shown: Show[Int] = _.toString
}

trait Show[A] {
  def show(a: A): String
}

object Shows {
  inline def show(a: Int): String = {
    import ImportedInInline.given
    scala.compiletime.summonInline[Show[Int]].show(a)
  }
}

object UsesShows {
  val shown: String = Shows.show(1)
}

object Summon {
  inline def show[A]: Show[A] = scala.compiletime.summonInline[Show[A]]
}

// `ChainBase.base` is only summoned by `ChainTop.top`'s expansion, which only the bytecode shows, and `ChainTop` is
// never used, so both are unused
object ChainBase {
  given base: Show[Long] = _.toString
}

object ChainTop {
  given top: Show[Option[Long]] = {
    import ChainBase.given
    val s = Summon.show[Long]
    _.fold("")(s.show)
  }
}

// The same, but used by `Main`
object LiveChainBase {
  given base: Show[Short] = _.toString
}

object LiveChainTop {
  given top: Show[Option[Short]] = {
    import LiveChainBase.given
    val s = Summon.show[Short]
    _.fold("")(s.show)
  }
}

object UsedByTests {
  def check: Boolean = true
}

// Never used, but each member is used by the other
object DeadCluster {
  def a(i: Int): Int = if (i > 0) b(i - 1) else 0
  def b(i: Int): Int = a(i)
}

// Never used, but recursive
object Recursive {
  def countdown(n: Int): Int = if (n <= 0) 0 else countdown(n - 1)
}

// Only used by code that's never used
object OnlyUsedByDeadCode {
  def value: Int = 3
}

object DeadCode {
  def usesIt: Int = OnlyUsedByDeadCode.value
}

// Never instantiated, so its `toString` override doesn't count as a use of `Format`
class NeverInstantiated {
  override def toString: String = Format.render(1)
}

object Format {
  def render(i: Int): String = i.toString
}

// A trait that's only implemented by an object that's never used
trait Runner {
  def run(): Int
}

object UnusedRunner extends Runner {
  def run(): Int = Live.value
}

// `Greeter.greet` is used, but only `LiveGreeter` and the anonymous and given implementations are ever created, so
// `DeadGreeter`, and `Greeting` which only it uses, are unused
trait Greeter {
  def greet: String
}

class LiveGreeter extends Greeter {
  def greet: String = "hi"
}

class DeadGreeter extends Greeter {
  def greet: String = Greeting.text
}

object Greeting {
  def text: String = "hello"
}

object Greeters {
  val all: List[Greeter] = List(new LiveGreeter, new Greeter { def greet: String = Anonymous.text }, GivenGreeter.given_Greeter)
}

object Anonymous {
  def text: String = "anon"
}

object GivenGreeter {
  given Greeter with {
    def greet: String = FromGiven.text
  }
}

object FromGiven {
  def text: String = "given"
}
