package bl.unused

import scala.annotation.{nowarn, unused}

trait Base { def run(x: Int): Int }

object Annotated extends Base {
  // Needed: nothing refers to these
  @unused private def keptForLater: Int = 1
  def withParam(@nowarn("msg=unused") x: Int): Int = List(1).sum
  // Needed: `y` is used, but nothing uses its default
  def withDefault(@unused y: Int = 1): Int = y
  // Needed: an override's parameter isn't referenced, just required by its signature
  def run(@nowarn("msg=unused") x: Int): Int = 1
  // Needed: only it calls itself
  @unused private def countdown(n: Int): Int = if (n > 0) countdown(n - 1) else 0
  // Needed, as far as find-unused can tell: it's only used through its expansion, which the compiler may not count
  @unused transparent inline def inlined(): Int = 3

  // Not needed: these are used
  @unused def stale: Int = 2
  def staleParam(@nowarn("msg=unused") y: Int): Int = y + 1

  // Silences a discarded value, not unused code, so it doesn't keep this used, and is reported
  @nowarn("msg=unused value of type")
  def discards: Int = List(1).sum
}

// A class parameter's annotation isn't checked: the compiler doesn't count the class passing its own field to its
// constructor, as `copy` does here, as a use
class Holder(@unused val field: Int) {
  def copy: Holder = new Holder(field)
}

// The forwarder `forwarded` gets `n`'s annotation, which belongs to `Impl.forwarded`
object Impl {
  def forwarded(@nowarn("msg=unused") n: Int): Int = List(1).sum
}
export Impl.forwarded

@main def main(): Unit =
  println(Annotated.stale + Annotated.staleParam(1) + Annotated.withParam(1) + Annotated.withDefault(2) + Annotated.run(1) + Annotated.inlined() + Holder(1).copy.field + forwarded(1) + MoreAnnotated.run)

object MoreAnnotated {
  // Needed: nothing refers to these
  @unused val keptVal: Int = 5
  @unused object KeptObject

  // Not needed: these are used
  @unused val usedVal: Int = 6
  @unused object UsedObject { def x = 1 }
  def lambdaParam(@nowarn("msg=unused") n: Int): List[Int] = List(1).map(_ + n)

  def withLocals(): Int = {
    // Needed, and not needed
    @unused def keptLocal = 1
    @unused def usedLocal = 2
    usedLocal
  }

  // Not needed by default, since `deadCaller` refers to it, and in transitive mode too, although `deadCaller` is unused
  @unused def onlyDeadUse: Int = 4
  def deadCaller: Int = onlyDeadUse

  def run: Int = usedVal + UsedObject.x + lambdaParam(1).sum + withLocals()
}
