package bl.unused

object Outer {
  object UsedInner { def x = 1 }
  object UnusedInner { def y = 2 }
  case object UnusedCase
  def use = UsedInner.x
}

class Holder {
  object UsedInClass { def y = 2 }
  object UnusedInClass
}

def local(): Int = {
  object UsedLocal { def z = 3 }
  object UnusedLocal
  UsedLocal.z
}

// The compiler annotates a sealed type with its children, and makes its companion its `Mirror.Sum`, whose `ordinal`
// method tests for each child, but neither uses them
sealed trait Shape
object Shape {
  case object Circle extends Shape
  case object UnusedSquare extends Shape
  case class UnusedPolygon(sides: Int) extends Shape
}

sealed trait Plain
case object UsedPlain extends Plain
case object UnusedPlain extends Plain

// Only `Values.of` refers to these, after TASTy is pickled
sealed trait Priority
object Priority {
  val values = Values.of[Priority]
  case object High extends Priority
  case object Low extends Priority
}

// Implements an abstract `val`, which `Controller.go` reads
abstract class Controller {
  val Context: Runner
  def go(i: Int): Int = Context.run(i)
}
trait Runner { def run(i: Int): Int }
object Impl extends Controller {
  object Context extends Runner { def run(i: Int) = i + 1 }
}

// `Section.empty` is excluded
object Config {
  object Section { lazy val empty: Int = 0 }
}

// Only referred to by its class, e.g. to look it up by reflection
object Reflected

@main def main(): Unit = {
  val shape: Shape = Shape.Circle
  val plain: Plain = UsedPlain
  println((Outer.use, new Holder().UsedInClass.y, local(), shape, plain, Priority.values, Impl.go(1), classOf[Reflected.type]))
}
