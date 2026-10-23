package bl.unused

object Lib {
  class Box[A]
}

object predef {
  export Lib.Box
}

trait Fold {
  def f[B](lb: Lib.Box[B]): Lib.Box[B]
}

// Overrides `Fold.f`, which tasty-query doesn't see because the parameter is written with an exported alias of a
// generic class, and is used through it
class FoldImpl extends Fold {
  def f[B](lb: predef.Box[B]): predef.Box[B] = lb
}

// Overloads of inherited methods, which nothing calls, so they're reported: their parameters erase to other types
final class Money(val cents: Long) extends AnyVal {
  def ==(that: Money): Boolean = cents == that.cents
}

class Shape {
  def scale(factor: Int): Shape = this
}

class Circle extends Shape {
  def scale(factor: Double): Circle = this
}

object Overloads {
  def main(args: Array[String]): Unit = {
    val fold: Fold = new FoldImpl
    println(fold.f(new Lib.Box[Int]))
    val shape: Shape = new Circle
    println(new Money(1).cents + shape.scale(1).hashCode)
  }
}
