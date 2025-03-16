package bondlink

object Linked {
  type Maybe[A, B] = B match {
    case Unit => A
    case _ => (A, B)
  }
}

abstract class Foo[L] {
  def test(i: Linked.Maybe[Int, L]): Linked.Maybe[Int, L] = i
}

object Bar extends Foo[String] {
  override def test(i: (Int, String)): (Int, String) = i
}
