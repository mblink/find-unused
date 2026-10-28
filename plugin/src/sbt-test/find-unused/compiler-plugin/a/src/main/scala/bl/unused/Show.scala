package bl.unused

import scala.compiletime.summonInline

trait Show[A] {
  def show(a: A): String
}

object Show {
  inline def show[A](a: A): String = summonInline[Show[A]].show(a)
}

object Instances {
  inline given intShow: Show[Int] = (i: Int) => i.toString
  // Never expanded, though without the compiler plugin it's considered used, since `Show.show` summons a `Show`
  inline given stringShow: Show[String] = (s: String) => s
  // Only expanded by `Uses.unused`
  inline given booleanShow: Show[Boolean] = (b: Boolean) => b.toString
}
