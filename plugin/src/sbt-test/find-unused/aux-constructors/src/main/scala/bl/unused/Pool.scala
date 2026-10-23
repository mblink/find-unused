package bl.unused

import scala.annotation.unused

class Pool(val size: Int) {
  // Only used by an anonymous subclass
  def this(name: String) = this(name.length)

  // Kept on purpose
  @unused def this(size: Long) = this(size.toInt)

  // Only used by the companion
  private def this(sizes: List[Int]) = this(sizes.sum)
}

object Pool {
  def anon: Pool = new Pool("anon") {}
  def fromCompanion: Pool = new Pool(List(1, 2))
}
