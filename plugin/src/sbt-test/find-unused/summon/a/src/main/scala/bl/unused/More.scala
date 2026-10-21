package bl.unused

class InClass {
  given inClass: Show[Option[Int]] = _.fold("")(_.toString)

  def shown: String = Summoners.withSummonInline[Option[Int]].show(Some(1))
}

object Outer {
  object Inner {
    given nested: Show[Option[Long]] = _.fold("")(_.toString)

    def shown: String = Summoners.withSummonInline[Option[Long]].show(Some(1L))
  }
}

object DefGivens {
  given listShow[A](using s: Show[A]): Show[List[A]] = _.map(s.show).mkString

  implicit def vectorShow[A](implicit s: Show[A]): Show[Vector[A]] = _.map(s.show).mkString

  implicit val setShow: Show[Set[Int]] = _.mkString

  def shown: String =
    Summoners.withSummonInline[List[Int]].show(Nil) +
      Summoners.withSummonInline[Vector[Int]].show(Vector()) +
      Summoners.withSummonInline[Set[Int]].show(Set())
}

// A subclass's private given with the same name is a different member, so its use doesn't keep this one
class PrivateBase {
  private given shared: Show[Char] = _.toString
}

class PrivateSub extends PrivateBase {
  private given shared: Show[Char] = _.toString

  def shown: String = Summoners.withSummonInline[Char].show('a')
}

// Mixed into a class, which gets a forwarder for each given that calls the trait's implementation, but that doesn't
// make them used
trait ClassShows {
  given mixedUsed: Show[Seq[Byte]] = _.mkString
  given mixedUnused: Show[Seq[Short]] = _.mkString
}

class MixesShows extends ClassShows {
  def shown: String = Summoners.withSummonInline[Seq[Byte]].show(Nil)
}
