package bl.unused

object Foo {
  def test(erased @annotation.nowarn("msg=unused") i: Int): Unit = ()
}
