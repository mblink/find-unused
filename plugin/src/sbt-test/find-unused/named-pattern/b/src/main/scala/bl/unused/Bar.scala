package bl.unused

object Bar {
  def onYAxis(p: Point): Boolean = p match {
    case Point(x = Constants.origin) => true
    case _ => false
  }
}
