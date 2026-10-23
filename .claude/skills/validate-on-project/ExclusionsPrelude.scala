import scala.util.matching.Regex
import scala.io.Source

// A `file:line:column` position, matched the way the CLI's `SourcePos` does
object SourcePos {
  def unapply(pos: String): Option[(String, Int, Int)] =
    pos.split(':') match {
      case Array(file, line, col) => line.toIntOption.zip(col.toIntOption).map((l, c) => (file, l, c))
      case _ => None
    }
}

case class FindUnusedExclusion(src: Option[Regex], sym: Option[Regex]) {
  private def srcMatches(pos: Option[String]): Boolean =
    (src, pos) match {
      case (Some(r), Some(SourcePos(f, _, _))) => r.findFirstIn(f).nonEmpty
      case (Some(r), Some(p)) => r.findFirstIn(p).nonEmpty
      case _ => true
    }
  def matches(name: String, pos: Option[String]): Boolean = srcMatches(pos) && sym.forall(_.findFirstIn(name).nonEmpty)
  def label: String = List("src" -> src, "sym" -> sym).flatMap((k, v) => v.map(r => s"$k=$r")).mkString(" & ")
}
object FindUnusedExclusion {
  def apply(src: Regex): FindUnusedExclusion = new FindUnusedExclusion(src = Some(src), sym = None)
  def apply(sym: Regex)(implicit d: DummyImplicit): FindUnusedExclusion = new FindUnusedExclusion(src = None, sym = Some(sym))
  def apply(src: Regex, sym: Regex): FindUnusedExclusion = new FindUnusedExclusion(src = Some(src), sym = Some(sym))
}

// The CLI excludes a method's default values along with it: `Foo.bar(baz = ...)` is excluded when `Foo.bar` is
def defaultOf(name: String): Option[String] =
  Option.when(name.endsWith(" = ...)"))(name.take(name.lastIndexOf('(')))

extension (e: FindUnusedExclusion) {
  def matchesWithDefaults(name: String, pos: Option[String]): Boolean =
    e.matches(name, pos) || defaultOf(name).exists(e.matches(_, pos))
}

@main def excl(reported: String*): Unit =
  reported.foreach { file =>
    val syms = Source.fromFile(file).getLines().toList.map(_.split('\t')).map(a => (a(0), a.lift(1).filter(_.nonEmpty)))
    println(s"##### $file: ${syms.size} reported")
    exclusions.zipWithIndex.foreach { (e, i) =>
      val hits = syms.filter(e.matchesWithDefaults)
      println(f"[$i%2d] ${hits.size}%4d  ${e.label.take(150)}")
      if (hits.nonEmpty && hits.size <= 12) hits.foreach((n, p) => println(s"           $n  ${p.getOrElse("")}"))
    }
    val left = syms.filterNot((n, p) => exclusions.exists(_.matchesWithDefaults(n, p)))
    println(s"== not excluded: ${left.size}")
    left.foreach((n, p) => println(s"   $n  ${p.getOrElse("")}"))
  }
