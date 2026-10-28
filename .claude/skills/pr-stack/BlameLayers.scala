// Shows which PR in a stack introduced the lines a change touches, to decide which PR to fold it into.
//
// Usage: scala-cli run BlameLayers.scala -- <layers file> <change base> <change head> [paths...]
//
// <layers file> lists the stack's branches bottom up, one per line, starting with the branch the stack is based on
// (e.g. `main`). For each hunk of `git diff <change base> <change head>`, it prints the layers that last touched the
// lines the hunk replaces, from blaming <change base>.
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scala.sys.process.*
import scala.util.Try

private val blameLine = raw"(?m)^([0-9a-f]{40}) \d+ \d+".r
private val hunkHeader = raw"(?m)^@@ -(\d+)(?:,(\d+))? \+\d+(?:,\d+)? @@(.*)$$".r

// Runs git, returning its output, or None if it failed
private def git(args: String*): Option[String] =
  Try(Process("git" +: args).!!(ProcessLogger(_ => ()))).toOption

@main def blameLayers(layersFile: String, base: String, head: String, paths: String*): Unit = {
  val layers = Files.readAllLines(Paths.get(layersFile)).asScala.map(_.trim).filter(_.nonEmpty).toList
  val layerOf = layers.zip(layers.drop(1)).flatMap { (below, layer) =>
    git("rev-list", s"$below..$layer").get.split("\\s+").filter(_.nonEmpty).map(_ -> layer)
  }.toMap
  for (path <- git(("diff" +: "--name-only" +: base +: head +: "--" +: paths)*).get.split("\\s+").filter(_.nonEmpty)) {
    git("blame", "--line-porcelain", base, "--", path) match {
      case None => println(s"== $path: new file")
      case Some(blame) =>
        val owners = blameLine.findAllMatchIn(blame).map(m => layerOf.getOrElse(m.group(1), layers.head)).toVector
        println(s"== $path")
        for (h <- hunkHeader.findAllMatchIn(git("diff", "-U0", base, head, "--", path).get)) {
          val (start, count) = (h.group(1).toInt, Option(h.group(2)).fold(1)(_.toInt))
          val touched = if (count > 0) owners.slice(start - 1, start - 1 + count) else owners.slice(start - 1, start)
          val counts = touched.groupMapReduce(identity)(_ => 1)(_ + _)
          println(s"  -$start,$count: ${counts.map((k, v) => s"'$k': $v").mkString("{", ", ", "}")} ${h.group(3).trim.take(60)}")
        }
    }
  }
}
