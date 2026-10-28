// Prints the union of the `Test / fullClasspath` that sbt last exported for each project of a build, one per line.
//
// Usage: scala-cli run Classpath.scala -- <build root> [--scala <version>]
//
// Compile the build's tests first. sbt 1 writes each project's to
// `<project>/target/streams/test/fullClasspath/_global/streams/export`, separated by the path separator. sbt 2 writes
// it to `target/out/jvm/<scala version>/<project>/streams/test/fullClasspath/_global/streams/export` as `List(...)`,
// with `${OUT}`, `${BASE}` and `${CSR_CACHE}` for the build's output directory, the build root and coursier's cache,
// and a content hash after each jar it built. Entries that don't exist are reported on stderr. sbt 2 keeps the output
// of each Scala version the build was compiled with, and combining them puts two copies of each class on the
// classpath, so when there's more than one it says so; pass `--scala <version>` to keep one.
import java.io.File
import java.nio.file.{Files, Paths}

private val exportDir = List("streams", "test", "fullClasspath", "_global", "streams").mkString(File.separator)
private val scalaVersionDir = raw"[/\\]jvm[/\\]scala-([^/\\]+)[/\\]".r

private def coursierCache(): String =
  sys.env.get("COURSIER_CACHE").filter(_.nonEmpty).getOrElse {
    val home = sys.props("user.home")
    if (sys.props("os.name").startsWith("Mac")) s"$home/Library/Caches/Coursier/v1" else s"$home/.cache/coursier/v1"
  }

private def exportEntries(line: String, root: String): List[String] = {
  val l = line.trim
  if (!l.startsWith("List(")) l.split(File.pathSeparator).filter(_.nonEmpty).toList
  else {
    val placeholders = List(
      "${OUT}" -> s"$root${File.separator}target${File.separator}out",
      "${BASE}" -> root,
      "${CSR_CACHE}" -> coursierCache()
    )
    l.stripPrefix("List(").dropRight(1).split(", ").toList.map { e =>
      placeholders.foldLeft(e.split(">", 2).head)((acc, kv) => acc.replace(kv._1, kv._2))
    }
  }
}

private def exportScalaVersion(path: String): Option[String] = scalaVersionDir.findFirstMatchIn(path).map(_.group(1))

private def findExports(d: File): List[File] = {
  val kids = Option(d.listFiles).fold(Array.empty[File])(identity)
  val here = if (d.getPath.endsWith(exportDir) && kids.exists(f => f.isFile && f.getName == "export")) List(new File(d, "export")) else Nil
  here ++ kids
    .filter(f => f.isDirectory && !Files.isSymbolicLink(f.toPath) && !f.getName.startsWith(".") && f.getName != "node_modules")
    .sortBy(_.getName)
    .flatMap(findExports)
}

@main def classpath(args: String*): Unit = {
  val root = Paths.get(args.head).toAbsolutePath.normalize.toString
  val only = args.drop(1) match { case Seq("--scala", v, _*) => Some(v); case _ => None }
  var exports = findExports(new File(root))
  val versions = exports.flatMap(e => exportScalaVersion(e.getPath)).distinct.sorted
  if (only.nonEmpty) exports = exports.filter(e => exportScalaVersion(e.getPath).forall(only.contains))
  else if (versions.size > 1) System.err.println(s"exports for Scala ${versions.mkString(", ")}; pass --scala <version> to keep one")
  if (exports.isEmpty) {
    System.err.println(s"no exported Test / fullClasspath under $root; compile the build's tests first")
    sys.exit(1)
  }
  val seen = collection.mutable.Set.empty[String]
  for (path <- exports) {
    val lines = Files.readAllLines(path.toPath).toArray.map(_.toString).toList
    for (e <- exportEntries(lines.lastOption.getOrElse(""), root) if seen.add(e)) {
      if (!new File(e).exists) System.err.println(s"missing: $e")
      println(e)
    }
  }
}
