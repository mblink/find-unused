package bl.unused

import cats.syntax.foldable.*
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * What find-unused's compiler plugin recorded: the `inline given`s that each definition's code expands, which TASTy
 * doesn't show when an implicit search during inlining finds them. See `FindUnusedCompilerPlugin`.
 *
 * The plugin writes one file per top-level class, at the path of its TASTy file with a `.tsv` extension, with a line
 * `<definition>\t<inline given>` for each expansion.
 */
object InlineGivenRecords {
  // Added to the name of the class directory the compiler plugin writes next to by default
  private val suffix = "-find-unused"

  private def read(file: Path): List[(String, String)] =
    Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toList.flatMap(_.split('\t') match {
      case Array(definition, inlineGiven) => List((definition, inlineGiven))
      case _ => Nil
    })

  /**
   * The definitions that expand each `inline given`, by name, if every one of `classes` (paths of TASTy files, without
   * the extension) has a record in `dirs`, or the classes that don't
   */
  def apply(dirs: Seq[Path], classes: Set[String]): Either[Set[String], Map[String, Set[String]]] = {
    // A record next to a class directory is stale if the class isn't there anymore, e.g. because it moved to another
    // project, since the compiler deletes its TASTy file but not the record. A record elsewhere is taken as it is.
    def isCurrent(dir: Path, c: String): Boolean =
      Option(dir.getFileName).map(_.toString).filter(_.endsWith(suffix)).map(n => dir.resolveSibling(n.stripSuffix(suffix)))
        .filter(Files.isDirectory(_))
        .forall(classDir => Files.isRegularFile(classDir.resolve(c ++ ".tasty")))

    val files = classes.toList.map(c =>
      c -> dirs.collectFirst { case d if Files.isRegularFile(d.resolve(c ++ ".tsv")) && isCurrent(d, c) => d.resolve(c ++ ".tsv") }
    )
    files.collect { case (c, None) => c }.toSet match {
      case missing if missing.nonEmpty => Left(missing)
      // Only the records of classes that are analyzed, not of ones that were deleted since they were written
      case _ => Right(files.flatMap(_._2).flatMap(read).foldMap((d, g) => Map(g -> Set(d))))
    }
  }
}
