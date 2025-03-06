package bondlink

import com.github.freva.asciitable.{AsciiTable, Column, HorizontalAlign}
import java.nio.file.Paths
import mainargs.{arg, main, ParserForMethods}
import scala.io.AnsiColor
import scala.jdk.CollectionConverters.*

object FindUnusedCli {
  private lazy val parser = ParserForMethods(this)

  private def red(s: String): String = AnsiColor.RED ++ s ++ AnsiColor.RESET

  @main
  def help(): Unit = println(parser.helpText())

  private def tableCol(name: String, width: Int): Column =
    new Column().header(name).maxWidth(math.max(width, 1)).dataAlign(HorizontalAlign.LEFT)

  @main
  def givens(
    @arg(short = 'd', doc = "Root directory of project") rootDirectory: Option[String],
    @arg(short = 'p', doc = "Packages to analyze") `package`: Seq[String],
    @arg(short = 'c', doc = "Classpath") classpath: Seq[String],
    @arg(short = 'w', doc = "Terminal width") width: Option[Int],
  ): Unit = {
    val givens = FindUnusedGivens.all(rootDirectory.map(Paths.get(_)), `package`, classpath.distinct.map(Paths.get(_)))
    val unused = givens.defined
      .filterNot((code, _) => givens.used.contains(code))
      .toList
      .map { case (_, (name, pos)) => (name, pos.getOrElse("")) }
      .sorted

    if (unused.lengthIs > 0) {
      val len = unused.length
      val header = red(s"Found $len unused given${if (len == 1) "" else "s"}")
      val tableWidth = width.map(_ - 3).filter(_ > 0).getOrElse(80)
      val instanceWidth = math.max(math.round(tableWidth / 3.0).toInt, 25)
      val locationWidth = tableWidth - instanceWidth
      val table = AsciiTable.getTable(
        AsciiTable.BASIC_ASCII_NO_DATA_SEPARATORS,
        unused.asJava,
        List(
          tableCol("Instance", instanceWidth).`with`[(String, String)](_._1),
          tableCol("Location", locationWidth).`with`[(String, String)](_._2),
        ).asJava
      )

      System.err.println("\n" ++ header ++ "\n\n" ++ table ++ "\n")
      System.exit(1)
    }
  }

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runEither(args.toSeq).fold(
      s => {
        System.err.println(red(s) ++ "\n\n" ++ parser.helpText())
        System.exit(1)
      },
      _ => ()
    )
}
