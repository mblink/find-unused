package bondlink

import cats.syntax.either.*
import cats.syntax.foldable.*
import com.github.freva.asciitable.{AsciiTable, Column, HorizontalAlign}
import java.nio.file.Paths
import mainargs.{arg, main, Flag, ParserForClass, ParserForMethods, TokensReader}
import scala.io.AnsiColor
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*
import scala.util.matching.Regex

object FindUnusedCli {
  private lazy val parser = ParserForMethods(this)

  private def red(s: String): String = AnsiColor.RED ++ s ++ AnsiColor.RESET

  @main
  def help(): Unit = println(parser.helpText())

  private object SourcePos {
    def unapply(pos: String): Option[(String, Int, Int)] =
      pos.split(':') match {
        case Array(file, IntFromString(line), IntFromString(col)) => Some((file, line, col))
        case _ => None
      }
  }

  case class Exclusion(
    src: Option[Regex],
    sym: Option[Regex],
  ) {
    private def srcMatches(pos: Option[String]): Boolean =
      (src, pos) match {
        case (Some(r), Some(SourcePos(f, _, _))) => r.findFirstIn(f).nonEmpty
        case (Some(r), Some(p)) => r.findFirstIn(p).nonEmpty
        case (Some(_), None) | (None, Some(_)) | (None, None) => true
      }

    private def symMatches(symName: String): Boolean =
      sym.forall(_.findFirstIn(symName).nonEmpty)

    def matches(pos: Option[String], symName: String): Boolean =
      srcMatches(pos) && symMatches(symName)
  }

  object Exclusion {
    given reader: TokensReader.Simple[Exclusion] =
      new TokensReader.Simple[Exclusion] {
        val shortName = "exclusion"
        def read(strs: Seq[String]): Either[String, Exclusion] =
          strs match {
            case Seq(arg) =>
              arg.split('&').toList.foldLeftM(Exclusion(None, None))((acc, filter) =>
                filter.split('=') match {
                  case Array("src", src) => acc.copy(src = Some(src.r)).asRight[String]
                  case Array("sym", sym) => acc.copy(sym = Some(sym.r)).asRight[String]
                  case _ => s"Invalid exclusion filter: $filter".asLeft[Exclusion]
                }
              )
          }
      }
  }

  @main
  case class Args(
    @arg(doc = "Enable debug logging") debug: Flag,
    @arg(short = 'd', doc = "Root directory of project") rootDirectory: Option[String],
    @arg(short = 'p', doc = "Packages to analyze") `package`: Seq[String],
    @arg(short = 'c', doc = "Classpath") classpath: Seq[String],
    @arg(short = 'e', doc = "Exclusions") exclusion: Seq[Exclusion],
    @arg(short = 'w', doc = "Terminal width") width: Option[Int],
  )

  object Args {
    given parser: ParserForClass[Args] = ParserForClass[Args]
  }

  private def tableCol(name: String, width: Int): Column =
    new Column().header(name).maxWidth(math.max(width, 1)).dataAlign(HorizontalAlign.LEFT)

  private object IntFromString {
    def unapply(s: String): Option[Int] = s.toIntOption
  }

  private def run(runner: FindUnused.Runner, args: Args, singularType: String): Unit = {
    val refs = runner(
      args.debug.value,
      args.rootDirectory.map(Paths.get(_)),
      args.`package`,
      args.classpath.distinct.map(Paths.get(_)),
    )

    if (args.debug.value)
      println(s"*********** Result: ${pprint(refs)}")

    val unused = refs.defined
      .filterNot((code, _) => refs.used.contains(code))
      .toList
      .pipe(l =>
        if (args.exclusion.nonEmpty)
          l.filterNot { case (_, (name, pos)) => args.exclusion.exists(_.matches(pos, name)) }
        else
          l
      )
      .sortBy {
        case (_, (name, Some(SourcePos(file, line, col)))) => (file, line, col, name)
        case (_, (name, Some(pos))) => (pos, 0, 0, name)
        case (_, (name, None)) => (name, 0, 0, "")
      }
      .map { case (_, (name, pos)) => (name, pos.getOrElse("")) }

    if (unused.lengthIs > 0) {
      val len = unused.length
      val header = red(s"Found $len unused ${singularType}${if (len == 1) "" else "s"}")
      val tableWidth = args.width.map(_ - 3).filter(_ > 0).getOrElse(80)
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

  @main def explicits(args: Args): Unit = run(FindUnused.explicits, args, "explicit")
  @main def givens(args: Args): Unit = run(FindUnused.givens, args, "given")
  @main def all(args: Args): Unit = run(FindUnused.all, args, "term")

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runEither(args.toSeq).fold(
      s => {
        System.err.println(red(s) ++ "\n\n" ++ parser.helpText())
        System.exit(1)
      },
      _ => ()
    )
}
