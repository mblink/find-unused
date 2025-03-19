package bl.unused

import cats.Align
import cats.syntax.either.*
import cats.syntax.foldable.*
import java.io.{File, FileWriter, PrintStream, PrintWriter, StringWriter}
import java.nio.file.Paths
import mainargs.{arg, main, Flag, ParserForClass, ParserForMethods, TokensReader}
import org.jline.terminal.TerminalBuilder
import scala.io.AnsiColor
import scala.util.Using
import scala.util.chaining.*
import scala.util.matching.Regex

object FindUnusedCli {
  private lazy val parser = ParserForMethods(this)

  private def bold(s: String): String = AnsiColor.BOLD ++ s ++ AnsiColor.RESET
  private def green(s: String): String = AnsiColor.GREEN ++ s ++ AnsiColor.RESET
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
    @arg(short = 'o', doc = "Output file") output: Option[String],
    @arg(doc = "Disable color output") noColor: Flag,
  )

  object Args {
    given parser: ParserForClass[Args] = ParserForClass[Args]
  }

  private object IntFromString {
    def unapply(s: String): Option[Int] = s.toIntOption
  }

  private def withOutputWriter(args: Args, sysOut: => PrintStream)(f: PrintWriter => Unit): Unit =
    args.output match {
      case Some(file) => Using.resource(new FileWriter(new File(file)))(fw => Using.resource(new PrintWriter(fw))(f))
      case None => Using.resource(new PrintWriter(sysOut))(f)
    }

  private def run(runner: FindUnused.Runner, args: Args, singularType: String): Unit =
    Either.catchNonFatal {
      val refs = runner(
        args.debug.value,
        args.rootDirectory.map(Paths.get(_)),
        args.`package`,
        args.classpath.distinct.map(Paths.get(_)),
      )

      if (args.debug.value)
        println(s"*********** Result: ${Debug.printer(refs)}")

      val unused = refs.computeUnused
        .pipe(l =>
          if (args.exclusion.nonEmpty)
            l.filterNot((name, pos) => args.exclusion.exists(_.matches(pos, name)))
          else
            l
        )
        .sortBy {
          case (name, Some(SourcePos(file, line, col))) => (file, line, col, name)
          case (name, Some(pos)) => (pos, 0, 0, name)
          case (name, None) => (name, 0, 0, "")
        }
        .map((name, pos) => (name, pos.getOrElse("")))

      val hasUnused = unused.lengthIs > 0

      withOutputWriter(args, if (hasUnused) System.err else System.out) { writer =>
        val (tableWidth, useColors) = args.output match {
          case Some(_) => (Int.MaxValue, false)
          case None =>
            (
              args.width
                .orElse(Using(TerminalBuilder.builder.system(true).dumb(false).build)(_.getWidth).toOption)
                .filter(_ > 0)
                .getOrElse(80),
              !args.noColor.value && sys.env.get("NO_COLOR").isEmpty
            )
        }

        val paddingWidth = 7
        val maxInstanceWidth = math.max(math.round(tableWidth / 3.0).toInt, 25)
        val instanceHeader = "Instance"
        val locationHeader = "Location"
        val (idealInstanceWidth, idealLocationWidth) = unused.foldLeft((instanceHeader.length, locationHeader.length)) {
          case ((accInst, accLoc), (name, pos)) =>
            val nameLen = name.length
            val posLen = pos.length
            (
              if (nameLen > accInst) nameLen else accInst,
              if (posLen > accLoc) posLen else accLoc
            )
        }
        val (instanceWidth, locationWidth) = (idealInstanceWidth, idealLocationWidth) match {
          case t @ (inst, loc) if inst + loc + paddingWidth < tableWidth => t
          case (inst, loc) =>
            val instWidth = math.min(inst, maxInstanceWidth)
            val locWidth = tableWidth - instWidth - paddingWidth
            (instWidth, locWidth)
        }

        def printSeparator(left: Char, middle: Char, right: Char): Unit = {
          writer.print(left)
          writer.print("─" * (instanceWidth + 2))
          writer.print(middle)
          writer.print("─" * (locationWidth + 2))
          writer.println(right)
        }

        def printRow(inst: String, loc: String, style0: String => String): Unit = {
          val style = if (useColors) style0 else identity
          val instLines = inst.grouped(instanceWidth).toList
          val locLines = loc.grouped(locationWidth).toList

          Align[List].zipAll(instLines, locLines, "", "").foreach { (instStr, locStr) =>
            val instSpaces = instanceWidth - instStr.length
            val locSpaces = locationWidth - locStr.length

            writer.print("│ ")
            writer.print(style(instStr))
            writer.print(" " * instSpaces)
            writer.print(" │ ")
            writer.print(style(locStr))
            writer.print(" " * locSpaces)
            writer.println(" │")
          }
        }

        val pluralType = singularType ++ (if (unused.lengthIs == 1) "" else "s")
        val header = if (hasUnused) s"Found ${unused.length} unused $pluralType" else s"No unused $pluralType found"
        val headerFmt = (hasUnused, useColors) match {
          case (_, false) => identity[String]
          case (true, true) => red.andThen(bold)
          case (false, true) => green.andThen(bold)
        }

        writer.println(args.output.fold("\n")(_ => "") ++ headerFmt(header))

        if (hasUnused) {
          writer.print("\n")
          printSeparator('┌', '┬', '┐')
          printRow(instanceHeader, locationHeader, bold)
          printSeparator('├', '┼', '┤')
          unused.zipWithIndex.foreach { case ((name, pos), idx) =>
            if (idx != 0)
              printSeparator('├', '┼', '┤')

            printRow(name, pos, identity)
          }
          printSeparator('└', '┴', '┘')
          args.output.fold(writer.println("\n"))(_ => ())
        }

        writer.flush
      }

      System.exit(if (hasUnused) 1 else 0)
    }.valueOr { err =>
      Using.resource(new StringWriter())(sw =>
        Using.resource(new PrintWriter(sw)) { pw =>
          err.printStackTrace(pw)
          System.err.println(red(s"Error finding unused ${singularType}s\n\n" ++ sw.toString))
        }
      )
      System.exit(1)
    }

  @main def explicits(args: Args): Unit = run(FindUnused.explicits, args, "explicit")
  @main def givens(args: Args): Unit = run(FindUnused.givens, args, "given")
  @main def implicits(args: Args): Unit = run(FindUnused.givens, args, "implicit")
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
