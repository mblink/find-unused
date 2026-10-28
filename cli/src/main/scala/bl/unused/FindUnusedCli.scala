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
import scala.util.matching.Regex

object FindUnusedCli {
  private lazy val parser = ParserForMethods(this)

  private def bold(s: String): String = AnsiColor.BOLD ++ s ++ AnsiColor.RESET
  private def green(s: String): String = AnsiColor.GREEN ++ s ++ AnsiColor.RESET
  private def red(s: String): String = AnsiColor.RED ++ s ++ AnsiColor.RESET
  private def yellow(s: String): String = AnsiColor.YELLOW ++ s ++ AnsiColor.RESET

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

    def matches(sym: Sym): Boolean =
      srcMatches(sym.pos) && symMatches(sym.name)

    override def toString: String = (src.map("src=" ++ _.regex).toList ++ sym.map("sym=" ++ _.regex)).mkString("&")
  }

  object Exclusion {
    given reader: TokensReader.Simple[Exclusion] =
      new TokensReader.Simple[Exclusion] {
        val shortName = "exclusion"
        def read(strs: Seq[String]): Either[String, Exclusion] =
          strs match {
            case Seq(arg) =>
              arg.split('&').toList.foldLeftM(Exclusion(None, None))((acc, filter) =>
                // Only the first `=` separates the filter's name, since the regex can contain more
                filter.split("=", 2) match {
                  case Array("src", src) => acc.copy(src = Some(src.r)).asRight[String]
                  case Array("sym", sym) => acc.copy(sym = Some(sym.r)).asRight[String]
                  case _ => s"Invalid exclusion filter: $filter".asLeft[Exclusion]
                }
              )
            case _ => s"Invalid exclusion filter: $strs".asLeft[Exclusion]
          }
      }
  }

  @main
  case class Args(
    @arg(doc = "Enable debug logging") debug: Flag,
    @arg(doc = "Also report code that's only used by unused code") transitive: Flag,
    @arg(short = 'd', doc = "Root directory of project") rootDirectory: Option[String],
    @arg(short = 'p', doc = "Packages to analyze") `package`: Seq[String],
    @arg(short = 'c', doc = "Classpath") classpath: Seq[String],
    @arg(short = 'e', doc = "Exclusions") exclusion: Seq[Exclusion],
    @arg(doc = "Directories find-unused's compiler plugin wrote the inline givens it saw expanded to") inlineGivenRecords: Seq[String],
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

  /** Rows of names and positions, sorted by position and then name, with one row per `rowKey` */
  private def rows(syms: Set[Sym])(rowKey: Sym => Any): List[(String, String)] =
    syms.toList
      .sortBy {
        case Sym(_, name, Some(SourcePos(file, line, col))) => (file, line, col, name)
        case Sym(_, name, Some(pos)) => (pos, 0, 0, name)
        case Sym(_, name, None) => (name, 0, 0, "")
      }
      .distinctBy(rowKey)
      .map(sym => (sym.name, sym.pos.getOrElse("")))

  /** What to print: the unused code, and, when `listRemovable`, what can be removed without changing it */
  private case class Report(
    unused: List[(String, String)],
    unneededExclusions: Seq[Exclusion],
    unnecessaryAnnotations: List[(String, String)],
  )

  private def report(refs: References, args: Args, listRemovable: Boolean): Report = {
    val isExcluded = refs.excludingDefaultsOf(sym => args.exclusion.exists(_.matches(sym)))
    // An `export` can generate several symbols with the same name and position, e.g. a forwarder and a type alias,
    // but symbols without a position, like overloads, are different
    val unused = rows(
      if (args.transitive.value) refs.computeTransitivelyUnused(isExcluded) else refs.computeUnusedExcluding(isExcluded)
    )(sym => sym.pos.fold(Left(sym.id))(pos => Right((sym.name, pos))))

    val unneededExclusions =
      if (listRemovable && args.exclusion.nonEmpty) {
        val reportable = refs.unusedWithoutExclusions(args.transitive.value)
        args.exclusion.filterNot(e => reportable.exists(refs.excludingDefaultsOf(e.matches)))
      } else Nil

    // An annotation is listed once, even if several symbols at its position carry it
    val unnecessaryAnnotations =
      if (listRemovable) rows(refs.unnecessaryUnusedAnnotations)(sym => sym.pos.fold(Left(sym.id))(Right(_)))
      else Nil

    Report(unused, unneededExclusions, unnecessaryAnnotations)
  }

  private def withOutputWriter(args: Args, sysOut: => PrintStream)(f: PrintWriter => Unit): Unit =
    args.output match {
      case Some(file) => Using.resource(new FileWriter(new File(file)))(fw => Using.resource(new PrintWriter(fw))(f))
      case None => Using.resource(new PrintWriter(sysOut))(f)
    }

  /**
   * @param listRemovable whether to list exclusions that don't exclude anything, and `@unused`/`@nowarn` annotations
   *                      that aren't needed, which only makes sense when every kind of code is analyzed: the explicits
   *                      and givens runners would flag each other's
   */
  private def run(runner: FindUnused.Runner, args: Args, singularType: String, listRemovable: Boolean): Unit =
    Either.catchNonFatal {
      val refs = runner(
        args.debug.value,
        args.rootDirectory.map(Paths.get(_)),
        args.`package`,
        args.classpath.distinct.map(Paths.get(_)),
        args.inlineGivenRecords.distinct.map(Paths.get(_)),
      )

      if (args.debug.value)
        println(s"*********** Result: ${Debug.printer(refs)}")

      val Report(unused, unneededExclusions, unnecessaryAnnotations) = report(refs, args, listRemovable)

      val hasUnused = unused.lengthIs > 0

      withOutputWriter(args, if (hasUnused) System.err else System.out) { writer =>
        val (tableWidth, useColors) = args.output match {
          case Some(_) => (Int.MaxValue, false)
          case None =>
            (
              args.width
                .orElse(Using(TerminalBuilder.builder.system(true).dumb(false).build)(_.getColumns).toOption)
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
          case (inst, _) =>
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

        // A list of things that can be removed, after the results
        def printRemovable(lines: Seq[String], header: String): Unit =
          if (lines.nonEmpty) {
            writer.println("\n" ++ (if (useColors) yellow(bold(header)) else header) ++ "\n")
            lines.foreach(l => writer.println(s"  $l"))
            args.output.fold(writer.println(""))(_ => ())
          }

        printRemovable(
          unneededExclusions.map(_.toString),
          if (unneededExclusions.lengthIs == 1) "1 exclusion doesn't exclude anything and can be removed:"
          else s"${unneededExclusions.length} exclusions don't exclude anything and can be removed:",
        )
        printRemovable(
          unnecessaryAnnotations.map((name, pos) => s"$name  $pos"),
          if (unnecessaryAnnotations.lengthIs == 1) "1 `@unused`/`@nowarn` annotation isn't needed, since what it's on is used:"
          else s"${unnecessaryAnnotations.length} `@unused`/`@nowarn` annotations aren't needed, since what they're on is used:",
        )

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

  // Transitive mode follows references through every kind of code, which the explicits and givens runners each skip
  // half of, so it would report code that's used
  private def oneKind(runner: FindUnused.Runner, args: Args, singularType: String): Unit =
    if (args.transitive.value) {
      System.err.println(red("--transitive needs the `all` command, which analyzes givens and explicits together"))
      System.exit(1)
    } else run(runner, args, singularType, listRemovable = false)

  @main def explicits(args: Args): Unit = oneKind(FindUnused.explicits, args, "explicit")
  @main def givens(args: Args): Unit = oneKind(FindUnused.givens, args, "given")
  @main def implicits(args: Args): Unit = oneKind(FindUnused.givens, args, "implicit")
  @main def all(args: Args): Unit = run(FindUnused.all, args, "term", listRemovable = true)

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runEither(args.toSeq).fold(
      s => {
        System.err.println(red(s) ++ "\n\n" ++ parser.helpText())
        System.exit(1)
      },
      _ => ()
    )
}
