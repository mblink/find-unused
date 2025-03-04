package bondlink

import java.nio.file.Paths
import mainargs.{arg, main, ParserForMethods}
import scala.io.AnsiColor

object FindUnusedCli {
  private lazy val parser = ParserForMethods(this)

  private def red(s: String): String = AnsiColor.RED ++ s ++ AnsiColor.RESET

  @main
  def help(): Unit = println(parser.helpText())

  @main
  def givens(
    @arg(short = 'p', doc = "Packages to analyze") `package`: Seq[String],
    @arg(short = 'c', doc = "Classpath") classpath: Seq[String],
  ): Unit = {
    val givens = FindUnusedGivens.all(`package`, classpath.distinct.map(Paths.get(_)))
    val unused = givens.defined
      .filterNot((code, _) => givens.used.contains(code))
      .toList
      .map { case (_, (name, pos)) => name ++ pos.fold("")(p => s" at $p") }
      .sorted

    if (unused.lengthIs > 0) {
      val len = unused.length
      System.err.println(
        "\n" ++
        red(s"Found $len unused given${if (len == 1) "" else "s"}") ++
        "\n\n" ++
        unused.mkString("\n") ++
        "\n"
      )
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
