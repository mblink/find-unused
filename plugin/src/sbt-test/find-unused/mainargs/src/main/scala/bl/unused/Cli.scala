package bl.unused

import mainargs.{arg, main, ParserForMethods}

// mainargs calls the `@main` methods from a macro expansion, which TASTy doesn't show
object Cli {
  @main
  def greet(@arg(doc = "Who to greet") name: String): Unit = println(Helper.greeting(name))

  @main
  def repeat(times: Int = 1, text: String): Unit = println(Helper.repeated(times, text))

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args.toSeq): Unit
}

object Helper {
  def greeting(name: String): String = s"Hello, $name"

  def repeated(times: Int, text: String): String = text * times

  // Nothing calls this, so it's reported in both modes
  def unused: Int = leaf

  // Only `unused` calls this, so it's only reported in transitive mode
  def leaf: Int = 0
}
