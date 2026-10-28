// Totals the CPU samples in `jfr print --events jdk.ExecutionSample --stack-depth 64 run.jfr` output.
//
// Usage: scala-cli run SummarizeJfr.scala -- samples.txt
//
// Prints the methods most often at the top of a stack (self time), then the find-unused and tasty-query methods most
// often anywhere in it (inclusive time), as percentages of all samples.
import java.nio.file.{Files, Paths}

private val jfrFrame = raw"(?m)^\s+([\w.$$<>]+)\(".r

@main def summarizeJfr(samples: String): Unit = {
  val events = Files.readString(Paths.get(samples)).split(java.util.regex.Pattern.quote("jdk.ExecutionSample {")).drop(1).toList
  val stacks = events.map(e => jfrFrame.findAllMatchIn(e).map(_.group(1)).toList).filter(_.nonEmpty)
  val selfCounts = stacks.map(_.head).groupMapReduce(identity)(_ => 1)(_ + _)
  val totalCounts = stacks.flatMap(_.distinct).groupMapReduce(identity)(_ => 1)(_ + _)
  val n = events.size
  def pct(c: Int) = f"${c * 100.0 / n}%5.1f%%"
  println(s"$n samples\n\nself:")
  selfCounts.toList.sortBy(-_._2).take(15).foreach((f, c) => println(s"${pct(c)} $f"))
  println("\ninclusive:")
  totalCounts.toList.sortBy(-_._2).take(1000)
    .filter((f, _) => f.startsWith("bl.unused") || f.startsWith("tastyquery")).take(40)
    .foreach((f, c) => println(s"${pct(c)} $f"))
}
