// Benchmark harness for find-unused's core, run with scala-cli against the core classpath -- see SKILL.md.
// It uses core internals (`FindUnused.Runner#analyze`, `Bytecode`), so update it when those change.

import bl.unused.*
import java.net.URI
import java.nio.file.{Files, FileSystems, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters.*
import tastyquery.Contexts.Context
import tastyquery.jdk.ClasspathLoaders

def time[A](f: => A): (A, Double) = {
  val t0 = System.nanoTime
  val a = f
  (a, (System.nanoTime - t0) / 1e6)
}

@main def bench(packagesArg: String, cpFile: String, outDir: String): Unit = {
  val packages = packagesArg.split(',').toList
  val classpath = Source.fromFile(cpFile).getLines().map(_.trim).filter(_.nonEmpty).map(Paths.get(_)).toList
  println(s"classpath entries: ${classpath.size} (${classpath.count(_.toString.endsWith(".jar"))} jars)")

  // The first scan is what a CLI run pays: cold JIT and cold file cache
  val bytecodeRefs = (1 to 3).map { i =>
    val (refs, ms) = time(Bytecode.references(packages, classpath))
    println(f"bytecode scan $i: $ms%.0f ms, classes=${refs.supertypes.size}, member names=${refs.references.size}")
    refs
  }.last

  val javaModules = Files.list(FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules")).iterator.asScala.toList
  val (ctx, ctxMs) = time(Context.initialize(ClasspathLoaders.read(javaModules ::: classpath)))
  println(f"tasty-query context: $ctxMs%.0f ms")
  given Context = ctx

  // The first analysis also warms the JIT, so compare the counts, not these two timings
  val (without, withoutMs) = time(FindUnused.givens.analyze(packages, BytecodeReferences(Map.empty, Map.empty, Map.empty)))
  val (withBytecode, withMs) = time(FindUnused.givens.analyze(packages, bytecodeRefs))
  println(f"givens analysis without bytecode: $withoutMs%.0f ms, unused=${without.computeUnused.size}")
  println(f"givens analysis with bytecode (scan excluded): $withMs%.0f ms, unused=${withBytecode.computeUnused.size}")

  def write(name: String, syms: Set[Sym]): Unit =
    Files.write(Paths.get(outDir, name), syms.toList.map(s => s"${s.name}\t${s.pos.getOrElse("")}").sorted.asJava): Unit

  write("rescued-by-bytecode.tsv", without.computeUnused -- withBytecode.computeUnused)
  write("unused-givens.tsv", withBytecode.computeUnused)
  println(s"wrote rescued-by-bytecode.tsv and unused-givens.tsv to $outDir")
}
