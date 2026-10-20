package bl.unused

import cats.syntax.foldable.*
import java.net.URI
import java.nio.file.{Files, FileSystems, Path}
import scala.jdk.CollectionConverters.*
import tastyquery.Contexts.Context
import tastyquery.jdk.ClasspathLoaders
import tastyquery.Symbols.Symbol

object FindUnused {
  class Runner(symbolIsValid: Symbol => Boolean) {
    final def apply(debug: Boolean, rootDirectory: Option[Path], packages: Seq[String], classpath: Seq[Path]): References = {
      println("[find-unused] Initializing classpath")

      val javaModules = Files.list(FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules")).iterator.asScala.toList

      // `implicit val` instead of `given` so it's initialized eagerly for logging/timing purposes
      implicit val ctx: Context = Context.initialize(ClasspathLoaders.read(javaModules ::: classpath.toList))

      val pkgSyms = packages.map(ctx.findPackage).toSet

      // Lazy so it's only read when a `given` is analyzed
      lazy val bytecodeRefs = {
        println("[find-unused] Reading bytecode")
        Bytecode.references(packages, classpath)
      }

      val env = Env(
        debug,
        rootDirectory,
        packages,
        Symbols.isInPackage(pkgSyms),
        sym => symbolIsValid(sym) && Symbols.isInPackage(pkgSyms)(sym),
        // Only `given`s are affected by implicit searches that happen after TASTy is pickled
        sym => Symbols.isGiven(sym) && bytecodeRefs.isReferenced(sym),
      )

      val refs = packages.foldMap { p =>
        println(s"[find-unused] Analyzing package $p")
        Symbols.references(ctx.findPackage(p)).run(env, SeenSymbols.empty)._2
      }
      markPossiblySummonedInlineGivens(refs, debug)
    }
  }

  /** Consider `inline given`s used if an implicit search after TASTy is pickled might find them, see `InlineSummons` */
  def markPossiblySummonedInlineGivens(refs: References, debug: Boolean)(using ctx: Context): References =
    if (refs.inlineGivens.isEmpty) refs
    else {
      println("[find-unused] Checking inline givens")
      val summoned = InlineSummons.summonedClasses(refs.inlineCalls)
      val possiblyUsed = refs.inlineGivens.collect { case (s, g) if InlineSummons.mightFind(summoned, g) => s }
      if (debug) possiblyUsed.foreach(s => println(s"*********** possibly summoned after pickling: ${s.name}"))
      refs.copy(used = refs.used ++ possiblyUsed)
    }

  object explicits extends Runner(s => !Symbols.isGiven(s))
  object givens extends Runner(Symbols.isGiven)
  object all extends Runner(_ => true)
}
