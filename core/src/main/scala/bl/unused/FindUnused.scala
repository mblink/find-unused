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
      val env = Env(debug, rootDirectory, packages, sym => symbolIsValid(sym) && Symbols.isInPackage(pkgSyms)(sym))

      packages.foldMap { p =>
        println(s"[find-unused] Analyzing package $p")
        Symbols.references(ctx.findPackage(p)).run(env, SeenSymbols.empty)._2
      }
    }
  }

  object explicits extends Runner(s => !Symbols.isGiven(s))
  object givens extends Runner(Symbols.isGiven)
  object all extends Runner(_ => true)
}
