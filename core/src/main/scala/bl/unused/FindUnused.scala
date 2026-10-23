package bl.unused

import cats.syntax.foldable.*
import java.net.URI
import java.nio.file.{Files, FileSystems, Path}
import scala.jdk.CollectionConverters.*
import tastyquery.Contexts.Context
import tastyquery.jdk.ClasspathLoaders
import tastyquery.Symbols.{ClassSymbol, PackageSymbol, Symbol, TermSymbol}

object FindUnused {
  class Runner(symbolIsValid: Symbol => Boolean) {
    /** @param inlineGivenRecords the directories find-unused's compiler plugin wrote to, see `InlineGivenRecords` */
    final def apply(
      debug: Boolean,
      rootDirectory: Option[Path],
      packages: Seq[String],
      classpath: Seq[Path],
      inlineGivenRecords: Seq[Path] = Nil,
    ): References = {
      println("[find-unused] Initializing classpath")

      val javaModules = Files.list(FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules")).iterator.asScala.toList

      // `implicit val` instead of `given` so it's initialized eagerly for logging/timing purposes
      implicit val ctx: Context = Context.initialize(ClasspathLoaders.read(javaModules ::: classpath.toList))

      val pkgSyms = packages.map(ctx.findPackage).toSet

      // Lazy so it's only read when a `given`, a default value or an `object` is analyzed
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
        // `given`s can be summoned by implicit searches that happen after TASTy is pickled, and macros can read
        // default values, e.g. to decode a missing JSON field, and `object`s, e.g. to list the cases of an enumeration
        // or to derive an instance for a `case object`
        {
          case c: ClassSymbol if c.isModuleClass => bytecodeRefs.moduleCallers(c)
          case sym if Symbols.isGiven(sym) || Symbols.isDefaultGetter(sym) => bytecodeRefs.callers(sym)
          case _ => Set.empty
        },
      )

      val refs = packages.foldMap { p =>
        println(s"[find-unused] Analyzing package $p")
        Symbols.references(ctx.findPackage(p)).run(env, SeenSymbols.empty)._2
      }
      val withBytecode = if (refs.bytecodeCallers.isEmpty) refs else refs.attributeBytecodeCallers(bytecodeRefs.outers)
      if (withBytecode.inlineGivens.isEmpty) withBytecode
      else if (inlineGivenRecords.isEmpty) markPossiblySummonedInlineGivens(withBytecode, debug)
      else
        InlineGivenRecords(inlineGivenRecords, pkgSyms.toList.flatMap(topLevelClasses).flatMap(tastyName).toSet) match {
          case Right(records) => markRecordedInlineGivens(withBytecode, records)
          case Left(missing) =>
            val classes = missing.toList.sorted.map(_.replace('/', '.')) match {
              case c :: Nil => s"$c wasn't"
              case cs => s"${cs.length} classes, like ${cs.head}, weren't"
            }
            println(s"[find-unused] $classes compiled with find-unused's compiler plugin, so inline givens are checked conservatively")
            markPossiblySummonedInlineGivens(withBytecode, debug)
        }
    }
  }

  private def topLevelClasses(pkg: PackageSymbol)(using ctx: Context): List[ClassSymbol] =
    pkg.declarations.flatMap {
      case c: ClassSymbol => List(c)
      case p: PackageSymbol => topLevelClasses(p)
      case _ => Nil
    }

  // The path of the TASTy file a top-level class is in, if it has one, without the extension
  private def tastyName(cls: ClassSymbol): Option[String] =
    cls.tree.flatMap(_ => Bytecode.internalClassName(cls)).map(_.stripSuffix("$"))

  /** Consider `inline given`s used by the definitions find-unused's compiler plugin recorded expanding them */
  def markRecordedInlineGivens(refs: References, records: Map[String, Set[String]]): References = {
    val byName = refs.defined.groupBy(_.name)
    // The definition a name is in: a lambda or a local value that isn't analyzed is part of the definition around it
    def definitions(name: String): Set[Sym] =
      Iterator.iterate(name)(n => n.take(math.max(n.lastIndexOf('.'), 0))).takeWhile(_.nonEmpty)
        .map(byName.getOrElse(_, Set.empty)).find(_.nonEmpty).getOrElse(Set.empty)

    refs.usedFromSources(refs.inlineGivens.flatMap((sym, inlineGiven) =>
      records.get(Symbols.name(inlineGiven)).map(names => sym -> names.toList.map(definitions))
    ))
  }

  /** Consider `inline given`s used if an implicit search after TASTy is pickled might find them, see `InlineSummons` */
  def markPossiblySummonedInlineGivens(refs: References, debug: Boolean)(using ctx: Context): References =
    if (refs.inlineGivens.isEmpty) refs
    else {
      println("[find-unused] Checking inline givens")
      // An `inline given` that might be found can summon more when it's expanded, so keep walking the bodies of the
      // ones found until no more are
      @annotation.tailrec
      def go(roots: Set[TermSymbol], summoned: Set[ClassSymbol], found: Map[Sym, TermSymbol]): Map[Sym, TermSymbol] =
        InlineSummons.summonedClasses(roots) match {
          case None =>
            println("[find-unused] Too many inline methods to check which type classes they summon, so every inline given is considered used")
            refs.inlineGivens
          case Some(classes) =>
            val all = summoned ++ classes
            val more = refs.inlineGivens.filter((s, g) => !found.contains(s) && InlineSummons.mightFind(all, g))
            if (more.isEmpty) found else go(more.values.toSet, all, found ++ more)
        }

      val possiblyUsed = go(refs.inlineCalls, InlineSummons.classes(refs.inlineTypeArgs), Map.empty).keySet
      if (debug) possiblyUsed.foreach(s => println(s"*********** possibly summoned after pickling: ${s.name}"))
      refs.copy(used = refs.used ++ possiblyUsed)
    }

  object explicits extends Runner(s => !Symbols.isGiven(s))
  object givens extends Runner(Symbols.isGiven)
  object all extends Runner(_ => true)
}
