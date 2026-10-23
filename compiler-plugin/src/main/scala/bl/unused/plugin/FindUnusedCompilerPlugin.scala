package bl.unused.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report
import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

/**
 * Records which `inline given`s each top-level class's code expands, for find-unused.
 *
 * TASTy is pickled before the `inlining` phase, so it doesn't show the `inline given`s that implicit searches performed
 * while inlining find, e.g. `summonInline` in an inline method. After that phase, each expansion is an `Inlined` tree
 * whose `call` is the `inline given` it came from.
 *
 * Writes one file per top-level class, named after its TASTy file, with a line `<definition>\t<inline given>` for each
 * expansion in it, to the directory next to the class directory with `-find-unused` added to its name, or to the one
 * passed as `-P:find-unused:out:<dir>`. A class with none gets an empty file, so find-unused can tell that it was
 * compiled with this plugin.
 */
class FindUnusedCompilerPlugin extends StandardPlugin {
  val name = "find-unused"
  override val description = "Records the inline givens that inlining expands, for find-unused"

  override def initialize(options: List[String])(using Context): List[PluginPhase] = {
    val classes = Option(ctx.settings.outputDir.value.file).map(_.toPath).filter(Files.isDirectory(_))
    options.collectFirst { case o if o.startsWith("out:") => Paths.get(o.stripPrefix("out:")) }
      .orElse(classes.map(c => c.resolveSibling(s"${c.getFileName}-find-unused"))) match {
      case Some(out) => List(new InlineGivensPhase(out))
      // Not a warning, which would fail builds that treat warnings as errors
      case None =>
        report.echo("find-unused: the class directory isn't a directory, so pass `-P:find-unused:out:<dir>`")
        Nil
    }
  }
}

class InlineGivensPhase(out: Path) extends PluginPhase {
  val phaseName = "findUnusedInlineGivens"

  override val runsAfter = Set("inlining")
  override val runsBefore = Set("postInlining")

  // Each top-level class's TASTy file name, and the definitions and `inline given`s it expands
  private val expanded = mutable.Map.empty[String, mutable.Set[(String, String)]]

  // The name find-unused gives a symbol, like `bl.unused.Foo.bar` for `bar` in `object Foo` in package `bl.unused`
  private def name(sym: Symbol)(using Context): String =
    (if (sym.owner.is(Flags.Package)) s"${sym.owner.fullName}.${sym.name}" else s"${name(sym.owner)}.${sym.name}")
      .replace("$.", ".")
      .stripSuffix("$")

  // The path of a top-level class's TASTy file, without the extension: an object's is named like its class, and a
  // symbolic name is encoded, e.g. `$div$tilde$bslash` for `/~\`
  private def tastyName(cls: Symbol)(using Context): String =
    cls.topLevelClass.fullName.mangledString.stripSuffix("$").replace('.', '/')

  override def prepareForUnit(tree: tpd.Tree)(using Context): Context = {
    expanded.clear()
    new tpd.TreeTraverser {
      def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match {
          case p: tpd.PackageDef => traverseChildren(p)
          case t: tpd.TypeDef if t.symbol.isClass => expanded.getOrElseUpdate(tastyName(t.symbol), mutable.Set.empty): Unit
          case _ => ()
        }
    }.traverse(tree)
    ctx
  }

  override def transformInlined(tree: tpd.Inlined)(using Context): tpd.Tree = {
    val sym = tree.call.symbol
    if (sym.exists && sym.is(Flags.Inline) && sym.isOneOf(Flags.GivenOrImplicit) && ctx.owner.exists)
      expanded.getOrElseUpdate(tastyName(ctx.owner), mutable.Set.empty) += ((name(ctx.owner), name(sym)))
    tree
  }

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree = {
    expanded.foreach { (cls, calls) =>
      val file = out.resolve(cls ++ ".tsv")
      // Without the record, find-unused falls back to its conservative check, so this shouldn't fail the compile
      try {
        Files.createDirectories(file.getParent)
        Files.write(file, calls.toList.sorted.map((d, g) => s"$d\t$g\n").mkString.getBytes(StandardCharsets.UTF_8))
      } catch {
        case e: IOException => report.echo(s"find-unused: couldn't write $file: $e")
      }
    }
    tree
  }
}
