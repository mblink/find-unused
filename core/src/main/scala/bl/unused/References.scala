package bl.unused

import cats.Monoid
import cats.syntax.applicative.*
import cats.syntax.semigroup.*
import tastyquery.Contexts.Context
import tastyquery.Symbols.*

case class References(
  defined: Map[Int, (String, Option[String])],
  used: Set[Int],
)

object References {
  given monoid: Monoid[References] =
    Monoid.instance(References(Map.empty, Set.empty), (x, y) => References(x.defined ++ y.defined, x.used ++ y.used))

  lazy val empty: EnvR[References] = EnvR(_ => monoid.empty)

  def defined(sym: Symbol)(using ctx: Context): EnvR[References] =
    EnvR { env =>
      References(
        defined = Map(sym.hashCode -> (Symbols.name(sym), sym.tree.map(t => Positions.format(env.rootDirectory, t.pos)))),
        used = sym match {
          // If a symbol overrides another, consider it used
          // This accounts for cases where a term is used in a parent class, but not in the subclass
          case t: TermOrTypeSymbol => Symbols.nextOverriddenSymbol(t).fold(Set.empty)(_ => Set(t.hashCode))
          case _ => Set.empty
        },
      )
    }

  def used(sym: Symbol)(using ctx: Context): EnvR[References] =
    References(Map.empty, Set(sym.hashCode)).pure[EnvR]

  def fromSymbol(sym: Symbol, mk: Symbol => EnvR[References])(using ctx: Context): EnvR[References] = {
    EnvR.env.flatMap { env =>
      if (env.symbolIsValid(sym))
        sym match {
          case _: PackageSymbol => empty

          case t: TermSymbol =>
            mk(t) |+|
              // If a TermSymbol is used and it has a corresponding module class, consider the module class used too
              t.moduleClass.fold(empty)(mk)

          case _ => mk(sym)
        }
      else
        empty
    } |+| (sym match {
      /*
      If sym is an exported term, look up the symbol in the Context and consider the result used

      This might be a bug in tasty-query -- some symbols have different `hashCode`s when found in an
      `Ident` in `treeRefs` vs. when found as a `TermSymbol` in `symRefs`
      */
      case t: TermSymbol if t.isExport => Symbols.matchingTermSym(t).fold(empty)(fromSymbol(_, mk))

      /*
      If sym is a type member and there's a matching term symbol that's an exported term, consider the term used

      This covers cases where an export is only used as a type and the companion object isn't used
      */
      case t: TypeMemberSymbol => Symbols.matchingTermSym(t).filter(_.isExport).fold(empty)(fromSymbol(_, mk))

      case _ => empty
    })
  }
}
