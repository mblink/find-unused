package bl.unused

import cats.{Monoid, Order}
import cats.data.NonEmptySet
import cats.syntax.applicative.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import cats.syntax.set.*
import cats.syntax.traverse.*
import scala.collection.mutable
import scala.collection.immutable.SortedSet
import scala.util.chaining.*
import tastyquery.Contexts.Context
import tastyquery.Symbols.*

sealed case class Sym private (id: Int, name: String, pos: Option[String])
object Sym {
  given order: Order[Sym] = Order.by(_.id)
  given ordering: Ordering[Sym] = order.toOrdering

  // A unique id per symbol. `Symbol` doesn't override `hashCode`, so different symbols can share an identity hash code
  private val ids = mutable.Map.empty[Symbol, Int]
  def id(sym: Symbol): Int = ids.synchronized(ids.getOrElseUpdate(sym, ids.size))

  private val cache = mutable.Map.empty[Symbol, Sym]

  private def cached(sym: Symbol)(compute: => EnvR[Sym]): EnvR[Sym] =
    cache.synchronized(cache.get(sym).fold(compute.map(s => cache.put(sym, s).pipe(_ => s)))(_.pure[EnvR]))

  def fromSymbol(sym: Symbol): EnvR[Sym] =
    cached(sym)(EnvR.noSeenSymbols(env => new Sym(
      id = id(sym),
      name = Symbols.name(sym),
      pos = sym.tree.map(t => Positions.format(env.rootDirectory, t.pos)),
    )))
}

case class References(
  defined: Set[Sym],
  used: Set[Sym],
  // If the symbol represented by the map key is used, also mark the symbol represented by the map value as used
  usedProxy: Map[Sym, NonEmptySet[Sym]],
  // The non-`transparent` inline methods the analyzed code calls, and the `inline given`s it defines -- see `InlineSummons`
  inlineCalls: Set[TermSymbol] = Set.empty,
  inlineGivens: Map[Sym, TermSymbol] = Map.empty,
) {
  final def removeUsed(sym: Symbol): References = {
    val c = Sym.id(sym)
    copy(used = used.filterNot(_.id === c))
  }

  final def addUsedProxy(m: Map[Symbol, Set[Symbol]]): EnvR[References] =
    m.toList.foldMapM((s, ss) =>
      ss.toList.traverse(Sym.fromSymbol).flatMap(_.to(SortedSet).toNes.fold(Map().pure[EnvR])(ss =>
        Sym.fromSymbol(s).map(s => Map(s -> ss))
      ))
    ).map(m => copy(usedProxy = usedProxy |+| m))

  final def addUsedProxy(ifUsed: Symbol, thenUsed: Set[Symbol]): EnvR[References] = addUsedProxy(Map(ifUsed -> thenUsed))

  final lazy val reverseUsedProxy: Map[Sym, NonEmptySet[Sym]] =
    usedProxy.toList.foldMap((ifUsed, thenUsed) => thenUsed.foldMap(s => Map(s -> NonEmptySet.one(ifUsed))))

  private def allSymsToCheckForUsage(sym: Sym): NonEmptySet[Sym] = {
    @annotation.tailrec
    def go(acc: NonEmptySet[Sym], syms: List[Sym]): NonEmptySet[Sym] =
      syms match {
        case Nil => acc
        case s :: rest =>
          reverseUsedProxy.get(s) match {
            case Some(proxySyms) => go(acc ++ proxySyms, rest ++ proxySyms.toSortedSet)
            case None => go(acc, rest)
          }
      }

    go(NonEmptySet.one(sym), List(sym))
  }

  final lazy val computeUnused: Set[Sym] = {
    val allUsed = usedProxy.foldLeft(used) { case (acc, (k, v)) =>
      if (allSymsToCheckForUsage(k).exists(acc.contains)) acc ++ v.toSortedSet else acc
    }
    defined.filterNot(allUsed.contains)
  }
}

object References {
  given monoid: Monoid[References] =
    Monoid.instance(
      References(Set.empty, Set.empty, Map.empty),
      (x, y) => References(
        x.defined ++ y.defined,
        x.used ++ y.used,
        x.usedProxy |+| y.usedProxy,
        x.inlineCalls ++ y.inlineCalls,
        x.inlineGivens ++ y.inlineGivens,
      ),
    )

  lazy val empty: EnvR[References] = monoid.empty.pure[EnvR]

  val defined = (sym: Symbol) => (ctx: Context) ?=>
    if (Symbols.isValidDefinition(sym))
      (for {
        env <- EnvR.env
        defSym <- Sym.fromSymbol(sym)
        usedSyms <- sym match {
          // If a symbol overrides another, consider it used
          // This accounts for cases where a term is used in a parent class, but not in the subclass
          case t: TermOrTypeSymbol =>
            Symbols.nextOverriddenSymbol(t).fold(Set.empty.pure[EnvR])(_ => Sym.fromSymbol(t).map(Set(_)))
          case _ => Set.empty.pure[EnvR]
        }
        // If a symbol is referenced in bytecode, consider it used
        // This accounts for `given`s summoned by inline methods and macros, which TASTy doesn't record
        usedInBytecode =
          if (env.usedInBytecode(sym)) {
            if (env.debug) println(s"*********** used in bytecode: ${Symbols.name(sym)}")
            Set(defSym)
          } else Set.empty[Sym]
        inlineGivens = sym match {
          case t: TermSymbol if t.isInline && Symbols.isGiven(t) => Map(defSym -> t)
          case _ => Map.empty
        }
        refs = References(defined = Set(defSym), used = usedSyms ++ usedInBytecode, usedProxy = Map.empty, inlineGivens = inlineGivens)
      } yield refs) |+| Annotations.checkForUnused(sym)
    else
      empty

  val used = (sym: Symbol) => (ctx: Context) ?=>
    Sym.fromSymbol(sym).map(s => References(Set.empty, Set(s), Map.empty))

  def inlineCalls(syms: Set[Symbol]): EnvR[References] =
    monoid.empty.copy(inlineCalls = syms.collect { case t: TermSymbol if t.isInline && !t.isTransparentInline => t }).pure[EnvR]

  def usedProxy(m: Map[Symbol, Set[Symbol]]): EnvR[References] = empty.flatMap(_.addUsedProxy(m))
  def usedProxy(ifUsed: Symbol, thenUsed: Set[Symbol]): EnvR[References] = empty.flatMap(_.addUsedProxy(ifUsed, thenUsed))

  def fromSymbol(
    sym: Symbol,
    mk: Symbol => Context ?=> EnvR[References],
    skipExportCheck: Boolean = false,
  )(using ctx: Context): EnvR[References] =
    EnvR.env.flatMap { env =>
      (if (env.symbolIsValid(sym))
        sym match {
          case _: PackageSymbol => empty

          case t: TermSymbol =>
            mk(t) |+|
              // If a TermSymbol is used and it has a corresponding module class, consider the module class used too
              t.moduleClass.fold(empty)(mk(_))

          case _ => mk(sym)
        }
      else
        empty) |+|
        // This doesn't depend on whether `sym` itself is valid, because the symbols an export generates can be split
        // between the explicits and givens analyses. For example, exporting an implicit class generates an implicit
        // conversion, which is a given, along with a companion and a type, which aren't.
        (if (skipExportCheck || !env.isInPackages(sym)) References.empty else sym match {
        /*
        If sym is an exported term:

          1. Look up the matching term symbol in the Context and consider the result used
            - This might be a bug in tasty-query -- some symbols are different `Symbol` objects when found in an
              `Ident` in `treeRefs` vs. when found as a `TermSymbol` in `symRefs`
          2. Look up the matching type symbol in the Context and consider the result used
            - This covers cases where an export is only used as a value, not also as a type
        */
        case t: TermSymbol if t.isExport =>
          Symbols.allMatchingSymbols(t).foldMap(fromSymbol(_, mk, true))

        /*
        If sym is a type member and there's a matching term symbol that's an exported term, consider the term used

        This covers cases where an export is only used as a type and the companion object isn't used
        */
        case t: TypeMemberSymbol =>
          Symbols.allMatchingSymbols(t).collect { case t: TermSymbol if t.isExport => t }.foldMap(fromSymbol(_, mk, true))

        case _ => empty
      })
    }
}
