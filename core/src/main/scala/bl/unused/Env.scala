package bl.unused

import cats.{Id, Monad, Monoid}
import cats.data.{Kleisli, ReaderT, StateT}
import cats.syntax.semigroup.*
import java.nio.file.Path
import tastyquery.Symbols.Symbol

case class Env(
  debug: Boolean,
  rootDirectory: Option[Path],
  packages: Seq[String],
  symbolIsValid: Symbol => Boolean,
)

case class SeenSymbols(symbols: Set[Int])

object SeenSymbols {
  given monoid: Monoid[SeenSymbols] =
    Monoid.instance(SeenSymbols(Set.empty), (x, y) => SeenSymbols(x.symbols ++ y.symbols))

  lazy val empty: SeenSymbols = monoid.empty
}

opaque type EnvR[A] = ReaderT[StateT[Id, SeenSymbols, *], Env, A]

object EnvR {
  extension [A](e: EnvR[A]) {
    def run(env: Env, syms: SeenSymbols): (SeenSymbols, A) = e.run(env).run(syms)

    def map[B](f: A => B): EnvR[B] = e.map(f)

    def flatMap[B](f: A => EnvR[B]): EnvR[B] = e.flatMap(f)
  }

  def apply[A](f: (Env, SeenSymbols) => (SeenSymbols, A)): EnvR[A] = ReaderT(env => StateT(syms => f(env, syms)))

  lazy val getSeenSymbols: EnvR[Set[Int]] = ReaderT(_ => StateT(syms => (syms, syms.symbols)))

  def noSeenSymbols[A](f: Env => A): EnvR[A] = ReaderT(env => StateT(syms => (syms, f(env))))

  def addSeenSymbol(sym: Symbol): EnvR[Unit] =
    ReaderT(_ => StateT(syms => (syms |+| SeenSymbols(Set(sym.hashCode)), ())))

  def hasSeenSymbol(sym: Symbol): EnvR[Boolean] = getSeenSymbols.map(_.contains(sym.hashCode))

  lazy val env: EnvR[Env] = noSeenSymbols(identity)

  lazy val debug: EnvR[Boolean] = env.map(_.debug)

  given monad: Monad[EnvR] = Monad[EnvR]

  given monoid[A](using m: Monoid[A]): Monoid[EnvR[A]] =
    Monoid.instance(
      EnvR((_, syms) => (syms, m.empty)),
      (x, y) => EnvR { (env, syms1) =>
        val (syms2, a1) = run(x)(env, syms1)
        val (syms3, a2) = run(y)(env, syms2)
        (syms3, m.combine(a1, a2))
      }
    )
}
