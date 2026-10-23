package bl.unused

import cats.{Id, Monad, Monoid}
import cats.data.ReaderT
import java.nio.file.Path
import scala.collection.mutable
import tastyquery.Symbols.Symbol

case class Env(
  debug: Boolean,
  rootDirectory: Option[Path],
  isInPackages: Symbol => Boolean,
  symbolIsValid: Symbol => Boolean,
  // The classes and methods that reference a symbol in bytecode in a way that TASTy can't see, see `Bytecode`
  bytecodeCallers: Symbol => Set[Bytecode.Caller],
  // The symbols the traversal has already visited, so it visits each one once. The symbols themselves, rather than
  // their hash codes, because `Symbol` doesn't override `hashCode`, so different symbols can share an identity hash code.
  seen: mutable.Set[Symbol] = mutable.Set.empty,
  // The definitions enclosing the tree being traversed, innermost first, so references can be attributed to them
  enclosing: List[Symbol] = Nil,
)

// Reads an `Env`
opaque type EnvR[A] = ReaderT[Id, Env, A]

object EnvR {
  extension [A](e: EnvR[A]) {
    def run(env: Env): A = e.run(env)

    def map[B](f: A => B): EnvR[B] = e.map(f)

    def flatMap[B](f: A => EnvR[B]): EnvR[B] = e.flatMap(f)

    /** Runs with the `Env` changed by `f` */
    def local(f: Env => Env): EnvR[A] = e.local(f)
  }

  def fromEnv[A](f: Env => A): EnvR[A] = ReaderT[Id, Env, A](f)

  /** Marks `sym` visited, and returns whether it was already */
  def markSeen(sym: Symbol): EnvR[Boolean] = fromEnv(env => !env.seen.add(sym))

  lazy val env: EnvR[Env] = fromEnv(identity)

  lazy val debug: EnvR[Boolean] = env.map(_.debug)

  given monad: Monad[EnvR] = Monad[EnvR]

  // Runs `x` before `y`, so what `x` visits, `y` doesn't visit again
  given monoid[A](using m: Monoid[A]): Monoid[EnvR[A]] =
    Monoid.instance(
      fromEnv(_ => m.empty),
      (x, y) => fromEnv { env =>
        val a = x.run(env)
        m.combine(a, y.run(env))
      },
    )
}
