package bl.unused

import cats.Id
import cats.data.{Kleisli, Reader}
import java.nio.file.Path
import tastyquery.Symbols.Symbol

case class Env(
  debug: Boolean,
  rootDirectory: Option[Path],
  packages: Seq[String],
  seenSymbols: Set[Int],
  symbolIsValid: Symbol => Boolean,
)

type EnvR[A] = Reader[Env, A]

object EnvR {
  def apply[A](f: Env => A): EnvR[A] = Reader(f)

  lazy val env: EnvR[Env] = Kleisli.ask[Id, Env]
}
