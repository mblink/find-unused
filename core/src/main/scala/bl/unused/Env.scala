package bl.unused

import cats.data.Reader
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
