package tastyquery

import tastyquery.Symbols.{Symbol, ClassSymbol}

object SymbolsOps {
  private lazy val flagsMethod = classOf[Symbol].getDeclaredMethod("flags")

  extension (s: Symbol) {
    def isErased: Boolean = flagsMethod.invoke(s).asInstanceOf[Flags.FlagSet].is(Flags.Erased)
  }

  extension (c: ClassSymbol) {
    def isRefinementClass: Boolean = c.isRefinementClass
  }
}
