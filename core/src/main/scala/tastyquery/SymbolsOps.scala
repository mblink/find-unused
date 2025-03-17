package tastyquery

import tastyquery.Symbols.ClassSymbol

object SymbolsOps {
  extension (c: ClassSymbol) {
    def isRefinementClass: Boolean = c.isRefinementClass
  }
}
