package tastyquery

import tastyquery.Contexts.Context
import tastyquery.Symbols.{Symbol, ClassSymbol}
import tastyquery.Types.Type

object SymbolsOps {
  private lazy val flagsMethod = classOf[Symbol].getDeclaredMethod("flags")

  extension (s: Symbol) {
    def isErased: Boolean = flagsMethod.invoke(s).asInstanceOf[Flags.FlagSet].is(Flags.Erased)
  }

  extension (c: ClassSymbol) {
    def isRefinementClass: Boolean = c.isRefinementClass
  }

  extension (t: Type) {
    // The class a type is an instance of, looking through aliases and applications
    def instanceClass(using ctx: Context): Option[ClassSymbol] = t.classSymbol
  }
}
