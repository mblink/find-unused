package tastyquery

import tastyquery.Contexts.Context
import tastyquery.Signatures.{ParamSig, Signature}
import tastyquery.Symbols.{ClassSymbol, Symbol, TermSymbol}
import tastyquery.Types.{MatchType, MethodType, PolyType, Type, TypeOrMethodic}

object SymbolsOps {
  private lazy val flagsMethod = classOf[Symbol].getDeclaredMethod("flags")

  extension (s: Symbol) {
    def isErased: Boolean = flagsMethod.invoke(s).asInstanceOf[Flags.FlagSet].is(Flags.Erased)
  }

  extension (t: TermSymbol) {
    /**
     * The erased types of the parameters of a method's type as seen from `site`, which the compiler's signatures use to
     * tell an override from an overload: one per term parameter and one per type parameter clause. A parameter whose
     * type is a match type tasty-query can't reduce is `None`, since tasty-query erases it to the match type's bound,
     * which may not be what the compiler erases it to.
     */
    def erasedParamsAsSeenFrom(site: Type)(using ctx: Context): List[Option[ParamSig]] = {
      val tpe = t.typeAsSeenFrom(site)
      def paramTypes(tpe: TypeOrMethodic): List[Option[Type]] =
        tpe match {
          case m: MethodType => m.paramTypes.map(Some(_)) ::: paramTypes(m.resultType)
          case p: PolyType => None :: paramTypes(p.resultType)
          case _ => Nil
        }
      def isUnreduced(tpe: Type): Boolean =
        tpe.dealias match {
          case m: MatchType => m.reduced.isEmpty
          case _ => false
        }
      val erased = Signature.fromType(tpe, t.sourceLanguage, None).paramsSig
      val types = paramTypes(tpe)
      if (types.sizeCompare(erased) != 0) erased.map(Some(_))
      else erased.lazyZip(types).map((sig, tp) => Option.unless(tp.exists(isUnreduced))(sig))
    }
  }

  extension (t: Type) {
    // The class a type is an instance of, looking through aliases and applications
    def instanceClass(using ctx: Context): Option[ClassSymbol] = t.classSymbol
  }
}
