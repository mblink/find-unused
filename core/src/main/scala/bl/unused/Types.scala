package bl.unused

import cats.syntax.foldable.*
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*

object Types {
  def typeBoundsTypes(bounds: TypeBounds): List[Type] = List(bounds.low, bounds.high)

  def symbols(tpe: Type)(using ctx: Context): Set[Symbol] = {
    @annotation.tailrec
    def go(acc: Set[Symbol], seen: Set[Int], types: List[Type]): Set[Symbol] =
      types.filterNot(t => seen.contains(t.hashCode)) match {
        case Nil => acc

        case (t: TypeRef) :: rest => go(acc ++ t.optSymbol, seen + t.hashCode, rest)

        case (t: TermRef) :: rest => go(acc + t.symbol, seen + t.hashCode, rest)

        // TODO - change to this? go(acc, t.underlying :: rest)
        case (t: ThisType) :: rest => go(acc ++ t.tref.optSymbol, seen + t.hashCode, rest)

        case (t: AppliedType) :: rest =>
          go(
            acc,
            seen + t.hashCode,
            t.tycon ::
              t.args.flatMap {
                case t: Type => List(t)
                case w: WildcardTypeArg => typeBoundsTypes(w.bounds)
              } :::
              rest,
          )

        case (t: TypeParamRef) :: rest => go(acc, seen + t.hashCode, typeBoundsTypes(t.bounds) ::: rest)

        case (t: TypeRefinement) :: rest => go(acc, seen + t.hashCode, t.parent :: typeBoundsTypes(t.refinedBounds) ::: rest)

        case (t: TermRefinement) :: rest =>
          go(
            acc,
            seen + t.hashCode,
            t.parent ::
              (t.refinedType match {
                case t: Type => List(t)
                case _: MethodicType => Nil
              }) :::
              rest
          )

        case (t: MatchType) :: rest =>
          go(
            acc,
            seen + t.hashCode,
            t.bound ::
              t.scrutinee ::
              t.cases.flatMap(c => c.pattern :: c.result :: c.paramTypeBounds.flatMap(typeBoundsTypes)) :::
              rest
          )

        case (t: TypeLambda) :: rest =>
          go(
            acc,
            seen + t.hashCode,
            t.paramTypeBounds.flatMap(typeBoundsTypes) :::
              t.resultType ::
              rest
          )

        case (t: AndType) :: rest => go(acc, seen + t.hashCode, t.first :: t.second :: rest)

        case (t: OrType) :: rest => go(acc, seen + t.hashCode, t.first :: t.second :: rest)

        case (t: (
          AnnotatedType
          | ByNameType
          | ConstantType
          | FlexibleType
          | RecThis
          | RecType
          | RepeatedType
          | SkolemType
          | SuperType
          | TermParamRef
        )) :: rest =>
          go(acc, seen + t.hashCode, t.underlying :: rest)

        case (t: (AnyKindType | CustomTransientGroundType | NothingType)) :: rest => go(acc, seen + t.hashCode, rest)
      }

    go(Set.empty, Set.empty, List(tpe))
  }

  def references(tpe: Type)(using ctx: Context): EnvR[References] =
    symbols(tpe).toList.foldMap(References.fromSymbol(_, References.used))

  def prefixReferences(prefix: NonEmptyPrefix)(using ctx: Context): EnvR[References] =
    prefix match {
      case t: Type => references(t)
      case _: PackageRef => References.empty
    }
}
