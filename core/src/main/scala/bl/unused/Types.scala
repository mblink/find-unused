package bl.unused

import cats.syntax.either.*
import cats.syntax.foldable.*
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*

object Types {
  def typeBoundsTypes(bounds: TypeBounds): List[Type] = List(bounds.low, bounds.high)

  // Resolving a symbol can throw when tasty-query can't load the class it's in, e.g. because it fails to unpickle a
  // default getter's name in a library's TASTy. A symbol that can't be resolved can't be reported on, so it's skipped
  // rather than stopping the whole analysis.
  private def resolved(symbol: => Option[Symbol]): Option[Symbol] = Either.catchNonFatal(symbol).toOption.flatten

  def symbols(tpe: Type)(using ctx: Context): Set[Symbol] = {
    // `seen` holds the types themselves rather than their hash codes, since types don't override `hashCode`
    @annotation.tailrec
    def go(acc: Set[Symbol], seen: Set[Type], types: List[Type]): Set[Symbol] =
      types.filterNot(seen.contains) match {
        case Nil => acc

        case (t: TypeRef) :: rest => go(acc ++ resolved(t.optSymbol), seen + t, rest)

        case (t: TermRef) :: rest => go(acc ++ resolved(Some(t.symbol)), seen + t, rest)

        // TODO - change to this? go(acc, t.underlying :: rest)
        case (t: ThisType) :: rest => go(acc ++ resolved(t.tref.optSymbol), seen + t, rest)

        case (t: AppliedType) :: rest =>
          go(
            acc,
            seen + t,
            t.tycon ::
              t.args.flatMap {
                case t: Type => List(t)
                case w: WildcardTypeArg => typeBoundsTypes(w.bounds)
              } :::
              rest,
          )

        case (t: TypeParamRef) :: rest => go(acc, seen + t, typeBoundsTypes(t.bounds) ::: rest)

        case (t: TypeRefinement) :: rest => go(acc, seen + t, t.parent :: typeBoundsTypes(t.refinedBounds) ::: rest)

        case (t: TermRefinement) :: rest =>
          go(
            acc,
            seen + t,
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
            seen + t,
            t.bound ::
              t.scrutinee ::
              t.cases.flatMap(c => c.pattern :: c.result :: c.paramTypeBounds.flatMap(typeBoundsTypes)) :::
              rest
          )

        case (t: TypeLambda) :: rest =>
          go(
            acc,
            seen + t,
            t.paramTypeBounds.flatMap(typeBoundsTypes) :::
              t.resultType ::
              rest
          )

        case (t: AndType) :: rest => go(acc, seen + t, t.first :: t.second :: rest)

        case (t: OrType) :: rest => go(acc, seen + t, t.first :: t.second :: rest)

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
          go(acc, seen + t, t.underlying :: rest)

        case (t: (AnyKindType | CustomTransientGroundType | NothingType)) :: rest => go(acc, seen + t, rest)
      }

    go(Set.empty, Set.empty, List(tpe))
  }

  def references(tpe: Type)(using ctx: Context): EnvR[Refs] =
    symbols(tpe).toList.foldMap(References.fromSymbol(_, References.used))

  def prefixReferences(prefix: NonEmptyPrefix)(using ctx: Context): EnvR[Refs] =
    prefix match {
      case t: Type => references(t)
      case _: PackageRef => References.empty
    }
}
