package bl.unused

import scala.quoted.*

object Quoted {
  def contextBound[A: Type](using Quotes): Expr[List[A]] = '{ List.empty[A] }

  def namedUsing[A](using t: Type[A], q: Quotes): Expr[Option[A]] = '{ Option.empty[A] }

  def inferred[A: Type](a: Expr[A])(using Quotes): Expr[List[A]] = '{ List($a) }

  def nested[A: Type](using Quotes): Expr[List[A]] = '{ ${ contextBound[A] } ++ List.empty[A] }

  def nestedQuote(using Quotes): Expr[Int] = '{ ${ '{ 1 } } + 1 }

  // Not needed by the quote, so it's actually unused
  def unusedEvidence[A: Type](using Quotes): Expr[Int] = '{ 1 }

  // Only the splice's type mentions `A`
  def spliceType[A: Type](a: Expr[A])(using Quotes): Expr[String] = '{ $a.toString }

  // The quote only mentions `A`, so `B`'s evidence is unused
  def partial[A: Type, B: Type](using Quotes): Expr[List[A]] = '{ List.empty[A] }

  // `A` is only mentioned by code inside the splice, which isn't healed, so its evidence is unused
  def inSplice[A: Type](a: Expr[A])(using Quotes): Expr[Int] = '{ ${ size(a) } + 1 }

  def size[T](a: Expr[T])(using Quotes): Expr[Int] = Expr(a.hashCode)

  // A quote nested in a splice is healed too
  def quoteInSplice[A: Type](using Quotes): Expr[List[A]] = '{ ${ '{ List.empty[A] } } }

  // So is one through an alias in a local object
  def aliasInObject[A: Type](using Quotes): Expr[List[A]] = {
    object Aliases { type L = List[A] }
    '{ Nil: Aliases.L }
  }

  // Healing sees through the local alias
  def aliased[A: Type](using Quotes): Expr[List[A]] = {
    type L = List[A]
    '{ Nil: L }
  }
}

// A class's `Type` evidence is a field, and only the bytecode shows the healed quote reading it
class QuotedClass[A: Type](using Quotes) {
  def empty: Expr[List[A]] = '{ List.empty[A] }
}

object QuotedTypes {
  def typeOf[T: Type](using Quotes): Boolean = Type.of[T] match {
    case '[Int] => true
    case _ => false
  }

  def synthesizedTypeOf[F[_]: Type, T: Type](using Quotes): Option[Expr[F[T]]] = Expr.summon[F[T]]

  def pattern[T: Type](e: Expr[Any])(using Quotes): Boolean = e match {
    case '{ $x: List[T] } => true
    case _ => false
  }
}
