package bl.unused

import scala.quoted.*

// Macros whose expansion is a constant, which may not keep a position in the macro's definition
object ConstantMacros {
  transparent inline def lifted: Int = ${ liftedImpl }
  def liftedImpl(using Quotes): Expr[Int] = Expr(42)

  transparent inline def reflected: Int = ${ reflectedImpl }
  def reflectedImpl(using q: Quotes): Expr[Int] = {
    import q.reflect.*
    Literal(IntConstant(7)).asExprOf[Int]
  }
}
