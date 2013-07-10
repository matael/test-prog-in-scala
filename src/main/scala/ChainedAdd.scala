package expr.cs
import expr._

object ChainSimplifier {

  def simplify(expr: Expr): Expr = expr match {
    case BinOp("+", x, BinOp("+", y, _)) if x == y =>
      BinOp("*", Number(2), simplify(x))
    case BinOp("+", x, y) if x == y => BinOp("*", Number(2), simplify(x))
    case BinOp("*", Number(a), BinOp("*", Number(b), e)) => BinOp("*", Number(a*b), e)
    case _ => expr
  }
}
