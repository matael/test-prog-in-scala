package expr.cs
import expr._

object ChainSimplifier {

  def simplify(expr: Expr): Expr = expr match {
    case BinOp("+", x, BinOp("+", y, e)) if x == y =>
      simplify(BinOp("+", BinOp("*", Number(2), simplify(x)), simplify(e)))
    case BinOp("+", x, y) if x == y => simplify(BinOp("*", Number(2), simplify(x)))
    case BinOp("*", Number(a), BinOp("*", Number(b), e)) =>
      simplify(BinOp("*", Number(a*b), simplify(e)))
    case _ => expr
  }
}
