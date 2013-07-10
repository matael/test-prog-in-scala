import org.scalatest.FunSuite
import expr._
import expr.cs._

class ChainSuite extends FunSuite {
  test("x+x should become 2x") {
    val x = Var("x")
    assert(BinOp("*", Number(2), x) == ChainSimplifier.simplify(BinOp("+", x, x)))
  }
  test("x+x+3 should become 2x+3") {
    val x = Var("x")
    val ex = BinOp("+", BinOp("*", Number(2), x), Number(3))
    val init =
      BinOp("+", x,
        BinOp("+", x, Number(3))
      )

    val res = ChainSimplifier.simplify(init)
    assert(ex == res)
  }

  test("x+x+x+x should become 4x") {
    val x = Var("x")
    val ex = BinOp("*", Number(4), x)
    val init =
      BinOp("+", x,
        BinOp("+", x,
          BinOp("+", x, x)
        )
      )
    val res = ChainSimplifier.simplify(init)

    assert(ex === res)
  }
}

