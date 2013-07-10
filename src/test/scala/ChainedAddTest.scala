import org.scalatest.FunSuite
import expr._
import expr.cs._

class ChainSuite extends FunSuite {
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

    assert(ex == res)
  }
}

