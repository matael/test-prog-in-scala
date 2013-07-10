import expr._
import expr.Simplifier.simplify

object Express extends App {

  val f = new ExprFormatter

  val e1 = BinOp("*", BinOp("/", Number(1), Number(2)), BinOp("+", Var("x"), Number(1)))
  val e2 = BinOp("+", BinOp("/", Var("x"), Number(2)), BinOp("/", Number(1.5), Var("x")))
  val e3 = BinOp("/", e1, e2)

  val x = Var("x")
  val e4 = BinOp("/",
    BinOp("-",
      BinOp("+",
        Number(4),
        BinOp("+", x,
          BinOp("+", x,
            BinOp("+", x,
              BinOp("+", x,
                BinOp("+", x, x)
              )
            )
          )
        )
      ),
      Var("b")
    ),
    BinOp("*",
      Number(2),
      BinOp("/",
        BinOp("*",
          Number(3),
          Var("a")),
        Number(4)
      )
    )
  )
  def show(e: Expr) = println(f.format(e)+ "\n\n")

  for (e <- Array(e1, e2, e3, e4)) show(e)

  println("--- Simplification ---")
  for {
    e <- Array(e1, e2, e3, e4)
  } println(f.format(simplify(e))+"\n\n")
}

