package expr
import layout.Element
import layout.Element.elem

// sealed means that subclasses can only be defined in this file.
sealed abstract class Expr

// case in front of class adds :
//
//  - a factory mathod named after the class
//  - a val keyword on each arguments
//  - methods : hashCode, equals and toString
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

trait OpPrecedences {

  // contains operator in groups of increasing precedence
  val opGroups =
    Array(
      Set("|", "||"),
      Set("&", "&&"),
      Set("^"),
      Set("==", "!="),
      Set("<", "<=", ">", ">="),
      Set("+", "-"),
      Set( "*", "%")
    )

  // mapping operator -> precedence
  val precedence = {
    val assocs =
      for {
        i <- 0 until opGroups.length
        op <- opGroups(i)
      } yield op -> i
    assocs.toMap
  }

  val unaryPrecedence = opGroups.length
  val fractionPrecedence = -1

}

class ExprFormatter extends OpPrecedences {

  private def format(e: Expr, enclPrec: Int): Element =
    e match {

      case Var(name) => elem(name)
      case Number(num) =>
        def stripDot(s: String) =
          if (s endsWith ".0") s.substring(0, s.length -2)
          else s
        elem(stripDot(num.toString))

      case UnOp(op, arg) =>
        elem(op) beside format(arg, unaryPrecedence)

      case BinOp("/", left, right) =>
        val top = format(left, fractionPrecedence)
        val bot = format(right, fractionPrecedence)
        val line = elem('-', top.width max bot.width, 1)
        val frac = top above line above bot
        if (enclPrec != fractionPrecedence) frac
        else elem(" ") beside frac beside elem(" ")

      case BinOp(op, left, right) =>
        val opPrec = precedence(op)
        val l = format(left, opPrec)
        val r = format(right, opPrec)
        val oper = l beside elem(" "+ op + " ") beside r
        if (enclPrec <= opPrec) oper
        else elem("(") beside oper beside elem(")")

    }

  def format(e: Expr): Element = format(e, 0)
}

object Simplifier {

  def simplify(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => simplify(e)
    case BinOp("+", e, Number(0.0)) => simplify(e)
    case BinOp("*", e, Number(1.0)) => simplify(e)
    case BinOp("-", e, Number(0.0)) => simplify(e)
    case BinOp("+", e1, e2) if e1 == e2 =>
      BinOp("*", simplify(e1), Number(2))
    case BinOp("+", Number(a), Number(b)) =>
      Number(a*b)
    case _ => expr
  }
}
