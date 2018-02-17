object NestedArithmeticExpressionEvaluator {

  abstract class Operator
  case object Add extends Operator
  case object Sub extends Operator
  case object Mul extends Operator
  case object Div extends Operator

  abstract class Expr
  case class Val(n: Int) extends Expr
  case class App(operator: Operator, left: Expr, right: Expr) extends Expr

  def evaluate(expr: Expr): Option[Int] = expr match {
    case Val(n) => Some(n)
    case app@App(operator, left, right) if valid(app) => for {
      x: Int <- evaluate(left)
      y: Int <- evaluate(right)
    } yield apply(operator, x, y)
    case _ => None
  }

  def valid(app: App): Boolean = app match {
    case App(Div, _, Val(right)) if right == 0 => false
    case _ => true
  }

  def apply(operator: Operator, left: Int, right: Int): Int = operator match {
    case Add => left + right
    case Sub => left - right
    case Mul => left * right
    case Div => left / right
  }
}
