object SimpleExpressionEvaluator {

  def evaluate(expr: Expr): Answer = expr match {
    case IntVal(n) => IntAnswer(Some(n))
    case app@App(_: ArithmeticOperator, _, _) if valid(app) => IntAnswer(Some(applyArithmetic(app)))
    case App(_: ArithmeticOperator, _, _) => IntAnswer(None)
    case app@App(_: ComparisonOperator, _, _) if valid(app) => BooleanAnswer(Some(applyComparison(app)))
    case App(_: ComparisonOperator, _, _) => BooleanAnswer(None)
    case app@App(_: BooleanOperator, _, _) if valid(app) => BooleanAnswer(Some(applyLogicalComparison(app)))
    case App(_: BooleanOperator, _, _) => BooleanAnswer(None)
  }

  def applyArithmetic(app: App): Int = app match {
    case App(Add, IntVal(left), IntVal(right)) => left + right
    case App(Sub, IntVal(left), IntVal(right)) => left - right
    case App(Mul, IntVal(left), IntVal(right)) => left * right
    case App(Div, IntVal(left), IntVal(right)) => left / right
    case _ => throw new Error("This shouldn't happen!")
  }

  def applyComparison(app: App): Boolean = app match {
    case App(_, _: BoolVal, _: IntVal) => throw new Error("Incomparable types!")
    case App(_, _: IntVal, _: BoolVal) => throw new Error("Incomparable types!")
    case App(Lt, BoolVal(left), BoolVal(right)) => left < right
    case App(LTe, BoolVal(left), BoolVal(right)) => left <= right
    case App(GTe, BoolVal(left), BoolVal(right)) => left >= right
    case App(Gt, BoolVal(left), BoolVal(right)) => left > right
    case App(Ne, BoolVal(left), BoolVal(right)) => left != right
    case App(Eq, BoolVal(left), BoolVal(right)) => left == right
    case App(Lt, IntVal(left), IntVal(right)) => left < right
    case App(LTe, IntVal(left), IntVal(right)) => left <= right
    case App(GTe, IntVal(left), IntVal(right)) => left >= right
    case App(Gt, IntVal(left), IntVal(right)) => left > right
    case App(Ne, IntVal(left), IntVal(right)) => left != right
    case App(Eq, IntVal(left), IntVal(right)) => left == right
    case _ => throw new Error("This shouldn't happen!")
  }

  def applyLogicalComparison(app: App): Boolean = app match {
    case App(Or, BoolVal(left), BoolVal(right)) => left || right
    case App(And, BoolVal(left), BoolVal(right)) => left && right
    case App(Not, BoolVal(left), BoolVal(right)) => left != right
    case _ => throw new Error("This shouldn't happen!")
  }

  def valid(app: App): Boolean = app match {
    case App(Div, _, IntVal(right)) if right == 0 => false
    case _ => true
  }

  abstract class Expr
  case class IntVal(n: Int) extends Expr
  case class BoolVal(n: Boolean) extends Expr
  case class App(operator: Operator, left: Expr, right: Expr) extends Expr

  abstract class Operator
  abstract class ArithmeticOperator extends Operator
  abstract class ComparisonOperator extends Operator
  abstract class BooleanOperator extends Operator


  abstract class Answer
  case class IntAnswer(value: Option[Int]) extends Answer
  case class BooleanAnswer(value: Option[Boolean]) extends Answer

  case object Add extends ArithmeticOperator
  case object Sub extends ArithmeticOperator
  case object Mul extends ArithmeticOperator
  case object Div extends ArithmeticOperator

  case object Lt extends ComparisonOperator
  case object LTe extends ComparisonOperator
  case object GTe extends ComparisonOperator
  case object Gt extends ComparisonOperator
  case object Ne extends ComparisonOperator
  case object Eq extends ComparisonOperator

  case object Or extends BooleanOperator
  case object And extends BooleanOperator
  case object Not extends BooleanOperator
}
