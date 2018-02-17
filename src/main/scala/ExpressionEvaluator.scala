object ExpressionEvaluator {
  trait Arithmetics[T] {
    def apply(basicArithmeticOperator: BasicArithmeticOperator, right: T): T
  }

  trait Comparisons[T] {
    def apply(comparisonOperator: ComparisonOperator, right: T): BooleanAnswer
  }

  trait Logicals[T] {
    def apply(logicalOperator: LogicalOperator, right: T): BooleanAnswer
  }

  abstract class Operator

  abstract class BasicArithmeticOperator extends Operator
  case object Add extends BasicArithmeticOperator
  case object Sub extends BasicArithmeticOperator
  case object Mul extends BasicArithmeticOperator
  case object Div extends BasicArithmeticOperator

  abstract class ComparisonOperator extends Operator
  case object Lt extends ComparisonOperator
  case object LTe extends ComparisonOperator
  case object GTe extends ComparisonOperator
  case object Gt extends ComparisonOperator
  case object Ne extends ComparisonOperator
  case object Eq extends ComparisonOperator

  abstract class LogicalOperator extends Operator
  case object Or extends LogicalOperator
  case object And extends LogicalOperator

  abstract class Expr
  object Expr {
    def evaluate(expr: Expr): Option[Answer] = expr match {
      case answer: Answer => Some(answer)
      case app@App(operator: Operator, left, right) if valid(app) => for {
        x <- evaluate(left)
        y <- evaluate(right)
      } yield x.apply(operator, y)
      case _ => None
    }

    def valid(app: App): Boolean = app match {
      case App(Div, _, IntAnswer(value)) if value == 0 => false
      case _ => true
    }
  }

  abstract class Answer extends Expr {
    def apply(operator: Operator, answer: Answer): Answer
  }

  final case class IntAnswer(value: Int) extends Answer with Arithmetics[Answer] with Comparisons[Answer] {
    def apply(operator: Operator, answer: Answer): Answer = operator match {
      case basicArithmeticOperator: BasicArithmeticOperator => this.apply(basicArithmeticOperator, answer)
      case comparisonOperator: ComparisonOperator => this.apply(comparisonOperator, answer)
      case _ => throw new UnsupportedOperationException(s"Unsupported operation: $operator")
    }

    def apply(operator: BasicArithmeticOperator, right: Answer): IntAnswer = {
      (operator, right) match {
        case (Add, IntAnswer(r)) => IntAnswer(this.value + r)
        case (Sub, IntAnswer(r)) => IntAnswer(this.value - r)
        case (Mul, IntAnswer(r)) => IntAnswer(this.value * r)
        case (Div, IntAnswer(r)) => IntAnswer(this.value / r)
        case (_, _) => throw new IllegalArgumentException(s"Incompatible type. Expected Int got $right")
      }
    }

    def apply(operator: ComparisonOperator, right: Answer): BooleanAnswer = {
      (operator, right) match {
        case (Lt, IntAnswer(r)) => BooleanAnswer(this.value < r)
        case (LTe, IntAnswer(r)) => BooleanAnswer(this.value <= r)
        case (GTe, IntAnswer(r)) => BooleanAnswer(this.value >= r)
        case (Gt, IntAnswer(r)) => BooleanAnswer(this.value > r)
        case (Ne, IntAnswer(r)) => BooleanAnswer(this.value != r)
        case (Eq, IntAnswer(r)) => BooleanAnswer(this.value == r)
        case (_, _) => throw new IllegalArgumentException(s"Incompatible type. Expected IntAnswer got $right")
      }
    }
  }

  final case class BooleanAnswer(value: Boolean) extends Answer with Comparisons[Answer] with Logicals[Answer] {
    def apply(operator: Operator, answer: Answer): Answer = operator match {
      case comparisonOperator: ComparisonOperator => this.apply(comparisonOperator, answer)
      case logicalOperator: LogicalOperator => this.apply(logicalOperator, answer)
      case _ => throw new UnsupportedOperationException(s"Unsupported operation: $operator")    }

    def apply(operator: ComparisonOperator, right: Answer): BooleanAnswer =
      (operator, right) match {
        case (Lt, BooleanAnswer(r)) => BooleanAnswer(this.value < r)
        case (LTe, BooleanAnswer(r)) => BooleanAnswer(this.value <= r)
        case (GTe, BooleanAnswer(r)) => BooleanAnswer(this.value >= r)
        case (Gt, BooleanAnswer(r)) => BooleanAnswer(this.value > r)
        case (Ne, BooleanAnswer(r)) => BooleanAnswer(this.value != r)
        case (Eq, BooleanAnswer(r)) => BooleanAnswer(this.value == r)
        case (_, _) => throw new IllegalArgumentException(s"Incompatible type. Expected BooleanAnswer got $right")
      }

    def apply(operator: LogicalOperator, right: Answer): BooleanAnswer =
      (operator, right) match {
        case (Or, BooleanAnswer(r)) => BooleanAnswer(this.value || r)
        case (And, BooleanAnswer(r)) => BooleanAnswer(this.value && r)
        case (_, _) => throw new IllegalArgumentException(s"Incompatible type. Expected BooleanAnswer got $right")
      }
  }

  final case class StringAnswer(value: String) extends Answer with Comparisons[Answer] {
    def apply(operator: Operator, answer: Answer): Answer = operator match {
      case comparisonOperator: ComparisonOperator => this.apply(comparisonOperator, answer)
      case _ => throw new UnsupportedOperationException(s"Unsupported operation: $operator")
    }

    def apply(operator: ComparisonOperator, right: Answer): BooleanAnswer =
      (operator, right) match {
        case (Lt, StringAnswer(r)) => BooleanAnswer(this.value < r)
        case (LTe, StringAnswer(r)) => BooleanAnswer(this.value <= r)
        case (GTe, StringAnswer(r)) => BooleanAnswer(this.value >= r)
        case (Gt, StringAnswer(r)) => BooleanAnswer(this.value > r)
        case (Ne, StringAnswer(r)) => BooleanAnswer(this.value != r)
        case (Eq, StringAnswer(r)) => BooleanAnswer(this.value == r)
        case (_, _) => throw new IllegalArgumentException(s"Incompatible type. Expected StringAnswer got $right")
      }
  }

  final case class App(operator: Operator, left: Expr, right: Expr) extends Expr
}
