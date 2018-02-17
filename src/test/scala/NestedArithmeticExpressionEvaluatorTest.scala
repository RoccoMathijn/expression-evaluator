import NestedArithmeticExpressionEvaluator._
import org.scalatest.{ Matchers, WordSpec }

class NestedArithmeticExpressionEvaluatorTest extends WordSpec with Matchers {

  "The Nested Expression evaluator" can {
    "perform nested arithmetic expressions" in {
      val expression1 = App(Add, Val(1), Val(2))
      val expression2 = App(Add, expression1, App(Add, Val(3), Val(4)))

      val answer: Option[Int] = evaluate(expression2)

      answer should be (Some(10))
    }
  }
}
