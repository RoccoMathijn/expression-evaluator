import ExpressionEvaluator._
import org.scalatest.{ Matchers, WordSpec }

class ExpressionEvaluatorTest extends WordSpec with Matchers {
  "The Expression evaluator" can {
    "perform arithmetic expressions" in {
      val expression = App(Add, IntAnswer(1), IntAnswer(2))

      val answer: Option[Answer] = evaluate(expression)

      answer should be (Some(IntAnswer(3)))
    }

    "perform nested arithmetics" in {
      val expression1 = App(Add, IntAnswer(1), IntAnswer(2))
      val expression2 = App(Add, expression1, App(Add, IntAnswer(3), IntAnswer(4)))

      val answer: Option[Answer] = evaluate(expression2)

      answer should be (Some(IntAnswer(10)))
    }

    "throw an error for unsupported operations" in {
      val expression = App(Bla, IntAnswer(1), IntAnswer(2))

      assertThrows[IllegalArgumentException](evaluate(expression))
    }

    "perform comparison" in {
      val expression = App(Eq, IntAnswer(5), IntAnswer(2))

      val answer: Option[Answer] = evaluate(expression)

      answer should be (Some(BooleanAnswer(false)))
    }

    "perform boolean comparisons" in {
      val expression = App(Eq, BooleanAnswer(true), BooleanAnswer(true))

      val answer: Option[Answer] = evaluate(expression)

      answer should be (Some(BooleanAnswer(true)))
    }

    "not mix answertypes" in {
      val expression = App(Eq, BooleanAnswer(true), IntAnswer(5))

      assertThrows[IllegalArgumentException](evaluate(expression))
    }
  }
}
