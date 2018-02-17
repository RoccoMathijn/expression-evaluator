import ExpressionEvaluator.{ Eq, Gt, Ne, _ }
import org.scalatest.{ Matchers, WordSpec }

class ExpressionEvaluatorTest extends WordSpec with Matchers {
  "The Expression evaluator" can {
    "do basic arithmetic operations on integers" should {
      "add" in {
        val expression = App(Add, IntVal(5), IntVal(3))

        val answer: Answer = evaluate(expression)

        answer should be(IntAnswer(Some(8)))
      }
      "subtract" in {
        val expression = App(Sub, IntVal(5), IntVal(3))

        val answer: Answer = evaluate(expression)

        answer should be(IntAnswer(Some(2)))
      }
      "multiply" in {
        val expression = App(Mul, IntVal(5), IntVal(3))

        val answer: Answer = evaluate(expression)

        answer should be(IntAnswer(Some(15)))
      }
      "divide" in {
        val expression = App(Div, IntVal(10), IntVal(5))

        val answer: Answer = evaluate(expression)

        answer should be(IntAnswer(Some(2)))
      }
      "return None when dividing by zero" in {
        val expression = App(Div, IntVal(10), IntVal(0))

        val answer: Answer = evaluate(expression)

        answer should be(IntAnswer(None))
      }
    }

    "do comparison operations on integers" should {
      "Lt positive" in {
        val expression = App(Lt, IntVal(1), IntVal(2))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "Lt negative" in {
        val expression = App(Lt, IntVal(2), IntVal(1))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "LTe positive" in {
        val expression = App(LTe, IntVal(1), IntVal(1))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "LTe negative" in {
        val expression = App(LTe, IntVal(2), IntVal(1))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "GTe positive" in {
        val expression = App(GTe, IntVal(5), IntVal(3))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "GTe negative" in {
        val expression = App(GTe, IntVal(3), IntVal(5))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "Gt positive" in {
        val expression = App(Gt, IntVal(5), IntVal(3))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "Gt negative" in {
        val expression = App(Gt, IntVal(3), IntVal(5))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "Ne positive" in {
        val expression = App(Ne, IntVal(5), IntVal(3))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "Ne negative" in {
        val expression = App(Ne, IntVal(5), IntVal(5))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "Eq positive" in {
        val expression = App(Eq, IntVal(5), IntVal(5))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "Eq negative" in {
        val expression = App(Eq, IntVal(5), IntVal(3))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
    }
    "do comparison operations on booleans" should {
      "Lt positive" in {
        val expression = App(Lt, BoolVal(false), BoolVal(true))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "Lt negative" in {
        val expression = App(Lt, BoolVal(true), BoolVal(false))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "LTe positive" in {
        val expression = App(LTe, BoolVal(true), BoolVal(true))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "LTe negative" in {
        val expression = App(LTe, BoolVal(true), BoolVal(false))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "GTe positive" in {
        val expression = App(GTe, BoolVal(true), BoolVal(true))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "GTe negative" in {
        val expression = App(GTe, BoolVal(false), BoolVal(true))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "Gt positive" in {
        val expression = App(Gt, BoolVal(true), BoolVal(false))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "Gt negative" in {
        val expression = App(Gt, BoolVal(true), BoolVal(true))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "Ne positive" in {
        val expression = App(Ne, BoolVal(true), BoolVal(false))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "Ne negative" in {
        val expression = App(Ne, BoolVal(true), BoolVal(true))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
      "Eq positive" in {
        val expression = App(Eq, BoolVal(true), BoolVal(true))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(true)))
      }
      "Eq negative" in {
        val expression = App(Eq, BoolVal(true), BoolVal(false))

        val answer: Answer = evaluate(expression)

        answer should be(BooleanAnswer(Some(false)))
      }
    }
  }
}
