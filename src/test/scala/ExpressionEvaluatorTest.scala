import ExpressionEvaluator._
import ExpressionEvaluator.Expr.evaluate
import org.scalatest.{ Matchers, WordSpec }

class ExpressionEvaluatorTest extends WordSpec with Matchers {
  "The Expression evaluator" can {
    "do basic arithmetic operations" should {
      "add" in {
        val expression = App(Add, IntAnswer(5), IntAnswer(3))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(IntAnswer(8)))
      }
      "subtract" in {
        val expression = App(Sub, IntAnswer(5), IntAnswer(3))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(IntAnswer(2)))
      }
      "multiply" in {
        val expression = App(Mul, IntAnswer(5), IntAnswer(3))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(IntAnswer(15)))
      }
      "divide" in {
        val expression = App(Div, IntAnswer(10), IntAnswer(5))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(IntAnswer(2)))
      }
      "return None when dividing by zero" in {
        val expression = App(Div, IntAnswer(10), IntAnswer(0))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(None)
      }
    }
    
    "do comparison operations" should {
      "on strings Lt positive" in {
        val expression = App(Lt, StringAnswer("A"), StringAnswer("B"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on strings Lt negative" in {
        val expression = App(Lt, StringAnswer("B"), StringAnswer("A"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on strings LTe positive" in {
        val expression = App(LTe, StringAnswer("A"), StringAnswer("A"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on strings LTe negative" in {
        val expression = App(LTe, StringAnswer("B"), StringAnswer("A"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on strings GTe positive" in {
        val expression = App(GTe, StringAnswer("B"), StringAnswer("B"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on strings GTe negative" in {
        val expression = App(GTe, StringAnswer("A"), StringAnswer("B"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on strings Gt positive" in {
        val expression = App(Gt, StringAnswer("B"), StringAnswer("A"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on strings Gt negative" in {
        val expression = App(Gt, StringAnswer("A"), StringAnswer("B"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on strings Ne positive" in {
        val expression = App(Ne, StringAnswer("A"), StringAnswer("B"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on strings Ne negative" in {
        val expression = App(Ne, StringAnswer("A"), StringAnswer("A"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on strings Eq positive" in {
        val expression = App(Eq, StringAnswer("A"), StringAnswer("A"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on strings Eq negative" in {
        val expression = App(Eq, StringAnswer("A"), StringAnswer("B"))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on integers Lt positive" in {
        val expression = App(Lt, IntAnswer(1), IntAnswer(2))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on integers Lt negative" in {
        val expression = App(Lt, IntAnswer(2), IntAnswer(1))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on integers LTe positive" in {
        val expression = App(LTe, IntAnswer(1), IntAnswer(1))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on integers LTe negative" in {
        val expression = App(LTe, IntAnswer(2), IntAnswer(1))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on integers GTe positive" in {
        val expression = App(GTe, IntAnswer(5), IntAnswer(3))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on integers GTe negative" in {
        val expression = App(GTe, IntAnswer(3), IntAnswer(5))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on integers Gt positive" in {
        val expression = App(Gt, IntAnswer(5), IntAnswer(3))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on integers Gt negative" in {
        val expression = App(Gt, IntAnswer(3), IntAnswer(5))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on integers Ne positive" in {
        val expression = App(Ne, IntAnswer(5), IntAnswer(3))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on integers Ne negative" in {
        val expression = App(Ne, IntAnswer(5), IntAnswer(5))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on integers Eq positive" in {
        val expression = App(Eq, IntAnswer(5), IntAnswer(5))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on integers Eq negative" in {
        val expression = App(Eq, IntAnswer(5), IntAnswer(3))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on booleans Lt positive" in {
        val expression = App(Lt, BooleanAnswer(false), BooleanAnswer(true))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on booleans Lt negative" in {
        val expression = App(Lt, BooleanAnswer(true), BooleanAnswer(false))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on booleans LTe positive" in {
        val expression = App(LTe, BooleanAnswer(true), BooleanAnswer(true))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on booleans LTe negative" in {
        val expression = App(LTe, BooleanAnswer(true), BooleanAnswer(false))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on booleans GTe positive" in {
        val expression = App(GTe, BooleanAnswer(true), BooleanAnswer(true))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on booleans GTe negative" in {
        val expression = App(GTe, BooleanAnswer(false), BooleanAnswer(true))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on booleans Gt positive" in {
        val expression = App(Gt, BooleanAnswer(true), BooleanAnswer(false))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on booleans Gt negative" in {
        val expression = App(Gt, BooleanAnswer(true), BooleanAnswer(true))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on booleans Ne positive" in {
        val expression = App(Ne, BooleanAnswer(true), BooleanAnswer(false))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on booleans Ne negative" in {
        val expression = App(Ne, BooleanAnswer(true), BooleanAnswer(true))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "on booleans Eq positive" in {
        val expression = App(Eq, BooleanAnswer(true), BooleanAnswer(true))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "on booleans Eq negative" in {
        val expression = App(Eq, BooleanAnswer(true), BooleanAnswer(false))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
    }
    "do logical operations" should {
      "Or positive" in {
        val expression = App(Or, BooleanAnswer(true), BooleanAnswer(false))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "Or negative" in {
        val expression = App(Or, BooleanAnswer(false), BooleanAnswer(false))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
      "And positive" in {
        val expression = App(And, BooleanAnswer(true), BooleanAnswer(true))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "And negative" in {
        val expression = App(And, BooleanAnswer(true), BooleanAnswer(false))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(false)))
      }
    }
    "do nested operations" should {
      "nested arithmetic operation" in {
        val expression = App(Mul, IntAnswer(5), App(Mul, IntAnswer(5), IntAnswer(5)))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(IntAnswer(125)))
      }
      "nested logical operation" in {
        val expression = App(And, BooleanAnswer(true), App(And, BooleanAnswer(true), BooleanAnswer(true)))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
      "nested mixed operation" in {
        val expression = App(And, BooleanAnswer(true), App(Eq, StringAnswer("Foo"), StringAnswer("Foo")))

        val answer: Option[Answer] = evaluate(expression)

        answer should be(Some(BooleanAnswer(true)))
      }
    }
    "do error handling" should {
      "throw an error for arithmetic operations on unsupported types" in {
        val expression = App(Div, BooleanAnswer(true), BooleanAnswer(true))

        assertThrows[IllegalArgumentException](evaluate(expression))
      }
      "throw an error for logical operations on unsupported types" in {
        val expression = App(And, StringAnswer("Foo"), StringAnswer("Bar"))

        assertThrows[IllegalArgumentException](evaluate(expression))
      }
      "throw an error when evaluating mixed answertypes" in {
        val expression = App(Eq, BooleanAnswer(true), IntAnswer(5))

        assertThrows[IllegalArgumentException](evaluate(expression))
      }
    }
  }
}
