package fpinscala.gettingstarted

import org.scalatest._

class GettingStartedSpec extends FunSuite {

  test("isSorted") {
    def ascendingSort(a: Int, b: Int): Boolean = a <= b
    assert(!PolymorphicFunctions.isSorted(Array(1, 2, 5, 3), ascendingSort))
    assert(PolymorphicFunctions.isSorted(Array(1, 2, 3, 5), ascendingSort))
    assert(PolymorphicFunctions.isSorted(Array(1, 2, 2, 3), ascendingSort))
  }

  test("partial1") {
    def f(a: Int, b: Double): String = a.toString + " " + b.toString
    assertResult("5 6.0") {
      PolymorphicFunctions.partial1(5, f)(6.0)
    }
  }

  test("curry") {
    def f(a: Int, b: Double): String = a.toString + " " + b.toString
    assertResult("5 6.0") {
      PolymorphicFunctions.curry(f)(5)(6.0)
    }
  }

  test("uncurry") {
    def f(a: Int, b: Double): String = a.toString + " " + b.toString
    assertResult("5 6.0") {
      PolymorphicFunctions.uncurry(PolymorphicFunctions.curry(f))(5, 6.0)
    }
  }

  test ("compose") {
    def f(a: Int): Double = a.toDouble
    def g(b: Double): String = b.toString
    assertResult("5.0") {
      PolymorphicFunctions.compose(g,f)(5)
    }
  }
}
