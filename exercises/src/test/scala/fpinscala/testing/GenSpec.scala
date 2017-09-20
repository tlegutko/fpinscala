package fpinscala.testing

import org.scalatest._
import fpinscala.testing.Prop._
import fpinscala.testing.Gen._

class GenSpec extends FunSuite {

  test("listMax") {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf1(smallInt)) { l =>
      val max = l.max
      !l.exists(_ > max)
      true
    }
    assert(Prop.run(maxProp))
  }
}
