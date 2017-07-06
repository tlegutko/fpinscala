package fpinscala.parallelism

import java.util.concurrent.Executors
import org.scalatest.FunSuite
import Par._

class ParSpect extends FunSuite {

  val es = Executors.newFixedThreadPool(8)

  test("asyncF") {
    assert(asyncF((x: Int) => x * 2)(5)(es).get == 10)
  }

  test("product and mapPrimitive") {
    assert(product(async("A"), async(1))(es).get == (("A", 1)))
    assert(mapPrimitive(async(1))(_ * 2)(es).get == 2)
    assert(map2AsCombinator(async(2), async(3))(_ * _)(es).get == 6)
  }

  test("parFilter") {
    assert(parFilter(List(1, 2, 3))(_ % 2 == 0)(es).get == List(2))
  }

  test("parMax") {
    assert(parMax(Vector(1, 3, 4, 5, 3))((a, b) => if(a >= b) a else b)(0)(es).get == 5)
  }

  test("parCount") {
    assert(parCount(List("what a day", "it is", "today"))(es).get == 6)
  }
}
