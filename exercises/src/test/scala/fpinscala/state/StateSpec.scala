package fpinscala.state

import org.scalatest._
import RNG._

class StateSpec extends FunSuite {

  def rng(): RNG =
    Simple(System.currentTimeMillis())

  test("Generating random stuff") {
    assert(nonNegativeInt2(rng)._1 > 0)
    assert(nonNegativeInt(rng)._1 > 0)
    val rngDouble = double(rng)._1
    assert(rngDouble >= 0 && rngDouble < 1)
    val rngDoubleWithMap = doubleWithMap(rng)._1
    assert(rngDoubleWithMap >= 0 && rngDoubleWithMap < 1)
    val rngPosMax = positiveMax(10)(rng)._1
    assert(rngPosMax >= 0 && rngPosMax <= 10)
  }

  test("sequence") {
    val a = (rng: RNG) => ("a", rng)
    val b = (rng: RNG) => ("b", rng)
    val c = (rng: RNG) => ("c", rng)
    assert(sequence2(List(a, b, c))(rng)._1 == List("a", "b", "c"))
    assert(sequence(List(a, b, c))(rng)._1 == List("a", "b", "c"))
  }

  test("flatMap") {
    assert(nonNegativeIntWithFlatMap(rng)._1 >= 0)
    val a = (rng: RNG) => ("a", rng)
    val b = (rng: RNG) => ("b", rng)
    assert(mapWithFlatMap(a)(x => x + x)(rng)._1 == "aa")
    assert(map2WithFlatMap2(a, b)((x, y) => x + y)(rng)._1 == "ab")
    assert(map2WithFlatMap(a, b)((x, y) => x + y)(rng)._1 == "ab")
  }

  test("State") {
    val s = State[Int, Char](x => ((97 + x).toChar, x + 1))
    val s2 = State[Int, Int](x => (x, x + 1))
    assert(s.mapV2(c => c.toString + c.toString).run(0) == (("aa", 1)))
    assert(s.map2V2(s2)((a, b) => a.toString + b.toString).run(0) == (("a1", 2)))
    assert(s.map(c => c.toString + c.toString).run(0) == (("aa", 1)))
    assert(s.map2(s2)((a, b) => a.toString + b.toString).run(0) == (("a1", 2)))
    assert(s2.flatMap(x => State(s => (x*3, s + 1))).run(1) == ((3, 3)))
    assert(State.sequence(List(s, s2)).run(0) == ((List('a', 1), 2)))
  }
}
