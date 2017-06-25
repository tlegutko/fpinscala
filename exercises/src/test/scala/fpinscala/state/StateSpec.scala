package fpinscala.state

import org.scalatest._

class StateSpec extends FunSuite {

  def rng(): RNG =
    RNG.Simple(System.currentTimeMillis())

  test("Generating random stuff") {
    assert(RNG.nonNegativeInt2(rng)._1 > 0)
    assert(RNG.nonNegativeInt(rng)._1 > 0)
    val rngDouble = RNG.double(rng)._1
    assert(rngDouble >= 0 && rngDouble < 1)
    val rngDoubleWithMap = RNG.doubleWithMap()(rng)._1
    assert(rngDoubleWithMap >= 0 && rngDoubleWithMap < 1)
    val rngPosMax = RNG.positiveMax(10)(rng)._1 
    assert(rngPosMax >= 0 && rngPosMax <= 10)
  }
}
