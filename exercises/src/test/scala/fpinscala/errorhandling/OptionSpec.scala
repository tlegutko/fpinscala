package fpinscala.errorhandling

import org.scalatest._

class OptionSpec extends FunSuite {
  test("basic option stuff") {
    val s = Some(1)
    val n = None: Option[Int]
    assert(s.map(_ + 1) == Option(2))
    assert(n.map(_ + 1) == None)
    assert(s.getOrElse(2) == 1)
    assert(n.getOrElse(2) == 2)
    assert(s.flatMap(_ => Option(2)) == Option(2))
    assert(n.flatMap(_ => Option(2)) == None)
    assert(s.orElse(Option(2)) == Option(1))
    assert(n.orElse(Option(2)) == Option(2))
    assert(s.filter(_ == 1) == Option(1))
    assert(s.filter(_ != 1) == None)
    assert(n.filter(_ != 1) == None)
  }

  test("variance") {
    assert(Option.variance(Seq(-5, 1, 8, 7, 2)) == Option(21.84))
  }

  test("map2") {
    val s1 = Some(1)
    val s2 = Some(2)
    val n = None: Option[Int]
    assert(Option.map2(s1, s2)(_ + _) == Option(3))
    assert(Option.map2(n, s2)(_ + _) == None)
    assert(Option.map2(s1, n)(_ + _) == None)
  }

  test("sequence") {
    assert(Option.sequence(List(Option(1), Option(2))) == Option(List(1, 2)))
    assert(Option.sequence(List(Option(1), Option(2), None)) == None)
    assert(Option.sequenceWithTraverse(List(Option(1), Option(2))) == Option(List(1, 2)))
    assert(Option.sequenceWithTraverse(List(Option(1), Option(2), None)) == None)
  }

  test("traverse") {
    assert(Option.traverse(List(1, 2))(x => Option(x.toString)) == Option(List("1", "2")))
    assert(Option.traverse(List(1, 2))(x => None) == None)
  }
}
