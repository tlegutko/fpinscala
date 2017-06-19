package fpinscala.errorhandling

import org.scalatest._

class EitherSpec extends FunSuite {
  test("Either basic stuff") {
    val e = Right(1)
    assert(e.map(_.toString()) == Right("1"))
    assert(e.flatMap(_ => Left("nope")) == Left("nope"))
    assert(e.orElse(Left("nope")) == e)
    assert(e.map2_2(Right(2))(_ + _) == Right(3))
    assert(e.map2(Right(2))(_ + _) == Right(3))
    assert(e.map2(Left(2))(_ + _) == Left(2))
  }

  test ("Sequence and traverse") {
    val l = List(1, 2, 3)
    assert(Either.traverse(l)(x => Right(x.toString)) == Right(List("1", "2", "3")))
    assert(Either.traverse(l){ x =>
             if (x == 2) Left("failure")
             else Right(x.toString)
           } == Left("failure"))
    assert(Either.sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
    assert(Either.sequence(List(Right(1), Left("fail"))) == Left("fail"))
  }
}
