
package fpinscala.laziness

import org.scalatest._

class StreamSpec extends FunSuite {
  test("Stream to List") {
    val stream = Cons(() => "a", () => Cons(() => "b", () => Empty))
    assert(stream.toList() == List("a", "b"))
    assert(stream.toList1() == List("a", "b"))
    assert(stream.toList2() == List("a", "b"))
  }

  test("take, drop and takeWhile") {
    val stream = Stream(1, 2, 3, 4)
    assert(stream.take(3).toList() == List(1, 2, 3))
    assert(stream.take2(3).toList() == List(1, 2, 3))
    assert(stream.drop(3).toList() == List(4))
    assert(stream.drop2(3).toList() == List(4))
    assert(stream.takeWhile(_ <= 3).toList() == List(1, 2, 3))
  }

  test("forAll, takeWhileWithFold and headOption") {
    val s = Stream(2, 4, 7, 8)
    assert(s.forAll(_ % 2 == 0) == false)
    assert(s.forAll(_ > 1) == true)
    assert(s.takeWhileWithFold(_ < 8).toList() == List(2, 4, 7))
    assert(s.headOption == Some(2))
    assert(Empty.headOption == None)
  }

  test("map, filter, append and flatMap using foldRight") {
    val s = Stream(1, 2, 3)
    assert(s.map(_ * 2).toList() == List(2, 4, 6))
    assert(s.filter(_ % 2 == 1).toList() == List(1, 3))
    assert(s.append(Stream(4, 5)).toList() == List(1, 2, 3, 4, 5))
    assert(s.flatMap(x => Stream(x * 2)).toList() == List(2, 4, 6))
  }

  test("constant, from, fibs") {
    assert(Stream.constant(("a", 3)).take(2).toList() == List(("a", 3), ("a", 3)))
    assert(Stream.from(10).take(4).toList() == List(10, 11, 12, 13))
    assert(Stream.fibs().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("unfold") {
    assert(Stream.unfold(List(1, 2, 3))(l => l match {
      case h :: t => Some((h * 3, t))
      case Nil => None
    }).toList() == List(3, 6, 9))
  }

  test("constant, from, fibs using unfold") {
    assert(Stream.constantWithUnfold(("a", 3)).take(2).toList() == List(("a", 3), ("a", 3)))
    assert(Stream.fromWithUnfold(10).take(4).toList() == List(10, 11, 12, 13))
    assert(Stream.fibsWithUnfold().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("map, take, takeWhile, zip, zipAll with unfold") {
    val s = Stream(1, 2, 3, 4)
    assert(s.takeWithUnfold(3).toList() == List(1, 2, 3))
    assert(s.takeWhileWithUnfold(_ <= 3).toList() == List(1, 2, 3))
    assert(s.mapWithUnfold(_ * 2).toList() == List(2, 4, 6, 8))
    assert(s.zip(Stream("a", "b", "c", "d")).toList() == List((1, "a"), (2, "b"), (3, "c"), (4, "d")))
    assert(s.zipAll(Stream("a", "b")).toList() == List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), None), (Some(4), None)))
  }

  test("startsWith") {
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)) == true)
    assert(Stream(1, 2, 3).startsWith(Stream(1, 3)) == false)
  }

  test("tails") {
    assert(Stream(1, 2, 3).tails().toList().map(_.toList()) == List(List(1, 2, 3), List(2, 3), List(3), List.empty))
  }

  test("scanRight") {
    assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList() == List(6, 5, 3, 0))
  }
}
